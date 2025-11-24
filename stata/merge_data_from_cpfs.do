version 14.2
clear all
set more off
set maxvar 10000

cd "/dados01/gbelle/IR"

// --- Parâmetros ---
local input_cpf_csv = "cpfs_enfermagem_renda_filtrada.csv" // CSV com CPFs
local all_years = "2014 2015 2016"
local regions = "sul sudeste sp norte nordeste centrooeste" // Ajuste conforme necessário
local var_municipio_rais = "municipio" // <<<<<<<<<<<<<<< NOME DA VARIÁVEL DE MUNICÍPIO NOS ARQUIVOS RAIS .DTA

display "=== PARTE 1: Lendo CPFs Amostrados do CSV ==="

// Verifica se o arquivo CSV de entrada existe
capture confirm file "`input_cpf_csv'"
if _rc != 0 {
    display as error "ERRO FATAL: Arquivo de CPFs amostrados (`input_cpf_csv') nao encontrado em `c(pwd)'."
    exit 601
}

// Importa o CSV com os CPFs
import delimited using "`input_cpf_csv'", clear varname(1) // Assume cabeçalho 'cpf'
if _rc != 0 {
    display as error "ERRO FATAL: Falha ao importar o arquivo CSV `input_cpf_csv'."
    exit _rc
}

// Verifica se a variável cpf existe e remove duplicatas (por segurança)
capture confirm variable cpf
if _rc != 0 {
     display as error "ERRO FATAL: Coluna 'cpf' nao encontrada no arquivo CSV `input_cpf_csv'."
     exit 111
}
duplicates drop cpf, force
local n_sampled_cpfs = _N
display "  `n_sampled_cpfs' CPFs unicos lidos de `input_cpf_csv'."
if `n_sampled_cpfs' == 0 {
    display as error "ERRO FATAL: Nenhum CPF lido do arquivo CSV."
    exit 198
}

// Prepara para merge
sort cpf
gen _insample = 1
tempfile sampled_cpfs_dta // Cria nome para arquivo .dta temporário
save `"`sampled_cpfs_dta'"', replace // Salva os CPFs em formato .dta para merge eficiente
display "  Arquivo temporario com CPFs amostrados preparado para merge."
clear // Limpa a memória

display "=== PARTE 2: Processando Arquivos e Gerando CSVs Individuais ==="

// Loop pelos anos e regiões
foreach year of local all_years {
    local data_dir "/dados01/RAIS/`year'"
    display "--- Processando Ano: `year' ---"
    foreach region of local regions {
        local dta_filename = "`region'`year'.dta"
        local fullpath = "`data_dir'/`dta_filename'"
        local output_csv = "`region'`year'.csv" // Nome do CSV de saída

        display "  Processando: `dta_filename' -> `output_csv'"

        // Verifica se o arquivo .dta existe
        capture confirm file `"`fullpath'"'
        if _rc != 0 {
            display as error "    Aviso: Arquivo `dta_filename' nao encontrado. Pulando."
            continue // Pula para a próxima região/ano
        }

        // Bloco para carregar, filtrar e exportar, garantindo limpeza
        preserve

        // Tenta carregar o arquivo regional inteiro
        capture use `"`fullpath'"', clear
        if _rc != 0 {
             display as error "    Erro `_rc' ao carregar `dta_filename'. Pulando."
             restore // Restaura o estado anterior (memória limpa)
             continue // Pula para a próxima região/ano
        }

        // Verifica se 'cpf' e a variável de município existem no arquivo carregado
        foreach var_check of varlist cpf `var_municipio_rais' {
            capture confirm variable `var_check'
            if _rc != 0 {
                display as error "    Erro: Variavel '`var_check'' (cpf ou municipio) nao encontrada em `dta_filename'. Pulando."
                restore
                continue 2 // Pula para a próxima iteração do loop de região, se uma das vars estiver faltando
            }
        }

        // Faz merge com os CPFs amostrados
        // (A consistência do tipo de 'cpf' entre sampled_cpfs_dta e o dta_filename é crucial aqui)
        sort cpf // Garante que o arquivo RAIS está ordenado por cpf para o merge
        merge m:1 cpf using `"`sampled_cpfs_dta'"', keepusing(_insample) nogen update replace 
        // A opção 'update replace' não é estritamente necessária aqui, já que só estamos usando _insample
        // Apenas 'nogen' seria suficiente, mas 'update replace' não prejudica se você não estiver atualizando variáveis.
        
        keep if _insample == 1 // Mantem apenas os registros dos CPFs da amostra
        drop _insample // Remove a variável auxiliar do merge

        // *** NOVO FILTRO: Remover linhas onde o município está vazio/missing ***
        // Este filtro é aplicado DEPOIS de selecionar os CPFs da sua lista.
        if _N > 0 { // Só aplicar se houver alguma linha restante
            // Se a variável de município (`var_municipio_rais`) for STRING:
            // replace `var_municipio_rais' = trim(`var_municipio_rais') // Remove espaços em branco ao redor
            // drop if `var_municipio_rais' == ""
            
            // Se a variável de município (`var_municipio_rais`) for NUMÉRICA (ex: código IBGE):
            drop if missing(`var_municipio_rais') // Para missing genérico . .a .b etc.
            // OU se você sabe que 0 ou outro valor específico significa "vazio":
            // drop if `var_municipio_rais' == 0 | `var_municipio_rais' == . // Exemplo
            
            display "    Registros apos filtro de municipio: `=_N'"
        }
        // *** FIM DO NOVO FILTRO ***

        local rows_found = _N
        if `rows_found' > 0 {
            display "    `rows_found' registros encontrados e com municipio preenchido. Exportando para `output_csv'..."
            // Exporta os dados filtrados (com todas as colunas originais do arquivo) para o CSV específico
            export delimited using "`output_csv'", replace nolabel
            if _rc == 0 {
                 display "      Sucesso: `output_csv' salvo."
            }
            else {
                 display as error "      ERRO `_rc' ao salvar `output_csv'."
            }
        }
        else {
             display "    Nenhum registro encontrado para CPFs amostrados (ou municipio vazio) em `dta_filename'. Nenhum CSV gerado."
        }

        // Restaura o estado anterior (memória limpa) antes de ir para o próximo arquivo
        restore

    } // fim loop region
} // fim loop year

display "=== Processamento Concluído ==="

exit, clear // Termina o script e limpa a memória