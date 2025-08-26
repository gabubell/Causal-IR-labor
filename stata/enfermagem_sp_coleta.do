version 14.2
clear all
set more off
set maxvar 10000

// --- Diretórios ---
cd "/dados01/gbelle/IR" // Diretório de trabalho e saída dos CSVs
local rais_data_dir "/dados01/RAIS" // Diretório base dos dados RAIS anuais

// --- Parâmetros ---
local all_years = "2014 2015 2016 2017" // Anos para processar e extrair dados
local reference_year = "2015"         // Ano base para identificar os CPFs
local region_prefix = "sp"            // Prefixo para os arquivos de São Paulo

// Variável CBO nos seus arquivos .dta (AJUSTE SE NECESSÁRIO)
local cbo_varname = "ocup2002"

// --- ETAPA 1: Identificar CPFs de Referência (Enfermagem SP, Ativos em Dez/2015) ---
display "=== ETAPA 1: Identificando CPFs de Referencia (Enfermagem SP, Ativos em Dez/`reference_year') ==="

local ref_dta_filename = "`region_prefix'`reference_year'.dta"
local ref_fullpath = "`rais_data_dir'/`reference_year'/`ref_dta_filename'"

capture confirm file "`ref_fullpath'"
if _rc != 0 {
    display as error "ERRO FATAL: Arquivo de referencia `ref_dta_filename' nao encontrado em `ref_fullpath'."
    exit 601
}

preserve
use "`ref_fullpath'", clear
if _rc != 0 {
    display as error "ERRO FATAL: Falha ao carregar o arquivo de referencia `ref_dta_filename'."
    restore
    exit _rc
}

// Verificar variáveis essenciais para a Etapa 1
foreach var of varlist cpf `cbo_varname' mesdeslig {
    capture confirm variable `var'
    if _rc != 0 {
        display as error "ERRO FATAL: Variavel '`var'' nao encontrada em `ref_dta_filename'."
        restore
        exit 111
    }
}

// Filtrar pelos CBOs de Enfermagem
gen byte is_enfermagem_ref_year = 0

// Técnicos de Enfermagem
replace is_enfermagem_ref_year = 1 if inlist(`cbo_varname', ///
    "322205", "322210", "322215", "322220", "322225", "322245", "3222E1", "3222E3", "3222B3")

// Auxiliares de Enfermagem (adiciona à seleção anterior, se não já selecionado)
replace is_enfermagem_ref_year = 1 if inlist(`cbo_varname', ///
    "322230", "322235", "322240", "322250", "3222E2")

keep if is_enfermagem_ref_year == 1
drop is_enfermagem_ref_year

// Filtrar por "empregado em dezembro de 2015"
// Assumindo 'mesdeslig' == 0 (numérico) significa ativo em 31/12.
// Se for string "00", use: keep if mesdeslig == "00"
// Se tiver 'vinculo_ativo_3112' (1 se ativo), use: keep if vinculo_ativo_3112 == 1
// **AJUSTE ESTA CONDIÇÃO CONFORME SEUS DADOS RAIS**
keep if mesdeslig == 0

// Manter apenas CPFs únicos para a lista de referência
keep cpf
duplicates drop cpf, force

local n_target_cpfs = _N
display "  `n_target_cpfs' CPFs unicos de referencia (Enfermagem SP, ativos Dez/`reference_year') identificados."
if `n_target_cpfs' == 0 {
    display as error "ERRO FATAL: Nenhum CPF de referencia encontrado com os criterios. Verifique os filtros da Etapa 1."
    restore
    exit 198
}

gen byte _is_target_cpf = 1
sort cpf
tempfile target_cpfs_dta
save "`target_cpfs_dta'", replace
display "  Arquivo temporario com CPFs de referencia (`target_cpfs_dta') preparado."
restore

// --- ETAPA 2: Coletando TODOS os Vínculos dos CPFs de Referência nos Anos Selecionados ---
display "=== ETAPA 2: Coletando TODOS os vinculos dos CPFs de Referencia ==="

foreach year of local all_years {
    display "--- Processando Ano: `year' ---"
    local dta_filename = "`region_prefix'`year'.dta"
    local fullpath = "`rais_data_dir'/`year'/`dta_filename'"
    // Nome do CSV de saída mais descritivo para refletir o conteúdo
    local output_csv = "`region_prefix'_cpfs_ref_enf`reference_year`_todos_vinculos_`year'.csv"

    capture confirm file "`fullpath'"
    if _rc != 0 {
        display as error "    Aviso: Arquivo `dta_filename' nao encontrado em `fullpath'. Pulando ano `year'."
        continue
    }

    preserve
    capture use "`fullpath'", clear
    if _rc != 0 {
         display as error "    Erro `_rc' ao carregar `dta_filename'. Pulando ano `year'."
         restore
         continue
    }

    // Verificar variáveis essenciais para a Etapa 2 (cpf para merge, cbo_varname para info)
    foreach var of varlist cpf `cbo_varname' {
        capture confirm variable `var'
        if _rc != 0 {
            display as error "    Erro: Variavel '`var'' nao encontrada em `dta_filename'. Pulando ano `year'."
            restore
            continue 2
        }
    }

    // Opcional: Criar variável para identificar se o CBO NO ANO CORRENTE é de enfermagem
    // Esta variável é apenas informativa, não será usada para filtrar aqui.
    gen str30 role_enfermagem_ano_corrente = ""
    replace role_enfermagem_ano_corrente = "Técnico de Enfermagem" if inlist(`cbo_varname', ///
        "322205", "322210", "322215", "322220", "322225", "322245", "3222E1", "3222E3", "3222B3")
    replace role_enfermagem_ano_corrente = "Auxiliar de Enfermagem" if inlist(`cbo_varname', ///
        "322230", "322235", "322240", "322250", "3222E2")

    // Faz merge com os CPFs de referência.
    // Manterá todos os vínculos (linhas) do arquivo do ano corrente (`master`)
    // para os CPFs que estão na lista de `target_cpfs_dta`.
    sort cpf // Garante que o arquivo master (RAIS do ano) esteja ordenado por CPF
    merge m:1 cpf using "`target_cpfs_dta'", keepusing(_is_target_cpf) nogen update replace
    
    keep if _is_target_cpf == 1 // Mantem todos os registros (vínculos) dos CPFs da lista de referência
    drop _is_target_cpf         // Remove a variável auxiliar do merge

    local rows_found = _N
    if `rows_found' > 0 {
        display "    `rows_found' vinculos encontrados para os CPFs de referencia. Exportando para `output_csv'..."
        // Exporta os dados (todas as colunas originais + role_enfermagem_ano_corrente)
        export delimited using "`output_csv'", replace nolabel
        if _rc == 0 {
             display "      Sucesso: `output_csv' salvo."
        }
        else {
             display as error "      ERRO `_rc' ao salvar `output_csv'."
        }
    }
    else {
         display "    Nenhum vinculo encontrado para os CPFs de referencia em `dta_filename'."
    }
    restore
} // fim loop year

display "=== Processamento Concluído ==="
exit, clear