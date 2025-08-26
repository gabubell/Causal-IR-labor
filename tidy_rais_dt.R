# =================================================================================
# SCRIPT DE PREPARAÇÃO DE DADOS RAIS (VERSÃO DATA.TABLE)
# =================================================================================

# --- 0. PACOTES E CONFIGURAÇÃO ---
cat("INICIANDO SCRIPT DE PREPARAÇÃO\n")
cat("=========================================================\n")
# Para servidor:
# tryCatch({
#   setwd("/dados01/gbelle/IR/")
#   cat("Diretório de trabalho definido para:", getwd(), "\n")
#   library_path <- "/dados01/gbelle/R_lib"
#   library(data.table, lib.loc = library_path)
#   library(lubridate, lib.loc = library_path)
# }, error = function(e) { stop("ERRO FATAL: Falha ao carregar pacotes. Erro: ", e$message) })
# 
library(data.table)
library(lubridate)

# --- 1. CARREGAR BASE ---
cat("\nETAPA 1: Carregando arquivos CSV...\n")
# Disponível no gdrive em: data/IR/v2/input/enfermagem_sp/
data_path <- '/Users/macbook/Dados/rais_identificada/enfermagem_sp/'
arquivos_csv <- list.files(data_path, pattern = "\\.csv$", full.names = TRUE); stopifnot("Nenhum arquivo CSV encontrado!" = length(arquivos_csv) > 0)
lista_dt <- lapply(arquivos_csv, function(arquivo) { fread(arquivo, colClasses = "character", encoding = "Latin-1", na.strings = c("", "NA", "NULL", "000000"), showProgress = TRUE) })
rais_completa_dt <- rbindlist(lista_dt, idcol = "origem_arquivo", use.names = TRUE, fill = TRUE); rm(lista_dt, arquivos_csv); gc()
rais_completa_dt <- rais_completa_dt[!is.na(municipio)]; rais_completa_dt <- rais_completa_dt[tiposal == '1']; cat("   - Carga concluída. Linhas após filtros de qualidade:", nrow(rais_completa_dt), "\n")

# --- 2. LIMPEZA INICIAL E PREPARAÇÃO DE VARIÁVEIS CHAVE ---
cat("\nETAPA 2: Limpeza inicial e criação de variáveis chave...\n")
rais_limpa <- rais_completa_dt[!is.na(cpf) & !is.na(radiccnpj) & !is.na(dtadmissao)]
rais_limpa[, chave_vinculo_str := paste(cpf, radiccnpj, sep = "_")]
contract_ids <- unique(rais_limpa[, .(chave_vinculo_str)])
contract_ids[, id_vinculo_original := .I]
rais_limpa <- rais_limpa[contract_ids, on = "chave_vinculo_str"]
rais_limpa[, chave_vinculo_str := NULL]
cols_vlremmes <- names(rais_limpa)[startsWith(names(rais_limpa), "vlrem")]
cols_numericas <- c(cols_vlremmes, "salcontr", "remdezr", "remmedr", "qtdiasafas", "horascontr", "age")
rais_limpa[, `:=`(dtadmissao = dmy(dtadmissao, quiet = TRUE), year = as.integer(year), mesdeslig = as.integer(fifelse(mesdeslig == "0", NA_character_, mesdeslig)), diadesl = as.integer(fifelse(diadesl == "0", NA_character_, diadesl)))]
rais_limpa[, (cols_numericas) := lapply(.SD, as.numeric), .SDcols = cols_numericas]
rais_limpa[, data_desligamento_efetiva := fifelse(is.na(mesdeslig), as.Date(NA), fifelse(!is.na(diadesl), dmy(paste(diadesl, mesdeslig, year), quiet=TRUE), ceiling_date(make_date(year, mesdeslig, 1), "month") - days(1)))]
rais_limpa[!is.na(data_desligamento_efetiva) & data_desligamento_efetiva < dtadmissao, data_desligamento_efetiva := as.Date(NA)]
rais_limpa[, `:=`(vinculo_teve_algum_afastamento = fifelse(!is.na(qtdiasafas) & qtdiasafas > 0, 1, 0), qtd_vlrem_zero = rowSums(.SD == 0, na.rm = TRUE)), .SDcols = patterns("^vlrem")]
setnames(rais_limpa, "year", "ano_rais"); rais_limpa[, vinculos_na_empresa_ano := .N, by = .(cpf, id_vinculo_original, ano_rais)]
cpfs_ok <- rais_limpa[, .(ok = all(vinculos_na_empresa_ano <= 1)), by = cpf][ok == TRUE, .(cpf)]
rais_limpa <- rais_limpa[cpfs_ok, on = "cpf"]; rm(rais_completa_dt, contract_ids, cpfs_ok)
gc()

# --- 3. CORREÇÃO DOS VALORES DE REMUNERAÇÃO ---
cat("\nETAPA 3: Corrigindo valores de remuneração...\n")
# Aqui, há dados na RAIS em que os valores para novembro tiveram algum problema no servidor. 
param_semelhanca_min <- 0.25
param_semelhanca_max <- 2.5
cols_a_corrigir <- c(names(rais_limpa)[startsWith(names(rais_limpa), "vlrem")], "salcontr", "remdezr")

for(col in cols_a_corrigir) {
  rais_limpa[get(col) > 0 & get(col) < 100 & (get(col) * 100) > (param_semelhanca_min * remmedr) & (get(col) * 100) < (param_semelhanca_max * remmedr),
             (col) := get(col) * 100] 
}

# Para cada vínculo-cpf, preenchemos o valor de novembro utilizando a média da renda no ano e o salário nos demais meses
# É um encadeamento de if-else para considerar se: 
# a) o funcionário estava ativo o ano todo
# b) Desligamento em dezembro, mas teve algum salário
# c) Desligamento em dezembro, sem salário em dezembro
# d) Desligamento em novembro
# e) Então é nulo

rais_limpa[, vlremnovembro := fifelse(
  is.na(vlremnovembro) & is.na(mesdeslig), 
  ((remmedr*(12-qtd_vlrem_zero))-(vlremjaneiro+vlremfevereiro+vlremmarco+vlremabril+vlremmaio+vlremjunho+vlremjulho+vlremagosto+vlremsetembro+vlremoutubro+remdezr)),
  fifelse(
    is.na(vlremnovembro)&mesdeslig==12&remdezr!=0, 
    ((remmedr*(12-qtd_vlrem_zero))-(vlremjaneiro+vlremfevereiro+vlremmarco+vlremabril+vlremmaio+vlremjunho+vlremjulho+vlremagosto+vlremsetembro+vlremoutubro+remdezr)),
    fifelse(
      is.na(vlremnovembro)&mesdeslig==12&remdezr==0,
      ((remmedr*(11-qtd_vlrem_zero))-(vlremjaneiro+vlremfevereiro+vlremmarco+vlremabril+vlremmaio+vlremjunho+vlremjulho+vlremagosto+vlremsetembro+vlremoutubro)),
      fifelse(is.na(vlremnovembro)&mesdeslig==11,
              ((remmedr*(11-qtd_vlrem_zero))-(vlremjaneiro+vlremfevereiro+vlremmarco+vlremabril+vlremmaio+vlremjunho+vlremjulho+vlremagosto+vlremsetembro+vlremoutubro)),
              vlremnovembro)
      )
    )
  )
]

gc()

# --- 4. TRANSFORMAR DE WIDE PARA LONG (VERSÃO FINAL E CORRETA) ---
cat("\nETAPA 4: Transformando de formato wide para long...\n")

# a) Define explicitamente as 12 colunas de remuneração que devem ser "derretidas".
cols_to_melt <- grep("^vlrem|^remdezr$", names(rais_limpa), value = TRUE)

# Confirma que encontramos as 12 colunas
if (length(cols_to_melt) != 12) {
  warning("AVISO: Não foram encontradas exatamente 12 colunas de remuneração para pivotar. Verifique os nomes das colunas.")
}

# b) Executa o melt, especificando 'measure.vars'. 
# data.table irá tratar todas as outras colunas automaticamente como 'id.vars'.
vinculo_salarios <- melt(rais_limpa,
                         measure.vars = cols_to_melt,
                         variable.name = "nome_mes_original",
                         value.name = "salario_vinculo_mes_bruto",
                         na.rm = TRUE)

# c) Processamento final (criação de mês e data)
map_mes <- data.table(
  nome_mes_original = c("vlremjaneiro", "vlremfevereiro", "vlremmarco", "vlremabril", "vlremmaio", "vlremjunho", 
                        "vlremjulho", "vlremagosto", "vlremsetembro", "vlremoutubro", "vlremnovembro", "remdezr"),
  mes_numero = 1:12
)
vinculo_salarios <- map_mes[vinculo_salarios, on = "nome_mes_original"]
vinculo_salarios <- vinculo_salarios[!is.na(mes_numero)]
vinculo_salarios[, data_corrente_vinculo_mes := make_date(ano_rais, mes_numero, 1)]

# Limpeza de memória
rm(rais_limpa, map_mes)
gc()

# --- 5. FILTRAR MESES ATIVOS E CRIAR FLAGS FINAIS ---
cat("\nETAPA 5: Filtrando meses ativos e criando flags finais...\n")

#' Mantém apenas os meses que o vínculo estava ativo
#' Limpa e preenche dados de algumas colunas (eg muicipio de trabalho)
#' Cria 'flags' (T/F) para o status de cada mes (eg estava afastado, emprego ativo, mes demissao)
#' Remove registros vazios após a demissao

vinculo_salarios <- vinculo_salarios[!is.na(salario_vinculo_mes_bruto) & floor_date(dtadmissao, "month") <= data_corrente_vinculo_mes & (is.na(data_desligamento_efetiva) | ceiling_date(data_desligamento_efetiva, "month") - days(1) >= data_corrente_vinculo_mes)]
setorder(vinculo_salarios, cpf, id_vinculo_original, ano_rais, mes_numero)

vinculo_salarios[, `:=`(muntrab = {f <- factor(fifelse(muntrab == "0", NA_character_, muntrab))

if(length(levels(f)) > 0) levels(f)[nafill(nafill(as.integer(f), "locf"), "nocb")] else as.character(f)}, municipio = {f <- factor(fifelse(municipio == "0", NA_character_, municipio))
if(length(levels(f)) > 0) levels(f)[nafill(nafill(as.integer(f), "locf"), "nocb")] else as.character(f)}), by = .(cpf, id_vinculo_original)]

cols_afast <- names(vinculo_salarios)[startsWith(names(vinculo_salarios), "mesiniaf|mesfimaf")]
if (length(cols_afast) > 0) { 
  vinculo_salarios[, (cols_afast) := lapply(.SD, function(x) as.numeric(fifelse(x == "99", NA_character_, x))), .SDcols = cols_afast]
  vinculo_salarios[, afastado_neste_mes := (mes_numero >= mesiniaf1 & mes_numero <= mesfimaf1) | (mes_numero >= mesiniaf2 & mes_numero <= mesfimaf2) | (mes_numero >= mesiniaf3 & mes_numero <= mesfimaf3)] 
  } else {
    vinculo_salarios[, afastado_neste_mes := FALSE] 
    }

vinculo_salarios[, emprego_ativo := salario_vinculo_mes_bruto > 0 | afastado_neste_mes]; vinculo_salarios[is.na(emprego_ativo), emprego_ativo := FALSE]; vinculo_salarios[, emprego_ativo_futuro := rev(cummax(rev(emprego_ativo))), by = .(cpf, id_vinculo_original)]; vinculo_salarios[, demitido := fifelse(!is.na(mesdeslig) & mesdeslig == mes_numero, TRUE, fifelse(emprego_ativo_futuro == 1 & shift(emprego_ativo_futuro, type="lead") == 0 & data_corrente_vinculo_mes < max(data_corrente_vinculo_mes, na.rm=TRUE), TRUE, FALSE)), by=.(cpf, id_vinculo_original)]; demissoes <- vinculo_salarios[demitido == TRUE, .(data_demissao = min(data_corrente_vinculo_mes, na.rm=TRUE)), by = .(cpf, id_vinculo_original)]; vinculo_salarios <- demissoes[vinculo_salarios, on = .(cpf, id_vinculo_original)]; vinculo_salarios <- vinculo_salarios[is.na(data_demissao) | data_corrente_vinculo_mes <= data_demissao]; vinculo_salarios[, data_demissao := NULL]; gc()

# --- 6. CRIAR PAINEL FINAL BALANCEADO (VERSÃO CORRIGIDA E SIMPLIFICADA) ---
cat("\nETAPA 6: Criando painel final balanceado...\n")

# a) Sumário agregado por CPF-Mês
sumario_cpf_mes <- vinculo_salarios[, .(
  num_empregos_cpf_mes = uniqueN(id_vinculo_original),
  salario_total_cpf_mes = sum(salario_vinculo_mes_bruto, na.rm = TRUE)
), by = .(cpf, ano = ano_rais, mes = mes_numero)]

# b) Criar a grade de todos os CPF-Mês do período
data_inicio_painel <- make_date(2014, 12, 1)
data_fim_painel <- make_date(2017, 12, 1)
todos_os_meses <- seq(from = data_inicio_painel, to = data_fim_painel, by = "month")
lista_cpfs_unicos <- unique(c(vinculo_salarios$cpf, sumario_cpf_mes$cpf))

# Usando CJ (Cross Join) do data.table, equivalente a expand_grid
painel_mestre <- CJ(cpf = lista_cpfs_unicos, data_referencia_mes = todos_os_meses)
painel_mestre[, `:=` (ano = year(data_referencia_mes), mes = month(data_referencia_mes))]

# c) Juntar tudo em uma única etapa usando 'merge' explícito

# c.1) Junta o painel mestre com os dados de vínculo (um left join)
setnames(vinculo_salarios, c("ano_rais", "mes_numero"), c("ano", "mes"))
painel_final_dt <- merge(
  painel_mestre, 
  vinculo_salarios, 
  by = c("cpf", "ano", "mes"), 
  all.x = TRUE # Mantém todas as linhas do painel mestre (left join)
)

# c.2) Junta o resultado com o sumário (outro left join)
painel_final_dt <- merge(
  painel_final_dt,
  sumario_cpf_mes,
  by = c("cpf", "ano", "mes"),
  all.x = TRUE
)

# d) Limpeza final e ordenação
painel_final_dt[is.na(num_empregos_cpf_mes), num_empregos_cpf_mes := 0]
painel_final_dt[is.na(salario_total_cpf_mes), salario_total_cpf_mes := 0]
setcolorder(painel_final_dt, c("cpf", "ano", "mes", "data_referencia_mes", "id_vinculo_original"))
setkey(painel_final_dt, cpf, ano, mes, id_vinculo_original)

gc()

# --- 7. SALVAR O RESULTADO ---
cat("\nETAPA 7: Salvando o resultado...\n")
output_file <- 'painel_enfermagem_br.rds'
saveRDS(painel_final_dt, output_file)
cat("   - Painel final salvo como '", output_file, "'.\n")
cat("   - Dimensões finais:", paste(dim(painel_final_dt), collapse = " x "), "\n")
cat("\nSCRIPT CONCLUÍDO COM SUCESSO!\n")