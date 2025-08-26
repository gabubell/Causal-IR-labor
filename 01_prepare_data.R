# ===========================================================================
# SCRIPT 01: PREPARAÇÃO E FEATURE ENGINEERING DOS DADOS
# ===========================================================================

# --- 0. Limpeza do Ambiente e Carregamento de Pacotes ---
rm(list = ls()); gc()
source("00_config.R")
library(dplyr)

# --- Passo 1: Carregar Dados Brutos (já em formato long) ---
cat("Carregando dados brutos:", RAW_DATA_PATH, "\n")
raw_dt <- readRDS(RAW_DATA_PATH)
setDT(raw_dt)
ir_limits_dt <- copy(HISTORICAL_IR_LIMITS_DT)

setnames(raw_dt, old = c("ano", "mes"), new = c("year", "month"))

# --- Preenchimento de Dados Ausentes ---
locf_robust <- function(x) { 
  if (all(is.na(x)) || length(x) < 2) return(x)
  for (i in 2:length(x)) if (is.na(x[i])) x[i] <- x[i-1]
  return(x) 
}
setorder(raw_dt, cpf, id_vinculo_original, data_referencia_mes) 
raw_dt[, `:=` (municipio = locf_robust(municipio), uf = locf_robust(uf)), by = .(cpf, id_vinculo_original)]
setorder(raw_dt, cpf, id_vinculo_original, -data_referencia_mes) 
raw_dt[, `:=` (municipio = locf_robust(municipio), uf = locf_robust(uf)), by = .(cpf, id_vinculo_original)]
setorder(raw_dt, cpf, data_referencia_mes)
cat("    - Preenchimento concluído.\n")


# --- Filtro com dplyr ---
if (exists("MUNICIPIO_TO_FILTER") && !is.null(MUNICIPIO_TO_FILTER) && length(MUNICIPIO_TO_FILTER) > 0) {
  cpfs_to_keep <- raw_dt %>%
    filter(
      year == 2015,
      month == 12,
      as.character(municipio) == as.character(MUNICIPIO_TO_FILTER) 
    ) %>%
    distinct(cpf) %>%
    pull(cpf)
  
  if (length(cpfs_to_keep) > 0) {
    cat("    - Encontrados", length(cpfs_to_keep), "indivíduos únicos que atendem ao critério.\n")
    rows_before_filter <- nrow(raw_dt)
    raw_dt <- raw_dt %>%
      filter(cpf %in% cpfs_to_keep)
    
    cat("    - Filtro concluído.\n")
    cat("    - Observações antes do filtro:", format(rows_before_filter, big.mark = "."), "\n")
    cat("    - Observações restantes após filtro:", format(nrow(raw_dt), big.mark = "."), "\n")
  } else {
    cat("    - AVISO: Nenhum indivíduo na base de dados (já preenchida) atendeu ao critério de filtro.\n")
    raw_dt <- raw_dt[0, ] 
  }
} else {
  cat("  - Filtro de município não foi ativado.\n")
}

setDT(raw_dt)

# --- 2. Limpeza Inicial e Criação de Variáveis de Tempo ---
cat("ETAPA 2: Limpeza inicial e criação de variáveis de tempo...\n")
event_date_obj <- as.Date(paste0(EVENT_DATE_STR, "-01"))
raw_dt[, `:=`(
  data_referencia_mes = as.Date(paste(year, month, 1, sep = '-')),
  cpf = as.character(cpf),
  relative_time_month = interval(event_date_obj, as.Date(paste(year, month, 1, sep = '-'))) %/% months(1)
)]
setorder(raw_dt, cpf, data_referencia_mes)

# --- 3. Cálculo da Base Tributável e 'paid_ir' ---
cat("ETAPA 3: Calculando a dedução do INSS e a base de cálculo do IR...\n")

inss_table <- copy(INSS_TABLE_EXPANDED)

# 1. Obter o teto e a alíquota da faixa mais alta para cada período.
teto_info_dt <- inss_table[, .SD[which.max(salary_ceiling)], by = .(year, month)]
setnames(teto_info_dt, 
         old = c("salary_ceiling", "rate"), 
         new = c("teto_inss", "aliquota_teto_inss"))

# 2. Atribuir a alíquota correta para cada observação usando um non-equi join.
#    Este join encontra a faixa salarial correta para cada trabalhador.
raw_dt[inss_table, 
       on = .(year, month, 
              salario_vinculo_mes_bruto >= salary_floor, 
              salario_vinculo_mes_bruto <= salary_ceiling),
       aliquota_especifica := i.rate]

# 3. Juntar a informação do teto.
raw_dt[teto_info_dt, on = .(year, month), 
       `:=`(teto_inss = i.teto_inss, 
            aliquota_teto_inss = i.aliquota_teto_inss)]

# 4. Calcular a dedução do INSS
raw_dt[, inss_deduction := case_when(
  # Caso 1: Salário está acima do teto. A dedução é o teto * alíquota do teto.
  salario_vinculo_mes_bruto > teto_inss ~ teto_inss * aliquota_teto_inss,
  # Caso 2: Salário está dentro de uma faixa. Usa a alíquota específica encontrada.
  !is.na(aliquota_especifica) ~ salario_vinculo_mes_bruto * aliquota_especifica,
  # Caso 3 (Fallback): Se por algum motivo não encontrou faixa (não deveria acontecer), a dedução é zero.
  TRUE ~ 0
)]

# 5. Calcular a base de cálculo final do IR (salário bruto - dedução do INSS)
raw_dt[, taxable_salary := salario_vinculo_mes_bruto - inss_deduction]

# 6. Calcular a variável 'paid_ir' usando a base de cálculo correta
cat("   - Calculando 'paid_ir' com base no salário tributável...\n")
raw_dt[ir_limits_dt, 
       on = .(year, month >= month_start, month <= month_end), 
       current_exemption_limit := i.exemption_limit]
raw_dt[, paid_ir := fifelse(taxable_salary > current_exemption_limit, 1, 0, na = 0)]

# 7. Limpar as colunas auxiliares
raw_dt[, c("aliquota_especifica", "teto_inss", "aliquota_teto_inss", "current_exemption_limit") := NULL]

# --- 4. Criação da Variável de Variação Salarial para Pareamento ---
cat("ETAPA 4: Calculando 'total_relative_complement_salary'...\n")

salcontr_2014_dt <- raw_dt[year == 2014 & !is.na(salcontr), .(salcontr_2014 = last(salcontr)), by = .(cpf, id_vinculo_original)]
raw_dt[salcontr_2014_dt, on = .(cpf, id_vinculo_original), salcontr_2014 := i.salcontr_2014]

locf_robust <- function(x) {
  if (all(is.na(x)) || length(x) < 2) {
    return(x)
  }
  for (i in 2:length(x)) {
    if (is.na(x[i])) {
      x[i] <- x[i-1]
    }
  }
  return(x)
}

raw_dt[, salcontr_2014 := locf_robust(salcontr_2014), by = .(cpf, id_vinculo_original)]
raw_dt[, salcontr_total_2014 := sum(salcontr_2014, na.rm = TRUE), by = .(cpf, year, month)]
raw_dt[, total_relative_complement_salary := fifelse(salcontr_total_2014 > 0,
                                                     (salario_total_cpf_mes - salcontr_total_2014) / salcontr_total_2014,
                                                     NA_real_)]

# --- 5. Criação de Variáveis de Heterogeneidade e Controle ---
cat("ETAPA 5: Criando variáveis de heterogeneidade...\n")
raw_dt[, `:=`(grauinstr = locf_robust(grauinstr), sexo = locf_robust(sexo)), by = cpf]
raw_dt[, `:=`(grauinstr = as.integer(grauinstr), sexo = as.character(sexo))]

raw_dt[, `:=`(
  education_level_grouped = factor(case_when(
    grauinstr %in% 1:5 ~ "Fundamental_MenosCompleto", grauinstr %in% 6:7 ~ "Medio_IncompletoCompleto",
    grauinstr %in% 8:9 ~ "Superior_IncompletoCompleto", grauinstr %in% 10:11 ~ "PosGraduado", TRUE ~ "OutroIgnorado")),
  tenure_group = factor(case_when(
    is.na(tempempr) ~ "Desconhecido", tempempr < 12 ~ "Ate1Ano",
    tempempr <= 36 ~ "De1a3Anos", tempempr <= 60 ~ "De3a5Anos", TRUE ~ "Mais5Anos")),
  sex_factor = factor(case_when(sexo == "1" ~ "Masculino", sexo == "2" ~ "Feminino", TRUE ~ "Ignorado"))
)]
raw_dt[, cnae_secao := substr(sbclas20, 1, 1)]
raw_dt[, cnae_secao := as.factor(cnae_secao)]

# --- 6. Criação de Variáveis de Movimento de Emprego ---
cat("ETAPA 6: Criando variáveis de movimento de emprego...\n")
separation_ids <- c('10', '11', '20', '21', '22', '90', '70', '72', '79')
raw_dt[, `:=`(
  left_job = fifelse(causadesli %in% separation_ids | (demitido == 1 & (is.na(causadesli) | causadesli == '0')), 1, 0),
  asked_to_leave = fifelse(causadesli %in% c('20', '21', '90'), 1, 0),
  retired_termination = fifelse(causadesli %in% c('70', '72', '79'), 1, 0),
  retired_without = fifelse(causadesli %in% c('71', '78', '80'), 1, 0)
)]
vinculos_start_year <- unique(raw_dt[, .(cpf, id_vinculo_original, year)])
setorder(vinculos_start_year, cpf, id_vinculo_original, year)
vinculos_start_year[, is_start_year := is.na(shift(year)) | year != shift(year) + 1, by = .(cpf, id_vinculo_original)]
vinculos_start_year <- vinculos_start_year[is_start_year == TRUE & !is.na(id_vinculo_original)]
raw_dt[vinculos_start_year, on = .(cpf, id_vinculo_original, year), is_start_year := i.is_start_year]
raw_dt[, started_job_temp := 0]
raw_dt[is_start_year == TRUE, started_job_temp := fifelse(month == min(month), 1, 0), by = .(cpf, id_vinculo_original, year)]
raw_dt[, min_data_in_panel := min(data_referencia_mes), by = .(cpf, id_vinculo_original)]
raw_dt[data_referencia_mes == min_data_in_panel & dtadmissao < data_referencia_mes, started_job_temp := 0]
raw_dt[, started_job := fifelse(is.na(started_job_temp), 0, started_job_temp)]
raw_dt[, c("is_start_year", "started_job_temp", "min_data_in_panel") := NULL]
rm(vinculos_start_year); gc()
raw_dt[, left_in_window := frollsum(left_job, n = 3, align = "center", fill = 0) > 0, by = cpf]
raw_dt[, changed_job := fifelse(started_job == 1 & left_in_window == TRUE, 1, 0)]
raw_dt[, left_in_window := NULL]

# --- 7. Desemprego Acumulado e Saída Permanente ---
cat("ETAPA 7: Calculando desemprego e saída permanente...\n")
raw_dt[, is_employed := !is.na(id_vinculo_original)]
raw_dt[, spell_id := rleid(is_employed), by = cpf]
raw_dt[, unemployed_at_panel_end := !last(is_employed), by = cpf]
raw_dt[, is_last_spell := spell_id == max(spell_id), by = cpf]
raw_dt[, is_permanent_exit_spell := is_last_spell & unemployed_at_panel_end]
raw_dt[, permanent_exit := fifelse(is_permanent_exit_spell & rowid(is_permanent_exit_spell) == 1, 1, 0), by = cpf]

raw_dt[, cumulative_unemployment := fifelse(!is_employed & !is_permanent_exit_spell, rowid(spell_id), 0), by = cpf]
raw_dt[, cumulative_unemployment_2 := fifelse(!is_employed, rowid(spell_id), 0), by = cpf]
raw_dt[, c("is_employed", "spell_id", "unemployed_at_panel_end", "is_last_spell", "is_permanent_exit_spell") := NULL]

# --- 8. Mudança de CBO ---
cat("ETAPA 8: Detectando mudanças de CBO...\n")
december_data <- raw_dt[month == 12, .(cpf, year, id_vinculo_original, ocup2002)]
setorder(december_data, cpf, id_vinculo_original, year)
december_data[, cbo_change_in_company := fifelse(year == shift(year) + 1 & ocup2002 != shift(ocup2002), 1, 0, na=0), by = .(cpf, id_vinculo_original)]
raw_dt[december_data, on=.(cpf, year, id_vinculo_original), cbo_change_in_company := i.cbo_change_in_company]
raw_dt[month != 12, cbo_change_in_company := NA_integer_]
rm(december_data)

monthly_summary <- raw_dt[!is.na(ocup2002), .(cbo_set_string = paste0(";", paste(sort(unique(ocup2002)), collapse = ";"), ";")), by = .(cpf, data_referencia_mes)]
raw_dt[monthly_summary, on=.(cpf, data_referencia_mes), cbo_set_string := i.cbo_set_string]
rm(monthly_summary)
raw_dt[, cbo_set_string := locf_robust(cbo_set_string), by = cpf]
raw_dt[, previous_cbo_set := shift(cbo_set_string), by = cpf]
raw_dt[, cbo_to_check := fifelse(!is.na(ocup2002), paste0(";", ocup2002, ";"), NA_character_)]

# === SUBSTITUIÇÃO DE FCASE POR CASE_WHEN ===
raw_dt[, cbo_change_at_start := case_when(
  started_job == 1 & is.na(previous_cbo_set) ~ 0,
  started_job == 1 & !grepl(cbo_to_check, previous_cbo_set, fixed = TRUE) ~ 1,
  TRUE ~ 0
)]
raw_dt[, c("cbo_set_string", "previous_cbo_set", "cbo_to_check") := NULL]
gc()

# ETAPA 8b: Detectando mudanças para famílias específicas de CBO
#
cat("  - Detectando mudanças direcionadas de CBO...\n")

# 1. Criar uma variável auxiliar com o primeiro dígito da CBO do emprego atual.
#    A CBO (ocup2002) é um número, então primeiro convertemos para texto.
#    substr() extrai uma parte do texto. Aqui, do primeiro ao primeiro caractere.
raw_dt[, cbo_first_digit := substr(as.character(ocup2002), 1, 1)]

# 2. Criar as novas variáveis de evento usando case_when.
#    A lógica é: o evento só pode acontecer se 'started_job' for 1 (começou um novo emprego).
#    E, para ser rigoroso, verificamos se a CBO nova é diferente das CBOs antigas
#    (usando a lógica de 'cbo_change_at_start' que você já criou).
raw_dt[, `:=`(
  cbo_to_2 = case_when(
    started_job == 1 & cbo_change_at_start == 1 & cbo_first_digit == '2' ~ 1,
    started_job == 1 ~ 0, # Se começou um emprego mas não atende à condição, é 0.
    TRUE ~ 0 # Em todos os outros casos, é 0.
  ),
  cbo_to_4 = case_when(
    started_job == 1 & cbo_change_at_start == 1 & cbo_first_digit == '4' ~ 1,
    started_job == 1 ~ 0,
    TRUE ~ 0
  ),
  cbo_not_3 = case_when(
    # A condição aqui é se o primeiro dígito está no conjunto ('2', '3').
    started_job == 1 & cbo_change_at_start == 1 & cbo_first_digit %in% c('2', '4') ~ 1,
    started_job == 1 ~ 0,
    TRUE ~ 0
  )
)]

# 3. Limpar a variável auxiliar
raw_dt[, cbo_first_digit := NULL]

# ======================================================================================

# Limpeza final das variáveis auxiliares originais
raw_dt[, c("cbo_set_string", "previous_cbo_set", "cbo_to_check") := NULL]
gc()

# --- 9. Variações Salariais Anuais ---
cat("ETAPA 9: Calculando variações salariais anuais...\n")
raw_dt[, lag_sal_vinc := shift(salario_vinculo_mes_bruto, n = 12, type = "lag"), by = .(cpf, id_vinculo_original)]
raw_dt[, var_sal := fifelse(lag_sal_vinc > 0, (salario_vinculo_mes_bruto / lag_sal_vinc) - 1, NA_real_)]
raw_dt[, lag_sal_tot := shift(salario_total_cpf_mes, n = 12, type = "lag"), by = cpf]
raw_dt[, var_sal_tot := fifelse(lag_sal_tot > 0, (salario_total_cpf_mes / lag_sal_tot) - 1, NA_real_)]
december_contract <- raw_dt[month == 12 & !is.na(id_vinculo_original), .(cpf, year, id_vinculo_original, salcontr)]
setorder(december_contract, cpf, id_vinculo_original, year)
december_contract[, lag_contract_sal := shift(salcontr), by = .(cpf, id_vinculo_original)]
december_contract[, var_contract := fifelse(lag_contract_sal > 0, (salcontr / lag_contract_sal) - 1, NA_real_)]
raw_dt[december_contract, on=.(cpf, year, id_vinculo_original), var_contract := i.var_contract]
raw_dt[, c("lag_sal_vinc", "lag_sal_tot") := NULL]

vars_to_accumulate <- c("paid_ir", "started_job", "left_job", "asked_to_leave", "changed_job",
                        "cbo_change_in_company", "cbo_change_at_start", "retired_termination",
                        "retired_without", 'permanent_exit', "cbo_to_2", "cbo_to_4", "cbot_not_3")

for (col in vars_to_accumulate) {
  if (any(is.na(raw_dt[[col]]))) {
    set(raw_dt, which(is.na(raw_dt[[col]])), col, 0)
  }
}
new_cumulative_names <- paste0("has_", vars_to_accumulate)

# # --- 10. Criação de Variáveis Cumulativas Contínuas ---

cat("ETAPA 10: Criando variáveis cumulativas contínuas...\n")
raw_dt[, (new_cumulative_names) := lapply(.SD, function(x) as.integer(cumsum(x) > 0)),
       by = cpf,
       .SDcols = vars_to_accumulate]

# --- 10. Criação de Variáveis Cumulativas "Dentro do Ano" ---
# A soma cumulativa agora é calculada DENTRO de cada grupo de CPF-ANO.
# Isso faz com que o contador "resete" para zero no início de cada novo ano.

# cat("ETAPA 10: Criando variáveis cumulativas anuais (com 'reset' a cada ano)...\n")
# raw_dt[, (new_cumulative_names) := lapply(.SD, function(x) as.integer(cumsum(x) > 0)),
#        by = .(cpf, year),  # <-- 'year' foi adicionado aqui
#        .SDcols = vars_to_accumulate]


# --- 11. Limpeza Final e Salvamento ---
cat("ETAPA 11: Limpeza final e salvamento dos dados preparados...\n")
raw_dt[, cbo_5digits := substr(ocup2002, 1, 5)]
raw_dt[, num_empregos_cpf_mes := .N, by = .(cpf, data_referencia_mes)]
cat("  - Variável 'num_empregos_cpf_mes' criada.\n")

cols_to_keep <- c(
  "cpf", "id_vinculo_original", "year", "month", "data_referencia_mes",
  "dtadmissao", "data_desligamento_efetiva","relative_time_month",
  "paid_ir", "taxable_salary", "total_relative_complement_salary", "salcontr_total_2014",
  "salario_total_cpf_mes", "salario_vinculo_mes_bruto", "salcontr",
  "left_job", "started_job", "changed_job", "asked_to_leave", "retired_termination", "retired_without",
  "cbo_change_in_company", "cbo_to_2", "cbo_to_4", "cbo_not_3",  "has_cbo_to_2", "has_cbo_to_4", "has_cbo_not_3",
  "cbo_change_at_start", "cumulative_unemployment", 
  "permanent_exit", "cumulative_unemployment_2",
  "var_sal", "var_sal_tot", "var_contract",
  "has_paid_ir", "has_started_job", "has_left_job", "has_changed_job", "has_asked_to_leave",
  "has_retired_termination", "has_retired_without", "has_cbo_change_in_company", "has_cbo_change_at_start",
  "num_empregos_cpf_mes", "municipio", 'uf', "cbo_5digits", "cnae_secao", 
  "education_level_grouped", "tenure_group", "sex_factor",
  "horascontr", "tempempr", "raca_cor",  'tipoadm', 'has_permanent_exit'
)

cols_to_keep_existing <- intersect(cols_to_keep, names(raw_dt))
prepared_data_final <- raw_dt[, ..cols_to_keep_existing]

if (!dir.exists(OUTPUT_DIR)) {
  dir.create(OUTPUT_DIR, recursive = TRUE)
}
saveRDS(prepared_data_final, file = PREPARED_DATA_PATH)

cat("Dados preparados salvos com sucesso em:", PREPARED_DATA_PATH, "\n")
cat("Total de linhas:", nrow(prepared_data_final), "| Total de CPFs únicos:", uniqueN(prepared_data_final$cpf), "\n")
gc()