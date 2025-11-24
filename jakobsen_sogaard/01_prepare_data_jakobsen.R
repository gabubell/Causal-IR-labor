# ===========================================================================
# SCRIPT 01 (Jakobsen & Sogaard): PREPARAÇÃO E FEATURE ENGINEERING
# ===========================================================================

# --- 0. Limpeza do Ambiente e Carregamento de Pacotes ---
rm(list = ls()); gc()
source("00_config.R")
library(dplyr)

# --- 1. Carregar Dados Brutos ---
cat("Carregando dados brutos:", RAW_DATA_PATH, "\n")
raw_dt <- readRDS(RAW_DATA_PATH)
setDT(raw_dt)
ir_limits_dt <- copy(HISTORICAL_IR_LIMITS_DT)
inss_table <- copy(INSS_TABLE_EXPANDED)

setnames(raw_dt, old = c("ano", "mes"), new = c("year", "month"))

# --- 2. Limpeza Inicial e Variáveis de Tempo ---
cat("ETAPA 2: Limpeza inicial e variáveis de tempo...\n")
raw_dt[, `:=`(
  data_referencia_mes = as.Date(paste(year, month, 1, sep = '-')),
  cpf = as.character(cpf)
)]
setorder(raw_dt, cpf, data_referencia_mes)

# --- 3. Cálculo da Base Tributável (taxable_salary) ---
cat("ETAPA 3: Calculando a dedução do INSS e a base de cálculo do IR...\n")

# 3.1. Obter o teto e a alíquota da faixa mais alta para cada período.
teto_info_dt <- inss_table[, .SD[which.max(salary_ceiling)], by = .(year, month)]
setnames(teto_info_dt,
         old = c("salary_ceiling", "rate"),
         new = c("teto_inss", "aliquota_teto_inss"))

# 3.2. Atribuir a alíquota correta para cada observação
raw_dt[inss_table,
       on = .(year, month,
              salario_vinculo_mes_bruto >= salary_floor,
              salario_vinculo_mes_bruto <= salary_ceiling),
       aliquota_especifica := i.rate]

# 3.3. Juntar a informação do teto.
raw_dt[teto_info_dt, on = .(year, month),
       `:=`(teto_inss = i.teto_inss,
            aliquota_teto_inss = i.aliquota_teto_inss)]

# 3.4. Calcular a dedução do INSS
raw_dt[, inss_deduction := case_when(
  salario_vinculo_mes_bruto > teto_inss ~ teto_inss * aliquota_teto_inss,
  !is.na(aliquota_especifica) ~ salario_vinculo_mes_bruto * aliquota_especifica,
  TRUE ~ 0
)]

# 3.5. Calcular a base de cálculo final do IR
raw_dt[, taxable_salary := salario_vinculo_mes_bruto - inss_deduction]

# 3.6. Limpar colunas auxiliares
raw_dt[, c("aliquota_especifica", "teto_inss", "aliquota_teto_inss") := NULL]
cat("   - 'taxable_salary' criado.\n")

# 3.7. Calcular 'paid_ir'
cat("   - Calculando 'paid_ir'...\n")
raw_dt[ir_limits_dt,
       on = .(year, month >= month_start, month <= month_end),
       current_exemption_limit := i.exemption_limit]
raw_dt[, paid_ir := fifelse(taxable_salary > current_exemption_limit, 1, 0, na = 0)]
raw_dt[, current_exemption_limit := NULL]


# --- 4. Ajuste das Horas Trabalhadas ---
cat("ETAPA 4: Calculando horas proporcionais aos dias trabalhados no mês...\n")

raw_dt[, `:=` (
  dtadmissao = as.Date(dtadmissao),
  data_desligamento_efetiva = as.Date(data_desligamento_efetiva)
)]

raw_dt[, `:=`(
  inicio_mes = floor_date(data_referencia_mes, "month"),
  fim_mes = ceiling_date(data_referencia_mes, "month") - days(1)
)]
raw_dt[, `:=`(
  data_inicio_efetiva = pmax(dtadmissao, inicio_mes, na.rm = TRUE),
  data_fim_efetiva = pmin(data_desligamento_efetiva, fim_mes, na.rm = TRUE)
)]
raw_dt[, dias_trabalhados_no_mes := as.numeric(data_fim_efetiva - data_inicio_efetiva) + 1]
raw_dt[, proporcao_mes := dias_trabalhados_no_mes / as.numeric(days_in_month(data_referencia_mes))]
raw_dt[, horas_ajustadas := horascontr * proporcao_mes]
raw_dt[, c("inicio_mes", "fim_mes", "data_inicio_efetiva", "data_fim_efetiva", "dias_trabalhados_no_mes", "proporcao_mes") := NULL]
cat("   - 'horas_ajustadas' criada.\n")


# --- 5. Seleção de Variáveis e Salvamento ---
cat("ETAPA 5: Selecionando variáveis de controle e salvando...\n")

# Preenchimento de dados ausentes para variáveis de controle
locf_robust <- function(x) {
  if (all(is.na(x)) || length(x) < 2) return(x)
  for (i in 2:length(x)) if (is.na(x[i])) x[i] <- x[i-1]
  return(x)
}
setorder(raw_dt, cpf, id_vinculo_original, data_referencia_mes)
raw_dt[, `:=` (municipio = locf_robust(municipio), uf = locf_robust(uf), grauinstr = locf_robust(grauinstr), sexo = locf_robust(sexo)), by = .(cpf, id_vinculo_original)]
setorder(raw_dt, cpf, id_vinculo_original, -data_referencia_mes)
raw_dt[, `:=` (municipio = locf_robust(municipio), uf = locf_robust(uf), grauinstr = locf_robust(grauinstr), sexo = locf_robust(sexo)), by = .(cpf, id_vinculo_original)]
setorder(raw_dt, cpf, data_referencia_mes)


cols_to_keep <- c(
  "cpf", "id_vinculo_original", "year", "month", "data_referencia_mes",
  "dtadmissao", "data_desligamento_efetiva",
  "taxable_salary", "salario_vinculo_mes_bruto", "salcontr", "paid_ir",
  "municipio", 'uf', "ocup2002", "sbclas20",
  "grauinstr", "sexo", "horascontr", "horas_ajustadas", "tempempr", "raca_cor", 'tipoadm'
)

# Garantir que todas as colunas existem
cols_to_keep_existing <- intersect(cols_to_keep, names(raw_dt))

prepared_data_jakobsen_link_level <- raw_dt[, ..cols_to_keep_existing]

# --- 6. Agregação para o Nível Indivíduo-Mês (CPF-Mês) ---
cat("ETAPA 6: Agregando múltiplos vínculos para o nível Indivíduo-Mês...\n")

# Order by salary to pick the main job's characteristics where we use first()
# Note: We use 'prepared_data_jakobsen_link_level' as the input for aggregation
prepared_data_jakobsen_agg <- prepared_data_jakobsen_link_level[order(cpf, year, month, -salario_vinculo_mes_bruto), .(
  
  # --- Características do Indivíduo/Vínculo Principal do Mês ---
  municipio = first(municipio),
  uf = first(uf),
  ocup2002 = first(ocup2002),
  sbclas20 = first(sbclas20),
  grauinstr = first(grauinstr),
  sexo = first(sexo),
  raca_cor = first(raca_cor),
  tempempr = first(tempempr), # Tenure of the main job
  tipoadm = first(tipoadm),   # Admission type of the main job

  # --- Agregação das Variáveis ---
  num_empregos_cpf_mes = .N, # Count of employment links for this CPF-month
  paid_ir = max(paid_ir, na.rm = TRUE), # Paid IR if paid in any job
  taxable_salary_total = sum(taxable_salary, na.rm = TRUE),
  salario_bruto_total = sum(salario_vinculo_mes_bruto, na.rm = TRUE),
  salcontr_total = sum(salcontr, na.rm = TRUE),
  horas_ajustadas_total = sum(horas_ajustadas, na.rm = TRUE),
  horascontr_total = sum(horascontr, na.rm = TRUE),
  data_referencia_mes = first(data_referencia_mes) # Keep the reference month date
  
), by = .(cpf, year, month)]

# --- 7. Salvamento do Painel Final ---
cat("ETAPA 7: Salvando o painel de análise final...\n")
OUTPUT_DIR_JAKOBSEN <- "data"
if (!dir.exists(OUTPUT_DIR_JAKOBSEN)) dir.create(OUTPUT_DIR_JAKOBSEN, recursive = TRUE)
PREPARED_DATA_JAKOBSEN_PATH <- file.path(OUTPUT_DIR_JAKOBSEN, "01_prepared_jk.rds")

saveRDS(prepared_data_jakobsen_agg, file = PREPARED_DATA_JAKOBSEN_PATH)

cat("\n--- SCRIPT CONCLUÍDO ---\n")
cat("Painel final para análise (Jakobsen & Sogaard) salvo em:", PREPARED_DATA_JAKOBSEN_PATH, "\n")
gc()
