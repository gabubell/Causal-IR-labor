# ===========================================================================
# SCRIPT 03b: GERAÇÃO DO RELATÓRIO DE BALANCEAMENTO DO PAREAMENTO
# ===========================================================================

# --- 0. Limpeza, Pacotes e Configuração ---
rm(list = ls()); gc()
source("00_config.R")
library(data.table)
library(stringr)

# --- 1. Carregamento dos Dados Necessários ---
cat("ETAPA 1: Carregando dados pré e pós-pareamento...\n")

# Carrega a base de dados completa usada ANTES do pareamento
filtered_dt <- readRDS(FILTERED_DATA_PATH)

# ==================== ATUALIZAÇÃO DA REPLICAÇÃO DOS DADOS ====================
# A seção a seguir agora é uma cópia exata da preparação de dados do SCRIPT 03
# para garantir que todas as novas variáveis (paid_ir) sejam incluídas.
# ===========================================================================

# --- 2a. Dados de Dez/2015 ---
dec_2015_dt <- filtered_dt[year == BASE_YEAR & month == 12]
base_vars_dt <- dec_2015_dt[order(cpf, -salario_vinculo_mes_bruto), .(
  assigned_group = first(assigned_group), municipio = first(municipio),
  cbo_5digits = first(cbo_5digits), cnae_secao = first(cnae_secao),
  education_level_grouped = first(education_level_grouped), tempempr = as.numeric(first(tempempr)),
  salcontr_total_2014 = first(salcontr_total_2014)
), by = cpf]

# --- 2b. Perfil Mensal de 2015 (com paid_ir) ---
monthly_agg_dt <- filtered_dt[year == BASE_YEAR, .(
  total_relative_complement_salary = first(total_relative_complement_salary),
  paid_ir_monthly = max(paid_ir, na.rm = TRUE), # ADICIONADO
  num_jobs_monthly = .N,
  total_hours_monthly = sum(horas_ajustadas, na.rm = TRUE)
), by = .(cpf, month)]

monthly_profile_wide <- dcast(monthly_agg_dt, cpf ~ month, 
                              value.var = c("total_relative_complement_salary", "paid_ir_monthly", "num_jobs_monthly", "total_hours_monthly"), # ADICIONADO
                              fill = NA)

salary_names <- paste0("var_sal_m", 1:12)
ir_names <- paste0("paid_ir_m", 1:12)
jobs_names <- paste0("jobs_m", 1:12)
hours_names <- paste0("hours_m", 1:12)
setnames(monthly_profile_wide, names(monthly_profile_wide)[-1], c(salary_names, ir_names, jobs_names, hours_names)) # ADICIONADO

na_dummy_names <- paste0(salary_names, "_is_na")
monthly_profile_wide[, (na_dummy_names) := lapply(.SD, function(x) as.integer(is.na(x))), .SDcols = salary_names]
monthly_profile_wide[, (salary_names) := lapply(.SD, function(x) fcoalesce(x, 0)), .SDcols = salary_names]
monthly_profile_wide[, (ir_names) := lapply(.SD, function(x) fcoalesce(x, 0)), .SDcols = ir_names] # ADICIONADO
monthly_profile_wide[, (jobs_names) := lapply(.SD, function(x) fcoalesce(x, 0L)), .SDcols = jobs_names]
monthly_profile_wide[, (hours_names) := lapply(.SD, function(x) fcoalesce(x, 0)), .SDcols = hours_names]

# --- 2c. Junção Final e Definição das Variáveis (com paid_ir) ---
unmatched_data <- merge(base_vars_dt, monthly_profile_wide, by = "cpf", all.x = TRUE)
unmatched_data[, treatment_dummy := fifelse(assigned_group == "Treatment", 1, 0)]

# ==================== ATUALIZAÇÃO DAS LISTAS DE VARIÁVEIS ====================
exact_vars_final <- c(PSM_EXACT_MATCH_VARS, na_dummy_names, jobs_names, ir_names) # ADICIONADO ir_names
nearest_vars_final <- c(PSM_NEAREST_MATCH_VARS_SCALAR, salary_names, hours_names)
# ===========================================================================

all_vars_to_check <- c(exact_vars_final, nearest_vars_final)
unmatched_data <- na.omit(unmatched_data, cols = all_vars_to_check)
cat("  - Base de dados ANTES do pareamento recriada com", nrow(unmatched_data), "observações.\n")

# Carrega os CPFs que sobreviveram ao pareamento
if (!file.exists(MATCHED_CPFS_PATH)) stop("Arquivo de CPFs pareados não encontrado. Rode o SCRIPT 03 primeiro.")
matched_cpfs <- readRDS(MATCHED_CPFS_PATH)

# Cria o dataset DEPOIS do pareamento
matched_data <- unmatched_data[cpf %in% matched_cpfs]
cat("  - Base de dados DEPOIS do pareamento criada com", nrow(matched_data), "observações.\n")

rm(filtered_dt, dec_2015_dt, base_vars_dt, monthly_agg_dt, monthly_profile_wide); gc()

# --- 2. Geração da Tabela de Balanceamento ---
cat("\nETAPA 2: Gerando a tabela de balanceamento das covariáveis...\n")

calculate_balance <- function(variable, unmatched_df, matched_df) {
  mean_t_unmatched <- mean(unmatched_df[treatment_dummy == 1][[variable]], na.rm = TRUE)
  mean_c_unmatched <- mean(unmatched_df[treatment_dummy == 0][[variable]], na.rm = TRUE)
  sd_t_unmatched <- sd(unmatched_df[treatment_dummy == 1][[variable]], na.rm = TRUE)
  smd_unmatched <- (mean_t_unmatched - mean_c_unmatched) / sd_t_unmatched
  mean_t_matched <- mean(matched_df[treatment_dummy == 1][[variable]], na.rm = TRUE)
  mean_c_matched <- mean(matched_df[treatment_dummy == 0][[variable]], na.rm = TRUE)
  smd_matched <- (mean_t_matched - mean_c_matched) / sd_t_unmatched
  return(list(
    mean_t_unmatched = mean_t_unmatched, mean_c_unmatched = mean_c_unmatched, smd_unmatched = smd_unmatched,
    mean_t_matched = mean_t_matched, mean_c_matched = mean_c_matched, smd_matched = smd_matched
  ))
}

# --- 2a. Variáveis de Pareamento por Distância (Nearest) ---
balance_results_nearest <- lapply(nearest_vars_final, calculate_balance, unmatched_df = unmatched_data, matched_df = matched_data)
balance_table_nearest_raw <- rbindlist(balance_results_nearest)
balance_table_nearest <- balance_table_nearest_raw[, .(
  Caracteristica = str_to_title(gsub("_", " ", nearest_vars_final)),
  `Tratamento (Antes)` = mean_t_unmatched, `Controle (Antes)` = mean_c_unmatched, `SMD (Antes)` = smd_unmatched,
  `Tratamento (Depois)` = mean_t_matched, `Controle (Depois)` = mean_c_matched, `SMD (Depois)` = smd_matched
)]

# --- 2b. Variáveis de Pareamento Exato ---
exact_vars_categorical <- PSM_EXACT_MATCH_VARS
exact_vars_numeric <- setdiff(exact_vars_final, exact_vars_categorical)
balance_results_exact_numeric <- lapply(exact_vars_numeric, calculate_balance, unmatched_df = unmatched_data, matched_df = matched_data)
balance_table_exact_numeric_raw <- rbindlist(balance_results_exact_numeric)
balance_table_exact_numeric <- balance_table_exact_numeric_raw[, .(
  Caracteristica = str_to_title(gsub("_", " ", exact_vars_numeric)),
  `Tratamento (Antes)` = mean_t_unmatched, `Controle (Antes)` = mean_c_unmatched, `SMD (Antes)` = smd_unmatched,
  `Tratamento (Depois)` = mean_t_matched, `Controle (Depois)` = mean_c_matched, `SMD (Depois)` = smd_matched
)]
balance_table_exact_categorical <- data.table(
  Caracteristica = str_to_title(gsub("_", " ", exact_vars_categorical)),
  `Tratamento (Antes)` = "N/A", `Controle (Antes)` = "N/A", `SMD (Antes)` = "N/A",
  `Tratamento (Depois)` = "Exato", `Controle (Depois)` = "Exato", `SMD (Depois)` = "0,00"
)

# --- 3. Montagem da Tabela Final para o Paper ---
cat("ETAPA 3: Montando a tabela final em formato de paper...\n")
sample_size_panel <- data.table(
  Caracteristica = c("Nº de Indivíduos (Tratamento)", "Nº de Indivíduos (Controle)", "Total"),
  `Tratamento (Antes)` = c(sum(unmatched_data$treatment_dummy == 1), NA, NA), `Controle (Antes)` = c(NA, sum(unmatched_data$treatment_dummy == 0), NA), `SMD (Antes)` = NA,
  `Tratamento (Depois)` = c(sum(matched_data$treatment_dummy == 1), NA, NA), `Controle (Depois)` = c(NA, sum(matched_data$treatment_dummy == 0), NA), `SMD (Depois)` = NA
)
sample_size_panel[Caracteristica == "Total", `Tratamento (Antes)` := sum(unmatched_data$treatment_dummy == 1)]
sample_size_panel[Caracteristica == "Total", `Controle (Antes)` := sum(unmatched_data$treatment_dummy == 0)]
sample_size_panel[Caracteristica == "Total", `Tratamento (Depois)` := sum(matched_data$treatment_dummy == 1)]
sample_size_panel[Caracteristica == "Total", `Controle (Depois)` := sum(matched_data$treatment_dummy == 0)]
format_cols_numeric <- function(dt) {
  cols <- names(dt)[-1]
  for (col in cols) {
    if (is.numeric(dt[[col]])) {
      dt[[col]] <- format(round(dt[[col]], 2), nsmall = 2, decimal.mark = ",")
    }
  }
  return(dt)
}
balance_table_nearest <- format_cols_numeric(balance_table_nearest)
balance_table_exact_numeric <- format_cols_numeric(balance_table_exact_numeric)
cols_to_format_ss <- c("Tratamento (Antes)", "Controle (Antes)", "Tratamento (Depois)", "Controle (Depois)")
sample_size_panel[, (cols_to_format_ss) := lapply(.SD, function(x) format(x, big.mark = ".", na.encode = FALSE)), .SDcols = cols_to_format_ss]
panel_a_header <- data.table(Caracteristica = "Painel A: Tamanho da Amostra")
panel_b_header <- data.table(Caracteristica = "Painel B: Balanceamento das Covariáveis de Distância (Médias)")
panel_c_header <- data.table(Caracteristica = "Painel C: Balanceamento das Covariáveis de Pareamento Exato")
final_paper_table <- rbindlist(list(
  panel_a_header,
  sample_size_panel,
  panel_b_header,
  balance_table_nearest,
  panel_c_header,
  balance_table_exact_numeric,
  balance_table_exact_categorical
), use.names = TRUE, fill = TRUE)
final_paper_table[is.na(final_paper_table)] <- ""

# --- 4. Exportação do Relatório ---
cat("ETAPA 4: Exportando a tabela de balanceamento para CSV...\n")
output_csv_path <- file.path(OUTPUT_DIR, "tabela_balanceamento_pareamento.csv")
fwrite(final_paper_table, file = output_csv_path, sep = ";", bom = TRUE)

cat("  - Tabela de balanceamento salva em:", output_csv_path, "\n")
cat("\n--- SCRIPT 03b CONCLUÍDO ---\n")
gc()