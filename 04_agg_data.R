# ===========================================================================
# SCRIPT 04: AGREGAÇÃO DO PAINEL FINAL (CPF-MÊS)
# ===========================================================================

# --- 0. Limpeza, Pacotes e Configuração ---
rm(list = ls()); gc()
source("00_config.R")
library(data.table)
library(lubridate) 

# --- 1. Carregamento dos Dados ---
cat("ETAPA 1: Carregando dados pós-pareamento para agregação...\n")

# Caminhos dos arquivos de entrada
FILTERED_DATA_INPUT_PATH <- file.path(OUTPUT_DIR, "02_filtered_data.rds") 
MATCHED_CPFS_INPUT_PATH  <- file.path(OUTPUT_DIR, "03_matched_cpfs.rds")

# Caminho para o arquivo de saída deste script
FINAL_PANEL_OUTPUT_PATH <- file.path(OUTPUT_DIR, "04_final_analysis_panel.rds")

# Validação dos arquivos de entrada
if (!file.exists(FILTERED_DATA_INPUT_PATH)) stop("Arquivo de dados filtrados (02) não encontrado. Rode o script 02 primeiro.")
if (!file.exists(MATCHED_CPFS_INPUT_PATH)) stop("Arquivo de CPFs pareados (03) não encontrado. Rode o script 03 primeiro.")

# Carrega os dados
filtered_dt <- readRDS(FILTERED_DATA_INPUT_PATH)
matched_cpfs <- readRDS(MATCHED_CPFS_INPUT_PATH)

# =================================================================================
# ETAPA 2: PREPARAÇÃO E AGREGAÇÃO DO PAINEL
# =================================================================================
cat("ETAPA 2: Criando o painel de análise final...\n")
analysis_panel <- filtered_dt[cpf %in% matched_cpfs]
rm(filtered_dt, matched_cpfs); gc()

# --- ETAPA 2.2: Agregação para o Nível Indivíduo-Mês ---
cat("  - ETAPA 2.2: Agregando múltiplos vínculos para o nível Indivíduo-Mês...\n")
final_data <- analysis_panel[order(cpf, year, month, -salario_vinculo_mes_bruto), .(
  
  # --- Características do Indivíduo/Vínculo Principal do Mês ---
  uf = first(uf), 
  municipio = first(municipio),
  assigned_group = first(assigned_group),
  
  # --- Variáveis de Tempo e Referência do Indivíduo ---
  data_referencia_mes = first(data_referencia_mes),
  relative_time_month = first(relative_time_month),
  
  num_empregos_cpf_mes = sum(!is.na(id_vinculo_original)),
  
  # Hora média
  hra_media = mean(horascontr, na.rm = TRUE),
  sal_contr_medio = mean(salcontr, na.rm = TRUE),
  sal_bruto_medio = mean(salario_vinculo_mes_bruto, na.rm = TRUE),

  # --- AGREGAÇÃO DAS VARIÁVEIS DE RESULTADO (Y) ---
  horascontr = sum(horas_ajustadas, na.rm = TRUE),
  sal_contratual = sum(salcontr, na.rm = TRUE), 
  taxable_salary = sum(taxable_salary, na.rm = TRUE),
  salario_total_cpf_mes = first(salario_total_cpf_mes),
  paid_ir = max(paid_ir, na.rm = TRUE),
  started_job = max(started_job, na.rm = TRUE),
  left_job = max(left_job, na.rm = TRUE),
  asked_to_leave = max(asked_to_leave, na.rm = TRUE),
  retired_termination = max(retired_termination, na.rm = TRUE),
  retired_without = max(retired_without, na.rm = TRUE),
  changed_job = max(changed_job, na.rm = TRUE),
  cbo_change_in_company = max(cbo_change_in_company, na.rm = TRUE),
  cbo_change_at_start = max(cbo_change_at_start, na.rm = TRUE),
  
  # cbo_to_2 = max(cbo_to_2, na.rm = TRUE), 
  # cbo_to_4 = max(cbo_to_4, na.rm = TRUE),
  # cbo_not_3 = max(cbo_not_3, na.rm = TRUE),
  # has_cbo_to_2 = first(has_cbo_to_2),
  # has_cbo_to_4 = first(has_cbo_to_4),
  # has_cbo_not_3 = first(has_cbo_not_3),
  
  cumulative_unemployment = first(cumulative_unemployment),
  #cumulative_unemployment_2 = first(cumulative_unemployment_2),
  permanent_exit = first(permanent_exit),
  var_contract = first(var_contract),
  var_sal = first(var_sal),
  var_sal_tot = first(var_sal_tot),
  has_paid_ir = first(has_paid_ir),
  has_started_job = first(has_started_job),
  has_left_job = first(has_left_job),
  has_asked_to_leave = first(has_asked_to_leave),
  has_changed_job = first(has_changed_job),
  has_cbo_change_at_start = first(has_cbo_change_at_start),
  has_cbo_change_in_company = first(has_cbo_change_in_company),
  has_retired_termination = first(has_retired_termination),
  has_retired_without = first(has_retired_without),
  has_paid_ir_consecutive = first(has_paid_ir_consecutive),
  has_permanent_exit = first(has_permanent_exit)
  
), by = .(cpf, year, month)]

cat("  - Painel final criado com", format(nrow(final_data), big.mark="."), "observações únicas de CPF-Mês.\n")
cat("  - Amostra final contém", format(uniqueN(final_data$cpf), big.mark="."), "indivíduos únicos.\n")
rm(analysis_panel); gc()

# =====================================================================
# ETAPA 3: CÁLCULO DE VARIÁVEIS AGREGADAS E ACUMULADAS
# =====================================================================
cat("ETAPA 3: Calculando salário-hora e variáveis acumuladas...\n")

# 1. Calcular salário-hora usando as horas ajustadas.
final_data[, sal_hra := fifelse(horascontr > 0, salario_total_cpf_mes / horascontr, 0)]

final_data[, sal_cntr_hra_medio := fifelse(horascontr > 0, sal_contr_medio / hra_media, 0)]
final_data[, sal_bruto_hra_medio := fifelse(horascontr > 0, sal_bruto_medio / hra_media, 0)]

final_data[is.na(sal_bruto_medio), sal_bruto_medio := 0]
final_data[is.na(sal_contr_medio), sal_contr_medio := 0]
final_data[is.na(hra_media), hra_media := 0]

# 2. Criar variáveis acumuladas.
setorder(final_data, cpf, data_referencia_mes)

cat("  - Calculando somas acumuladas de forma robusta a NAs...\n")
final_data[, `:=` (
  horas_acumuladas = cumsum(fifelse(is.na(horascontr), 0, horascontr)),
  salario_acumulado = cumsum(fifelse(is.na(salario_total_cpf_mes), 0, salario_total_cpf_mes)),
  sal_hra_acumulado = cumsum(fifelse(is.na(sal_hra), 0, sal_hra))
), by = .(cpf)]

cat("  - Variáveis 'sal_hra', 'horas_acumuladas', 'salario_acumulado' e 'sal_hra_acumulado' criadas.\n")

# =====================================================================
# ETAPA 4: SALVAMENTO DO PAINEL FINAL
# =====================================================================
cat("ETAPA 4: Salvando o painel de análise final...\n")
saveRDS(final_data, file = FINAL_PANEL_OUTPUT_PATH)
cat("\n--- SCRIPT 04 CONCLUÍDO ---\n")
cat("Painel final salvo com sucesso em:", FINAL_PANEL_OUTPUT_PATH, "\n")
gc()
