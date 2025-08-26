# ===================================================================================
# SCRIPT 06: ESTIMAÇÃO DO MODELO DE KINKED EVENT STUDY E GERAÇÃO DE GRÁFICOS
# ===================================================================================

# --- 0. Limpeza, Pacotes e Configuração ---
rm(list = ls()); gc()
source("00_config.R")
library(data.table)
library(fixest)
library(ggplot2)

# --- 1. Preparação dos Dados para o Modelo ---
cat("ETAPA 1: Carregando e preparando dados para o Kinked Event Study...\n")

# --- 1a. Carregar arquivos de origem ---
# Caminhos para os arquivos de entrada
FILTERED_DATA_PATH <- file.path(OUTPUT_DIR, "02_filtered_data.rds")
MATCHED_CPFS_PATH <- file.path(OUTPUT_DIR, "03_matched_cpfs.rds")
FINAL_PANEL_PATH <- file.path(OUTPUT_DIR, "04_final_analysis_panel.rds")

# Validação dos arquivos
if (!file.exists(FILTERED_DATA_PATH)) stop("Arquivo de dados filtrados (02) não encontrado.")
if (!file.exists(MATCHED_CPFS_PATH)) stop("Arquivo de CPFs pareados (03) não encontrado.")
if (!file.exists(FINAL_PANEL_PATH)) stop("Painel final (04) não encontrado.")

# Leitura dos dados
filtered_dt <- readRDS(FILTERED_DATA_PATH)
matched_cpfs <- readRDS(MATCHED_CPFS_PATH)
final_panel_dt <- readRDS(FINAL_PANEL_PATH)

cat("  - Arquivos de origem carregados com sucesso.\n")

# --- 1b. Calcular a Running Variable a partir dos dados granulares ---
cat("  - Calculando a running variable a partir dos dados do mês base...\n")
# Usa o arquivo granular (filtered_dt) para encontrar o maior salário de cada indivíduo
# no mês base (BASE_YEAR, mês 12) para os CPFs que estão na amostra final.
base_salaries_dt <- filtered_dt[
  cpf %in% matched_cpfs & year == BASE_YEAR & month == 12
][order(cpf, -salcontr), .SD[1], by = cpf # Pega o vínculo com maior salário
][, .(cpf, base_salary_metric = get(SALARY_COL_FOR_GROUPS))]

# --- 1c. Juntar tudo e criar as variáveis finais para o modelo ---
cat("  - Juntando running variable ao painel final e criando variáveis do modelo...\n")
# Faz o merge da informação do salário base com o painel de análise completo.
# A running variable é uma característica fixa de cada indivíduo ao longo do tempo.
event_study_data <- merge(final_panel_dt, base_salaries_dt, by = "cpf", all.x = TRUE)

# Remove observações que não possuem um salário base
event_study_data <- event_study_data[!is.na(base_salary_metric)]

# Define o ponto de corte (kink)
cutoff_point <- IR_LIMIT_BASE_YEAR * (1 - TREATMENT_PERCENT_BELOW_LIMIT)

# Cria as variáveis para o modelo Kink
event_study_data[, running_variable := base_salary_metric - cutoff_point]
event_study_data[, treatment_dummy := fifelse(running_variable >= 0, 1, 0)]

# Filtra para a janela de eventos e, opcionalmente, para uma banda em torno do kink
event_study_data <- event_study_data[relative_time_month >= EVENT_WINDOW_START & relative_time_month <= EVENT_WINDOW_END]

# Limpeza de memória
rm(filtered_dt, matched_cpfs, final_panel_dt, base_salaries_dt); gc()

cat("  - Dados para o Kinked Event Study preparados com", nrow(event_study_data), "observações.\n")

# --- 2. Estimação e Geração de Gráficos  ---
cat("ETAPA 2: Estimando modelos de Kink e gerando gráficos...\n")

fallback <- function(x, y) { if (is.null(x) || length(x) == 0) y else x }
plot_output_dir <- file.path(OUTPUT_DIR, "06_kink_event_study_plots")
if (!dir.exists(plot_output_dir)) dir.create(plot_output_dir, recursive = TRUE)

for (current_y in OUTCOME_VARIABLES_EV) {
  if (!current_y %in% names(event_study_data) || uniqueN(event_study_data[[current_y]], na.rm = TRUE) <= 1) {
    cat("  - AVISO: Pulando '", current_y, "' (não encontrada ou constante).\n")
    next
  }
  
  cat("\nPROCESSANDO:", current_y, "\n")
  
  # a) Estimação
  ref_period_str <- as.character(REFERENCE_PERIOD)
  event_study_data[, time_factor := relevel(factor(relative_time_month), ref = ref_period_str)]
  
  # A fórmula para o Kinked Event Study
  model_formula <- as.formula(paste0(
    current_y, " ~ time_factor * running_variable * treatment_dummy"
  ))
  
  model <- feols(model_formula, data = event_study_data, cluster = ~cpf)
  
  # b) Extração dos coeficientes para o gráfico
  results_dt <- as.data.table(coeftable(model), keep.rownames = "term")
  event_study_coeffs <- results_dt[grepl("time_factor.*:running_variable:treatment_dummy", term)]
  event_study_coeffs[, relative_time_month := as.numeric(gsub("time_factor|:running_variable:treatment_dummy", "", term))]
  
  # c) Preparação da tabela de resultados para o gráfico
  reference_point <- data.table(term = "Referência", Estimate = 0, `Std. Error` = 0, relative_time_month = REFERENCE_PERIOD)
  final_results_table <- rbindlist(list(event_study_coeffs, reference_point), use.names = TRUE, fill = TRUE)
  setnames(final_results_table, c("Estimate", "Std. Error"), c("estimate", "std_error"), skip_absent = TRUE)
  setorder(final_results_table, relative_time_month)
  
  # d) Geração do Gráfico
  plot_title_label <- fallback(PLOT_LABELS_PT[[current_y]], current_y)
  
  plot_object <- ggplot(final_results_table, aes(x = relative_time_month, y = estimate)) +
    geom_hline(yintercept = 0, color = "gray50", linetype = "dashed") +
    geom_vline(xintercept = -0.5, color = "red", linetype = "dotted", linewidth = 0.8) +
    geom_errorbar(aes(ymin = estimate - 1.96 * std_error, ymax = estimate + 1.96 * std_error), 
                  width = 0.2, linewidth = 0.8, alpha = 0.7, color = "darkblue") +
    geom_point(size = 3, color = "darkblue") +
    labs(
      title = paste("Evolução do Kink sobre:", plot_title_label), 
      subtitle = "Modelo de Kinked Event Study", 
      x = "Meses Relativos ao Evento", 
      y = "Estimativa da Mudança na Inclinação (Kink)"
    ) +
    theme_minimal(base_size = 14) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"))
  
  plot_filename <- paste0("kink_event_study_", tolower(current_y), ".png")
  ggsave(file.path(plot_output_dir, plot_filename), plot = plot_object, width = 8, height = 6, dpi = 300, bg = "white")
  cat("  - Gráfico salvo:", plot_filename, "\n")
  
  # e) Salva os dados do gráfico em um arquivo CSV para manipulação posterior
  csv_filename <- paste0("kink_event_study_data_", tolower(current_y), ".csv")
  data.table::fwrite(final_results_table, file.path(plot_output_dir, csv_filename))
  cat("  - Dados do gráfico salvos em:", csv_filename, "\n")
  # ========================================================================
}

cat("\n--- SCRIPT 06 (KINKED EVENT STUDY) CONCLUÍDO ---\n")