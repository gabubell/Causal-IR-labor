# ===========================================================================
# SCRIPT 05: ESTIMAÇÃO DO MODELO DE ESTUDO DE EVENTOS
# ===========================================================================

# --- 0. Limpeza, Pacotes e Configuração ---
rm(list = ls()); gc()
source("00_config - cópia.R")
library(data.table)
library(fixest)
library(ggplot2)

# --- 1. Preparação dos Dados para o Modelo ---
cat("ETAPA 1: Preparando dados para a estimação do Estudo de Eventos...\n")

# Carrega os dados agregados ao nível CPF-Mês
FINAL_PANEL_PATH <- file.path(OUTPUT_DIR, "04_final_analysis_panel.rds")
if (!file.exists(FINAL_PANEL_PATH)) stop("Arquivo de painel final não encontrado. Rode o script de agregação primeiro.")
final_data <- readRDS(FINAL_PANEL_PATH)

# Filtra para a janela de eventos definida no config.R
event_study_data <- final_data[relative_time_month >= EVENT_WINDOW_START & relative_time_month <= EVENT_WINDOW_END]
event_study_data[, treatment_dummy := fifelse(assigned_group == "Treatment", 1, 0)]
rm(final_data); gc()

cat("  - Dados para o Estudo de Eventos preparados com", nrow(event_study_data), "observações.\n")

# --- 2. Estimação e Geração de Gráficos ---
cat("ETAPA 2: Estimando modelos e gerando gráficos para cada variável de resultado...\n")

fallback <- function(x, y) { if (is.null(x) || length(x) == 0) y else x }

plot_output_dir <- file.path(OUTPUT_DIR, "05_event_study_plots")
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
  
  model_formula <- as.formula(paste0(current_y, " ~ treatment_dummy*time_factor"))
  model <- feols(model_formula, data = event_study_data)
  
  # b) Extração dos coeficientes para o gráfico
  results_dt <- as.data.table(coeftable(model, cluster = ~cpf), keep.rownames = "term")
  event_study_coeffs <- results_dt[grepl("treatment_dummy:time_factor", term)]
  event_study_coeffs[, relative_time_month := as.numeric(gsub(".*time_factor", "", term))]
  
  # Adiciona o ponto de referência (período -1)
  reference_point <- data.table(term = "Referência", Estimate = 0, `Std. Error` = 0, relative_time_month = REFERENCE_PERIOD)
  final_results_table <- rbindlist(list(event_study_coeffs, reference_point), use.names = TRUE, fill = TRUE)
  setnames(final_results_table, c("Estimate", "Std. Error"), c("estimate", "std_error"), skip_absent = TRUE)
  setorder(final_results_table, relative_time_month)
  
  # c) Geração do Gráfico
  plot_title_label <- fallback(PLOT_LABELS_PT[[current_y]], current_y)
  
  plot_object <- ggplot(final_results_table, aes(x = relative_time_month, y = estimate)) +
    geom_hline(yintercept = 0, color = "gray50", linetype = "dashed") +
    geom_vline(xintercept = -0.5, color = "red", linetype = "dotted", linewidth = 0.8) +
    geom_errorbar(aes(ymin = estimate - 1.96 * std_error, ymax = estimate + 1.96 * std_error), 
                  width = 0.2, linewidth = 0.8, alpha = 0.7, color = "darkblue") +
    geom_point(size = 3, color = "darkblue") +
    labs(
      title = paste("Efeito sobre:", plot_title_label), 
      subtitle = "Modelo de Estudo de Eventos", 
      x = "Meses Relativos ao Evento", 
      y = "Estimativa do Efeito (ATT)"
    ) +
    theme_minimal(base_size = 14) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"))
  
  plot_filename <- paste0("event_study_", tolower(current_y), ".png")
  ggsave(file.path(plot_output_dir, plot_filename), plot = plot_object, width = 8, height = 6, dpi = 300, bg = "white")
  cat("  - Gráfico salvo:", plot_filename, "\n")
  
  # d) Salva os dados do gráfico em um arquivo CSV para manipulação posterior
  csv_filename <- paste0("event_study_data_", tolower(current_y), ".csv")
  data.table::fwrite(final_results_table, file.path(plot_output_dir, csv_filename))
  cat("  - Dados do gráfico salvos em:", csv_filename, "\n")
  # ========================================================================
}

cat("\n--- SCRIPT 05 CONCLUÍDO ---\n")