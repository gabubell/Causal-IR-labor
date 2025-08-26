# ===========================================================================
# SCRIPT 08: GERAÇÃO DE GRÁFICOS EM LOTE (EVENT STUDY VS. KINK) - VERSÃO FINAL
# ===========================================================================

# --- 0. Limpeza, Pacotes e Configuração ---
rm(list = ls()); gc()

# Pacotes necessários
if (!require(data.table)) install.packages("data.table"); library(data.table)
if (!require(ggplot2)) install.packages("ggplot2"); library(ggplot2)
if (!require(gridExtra)) install.packages("gridExtra"); library(gridExtra)
if (!require(grid)) install.packages("grid"); library(grid)

# ===========================================================================
#                             CONFIGURAÇÃO
# ===========================================================================
BASE_DIR <- '/Users/macbook/Library/CloudStorage/GoogleDrive-gabriel.belle@ufpe.br/.shortcut-targets-by-id/1b00xGP2scppaeOVxu3d3TxC9m-ednOAe/Gabriel PIMES/data/IR/Anpec'
BASE_PLOT_OUTPUT_DIR <- file.path(BASE_DIR, "comparison_plots")
if (!dir.exists(BASE_PLOT_OUTPUT_DIR)) dir.create(BASE_PLOT_OUTPUT_DIR, recursive = TRUE)

# MUDANÇA: Carregando rótulos e outras configurações de um arquivo externo.
# Certifique-se de que este arquivo está no seu diretório de trabalho.
source("00_config.R")

# --- LISTA DE CRITÉRIOS DE PAREAMENTO PARA ITERAR ---
MATCH_CRITERIA_LIST <- c("match horas exato")

# --- LISTA DE VARIÁVEIS PARA PLOTAR ---
VARS_TO_PLOT <- c('horascontr',
                  'num_empregos_cpf_mes',
                  'cumulative_unemployment',
                  'sal_cntr_hra_medio',
                  # Acumulados
                  'left_job',
                  'has_changed_job',
                  'has_started_job',
                  'has_changed_job',
                  'has_cbo_change_at_start'
)
# ===========================================================================

#VARS_TO_PLOT = 'sal_hra_acumulado'
# --- 1. Funções Auxiliares ---

load_data <- function(analysis_type, match_type, ir_status, y_var, base_dir = BASE_DIR) {
  if (analysis_type == "event_study") {
    file_prefix <- "event_study_data_"; plot_dir_name <- "06_event_study_plots"
  } else if (analysis_type == "kink") {
    file_prefix <- "kink_event_study_data_"; plot_dir_name <- "07_kink_event_study_plots"
  } else { stop("Tipo de análise inválido.") }
  csv_filename <- paste0(file_prefix, y_var, ".csv")
  file_path <- file.path(base_dir, match_type, ir_status, plot_dir_name, csv_filename)
  if (!file.exists(file_path)) { warning(paste("Arquivo não encontrado:", file_path)); return(NULL) }
  return(fread(file_path))
}

create_plot <- function(plot_type, data_main, data_secondary, main_color, secondary_color, plot_title) {
  y_label <- if(plot_type == "event_study") "Mudança no Nível" else "Mudança na Inclinação"
  color_values <- setNames(c(main_color, secondary_color), c("Nunca pagou IR", "Pagou IR 1x"))
  z_value <- 1.645
  p <- ggplot() +
    geom_hline(yintercept = 0, color = "gray50", linetype = "dashed") +
    geom_vline(xintercept = -0.5, color = "red", linetype = "dotted", linewidth = 0.8) +
    geom_ribbon(data = data_main, aes(x = relative_time_month, ymin = estimate - z_value * std_error, ymax = estimate + z_value * std_error, fill = "Nunca pagou IR"), alpha = 0.2) +
    geom_ribbon(data = data_secondary, aes(x = relative_time_month, ymin = estimate - z_value * std_error, ymax = estimate + z_value * std_error, fill = "Pagou IR 1x"), alpha = 0.2) +
    geom_line(data = data_main, aes(x = relative_time_month, y = estimate, color = "Nunca pagou IR"), linewidth = 0.8) +
    geom_line(data = data_secondary, aes(x = relative_time_month, y = estimate, color = "Pagou IR 1x"), linewidth = 0.8) +
    geom_point(data = data_main, aes(x = relative_time_month, y = estimate, color = "Nunca pagou IR"), size = 3) +
    geom_point(data = data_secondary, aes(x = relative_time_month, y = estimate, color = "Pagou IR 1x"), size = 3, shape = 17) +
    scale_color_manual(name = NULL, values = color_values) +
    scale_fill_manual(name = NULL, values = color_values) +
    labs(title = plot_title, x = NULL, y = y_label) +
    theme_minimal(base_size = 14) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16), legend.position = "bottom")
  return(p)
}

get_legend <- function(my_ggplot) {
  tmp <- ggplot_gtable(ggplot_build(my_ggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  if (length(leg) > 0) return(tmp$grobs[[leg]])
  return(NULL)
}

# ===========================================================================
#     LOOP PRINCIPAL: GERAÇÃO DE GRÁFICOS EM LOTE
# ===========================================================================
cat("\n--- INICIANDO GERAÇÃO DE GRÁFICOS EM LOTE ---\n")

for (match_criteria in MATCH_CRITERIA_LIST) {
  
  cat(paste("\n\n=======================================================\n"))
  cat(paste("   PROCESSANDO CRITÉRIO DE PAREAMENTO:", toupper(match_criteria), "\n"))
  cat(paste("=======================================================\n"))
  
  current_output_dir <- file.path(BASE_PLOT_OUTPUT_DIR, gsub(" ", "_", match_criteria))
  if (!dir.exists(current_output_dir)) dir.create(current_output_dir, recursive = TRUE)
  
  for (y_var in VARS_TO_PLOT) {
    
    cat(paste("\n--- Processando variável:", y_var, "---\n"))
    
    event_data_main <- load_data("event_study", match_criteria, "não pagou IR em 2015", y_var)
    event_data_sec <- load_data("event_study", match_criteria, "pagou IR uma vez", y_var)
    kink_data_main <- load_data("kink", match_criteria, "não pagou IR em 2015", y_var)
    kink_data_sec <- load_data("kink", match_criteria, "pagou IR uma vez", y_var)
    
    if (any(sapply(list(event_data_main, event_data_sec, kink_data_main, kink_data_sec), is.null))) {
      cat("  -> ERRO: Arquivos de dados não encontrados. Pulando para a próxima variável.\n")
      next 
    }
    
    plot_event_study <- create_plot("event_study", event_data_main, event_data_sec, "darkblue", "firebrick", "A) Modelo LDD")
    plot_kink <- create_plot("kink", kink_data_main, kink_data_sec, "darkblue", "firebrick", "B) Modelo RK-LDD")
    
    shared_legend <- get_legend(plot_event_study)
    plot_event_study_no_leg <- plot_event_study + theme(legend.position = "none", axis.title.y = element_text(size=11))
    plot_kink_no_leg <- plot_kink + theme(legend.position = "none", axis.title.y = element_text(size=11))
    
    main_title_text <- PLOT_LABELS_PT[[y_var]]
    if (is.null(main_title_text)) { main_title_text <- gsub("_", " ", y_var) }
    
    title_grob <- textGrob(main_title_text, gp = gpar(fontsize = 20, fontface = "bold"))
    x_axis_grob <- textGrob("Meses Relativos ao Evento", gp = gpar(fontsize = 14))
    
    final_plot <- grid.arrange(
      title_grob,
      arrangeGrob(plot_event_study_no_leg, plot_kink_no_leg, ncol = 2),
      x_axis_grob,
      shared_legend,
      ncol = 1,
      heights = c(1, 8, 0.5, 1.2)
    )
    
    output_filename <- file.path(current_output_dir, paste0("y_", y_var, ".png"))
    ggsave(filename = output_filename, plot = final_plot, width = 14, height = 8, dpi = 300, bg = "white")
    cat("  -> Gráfico salvo com sucesso em:", output_filename, "\n")
  }
}

cat("\n--- GERAÇÃO DE GRÁFICOS EM LOTE CONCLUÍDA ---\n")