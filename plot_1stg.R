# ===========================================================================
# GRÁFICO COMPARATIVO DO PRIMEIRO ESTÁGIO
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
source("00_config.R")

BASE_DIR <- '/Users/macbook/Library/CloudStorage/GoogleDrive-gabriel.belle@ufpe.br/.shortcut-targets-by-id/1b00xGP2scppaeOVxu3d3TxC9m-ednOAe/Gabriel PIMES/data/IR/Anpec'
PLOT_OUTPUT_DIR <- file.path(BASE_DIR, "comparison_plots")
if (!dir.exists(PLOT_OUTPUT_DIR)) dir.create(PLOT_OUTPUT_DIR, recursive = TRUE)

# --- Variáveis e critério para o gráfico ---
VARS_TO_PLOT <- c("paid_ir", "has_paid_ir", "has_paid_ir_consecutive")
MATCH_CRITERIA <- "match horas exato"
# ===========================================================================


# --- 1. Funções Auxiliares ---

#' Carrega DADOS DE EVENT STUDY e filtra por período.
load_event_study_data <- function(match_type, ir_status, y_var, base_dir = BASE_DIR) {
  file_prefix <- "event_study_data_"
  plot_dir_name <- "06_event_study_plots"
  csv_filename <- paste0(file_prefix, y_var, ".csv")
  file_path <- file.path(base_dir, match_type, ir_status, plot_dir_name, csv_filename)
  
  if (!file.exists(file_path)) { warning(paste("Arquivo não encontrado:", file_path)); return(NULL) }
  
  dt <- fread(file_path)
  return(dt[relative_time_month >= -3 & relative_time_month <= 11])
}

#' Cria um gráfico de nível de variável (Event Study) em Pontos Percentuais.
create_level_plot <- function(data_main, data_secondary, main_color, secondary_color, plot_title) {
  color_values <- setNames(c(main_color, secondary_color), c("Nunca pagou IR", "Pagou IR 1x"))
  z_value <- 1.645 # 90% CI
  
  p <- ggplot() +
    geom_hline(yintercept = 0, color = "gray50", linetype = "dashed") +
    geom_vline(xintercept = -0.5, color = "red", linetype = "dotted", linewidth = 0.8) +
    
    geom_ribbon(data = data_main, aes(x = relative_time_month, ymin = (estimate - z_value * std_error) * 100, ymax = (estimate + z_value * std_error) * 100, fill = "Nunca pagou IR"), alpha = 0.2) +
    geom_ribbon(data = data_secondary, aes(x = relative_time_month, ymin = (estimate - z_value * std_error) * 100, ymax = (estimate + z_value * std_error) * 100, fill = "Pagou IR 1x"), alpha = 0.2) +
    
    geom_line(data = data_main, aes(x = relative_time_month, y = estimate * 100, color = "Nunca pagou IR"), linewidth = 0.8) +
    geom_line(data = data_secondary, aes(x = relative_time_month, y = estimate * 100, color = "Pagou IR 1x"), linewidth = 0.8) +
    
    geom_point(data = data_main, aes(x = relative_time_month, y = estimate * 100, color = "Nunca pagou IR"), size = 3) +
    geom_point(data = data_secondary, aes(x = relative_time_month, y = estimate * 100, color = "Pagou IR 1x"), size = 3, shape = 17) +
    
    # MUDANÇA: Formata os rótulos do eixo Y para incluir "p.p."
    scale_y_continuous(labels = function(x) paste(x, "p.p.")) +
    scale_x_continuous(breaks = seq(-3, 11, by = 3)) +
    scale_color_manual(name = NULL, values = color_values) +
    scale_fill_manual(name = NULL, values = color_values) +
    
    # MUDANÇA: Rótulo do eixo Y removido
    labs(title = plot_title, x = NULL, y = NULL) +
    theme_minimal(base_size = 14) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16), legend.position = "bottom")
  return(p)
}

#' Extrai a legenda de um objeto ggplot.
get_legend <- function(my_ggplot) {
  tmp <- ggplot_gtable(ggplot_build(my_ggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  if (length(leg) > 0) return(tmp$grobs[[leg]])
  return(NULL)
}

plots_list <- list()

# --- a) Cria os gráficos para cada variável ---
for (i in seq_along(VARS_TO_PLOT)) {
  y_var <- VARS_TO_PLOT[i]
  cat(paste("\n--- Processando variável:", y_var, "---\n"))
  
  data_main <- load_event_study_data(MATCH_CRITERIA, "não pagou IR em 2015", y_var)
  data_sec <- load_event_study_data(MATCH_CRITERIA, "pagou IR uma vez em 2015", y_var)
  
  if (is.null(data_main) || is.null(data_sec)) { stop("ERRO: Arquivos de dados não encontrados para '", y_var, "'.") }
  
  panel_letter <- LETTERS[i]
  panel_title_text <- PLOT_LABELS_PT[[y_var]]
  if (is.null(panel_title_text)) { panel_title_text <- gsub("_", " ", y_var) }
  full_panel_title <- paste0(panel_letter, ") ", panel_title_text)
  
  plots_list[[i]] <- create_level_plot(data_main, data_sec, "darkblue", "firebrick", full_panel_title)
}

# --- b) Padroniza o eixo Y em todos os gráficos ---
cat("\n--- Padronizando o eixo Y entre os painéis ---\n")
ranges <- lapply(plots_list, function(p) ggplot_build(p)$layout$panel_scales_y[[1]]$range$range)
global_y_range <- c(min(sapply(ranges, `[`, 1)), max(sapply(ranges, `[`, 2)))

standardized_plots <- lapply(plots_list, function(p) p + coord_cartesian(ylim = global_y_range, expand = TRUE))

# --- c) Monta o gráfico final com gridExtra ---
cat("--- Montando o gráfico final ---\n")
shared_legend <- get_legend(standardized_plots[[1]])
plots_list_no_legend <- lapply(standardized_plots, function(p) p + theme(legend.position = "none"))

main_title_grob <- textGrob("Primeiro Estágio", gp = gpar(fontsize = 20, fontface = "bold"))
x_axis_grob <- textGrob("Meses Relativos a Dez/2015", gp = gpar(fontsize = 14))

final_plot <- grid.arrange(
  main_title_grob,
  arrangeGrob(grobs = plots_list_no_legend, ncol = 3),
  x_axis_grob,
  shared_legend,
  ncol = 1,
  heights = c(1, 8, 0.5, 1)
)

# --- d) Salva o gráfico final ---
output_filename <- file.path(PLOT_OUTPUT_DIR, "primeiro_estagio.png")
ggsave(filename = output_filename, plot = final_plot, width = 18, height = 7, dpi = 300, bg = "white")
cat("  -> Gráfico salvo com sucesso em:", output_filename, "\n")