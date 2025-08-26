# ===========================================================================
# PLOT DESCRITIVO - PROPORÇÃO DE PAGANTES DE IR (VERSÃO FINAL)
# ===========================================================================

# --- 0. Limpeza, Pacotes e Configuração ---
rm(list = ls()); gc()

# Pacotes necessários
if (!require(data.table)) install.packages("data.table"); library(data.table)
if (!require(ggplot2)) install.packages("ggplot2"); library(ggplot2)

# ===========================================================================
#                             CONFIGURAÇÃO
# ===========================================================================
# --- ENTRADA ---
# O arquivo RDS deve estar no mesmo diretório em que este script é executado.
INPUT_RDS_FILENAME <- "output/04_final_analysis_panel.rds"

# --- SAÍDA ---
# Diretório base no Google Drive para salvar o gráfico.
BASE_OUTPUT_DIR <- '/Users/macbook/Library/CloudStorage/GoogleDrive-gabriel.belle@ufpe.br/.shortcut-targets-by-id/1b00xGP2scppaeOVxu3d3TxC9m-ednOAe/Gabriel PIMES/data/IR/Anpec'

# Diretório final para os plots descritivos.
PLOT_OUTPUT_DIR <- file.path(BASE_OUTPUT_DIR, "comparision_plots")
if (!dir.exists(PLOT_OUTPUT_DIR)) dir.create(PLOT_OUTPUT_DIR, recursive = TRUE)
# ===========================================================================

cat("\n--- INICIANDO GERAÇÃO DE GRÁFICO DESCRITIVO ---\n")

# --- 1. Carregar Dados ---
if (!file.exists(INPUT_RDS_FILENAME)) {
  stop("ERRO: Arquivo de dados '", INPUT_RDS_FILENAME, "' não encontrado no diretório de trabalho.")
}
cat("  - Carregando dados de:", INPUT_RDS_FILENAME, "\n")
panel_data <- readRDS(INPUT_RDS_FILENAME)
setDT(panel_data) # Garante que é um data.table

# --- 2. Preparar Dados para Plotagem ---
# Filtra a partir de Dez/15 (mês -1)
# Calcula a proporção de pagantes de IR (mean(paid_ir)) para cada grupo e mês
plot_data <- panel_data[relative_time_month >= -1 & relative_time_month <= 11, 
                        .(proportion = mean(paid_ir, na.rm = TRUE)), 
                        by = .(relative_time_month, assigned_group)]

# Renomeia os grupos para a legenda do gráfico
plot_data[, group := factor(assigned_group, levels = c('Control', 'Treatment'), labels = c("Controle", "Tratado"))]

# --- 3. Gerar o Gráfico ---
cat("  - Gerando o gráfico...\n")

descriptive_plot <- ggplot(plot_data, aes(x = relative_time_month, y = proportion, color = group)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 3, aes(shape = group)) +
  
  # Linha vertical para marcar o início do período pós-evento
  geom_vline(xintercept = -0.5, color = "red", linetype = "dashed", linewidth = 0.8) +
  
  # Escalas e rótulos
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, NA)) +
  scale_x_continuous(breaks = seq(0, max(plot_data$relative_time_month, na.rm = TRUE), by = 6)) +
  scale_color_manual(name = "Grupo", values = c("Controle" = "darkblue", "Tratado" = "firebrick")) +
  scale_shape_manual(name = "Grupo", values = c("Controle" = 16, "Tratado" = 17)) + # Ponto redondo e triângulo
  
  labs(
    title = "Evolução dos Pagantes de IR",
    x = "Meses a partir de Dez/2015",
    y = "Proporção"
  ) +
  
  theme_minimal(base_size = 15) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, face = "bold", size = 20),
    plot.subtitle = element_text(hjust = 0.5, size = 16, color = "gray30")
  )

# --- 4. Salvar o Gráfico ---
output_filename <- file.path(PLOT_OUTPUT_DIR, "descriptive_ir_proportion_exact_match.png")
ggsave(filename = output_filename, plot = descriptive_plot, width = 12, height = 7, dpi = 300, bg = "white")

cat("  -> Gráfico salvo com sucesso em:", output_filename, "\n")
cat("\n--- SCRIPT CONCLUÍDO ---\n")