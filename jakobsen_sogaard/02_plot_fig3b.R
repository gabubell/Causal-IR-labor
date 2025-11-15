# ===========================================================================
# SCRIPT 02 (Jakobsen & Sogaard): REPLICAÇÃO DA FIGURA 3B (HARMONIZADO COM FIG 3C)
# ===========================================================================

# --- 0. Limpeza, Pacotes e Configuração ---
rm(list = ls()); gc()
source("00_config.R")
library(data.table)
library(fixest)
library(ggplot2)

# --- 1. Carregamento e Preparação dos Dados ---
cat("ETAPA 1: Carregando e preparando dados...\n")

PREPARED_DATA_PATH <- "data/01_prepared_jk.rds"
if (!file.exists(PREPARED_DATA_PATH)) {
  stop("Arquivo '", PREPARED_DATA_PATH, "' não encontrado. Rode o script 01 primeiro.")
}
panel_dt <- readRDS(PREPARED_DATA_PATH)

# Extrai o salário de dezembro
december_salary_dt <- panel_dt[month == 12 & num_empregos_cpf_mes > 0, 
                               .(cpf, year, december_taxable_salary = taxable_salary_total)]

# Ajusta para preços de 2016
INFLATION_RATE <- 0.045
december_salary_dt[, salary_2016_prices := december_taxable_salary * (1 + INFLATION_RATE)^(2016 - year)]

# Determina se o indivíduo pagou IR em algum momento do ano
ir_status_dt <- panel_dt[, .(paid_ir_in_year = max(paid_ir, na.rm = TRUE)), by = .(cpf, year)]

# Isola os dados de cada ano
dt_2014 <- december_salary_dt[year == 2014, .(cpf, initial_income_2014 = salary_2016_prices)]
dt_2015 <- december_salary_dt[year == 2015, .(cpf, income_2015 = salary_2016_prices)]
dt_2016 <- december_salary_dt[year == 2016, .(cpf, income_2016 = salary_2016_prices)]

# --- Base Pré-Reforma (2014 -> 2015) ---
pre_reform_dt <- merge(dt_2014, dt_2015, by = "cpf")
non_payers_2014 <- ir_status_dt[year == 2014 & paid_ir_in_year == 0, .(cpf)]
pre_reform_dt <- pre_reform_dt[cpf %in% non_payers_2014$cpf] # Filtro IR
pre_reform_dt <- pre_reform_dt[initial_income_2014 > 0 & income_2015 > 0]
pre_reform_dt[, delta_log_income := log(income_2015) - log(initial_income_2014)]
setnames(pre_reform_dt, "initial_income_2014", "initial_income")

# --- Base Pós-Reforma (2015 -> 2016) ---
post_reform_dt <- merge(dt_2015, dt_2016, by = "cpf")
non_payers_2015 <- ir_status_dt[year == 2015 & paid_ir_in_year == 0, .(cpf)]
post_reform_dt <- post_reform_dt[cpf %in% non_payers_2015$cpf] # Filtro IR
post_reform_dt <- post_reform_dt[income_2015 > 0 & income_2016 > 0]
post_reform_dt[, delta_log_income := log(income_2016) - log(income_2015)]
setnames(post_reform_dt, "income_2015", "initial_income")

# --- 2. Normalização via Regressão (Eq. 11) ---
cat("ETAPA 2: Normalizando diferenciais de tendência via regressão (Eq. 11)...\n")

# Função para rodar a regressão e extrair coeficientes
run_normalization_reg <- function(data, bin_width, ref_bin_value) {
  # Filtra para a faixa de renda de interesse ANTES da regressão
  data <- data[initial_income >= 1700 & initial_income <= 2100]
  
  # Cria bins
  data[, income_bin := cut(initial_income, 
                           breaks = seq(1700, 2100 + bin_width, by = bin_width), 
                           labels = FALSE)]
  data[, income_bin_midpoint := 1700 + (income_bin - 1) * bin_width + (bin_width/2)]
  data <- data[!is.na(income_bin_midpoint)]
  
  # Converte para fator e define referência
  data[, income_bin_factor := factor(income_bin_midpoint)]
  data[, income_bin_factor := relevel(income_bin_factor, ref = as.character(ref_bin_value))]
  
  # Roda a regressão
  est <- feols(delta_log_income ~ i(income_bin_factor), data = data)
  
  # Extrai coeficientes
  coeffs_dt <- as.data.table(coeftable(est), keep.rownames = "term")
  coeffs_dt <- coeffs_dt[grepl("income_bin_factor::", term)]
  coeffs_dt[, income_bin_midpoint := as.numeric(gsub(".*::", "", term))]
  
  # Adiciona o ponto de referência (coeficiente = 0)
  ref_point_dt <- data.table(income_bin_midpoint = ref_bin_value, Estimate = 0)
  
  plot_data <- rbindlist(list(coeffs_dt[, .(income_bin_midpoint, Estimate)], ref_point_dt), use.names = TRUE)
  setorder(plot_data, income_bin_midpoint)
  
  return(plot_data)
}

# Define parâmetros comuns (harmonizados com script 03)
BIN_WIDTH <- 10
REF_BIN_VALUE <- floor(IR_LIMIT_BASE_YEAR / BIN_WIDTH) * BIN_WIDTH + (BIN_WIDTH/2)
cat("   - Largura do Bin:", BIN_WIDTH, "\n")
cat("   - Bin de Referência (ponto médio):", REF_BIN_VALUE, "\n")

# Roda para os dois períodos
pre_reform_coeffs <- run_normalization_reg(pre_reform_dt, BIN_WIDTH, REF_BIN_VALUE)
pre_reform_coeffs[, period := "Pre-reform (2014-2015)"]

post_reform_coeffs <- run_normalization_reg(post_reform_dt, BIN_WIDTH, REF_BIN_VALUE)
post_reform_coeffs[, period := "Post-reform (2015-2016)"]

# Combina para o gráfico
plot_data_normalized <- rbindlist(list(pre_reform_coeffs, post_reform_coeffs))
setnames(plot_data_normalized, "Estimate", "beta_coefficient")

# --- 3. Geração do Gráfico (Réplica da Figura 3B Harmonizada) ---
cat("ETAPA 3: Gerando o gráfico normalizado e harmonizado...\n")

fig3b_harmonized <- ggplot(plot_data_normalized, aes(x = income_bin_midpoint, y = beta_coefficient, color = period)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "grey50") +
  geom_vline(xintercept = IR_LIMIT_BASE_YEAR, linetype = "dashed", color = "red", linewidth = 1) +
  coord_cartesian(xlim = c(1700, 2100)) + # ylim removido para ser flexível
  labs(
    title = "Normalized Trend Differentials (Harmonized)",
    subtitle = "Non-IR Payers, Income 1700-2100, Bin Width = 10",
    x = "Initial Income (2016 Prices)",
    y = "Trend Differential (β_1t coefficient)",
    color = "Period"
  ) +
  scale_color_manual(values = c("Pre-reform (2014-2015)" = "blue", "Post-reform (2015-2016)" = "orange")) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10))

# Salva o gráfico
OUTPUT_DIR_JAKOBSEN <- "jakobsen_sogaard/output"
if (!dir.exists(OUTPUT_DIR_JAKOBSEN)) dir.create(OUTPUT_DIR_JAKOBSEN, recursive = TRUE)
PLOT_PATH <- file.path(OUTPUT_DIR_JAKOBSEN, "fig3b_replica_harmonized.png")
ggsave(PLOT_PATH, plot = fig3b_harmonized, width = 10, height = 7, dpi = 300, bg = "white")

cat("\n--- SCRIPT CONCLUÍDO ---\n")
cat("Gráfico harmonizado salvo em:", PLOT_PATH, "\n")