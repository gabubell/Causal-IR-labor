# ===========================================================================
# SCRIPT 03 (Jakobsen & Sogaard): REPLICAÇÃO DA FIGURA 3C (ZOOM E FILTRO IR, BIN=10)
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
pre_reform_dt <- pre_reform_dt[cpf %in% non_payers_2014$cpf]
pre_reform_dt <- pre_reform_dt[initial_income_2014 > 0 & income_2015 > 0]
pre_reform_dt[, delta_log_income := log(income_2015) - log(initial_income_2014)]
pre_reform_dt[, period := "Pre-reform"]
setnames(pre_reform_dt, "initial_income_2014", "initial_income")

# --- Base Pós-Reforma (2015 -> 2016) ---
post_reform_dt <- merge(dt_2015, dt_2016, by = "cpf")
non_payers_2015 <- ir_status_dt[year == 2015 & paid_ir_in_year == 0, .(cpf)]
post_reform_dt <- post_reform_dt[cpf %in% non_payers_2015$cpf]
post_reform_dt <- post_reform_dt[income_2015 > 0 & income_2016 > 0]
post_reform_dt[, delta_log_income := log(income_2016) - log(income_2015)]
post_reform_dt[, period := "Post-reform"]
setnames(post_reform_dt, "income_2015", "initial_income")

# Combina as duas bases
reg_data_full <- rbindlist(list(pre_reform_dt[, .(cpf, initial_income, delta_log_income, period)], 
                                post_reform_dt[, .(cpf, initial_income, delta_log_income, period)]))

# --- 2. Filtro de Renda e Binning ---
cat("ETAPA 2: Filtrando por renda (1700-2100) e criando bins (width=10)\n")

# Filtra para a faixa de renda de interesse
reg_data_zoom <- reg_data_full[initial_income >= 1700 & initial_income <= 2100]
cat("   - Observações na faixa de renda:", nrow(reg_data_zoom), "\n")

# Cria bins de renda inicial
BIN_WIDTH <- 10
reg_data_zoom[, income_bin := cut(initial_income, 
                                  breaks = seq(1700, 2100 + BIN_WIDTH, by = BIN_WIDTH), 
                                  labels = FALSE)]
reg_data_zoom[, income_bin_midpoint := 1700 + (income_bin - 1) * BIN_WIDTH + (BIN_WIDTH/2)]
reg_data_zoom <- reg_data_zoom[!is.na(income_bin_midpoint)]

# --- 3. Estimação da Regressão (Eq. 12) ---
cat("ETAPA 3: Estimando a regressão da Eq. 12...\n")

# Define os fatores e suas referências ANTES da regressão
reg_data_zoom[, period := factor(period, levels = c("Pre-reform", "Post-reform"))]
reg_data_zoom[, income_bin_factor := factor(income_bin_midpoint)]

# IMPORTANTE: Defina a referência do bin aqui
ref_bin_value <- floor((IR_LIMIT_BASE_YEAR) / BIN_WIDTH) * BIN_WIDTH + (BIN_WIDTH/2)
ref_bin <- as.character(ref_bin_value)
cat("   - Bin de referência:", ref_bin, "\n")

reg_data_zoom[, income_bin_factor := relevel(income_bin_factor, ref = ref_bin)]

# Roda a regressão com a fórmula de interação correta (period * income_bin_factor)
# Isso se expande para: period + income_bin_factor + period:income_bin_factor
est_eq12 <- feols(delta_log_income ~ period * income_bin_factor,
                  data = reg_data_zoom,
                  cluster = ~cpf)
summary(est_eq12)

# --- 4. Extração e Preparação dos Coeficientes para o Gráfico ---
cat("ETAPA 4: Extraindo coeficientes para o gráfico...\n")

results_dt <- as.data.table(coeftable(est_eq12), keep.rownames = "term")

# AGORA, os termos de interação representarão o delta_3 (a diferença)
# O padrão grepl mudará ligeiramente porque 'feols' usa ':' para interações
interaction_coeffs <- results_dt[grepl("periodPost-reform:income_bin_factor", term)]


# O resto do seu código de extração deve funcionar...
interaction_coeffs[, income_bin_midpoint := as.numeric(gsub(".*factor", "", term))]
setnames(interaction_coeffs, c("Estimate", "Std. Error"), c("estimate", "std_error"))

# Adiciona o ponto de referência (que agora é 0 por definição)
ref_point_dt <- data.table(income_bin_midpoint = ref_bin_value, estimate = 0, std_error = 0)
plot_coeffs <- rbindlist(list(interaction_coeffs[, .(income_bin_midpoint, estimate, std_error)], ref_point_dt), fill = TRUE)
plot_coeffs <- plot_coeffs[!is.na(income_bin_midpoint)]
setorder(plot_coeffs, income_bin_midpoint)

# --- 5. Geração do Gráfico ---
cat("ETAPA 5: Gerando o gráfico dos coeficientes...\n")

fig3c_zoom <- ggplot(plot_coeffs, aes(x = income_bin_midpoint, y = estimate)) + 
  geom_hline(yintercept = 0, color = "grey50", linetype = "dashed") + 
  geom_ribbon(aes(ymin = estimate - 1.96 * std_error, ymax = estimate + 1.96 * std_error), alpha = 0.2, fill = "lightblue") + 
  geom_line(color = "blue") + 
  geom_point(color = "blue", size = 2.5) + 
  geom_vline(xintercept = IR_LIMIT_BASE_YEAR, linetype = "dashed", color = "red") + 
  coord_cartesian(xlim = c(1700, 2100)) + 
  labs(
    title = "Changes in Trend Differentials around Cutoff (Non-IR Payers)",
    subtitle = "Income range 1700-2100, Bin Width = 10",
    x = "Initial Income (2016 Prices)",
    y = "Change in Trend Differential (δ'3)"
  ) + 
  theme_minimal(base_size = 14) + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10))

# Salva o gráfico
OUTPUT_DIR_JAKOBSEN <- "jakobsen_sogaard/output"
if (!dir.exists(OUTPUT_DIR_JAKOBSEN)) dir.create(OUTPUT_DIR_JAKOBSEN, recursive = TRUE)
PLOT_PATH <- file.path(OUTPUT_DIR_JAKOBSEN, "fig3c_replica_zoom_non_ir_bin10.png")
ggsave(PLOT_PATH, plot = fig3c_zoom, width = 10, height = 7, dpi = 300, bg = "white")

cat("\n--- SCRIPT CONCLUÍDO ---
")
cat("Gráfico salvo em:", PLOT_PATH, "\n")
