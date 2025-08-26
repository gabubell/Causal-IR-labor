# ===========================================================================
# SCRIPT 03: EXECUÇÃO DO PAREAMENTO (VERSÃO CONDICIONAL)
# ===========================================================================

# --- 0. Limpeza, Pacotes e Configuração ---
rm(list = ls()); gc()
library(data.table)
library(MatchIt)
source("00_config.R")

# --- 1. Carregamento dos Dados ---
cat("ETAPA 1: Carregando dados filtrados...\n")
filtered_dt <- readRDS(FILTERED_DATA_PATH)
# =======================================================================================
# ETAPA 2: Preparação com Perfil Mensal de Empregos no Pareamento EXATO
# =======================================================================================
cat("ETAPA 2: Preparando base de dados para o pareamento (1 linha por CPF)...\n")
BASE_YEAR <- 2015

# --- 2a. Dados de Dezembro/2015: Vínculo Principal e Variáveis Escalares ---
cat("   - Agregando dados de Dez/2015 para variáveis escalares...\n")
dec_2015_dt <- filtered_dt[year == BASE_YEAR & month == 12]
base_vars_dt <- dec_2015_dt[order(cpf, -salario_vinculo_mes_bruto), .(
  assigned_group = first(assigned_group), municipio = first(municipio),
  cbo_5digits = first(cbo_5digits), cnae_secao = first(cnae_secao),
  education_level_grouped = first(education_level_grouped), tempempr = as.numeric(first(tempempr)),
  salcontr_total_2014 = first(salcontr_total_2014)
), by = cpf]
cat("      - Base com variáveis escalares criada para", nrow(base_vars_dt), "indivíduos.\n")


# --- 2b. Perfil Mensal Agregado para o Ano Inteiro de 2015 ---
cat("   - Criando perfil mensal agregado para todo o ano de 2015...\n")

monthly_agg_dt <- filtered_dt[year == BASE_YEAR, .(
  total_relative_complement_salary = first(total_relative_complement_salary),
  paid_ir_monthly = max(paid_ir, na.rm = TRUE),
  num_jobs_monthly =  sum(!is.na(id_vinculo_original)),
  total_hours_monthly = sum(horas_ajustadas, na.rm = TRUE)
), by = .(cpf, month)]

monthly_profile_wide <- dcast(
  monthly_agg_dt,
  cpf ~ month,
  value.var = c("total_relative_complement_salary", "paid_ir_monthly", "num_jobs_monthly", "total_hours_monthly"),
  fill = NA
)

salary_names <- paste0("var_sal_m", 1:12)
ir_names <- paste0("paid_ir_m", 1:12)
jobs_names <- paste0("jobs_m", 1:12)
hours_names <- paste0("hours_m", 1:12)
setnames(monthly_profile_wide, names(monthly_profile_wide)[-1], c(salary_names, ir_names, jobs_names, hours_names))

na_dummy_names <- paste0(salary_names, "_is_na")
monthly_profile_wide[, (na_dummy_names) := lapply(.SD, function(x) as.integer(is.na(x))), .SDcols = salary_names]

monthly_profile_wide[, (salary_names) := lapply(.SD, function(x) fcoalesce(x, 0)), .SDcols = salary_names]

monthly_profile_wide[, (ir_names) := lapply(.SD, function(x) fcoalesce(x, 0)), .SDcols = ir_names]

monthly_profile_wide[, (jobs_names) := lapply(.SD, function(x) fcoalesce(x, 0L)), .SDcols = jobs_names]
monthly_profile_wide[, (hours_names) := lapply(.SD, function(x) fcoalesce(x, 0)), .SDcols = hours_names]

cat("      - Perfil mensal agregado criado.\n")

# --- 2c. Junção Final e Definição das Variáveis para Pareamento ---
cat("   - Juntando bases e preparando para o pareamento...\n")
data_for_matching <- merge(base_vars_dt, monthly_profile_wide, by = "cpf", all.x = TRUE)
data_for_matching[, treatment_dummy := fifelse(assigned_group == "Treatment", 1, 0)]

# Variáveis para pareamento EXATO
exact_vars_final <- c(
  PSM_EXACT_MATCH_VARS,
  na_dummy_names,      
  jobs_names,
  ir_names,
  hours_names
)

# Variáveis para pareamento por DISTÂNCIA
nearest_vars_final <- c(
  PSM_NEAREST_MATCH_VARS_SCALAR,
  salary_names
)

# Limpeza de NAs
all_vars_to_check <- c(exact_vars_final, nearest_vars_final)
data_for_matching <- na.omit(data_for_matching, cols = all_vars_to_check)

cat("   - Base de pareamento final preparada com", nrow(data_for_matching), "indivíduos únicos.\n")
cat("   - Variáveis para pareamento EXATO:", paste(exact_vars_final, collapse=", "), "\n")
cat("   - Variáveis para DISTÂNCIA:", paste(nearest_vars_final, collapse=", "), "\n")

# --- 3. LÓGICA CONDICIONAL DE PAREAMENTO ---
cat("\nETAPA 3: Executando o modo de pareamento selecionado:", PSM_MODE, "\n")

if (PSM_MODE == "sem_psm") {
  # MODO 1: SEM PAREAMENTO
  cat("--- Nenhum pareamento será executado. Usando todos os indivíduos dos grupos T/C definidos.\n")
  matched_cpfs <- data_for_matching$cpf
  
} else if (PSM_MODE == "psm_exato") {
  # MODO 2: APENAS PAREAMENTO EXATO
  cat("--- Executando apenas o pareamento EXATO...\n")
  cat("  - Variáveis de pareamento exato:", paste(exact_vars_final, collapse=", "), "\n")
  
  match_obj <- matchit(
    formula = treatment_dummy ~ 1,
    data = as.data.frame(data_for_matching),
    method = "exact",
    estimand = "ATT"
  )
  
  cat("  - Resumo do Pareamento Exato:\n")
  print(summary(match_obj, un = FALSE))
  matched_cpfs <- match.data(match_obj)$cpf
  
} else if (PSM_MODE == "psm_completo") {
  # MODO 3: PAREAMENTO COMPLETO (EXATO + DISTÂNCIA)
  cat("--- Executando o pareamento COMPLETO (Exato + Mahalanobis)...\n")
  match_formula <- as.formula(paste("treatment_dummy ~", paste(nearest_vars_final, collapse = " + ")))
  
  cat("  - Variáveis de pareamento exato:", paste(exact_vars_final, collapse=", "), "\n")
  cat("  - Variáveis para distância:", paste(nearest_vars_final, collapse=", "), "\n")
  
  match_obj <- matchit(
    formula = match_formula,
    data = as.data.frame(data_for_matching),
    method = "nearest",
    distance = "mahalanobis",
    exact = exact_vars_final,
    ratio = 1,
    replace = FALSE,
    estimand = "ATT"
  )
  
  cat("  - Resumo do Pareamento Completo:\n")
  print(summary(match_obj, un = FALSE))
  matched_cpfs <- match.data(match_obj)$cpf
  
} else {
  stop("Modo de PSM '", PSM_MODE, "' não reconhecido. Verifique 00_config.R.")
}


# --- 4. Salvamento dos Resultados ---
cat("\nETAPA 4: Salvando os resultados...\n")
saveRDS(matched_cpfs, file = MATCHED_CPFS_PATH)
cat("  - Lista de", length(matched_cpfs), "CPFs para análise salva em:", MATCHED_CPFS_PATH, "\n")
cat("\n--- SCRIPT 03 CONCLUÍDO ---\n")
gc()