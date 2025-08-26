# ===========================================================================
# SCRIPT 02: FILTRAGEM E DEFINIÇÃO DE GRUPOS (VERSÃO FINAL E CORRIGIDA)
# ===========================================================================

# --- 0. Limpeza do Ambiente e Carregamento de Pacotes ---
rm(list = ls()); gc()
library(dplyr)
library(lubridate)
library(data.table)

# Carrega as configurações do projeto
source("00_config.R")

# --- 1. Carregamento dos Dados Preparados ---
cat("ETAPA 1: Carregando dados preparados de:", PREPARED_DATA_PATH, "\n")
if (!file.exists(PREPARED_DATA_PATH)) stop("Arquivo de dados preparados não encontrado. Rode o script 01 primeiro.")
prepared_dt <- readRDS(PREPARED_DATA_PATH)
setDT(prepared_dt)

# ===========================================================================
# CRIAÇÃO DA FLAG CUMULATIVA DE PAGAMENTO CONSECUTIVO DE IR
# ===========================================================================
cat("Criando flag CUMULATIVA de pagamento consecutivo de Imposto de Renda...\n")
setorder(prepared_dt, cpf, data_referencia_mes)
prepared_dt[, trigger_event := fifelse(paid_ir > 0 & shift(paid_ir, type = "lag") > 0, 1, 0, na = 0), by = .(cpf)]
prepared_dt[, has_paid_ir_consecutive := as.integer(cumsum(trigger_event) > 0), by = .(cpf)]
prepared_dt[, trigger_event := NULL]
cat("  - Variável cumulativa 'has_paid_ir_consecutive' criada com sucesso.\n")

# ===========================================================================
# FILTRO POR TIPO DE ADMISSÃO
# ===========================================================================
if (exists("FILTER_BY_ADM_TYPE") && FILTER_BY_ADM_TYPE == TRUE) {
  if (!"tipoadm" %in% names(prepared_dt)) stop("ERRO: A coluna 'tipoadm' não foi encontrada.")
  
  cpfs_with_standard_contract <- unique(prepared_dt[year == BASE_YEAR & month == 12 & tipoadm == '1', cpf])
  if (length(cpfs_with_standard_contract) == 0) warning("AVISO: Nenhum CPF encontrado com tipoadm == '1'. O filtro não removerá ninguém.")
  
  n_before <- uniqueN(prepared_dt$cpf)
  prepared_dt <- prepared_dt[cpf %in% cpfs_with_standard_contract]
  n_after <- uniqueN(prepared_dt$cpf)
  
  cat("  - CPFs antes do filtro 'tipoadm':", n_before, "\n")
  cat("  - CPFs encontrados com vínculo padrão:", length(cpfs_with_standard_contract), "\n")
  cat("  - CPFs restantes após o filtro:", n_after, "\n")
  rm(cpfs_with_standard_contract, n_before, n_after)
  gc()
} else {
  cat("ETAPA 1.5: Filtro por tipo de admissão (tipoadm) está DESLIGADO.\n")
}

# =================================================================================
# --- ETAPA 2: FILTRAGEM POR HISTÓRICO DE PAGAMENTO DE IR ---
# =================================================================================
cat("ETAPA 2: Aplicando filtro por histórico de pagamento de IR (Modo:", IR_FILTER_MODE, ")...\n")

pre_event_data <- prepared_dt[year < BASE_YEAR + 1]

if (IR_FILTER_MODE == 'nunca_pagou') {
  # --- MODO (a): Mantém quem NUNCA pagou IR antes de 2016 ---
  cpfs_to_remove <- unique(pre_event_data[paid_ir == 1, cpf])
  cat("  - Modo 'nunca_pagou': Removendo", length(cpfs_to_remove), "CPFs que já pagaram IR antes de 2016.\n")
  filtered_dt <- prepared_dt[!(cpf %in% cpfs_to_remove)]
  
} else if (IR_FILTER_MODE == 'pagou_pouco') {
  
  # 1. Calcula quantas vezes cada CPF pagou IR no período pré-evento
  ir_payment_counts <- pre_event_data[paid_ir == 1, .N, by = cpf]
  setnames(ir_payment_counts, "N", "ir_payments_count")
  
  # Pega todos os CPFs do período pré-evento para garantir que quem pagou 0 vezes seja incluído
  all_cpfs_pre_event <- data.table(cpf = unique(pre_event_data$cpf))
  
  # Junta a contagem de pagamentos. Quem não pagou terá NA.
  ir_payment_counts <- merge(all_cpfs_pre_event, ir_payment_counts, by = "cpf", all.x = TRUE)
  
  # Substitui NA por 0 (quem não está na contagem original é porque pagou 0 vezes)
  ir_payment_counts[is.na(ir_payments_count), ir_payments_count := 0]
  
  # 2. Identifica os CPFs que devem ser MANTIDOS na amostra
  cpfs_to_keep <- ir_payment_counts[
    ir_payments_count == IR_FILTER_MAX_PAYMENTS, 
    cpf
  ]
  
  cat("  - Encontrados", length(cpfs_to_keep), "CPFs que atendem filtro de pagou_pouco.\n")
  # ================================================================================
  
  # 3. Filtra a base principal, mantendo APENAS quem está na lista
  filtered_dt <- prepared_dt[cpf %in% cpfs_to_keep]
  
} else if (IR_FILTER_MODE == 'sem_filtro') {
  # --- MODO (c): Sem filtro por histórico de IR ---
  cat("  - Modo 'sem_filtro': Nenhum filtro por histórico de IR será aplicado.\n")
  filtered_dt <- prepared_dt
  
} else {
  stop("ERRO: 'IR_FILTER_MODE' desconhecido: '", IR_FILTER_MODE, "'. Verifique 00_config.R.")
}

rm(pre_event_data)
if (exists("cpfs_to_remove")) rm(cpfs_to_remove)
if (exists("ir_payment_counts")) rm(ir_payment_counts)
if (exists("cpfs_to_keep")) rm(cpfs_to_keep)
gc()

cat("  - CPFs restantes na amostra após filtro de IR:", uniqueN(filtered_dt$cpf), "\n")

# --- 3. Definição dos Grupos de Tratamento e Controle ---
cat("ETAPA 3: Definindo grupos de T/C com base na renda do VÍNCULO PRINCIPAL em Dezembro de", BASE_YEAR, "...\n")
cat("   - Usando a coluna '", SALARY_COL_FOR_GROUPS, "' para definição dos grupos.\n", sep="")

base_month_data <- filtered_dt[year == BASE_YEAR & month == 12 & !is.na(id_vinculo_original)]
setorder(base_month_data, cpf, -salcontr)
principal_jobs_dt <- unique(base_month_data, by = "cpf")[, .(cpf, id_vinculo_original)]
base_salaries_dt <- filtered_dt[principal_jobs_dt, on = .(cpf, id_vinculo_original), nomatch = 0]
base_salaries_dt <- base_salaries_dt[year == BASE_YEAR & month == 12]
base_salaries_dt[, base_salary_metric := get(SALARY_COL_FOR_GROUPS)]
base_salaries_dt <- base_salaries_dt[!is.na(base_salary_metric), .(cpf, base_salary_metric)]

treatment_upper_thresh <- IR_LIMIT_BASE_YEAR
treatment_lower_thresh <- IR_LIMIT_BASE_YEAR * (1 - TREATMENT_PERCENT_BELOW_LIMIT)
control_upper_thresh <- treatment_lower_thresh * (1 - CONTROL_GAP_PERCENT)
control_lower_thresh <- control_upper_thresh - (treatment_lower_thresh * CONTROL_WIDTH_PERCENT)

cat("  - Limiar de isenção:", IR_LIMIT_BASE_YEAR, "\n")
cat("  - Faixa de Tratamento: [", round(treatment_lower_thresh, 2), ",", round(treatment_upper_thresh, 2), ")\n")
cat("  - Faixa de Controle:   [", round(control_lower_thresh, 2), ",", round(control_upper_thresh, 2), ")\n")

base_salaries_dt[, assigned_group := case_when(
  base_salary_metric >= treatment_lower_thresh & base_salary_metric < treatment_upper_thresh ~ "Treatment",
  base_salary_metric >= control_lower_thresh & base_salary_metric < control_upper_thresh ~ "Control",
  TRUE ~ "Other"
)]

initial_groups_dt <- base_salaries_dt[assigned_group %in% c("Treatment", "Control")]
n_treatment <- nrow(initial_groups_dt[assigned_group == "Treatment"])
n_control <- nrow(initial_groups_dt[assigned_group == "Control"])
cat("  -", n_treatment, "indivíduos definidos como Tratamento.\n")
cat("  -", n_control, "indivíduos definidos como Controle.\n")

if (n_treatment == 0 || n_control == 0) {
  stop("Um dos grupos (Tratamento ou Controle) ficou vazio. Verifique os parâmetros de definição de grupo.")
}

# --- 4. Preparação e Salvamento do Dataset Final ---
cat("ETAPA 4: Preparando e salvando o dataset final para pareamento...\n")
final_data_for_matching <- filtered_dt[cpf %in% initial_groups_dt$cpf]
final_data_for_matching[initial_groups_dt, on = "cpf", assigned_group := i.assigned_group]


# --- ETAPA 5: Cálculo Proporcional das Horas Trabalhadas ---
cat("ETAPA 5: Calculando horas proporcionais aos dias trabalhados no mês...\n")

final_data_for_matching[, `:=` (
  dtadmissao = as.Date(dtadmissao),
  data_desligamento_efetiva = as.Date(data_desligamento_efetiva)
)]

final_data_for_matching[, horas_ajustadas := {
  inicio_mes <- floor_date(data_referencia_mes, "month")
  fim_mes <- ceiling_date(data_referencia_mes, "month") - days(1)
  data_inicio_efetiva <- pmax(dtadmissao, inicio_mes, na.rm = TRUE)
  data_fim_efetiva <- pmin(data_desligamento_efetiva, fim_mes, na.rm = TRUE)
  dias_trabalhados_no_mes <- as.numeric(data_fim_efetiva - data_inicio_efetiva) + 1
  proporcao_mes <- dias_trabalhados_no_mes / as.numeric(days_in_month(data_referencia_mes))
  horascontr * proporcao_mes
}, by = 1:nrow(final_data_for_matching)]

saveRDS(final_data_for_matching, file = FILTERED_DATA_PATH)

cat("\n--- SCRIPT 02 CONCLUÍDO ---\n")
cat("Dados filtrados e com grupos definidos salvos com sucesso em:", FILTERED_DATA_PATH, "\n")
cat("Total de linhas:", nrow(final_data_for_matching), "| Total de CPFs para pareamento:", uniqueN(final_data_for_matching$cpf), "\n")
gc()