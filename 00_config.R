# ===========================================================================
# 00_config.R: ARQUIVO DE CONFIGURAÇÃO PRINCIPAL
# ===========================================================================

# --- 1. Definição de Caminhos do Ambiente do Servidor ---

# Define o caminho para a sua biblioteca R customizada no servidor.
library_path <- "/dados01/gbelle/R_lib"

# Define o diretório de trabalho como a raiz do seu projeto no servidor.
setwd("/dados01/gbelle/IR/")

# Para rodar usando dados do gdrive:
# data/IR/v2/input/

# Mensagem de confirmação para garantir que os caminhos estão corretos
cat("Ambiente do Servidor Configurado:\n")
cat("  - Diretório de Trabalho:", getwd(), "\n")
cat("  - Caminho da Biblioteca:", library_path, "\n")

# --- 2. Carregamento de Pacotes Essenciais ---
tryCatch({
  library(data.table, lib.loc = library_path)
  library(lubridate, lib.loc = library_path)
  library(fixest, lib.loc = library_path)
  library(MatchIt, lib.loc = library_path)
  library(tidyr, lib.loc = library_path)
  library(ggplot2, lib.loc = library_path)
  cat("Todos os pacotes foram carregados com sucesso.\n")
}, error = function(e) {
  stop(paste("ERRO: Falha ao carregar um dos pacotes. Verifique se todos estão instalados em '", library_path, "'.\nErro original: ", e$message))
})

# --- 3. Caminhos de Arquivos (Relativos ao Projeto) ---
RAW_DATA_PATH <- "painel_enfermagem_sp.rds"

MUNICIPIO_TO_FILTER <- '355030'

cat("Caminho para os dados brutos definido como:", RAW_DATA_PATH, "\n")
cat("---------------------------------------------------\n")

# --- Caminhos de Saída (Output) ---
OUTPUT_DIR <- "output"
PREPARED_DATA_PATH <- file.path(OUTPUT_DIR, "01_prepared_data.rds")
FILTERED_DATA_PATH <- file.path(OUTPUT_DIR, "02_filtered_data.rds")
MATCH_OBJECT_PATH  <- file.path(OUTPUT_DIR, "03_match_object.rds")
MATCHED_CPFS_PATH  <- file.path(OUTPUT_DIR, "03_matched_cpfs.rds")
FINAL_MODEL_PATH   <- file.path(OUTPUT_DIR, "04_final_model.rds")
FINAL_RESULTS_PATH <- file.path(OUTPUT_DIR, "04_final_results.rds")

# --- 3. Tabela Histórica de Limites do IRPF ---
# Esta tabela define o teto de isenção para cada período.
# É usada para criar a variável 'paid_ir'.
HISTORICAL_IR_LIMITS_DT <- data.table(
  year        = c(2014, 2015, 2015, 2016, 2017),
  month_start = c(1,    1,    4,    1,    1),
  month_end   = c(12,   3,    12,   12,   12),
  exemption_limit = c(
    1787.77, # Vigente pela Lei 12.469/2011
    1787.77, # Manteve o valor de 2014 até Março de 2015
    1903.98, # Mudança pela Lei 13.149/2015 a partir de Abril
    1903.98, # Congelamento da tabela em 2016
    1903.98  # Congelamento da tabela em 2017
  )
)

# --- Tabela Histórica de Contribuição do INSS ---

INSS_CONTRIBUTION_TABLE_DT <- rbindlist(list(
  # Período 1: Jan/2014 a Fev/2015
  data.table(year = 2014, month_start = 1, month_end = 12, 
             salary_floor = c(0, 1317.08, 2195.13), 
             salary_ceiling = c(1317.07, 2195.12, 4390.24), 
             rate = c(0.08, 0.09, 0.11)),
  data.table(year = 2015, month_start = 1, month_end = 2, 
             salary_floor = c(0, 1317.08, 2195.13), 
             salary_ceiling = c(1317.07, 2195.12, 4390.24), 
             rate = c(0.08, 0.09, 0.11)),
  
  # Período 2: Mar/2015 a Dez/2015
  data.table(year = 2015, month_start = 3, month_end = 12,
             salary_floor = c(0, 1399.13, 2331.89), 
             salary_ceiling = c(1399.12, 2331.88, 4663.75), 
             rate = c(0.08, 0.09, 0.11)),
  
  # Período 3: 2016
  data.table(year = 2016, month_start = 1, month_end = 12,
             salary_floor = c(0, 1556.95, 2594.93), 
             salary_ceiling = c(1556.94, 2594.92, 5189.82), 
             rate = c(0.08, 0.09, 0.11)),
  
  # Período 4: 2017
  data.table(year = 2017, month_start = 1, month_end = 12,
             salary_floor = c(0, 1659.39, 2765.67), 
             salary_ceiling = c(1659.38, 2765.66, 5531.31), 
             rate = c(0.08, 0.09, 0.11))
))

# Expande a tabela para ter uma linha por mês-faixa, facilitando o join
INSS_TABLE_EXPANDED <- INSS_CONTRIBUTION_TABLE_DT[, .(month = seq(month_start, month_end)), by = .(year, salary_floor, salary_ceiling, rate)]
INSS_TABLE_EXPANDED[, teto_contribuicao := max(salary_ceiling), by = .(year, month)]

# --- 4. Parâmetros da Análise ---
EVENT_DATE_STR <- "2016-01"
BASE_YEAR <- 2015

# --- 5. Parâmetros de Definição de Grupos ---
SALARY_COL_FOR_GROUPS <- "taxable_salary"
IR_LIMIT_BASE_YEAR <- 1903.98

# Grupo de Tratamento: até 6.5% abaixo do limite
TREATMENT_PERCENT_BELOW_LIMIT <- 0.065
# Grupo de Controle: faixa de 5.5% de largura, com um gap de 1%
# (ou seja, entre 7.5% e 13% abaixo do limite)
CONTROL_GAP_PERCENT <- 0.01
CONTROL_WIDTH_PERCENT <- 0.065

# Se TRUE, mantém na amostra APENAS os CPFs que tinham pelo menos um
# vínculo com tipoadm == '1' (CLT Padrão / Prazo Indeterminado) em Dez/2015.
FILTER_BY_ADM_TYPE <- FALSE

# Controla como a amostra é filtrada com base no histórico de pagamento de IR
# antes do ano do evento (BASE_YEAR + 1).
# Opções válidas:
#   - 'nunca_pagou': Mantém apenas quem NUNCA pagou IR antes (comportamento atual).
#   - 'pagou_pouco': Mantém quem pagou IR no máximo 'IR_FILTER_MAX_PAYMENTS' vezes.
#   - 'sem_filtro':  Não aplica nenhum filtro por histórico de IR.
IR_FILTER_MODE <- 'nunca_pagou'

# Limite para o modo 'pagou_pouco'. Só é usado se IR_FILTER_MODE == 'pagou_pouco'.
IR_FILTER_MAX_PAYMENTS <- 1

# --- 6. Parâmetros do Pareamento (Mahalanobis) ---
# Variáveis para pareamento EXATO
PSM_EXACT_MATCH_VARS <- c(
  'municipio',
  "cbo_5digits",
  #'horascontr',
  "cnae_secao"
  #"education_level_grouped"
)

# Variáveis para pareamento por DISTÂNCIA
PSM_NEAREST_MATCH_VARS_SCALAR <- c("tempempr")

# Event-study
# --- DEFINA AQUI TODAS AS SUAS VARIÁVEIS DE RESULTADO (Y) ---
OUTCOME_VARIABLES <- c(
  "paid_ir",
  'started_job',
  "left_job",
  'asked_to_leave',
  'retired_termination',
  'retired_without',
  'horascontr',
  "changed_job",
  'num_empregos_cpf_mes',
  "cbo_change_in_company",
  "cbo_change_at_start",
  'cumulative_unemployment',
  'permanent_exit',
  'salario_total_cpf_mes',
  'sal_hra',
  'taxable_salary', 
  'var_contract',
  'var_sal',
  'var_sal_tot',
  # Acumulados
  'has_paid_ir',
  'has_paid_ir_consecutive',
  'has_started_job',
  'has_left_job',
  'has_asked_to_leave',
  'has_changed_job',
  'has_cbo_change_at_start',
  'has_cbo_change_in_company',
  'has_retired_termination',
  'has_retired_without'
)

# DiD
# OUTCOME_VARIABLES <- c(
#   'cumulative_unemployment',
#   'permanent_exit',
#   'horascontr',
#   'taxable_salary',
#   'salario_total_cpf_mes',
#   'sal_hra',
#   'var_contract',
#   'var_sal',
#   'var_sal_tot',
#   # Acumulados
#   'has_paid_ir',
#   'has_started_job',
#   'has_left_job',
#   'has_asked_to_leave',
#   'has_changed_job',
#   'has_cbo_change_at_start',
#   'has_cbo_change_in_company',
#   'has_retired_termination',
#   'has_retired_without'
# )

# --- DE-PARA: Nomes das Variáveis para Rótulos em Português ---
# Usado no script 05_plot_results.R para gerar títulos amigáveis.
PLOT_LABELS_PT <- list(
  "paid_ir" = "Prob. de Pagar IR",
  "started_job" = "Prob. de Iniciar um Emprego",
  "left_job" = "Prob. de Deixar um Emprego (Qualquer Saída)",
  "asked_to_leave" = "Prob. de Pedir Demissão",
  "retired_termination" = "Prob. de Aposentadoria com Rescisão",
  "retired_without" = "Prob. de Aposentadoria sem Rescisão",
  "changed_job" = "Prob. de Trocar de Emprego",
  "num_empregos_cpf_mes" = "Número de Empregos",
  "cbo_change_in_company" = "Prob. de Mudar de Função (Mesma Empresa)",
  "cbo_change_at_start" = "Prob. de Mudar de Função (ao Trocar de Empresa)",
  "cumulative_unemployment" = "Meses Acumulados de Desemprego",
  "permanent_exit" = "Prob. de Saída Permanente do Mercado de Trabalho",
  "var_sal" = "Variação Anual do Salário (Vínculo)",
  "var_sal_tot" = "Variação Anual do Salário (Total)",
  'taxable_salary' = 'Salário Líquido de INSS',
  'salario_total_cpf_mes' = 'Salário Bruto Total',
  'sal_hra' = 'Salário Efetivo por Hora Contratual',
  
  # Acumulados
  "has_paid_ir" = "Prob. Acumulada de Ter Pago IR",
  "has_started_job" = "Prob. Acumulada de Ter Iniciado Emprego",
  "has_left_job" = "Prob. Acumulada de Ter Deixado Emprego",
  "has_asked_to_leave" = "Prob. Acumulada de Ter Pedido Demissão",
  "has_changed_job" = "Prob. Acumulada de Ter Trocado de Emprego",
  'has_cbo_change_at_start' = "Prob. Acumulada de Ter Mudado de função (ao Trocar de empresa)",
  "has_cbo_change_in_company" = "Prob. Acumulada de Ter Mudado de Função (Mesma Empresa)",
  "has_retired_termination" = "Prob. Acumulada de Aposentadoria com Rescisão",
  "has_retired_without" = "Prob. Acumulada de Aposentadoria sem Rescisão"
)

REFERENCE_PERIOD <- -1 # Mês de referência para o event study

# Janela do event study (em meses relativos ao choque)
EVENT_WINDOW_START <- -12
EVENT_WINDOW_END <- 11

# --- 8. CONFIGURAÇÃO DO MODO DE PAREAMENTO (PARA DiD TRADICIONAL) ---
# Define qual método de pareamento será usado.
# Opções válidas:
#   - "sem_psm": Apenas define os grupos por faixa de renda, sem pareamento.
#   - "psm_exato": Realiza apenas o pareamento exato nas variáveis definidas.
#   - "psm_completo": Realiza o pareamento exato E o pareamento por distância (Mahalanobis).
PSM_MODE <- "psm_completo"
