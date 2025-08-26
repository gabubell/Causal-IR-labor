# Estimação causal do efeito de pagar IR sobre mercado de trabalho

## Estrutura dos scripts:

a) stata/enfermagem_sp_coleta.do
- Organiza a coleta no servidor da UFPE dos dados da RAIS de interesse: aux/tec enfermagem para estado de São Paulo

b) tidy_rais_dt.R
- Realiza algumas correções na base de dados
- Cria algumas novas variáveis
- Objetivo principal: transformar os dados de 'wide' para 'long', ie., uma linha para cada cpf-vinculo-mes

c) 00_config.R
- Definie parâmetros a serem utilizados nos demais scripts

d) 01_prepare_data.R
- Cria as variáveis principais, incluindo o status de pagar IR com base no salário descontado INSS

e) 02_filter_and_groups.R
- Realiza filtragem conforme pré-definição 
- Cria grupo de tratado e controle

f) 03_matching.R & 03b_table_matching.R
- Realiza o paramento exato e similar
- Cria tabela descritiva da qualidade do matching

g) 04_agg_data.R
- Converte o formato de linha por cpf-vínculo-mes para cpf-mes necessário à estimação econométrica

h) 05_estimate_eventStudy.R
- Estima o EV, cria gráfico e salva os dados

i) 06_estimate_kinkEV.R
- Estima o Kink EV, cria gráfico e salva os dados

j) compare_plots.R
- Cria gráfico comparando o EV com kinkEV por grupo (nunca pagou e pagou 1x)

k) plot_1stg.R
- Gráfico do primeiro estágio para pontual, uma vez, duas vezes consecutivas

l) plot_ir_desc.R
- Gráfico descritivo do pagamento de IR