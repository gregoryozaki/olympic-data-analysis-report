###############################################################################
# P I P E L I N E C O M P L E T O · DADOS DAS OLIMPÍADAS
# -----------------------------------------------------------------------------
# OBJETIVO:
# Demonstrar, em apenas 2 scripts, todo o fluxo de:
#1. BAIXAR → salvar em "dados/"
#2. ANALISAR → ler de "dados/"
#
# Este código é 100 % re-executável.
###############################################################################

# ===================== ETAPA 1 - DOWNLOAD ==================================
# Execute SÓ uma vez — ou sempre que quiser atualizar a base local.
# -----------------------------------------------------------------------------

## ===================== 1. Pacotes ============================================
# Carrega pacotes necessários para download, queries e salvamento

library(basedosdados)# Acessar e executar queries SQL no BigQuery
library(bigrquery)# Autenticação de login e credenciais no Google Cloud
library(readr) # Leitura e escrita rápida de arquivos CSV

# -----------------------------------------------------------------------------
# 1.0 Autenticação no Google Cloud
# -----------------------------------------------------------------------------

# Abre janela de login do Google para autenticação na BasedosDados
# Se já autenticado e com credenciais em cache, não pedirá novamente
bq_auth()

# Define o projeto de billing no Google Cloud.
# Substitua "gds_projetofinal-olimpiadas" pelo ID do seu projeto se for diferente.
basedosdados::set_billing_id("g05projetofinal-olimpiadas")

# -----------------------------------------------------------------------------
# 1.1 Cria pasta local "dados/" se não existir
# -----------------------------------------------------------------------------

dir.create("dados", showWarnings = FALSE)# Cria a pasta "dados" caso não exista

# -----------------------------------------------------------------------------
# 1.2 Download dos Microdados das Olimpíadas
# -----------------------------------------------------------------------------

# Define a query SQL que será executada na BasedosDados via BigQuery
# Seleciona as variáveis que você escolheu
query_olimpiadas <- "
SELECT
    ano,
    pais,
    sexo,
    idade,
    altura,
    peso,
    esporte,
    medalha
FROM `basedosdados.mundo_kaggle_olimpiadas.microdados`
"

# Executa a query SQL no BigQuery e salva o resultado como CSV em "dados/olimpiadas.csv"
cat("Baixando dados das Olimpíadas do BigQuery...\n")
basedosdados::read_sql(query_olimpiadas,
                       billing_project_id = basedosdados::get_billing_id()) |>
  write_csv("dados/olimpiadas.csv")

# -----------------------------------------------------------------------------
# Mensagem final de conclusão
# -----------------------------------------------------------------------------

cat("✅  Etapa 1 concluída — base de dados das Olimpíadas salva em 'dados/olimpiadas.csv'.\n")
