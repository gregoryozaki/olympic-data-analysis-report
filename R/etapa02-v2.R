###############################################################################
# PROJETO FINAL – ANÁLISE DE DADOS DAS OLIMPÍADAS – ANÁLISE COMPLETA
# Tema: Perfis de Atletas, Desempenho em Medalhas e Tendências Históricas
# Objetivos:
# • Explorar dados reais com EDA, outliers e gráficos
# • Calcular e analisar IMC
# • Realizar testes de hipótese sobre grupos de atletas e proporções de medalhas
# • Identificar características de atletas medalhistas
# • Analisar tendências históricas e por categoria de esporte
# • Apresentar números arredondados para melhor leitura e visualização
###############################################################################

# ===================== ETAPA 2 - ANÁLISE COMPLETA =========================

## ===================== 1. Carregamento de pacotes ===========================
# Carrega os pacotes necessários para análise estatística e visualização

library(dplyr)      # Manipulação de dados (filter, mutate, group_by, etc)
library(readr)      # Leitura de arquivos CSV
library(ggplot2)    # Gráficos
library(DT)         # Tabelas interativas
library(skimr)      # Análise exploratória de dados (EDA)
library(psych)      # Estatísticas descritivas detalhadas (describeBy)
library(effsize)    # Cálculo do tamanho de efeito (Cohen's d)
library(tidyr)      # Para manipulação de dados (e.g., pivot_longer)
library(forcats)    # Para trabalhar com fatores (ordenar níveis)
library(stringr)    # Para manipulação de strings (útil para limpeza ou padronização)
library(RColorBrewer) # Oferece paletas de cores bonitas para os gráficos.


# -----------------------------------------------------------------------------
# 2.1 Ler bases salvas (cache local)
# -----------------------------------------------------------------------------

# Lê a base de dados das Olimpíadas salva como CSV no pipeline Etapa 1
dados_olimpiadas <- read_csv("dados/olimpiadas.csv", show_col_types = FALSE)

# Verifica a estrutura do dataframe antes da limpeza
cat("Estrutura inicial dos dados (após leitura):\n")
glimpse(dados_olimpiadas)

# -----------------------------------------------------------------------------
# 2.2 Limpeza e Criação de Novas Variáveis
# -----------------------------------------------------------------------------

# TRATAMENTO CORRETO DA COLUNA 'MEDALHA' (mantido do segundo código):
# Mapear os valores originais ("Gold", "Silver", "Bronze", NA)
# para os níveis de fator desejados ("Ouro", "Prata", "Bronze", "Sem Medalha").
dados_olimpiadas <- dados_olimpiadas |>
  mutate(
    medalha = case_when(
      str_to_lower(medalha) == "gold" ~ "Ouro",
      str_to_lower(medalha) == "silver" ~ "Prata",
      str_to_lower(medalha) == "bronze" ~ "Bronze",
      is.na(medalha) ~ "Sem Medalha", # Trata NA diretamente
      str_to_lower(medalha) == "na" ~ "Sem Medalha", # Caso "NA" esteja como string
      TRUE ~ "Sem Medalha" # Qualquer outro valor que não seja medalha
    ),
    # Agora que a coluna 'medalha' tem os nomes de níveis corretos,
    # podemos convertê-la para fator.
    medalha = factor(medalha, levels = c("Bronze", "Prata", "Ouro", "Sem Medalha")),
    ganhou_medalha = ifelse(medalha == "Sem Medalha", "Não", "Sim")
  )

# Remover linhas com valores NA em colunas essenciais para análise (idade, altura, peso)
dados_olimpiadas <- dados_olimpiadas |>
  drop_na(idade, altura, peso)

# Calcular IMC (Índice de Massa Corporal) = peso (kg) / (altura (m))^2
# Assumindo altura em cm no dataset, convertemos para metros
dados_olimpiadas <- dados_olimpiadas |>
  mutate(
    imc = peso / (altura / 100)^2,
    # ARREDONDA IMC NO DATAFRAME para 2 casas decimais, conforme solicitado.
    imc = round(imc, 2)
  )

# Filtrar para atletas com idade, altura e peso plausíveis
# (e.g., idade > 10, altura > 100cm, peso > 20kg)
dados_olimpiadas <- dados_olimpiadas |>
  filter(idade > 10, altura > 100, peso > 20)

# CRIAÇÃO DE NOVAS COLUNAS/CATEGORIAS (incorporado do primeiro código):
# Exemplo: Criar uma coluna 'era_olimpica' baseada no ano dos jogos.
dados_olimpiadas <- dados_olimpiadas |>
  mutate(era_olimpica = case_when(
    ano < 1945 ~ "Pré-Guerra",      # Antes de 1945
    ano >= 1945 & ano < 1990 ~ "Guerra Fria", # De 1945 até 1989
    TRUE ~ "Pós-Guerra Fria"        # De 1990 em diante
  ))

# Exemplo: Agrupar esportes em categorias amplas para simplificar a análise.
dados_olimpiadas <- dados_olimpiadas |>
  mutate(categoria_esporte = case_when(
    esporte %in% c("Athletics", "Swimming", "Gymnastics", "Cycling", "Fencing", "Wrestling", "Sailing", "Shooting", "Archery", "Equestrianism") ~ "Individuais Clássicos",
    esporte %in% c("Basketball", "Football", "Volleyball", "Hockey", "Handball", "Rugby", "Water Polo", "Baseball", "Softball") ~ "Esportes Coletivos",
    TRUE ~ "Outros Esportes"
  ))

# Converta 'era_olimpica' e 'categoria_esporte' para fator para melhor uso em gráficos
dados_olimpiadas <- dados_olimpiadas |>
  mutate(
    era_olimpica = factor(era_olimpica, levels = c("Pré-Guerra", "Guerra Fria", "Pós-Guerra Fria")),
    categoria_esporte = factor(categoria_esporte, levels = c("Individuais Clássicos", "Esportes Coletivos", "Outros Esportes"))
  )


cat("\nEstrutura dos dados após limpeza, cálculo e criação de novas variáveis:\n")
glimpse(dados_olimpiadas)

# -----------------------------------------------------------------------------
# 2.3 Análise Exploratória de Dados (EDA)
# -----------------------------------------------------------------------------

cat("\n--- Análise Exploratória de Dados ---\n")

# Estatísticas descritivas gerais (skimr oferece uma visão rápida e completa)
cat("\nEstatísticas Descritivas Gerais:\n")
skim(dados_olimpiadas)

# Estatísticas descritivas por grupo de medalha (média, desvio padrão, mediana, moda)
cat("\nEstatísticas Descritivas de Idade, Altura, Peso e IMC por Ganho de Medalha:\n")
dados_olimpiadas |>
  group_by(ganhou_medalha) |>
  summarise(
    n = n(),
    idade_media = round(mean(idade, na.rm = TRUE), 2),
    idade_mediana = round(median(idade, na.rm = TRUE), 2),
    idade_sd = round(sd(idade, na.rm = TRUE), 2),
    altura_media = round(mean(altura, na.rm = TRUE), 2),
    altura_mediana = round(median(altura, na.rm = TRUE), 2),
    altura_sd = round(sd(altura, na.rm = TRUE), 2),
    peso_media = round(mean(peso, na.rm = TRUE), 2),
    peso_mediana = round(median(peso, na.rm = TRUE), 2),
    peso_sd = round(sd(peso, na.rm = TRUE), 2),
    imc_media = round(mean(imc, na.rm = TRUE), 2),
    imc_mediana = round(median(imc, na.rm = TRUE), 2),
    imc_sd = round(sd(imc, na.rm = TRUE), 2)
  ) |>
  datatable(options = list(pageLength = 5), caption = "Estatísticas por Ganho de Medalha")

# Contagem da frequência de cada tipo de medalha (Ouro, Prata, Bronze, Sem Medalha)
# (incorporado e adaptado do primeiro código)
cat("\nContagem de Medalhas por Tipo:\n")
dados_olimpiadas |>
  group_by(medalha) |>
  summarise(contagem = n()) |>
  arrange(desc(contagem)) |>
  datatable(options = list(pageLength = 5),
            caption = "Contagem de Medalhas por Tipo")

# Conta o número de medalhas totais por país, listando os Top 10.
# (incorporado e adaptado do primeiro código)
cat("\nTop 10 Países por Total de Medalhas:\n")
dados_olimpiadas |>
  filter(ganhou_medalha == "Sim") |> # Filtra apenas atletas que ganharam alguma medalha.
  group_by(pais) |>
  summarise(total_medalhas = n()) |>
  arrange(desc(total_medalhas)) |>
  head(10) |> # Seleciona apenas os 10 primeiros.
  datatable(options = list(pageLength = 10),
            caption = "Top 10 Países por Total de Medalhas")


# -----------------------------------------------------------------------------
# 2.4 Visualizações
# -----------------------------------------------------------------------------

cat("\n--- Visualizações ---\n")

# Gráfico 1: Distribuição de Idade dos Atletas
ggplot(dados_olimpiadas, aes(x = idade, fill = ganhou_medalha)) +
  geom_histogram(bins = 30, position = "identity", alpha = 0.7) +
  labs(title = "Distribuição de Idade por Ganho de Medalha", x = "Idade", y = "Frequência") +
  theme_minimal() +
  scale_fill_manual(values = c("Sim" = "skyblue", "Não" = "salmon")) +
  facet_wrap(~ ganhou_medalha)

# Gráfico 2: Distribuição de Altura dos Atletas
ggplot(dados_olimpiadas, aes(x = altura, fill = ganhou_medalha)) +
  geom_histogram(bins = 30, position = "identity", alpha = 0.7) +
  labs(title = "Distribuição de Altura por Ganho de Medalha", x = "Altura (cm)", y = "Frequência") +
  theme_minimal() +
  scale_fill_manual(values = c("Sim" = "skyblue", "Não" = "salmon")) +
  facet_wrap(~ ganhou_medalha)

# Gráfico 3: Distribuição de Peso dos Atletas
ggplot(dados_olimpiadas, aes(x = peso, fill = ganhou_medalha)) +
  geom_histogram(bins = 30, position = "identity", alpha = 0.7) +
  labs(title = "Distribuição de Peso por Ganho de Medalha", x = "Peso (kg)", y = "Frequência") +
  theme_minimal() +
  scale_fill_manual(values = c("Sim" = "skyblue", "Não" = "salmon")) +
  facet_wrap(~ ganhou_medalha)

# Gráfico 4: Distribuição de IMC dos Atletas
ggplot(dados_olimpiadas, aes(x = imc, fill = ganhou_medalha)) +
  geom_histogram(bins = 30, position = "identity", alpha = 0.7) +
  labs(title = "Distribuição de IMC por Ganho de Medalha", x = "IMC", y = "Frequência") +
  theme_minimal() +
  scale_fill_manual(values = c("Sim" = "skyblue", "Não" = "salmon")) +
  facet_wrap(~ ganhou_medalha)

# Gráfico 5: Boxplot de Idade, Altura, Peso e IMC por Ganho de Medalha
dados_long <- dados_olimpiadas |>
  select(ganhou_medalha, idade, altura, peso, imc) |>
  pivot_longer(
    cols = c(idade, altura, peso, imc),
    names_to = "metrica",
    values_to = "valor"
  ) |>
  mutate(metrica = factor(metrica, levels = c("idade", "altura", "peso", "imc"))) # Ordenar para o plot

ggplot(dados_long, aes(x = ganhou_medalha, y = valor, fill = ganhou_medalha)) +
  geom_boxplot() +
  labs(title = "Distribuição de Métricas Corporais por Ganho de Medalha",
       x = "Ganhou Medalha", y = "Valor") +
  theme_minimal() +
  scale_fill_manual(values = c("Sim" = "darkgreen", "Não" = "darkred")) +
  facet_wrap(~ metrica, scales = "free_y")

# Gráfico 6: Top Países por Número de Medalhas
dados_olimpiadas |>
  filter(ganhou_medalha == "Sim") |>
  count(pais, sort = TRUE) |>
  head(10) |>
  ggplot(aes(x = fct_reorder(pais, n), y = n, fill = pais)) +
  geom_bar(stat = "identity") +
  labs(title = "Top 10 Países por Número de Medalhas", x = "País", y = "Número de Medalhas") +
  coord_flip() +
  theme_minimal() +
  theme(legend.position = "none")

# Gráfico 7: Distribuição de Medalhas por Tipo
dados_olimpiadas |>
  filter(medalha != "Sem Medalha") |>
  ggplot(aes(x = medalha, fill = medalha)) +
  geom_bar() +
  labs(title = "Distribuição de Tipos de Medalha", x = "Tipo de Medalha", y = "Contagem") +
  theme_minimal() +
  scale_fill_manual(values = c("Ouro" = "gold", "Prata" = "grey70", "Bronze" = "brown"))

# Gráfico 8: Contagem de Atletas por Esporte e Ganho de Medalha (Top 10 Esportes)
top_esportes <- dados_olimpiadas |>
  filter(ganhou_medalha == "Sim") |>
  count(esporte, sort = TRUE) |>
  head(10) |>
  pull(esporte)

dados_olimpiadas |>
  filter(esporte %in% top_esportes) |>
  ggplot(aes(x = fct_reorder(esporte, -as.numeric(factor(ganhou_medalha, levels = c("Sim", "Não")))), fill = ganhou_medalha)) +
  geom_bar(position = "dodge") +
  labs(title = "Contagem de Atletas por Esporte (Top 10 Medalhistas) e Ganho de Medalha",
       x = "Esporte", y = "Número de Atletas", fill = "Ganhou Medalha") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("Sim" = "darkgreen", "Não" = "darkred"))

# Gráfico 9 (NOVO/ADAPTADO): Participação de Atletas por Sexo ao Longo do Tempo (incorporado do primeiro código)
dados_olimpiadas |>
  group_by(ano, sexo) |>
  summarise(n_atletas = n_distinct(paste(ano, pais, sexo, esporte)), .groups = 'drop') |> # Corrigido aqui!
  ggplot(aes(x = ano, y = n_atletas, color = sexo)) +
  geom_line(linewidth = 1.2) + # AQUI: Troquei 'size' por 'linewidth'
  geom_point(size = 2) +
  scale_color_brewer(palette = "Set1") +
  labs(title = "Participação de Atletas por Sexo ao Longo do Tempo",
       x = "Ano",
       y = "Número de Atletas",
       color = "Sexo") +
  theme_minimal()

# Gráfico 10 (NOVO): Distribuição de Medalhas por Categoria de Esporte
dados_olimpiadas |>
  filter(ganhou_medalha == "Sim") |>
  ggplot(aes(x = categoria_esporte, fill = categoria_esporte)) +
  geom_bar() +
  labs(title = "Distribuição de Medalhas por Categoria de Esporte",
       x = "Categoria de Esporte", y = "Contagem de Medalhas") +
  theme_minimal() +
  theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Dark2")

# -----------------------------------------------------------------------------
# 2.5 Outliers (critério IQR) para IMC
# -----------------------------------------------------------------------------

cat("\n--- Detecção de Outliers para IMC ---\n")
q_imc <- quantile(dados_olimpiadas$imc, c(.25, .75), na.rm = TRUE)
iqr_imc <- diff(q_imc)
lim_imc <- list(inf = q_imc[1] - 1.5*iqr_imc, sup = q_imc[2] + 1.5*iqr_imc)

dados_olimpiadas <- dados_olimpiadas |>
  mutate(outlier_imc = case_when(
    imc < lim_imc$inf ~ "Inferior",
    imc > lim_imc$sup ~ "Superior",
    TRUE ~ "Normal"
  ))

n_out_imc <- dados_olimpiadas |> filter(outlier_imc != "Normal") |> nrow()
cat("Total de outliers de IMC:", n_out_imc,
    "(", round(100 * n_out_imc / nrow(dados_olimpiadas), 2), "% dos dados )\n")

dados_olimpiadas |>
  filter(outlier_imc != "Normal") |>
  select(ano, pais, sexo, idade, altura, peso, imc, outlier_imc, medalha) |>
  datatable(options = list(pageLength = 5),
            caption = "Observações consideradas outliers de IMC")

# -----------------------------------------------------------------------------
# Função de teste de hipótese completa (para médias)
# -----------------------------------------------------------------------------

teste_completo <- function(x, y, grupo1, grupo2, metrica_nome) {
  cat(paste0("\n### Teste de Hipótese: ", metrica_nome, " entre ", grupo1, " e ", grupo2, "\n"))
  
  # Remover NAs para o t.test e var.test
  x_clean <- na.omit(x)
  y_clean <- na.omit(y)
  
  if (length(x_clean) < 2 || length(y_clean) < 2) {
    cat("Dados insuficientes para realizar o teste de médias (mínimo de 2 observações por grupo).\n")
    return(invisible(NULL))
  }
  
  t_res <- t.test(x_clean, y_clean)
  ic_x <- t.test(x_clean)$conf.int
  ic_y <- t.test(y_clean)$conf.int
  
  cat("n =", length(x_clean), "/", length(y_clean), "\n")
  cat("Média ", grupo1, " =", round(mean(x_clean),2),
      "IC95% [", round(ic_x[1],2), ";", round(ic_x[2],2), "]\n")
  cat("Média ", grupo2, " =", round(mean(y_clean),2),
      "IC95% [", round(ic_y[1],2), ";", round(ic_y[2],2), "]\n")
  cat("Diferença de médias IC95%: [", round(t_res$conf.int[1],2), ";", round(t_res$conf.int[2],2), "]\n")
  cat("t =", round(t_res$statistic,3), "p =", round(t_res$p.value,4),
      ifelse(t_res$p.value < .05,"→ REJEITA H0 (há diferença significativa)","→ não rejeita H0 (não há diferença significativa evidente)"), "\n")
  
  f_res <- var.test(x_clean, y_clean)
  cat("F =", round(f_res$statistic,3), "p =", round(f_res$p.value,4),
      ifelse(f_res$p.value < .05,"→ REJEITA H0 (há diferença significativa nas variâncias)","→ não rejeita H0 (não há diferença significativa nas variâncias)"), "\n")
  
  d <- effsize::cohen.d(x_clean, y_clean)$estimate
  classe_d <- ifelse(abs(d)<0.2,"muito pequeno",
                     ifelse(abs(d)<0.5,"pequeno",
                            ifelse(abs(d)<0.8,"médio","grande")))
  cat("Cohen's d =", round(d,2), "→ efeito", classe_d, "\n")
}

# -----------------------------------------------------------------------------
# Função de teste de hipótese adaptada para proporções (incorporado do primeiro código)
# -----------------------------------------------------------------------------

teste_proporcao_medalhas <- function(df, grupo_var, valor_grupo1, valor_grupo2, tipo_medalha) {
  cat(paste0("\n### Comparação de proporção de medalhas (", tipo_medalha, ") em ", valor_grupo1, " vs. ", valor_grupo2, "\n"))
  
  # Filtra os dados para os dois grupos que queremos comparar e para o tipo de medalha
  # Considera todos os participantes para o 'n' total de cada grupo, não apenas medalhistas
  dados_grupo1 <- df |> filter({{grupo_var}} == valor_grupo1)
  dados_grupo2 <- df |> filter({{grupo_var}} == valor_grupo2)
  
  sucesso1 <- sum(dados_grupo1$medalha == tipo_medalha)
  total1 <- nrow(dados_grupo1)
  prop1 <- sucesso1 / total1
  
  sucesso2 <- sum(dados_grupo2$medalha == tipo_medalha)
  total2 <- nrow(dados_grupo2)
  prop2 <- sucesso2 / total2
  
  cat(paste0("Proporção de ", tipo_medalha, " em ", valor_grupo1, ": ", round(prop1, 4), " (", sucesso1, "/", total1, ")\n"))
  cat(paste0("Proporção de ", tipo_medalha, " em ", valor_grupo2, ": ", round(prop2, 4), " (", sucesso2, "/", total2, ")\n"))
  
  # Verifica se há dados suficientes para o teste de proporção
  if (total1 < 5 || total2 < 5 || sucesso1 + sucesso2 == 0 || total1 + total2 - sucesso1 - sucesso2 == 0) { # Condição para evitar erros com n pequeno ou todos iguais
    cat("Dados insuficientes para realizar o teste de proporções (contagens muito baixas ou todas iguais).\n")
    return(invisible(NULL))
  }
  
  prop_test_res <- prop.test(x = c(sucesso1, sucesso2), n = c(total1, total2))
  
  cat("Chi-quadrado =", round(prop_test_res$statistic, 3),
      "p =", round(prop_test_res$p.value, 4),
      ifelse(prop_test_res$p.value < .05, "→ REJEITA H0 (há diferença significativa)", "→ não rejeita H0 (não há diferença significativa)"), "\n")
  
  cat("Diferença nas proporções (IC95%): [", round(prop_test_res$conf.int[1], 4), ";", round(prop_test_res$conf.int[2], 4), "]\n")
}


# -----------------------------------------------------------------------------
# 2.6 Testes de Hipótese (Combinado)
# -----------------------------------------------------------------------------

cat("\n--- Testes de Hipótese ---\n")

# Testes de Médias (mantidos do segundo código, com aprimoramentos):

# Exemplo 1: Comparação de IMC entre atletas com medalha e sem medalha
medalha_sim_imc <- dados_olimpiadas |> filter(ganhou_medalha == "Sim") |> pull(imc)
medalha_nao_imc <- dados_olimpiadas |> filter(ganhou_medalha == "Não") |> pull(imc)
teste_completo(medalha_sim_imc, medalha_nao_imc, "Atletas com Medalha", "Atletas sem Medalha", "IMC")

# Exemplo 2: Comparação de Altura entre atletas com medalha e sem medalha
altura_medalha_sim <- dados_olimpiadas |> filter(ganhou_medalha == "Sim") |> pull(altura)
altura_medalha_nao <- dados_olimpiadas |> filter(ganhou_medalha == "Não") |> pull(altura)
teste_completo(altura_medalha_sim, altura_medalha_nao, "Atletas com Medalha", "Atletas sem Medalha", "Altura")

# Exemplo 3: Comparação de Idade entre atletas de Ouro e Prata
ouro_idade <- dados_olimpiadas |> filter(medalha == "Ouro") |> pull(idade)
prata_idade <- dados_olimpiadas |> filter(medalha == "Prata") |> pull(idade)
teste_completo(ouro_idade, prata_idade, "Atletas de Ouro", "Atletas de Prata", "Idade")

# Exemplo 4: Comparação de Peso entre Sexos (Masculino vs Feminino)
peso_masculino <- dados_olimpiadas |> filter(sexo == "M") |> pull(peso)
peso_feminino <- dados_olimpiadas |> filter(sexo == "F") |> pull(peso)
teste_completo(peso_masculino, peso_feminino, "Atletas Masculinos", "Atletas Femininos", "Peso")


# Testes de Proporção (incorporados do primeiro código):

# Exemplo 5: Comparação de Proporção de Medalhas de Ouro (Masculino vs Feminino)
teste_proporcao_medalhas(dados_olimpiadas, sexo, "M", "F", "Ouro")

# Exemplo 6: Comparação de Proporção de Medalhas de Bronze (Esportes Coletivos vs Individuais Clássicos)
teste_proporcao_medalhas(dados_olimpiadas, categoria_esporte, "Esportes Coletivos", "Individuais Clássicos", "Bronze")

# Exemplo 7 (NOVO): Comparação de Proporção de Medalhas (Guerra Fria vs Pós-Guerra Fria) para Prata
teste_proporcao_medalhas(dados_olimpiadas, era_olimpica, "Guerra Fria", "Pós-Guerra Fria", "Prata")


# -----------------------------------------------------------------------------
# Reflexões Finais / Pontos para o Relatório Markdown (Expandido)
# -----------------------------------------------------------------------------

cat("\n--- Reflexões Finais para o Relatório ---\n")
cat("1. Quais são as características (idade, altura, peso, IMC) predominantes dos atletas medalhistas em comparação com os não medalhistas? As diferenças são estatisticamente e praticamente significativas?\n")
cat("2. Qual o impacto da variável 'sexo' ou 'esporte' na distribuição das medalhas e nas métricas corporais? Existem tendências claras na participação feminina ao longo do tempo?\n")
cat("3. Quais países e esportes se destacam no quadro de medalhas? Isso se correlaciona com as características dos seus atletas ou a 'era olímpica'? Há especializações por esporte ou período?\n")
cat("4. As diferenças nas proporções de medalhas (ex: entre sexos, tipos de esporte ou eras olímpicas) foram estatisticamente significativas? Qual a implicação disso para a história e evolução dos jogos?\n")
cat("5. Houve algum esporte ou modalidade que se destacou por ter atletas com características corporais muito específicas (e.g., ginastas vs. jogadores de basquete)?\n")
cat("6. Os outliers de IMC são esperados em algum esporte (e.g., levantamento de peso, ginástica)? Como eles afetam a análise geral? Eles devem ser removidos ou analisados separadamente?\n")
cat("7. As diferenças estatisticamente significativas encontradas nos testes de hipótese possuem um tamanho de efeito (Cohen's d) prático relevante? O que isso significa para a interpretação dos resultados e para o entendimento do desempenho atlético?\n")
cat("8. Que outras variáveis ou perguntas poderiam ser exploradas com este dataset para aprofundar a análise (ex: análise por cidade-sede, impacto de eventos globais, relação entre idade e tipo de medalha)?\n")
cat("\nUtilize esses insights e os gráficos gerados para construir seu relatório em RMarkdown!")
