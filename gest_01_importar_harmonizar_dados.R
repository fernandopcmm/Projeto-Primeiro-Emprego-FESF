# ==============================================================================
# Projeto Primeiro Emprego (PPE) - Avaliação dos Pontos Focais (Gestores)
# Script: gest_01_importar_harmonizar_dados.R
# ==============================================================================
# Autoria: Fernando Antonio de Melo Pereira Lhamas (UFRN / NPGA-UFBA)
# Finalidade: Importar o questionário dos gestores, padronizar nomes das variáveis
#             (q1 a q20), extrair o ano da coleta (2025 vs 2026) e gerar a base
#             consolidada e o codebook de variáveis.
# ==============================================================================

suppressPackageStartupMessages({
  library(readxl)
  library(tidyverse)
})

caminho_xlsx <- "/home/nando/Documentos/gemini/ATUAL/INPUT/2025_Questionário gestores - Avaliação PPE (respostas).xlsx"
caminho_csv_saida <- "/home/nando/Documentos/gemini/ATUAL/OUTPUT/gest_PPE_pontos_focais_2025_2026.csv"
caminho_codebook  <- "/home/nando/Documentos/gemini/ATUAL/OUTPUT/gest_codebook_pontos_focais.csv"

# 1. Leitura da Planilha Bruta --------------------------------------------------
df_raw <- read_excel(caminho_xlsx)

# Mapeamento e renomeação uniforme
nomes_originais <- names(df_raw)

df_gest <- df_raw |>
  rename(data_hora = 1) |>
  rename_with(~ paste0("q", 1:20), .cols = 2:21) |>
  mutate(
    data_hora = as.POSIXct(data_hora),
    AnoCol = lubridate::year(data_hora)
  ) |>
  relocate(data_hora, AnoCol)

# 2. Gravação da Base CSV Harmonizada ------------------------------------------
write_csv(df_gest, caminho_csv_saida)

# 3. Geração do Codebook das Variáveis ------------------------------------------
codebook_gest <- tibble(
  Codigo = c("data_hora", "AnoCol", paste0("q", 1:20)),
  Enunciado_Original = c(
    nomes_originais[1],
    "Ano da coleta da pesquisa (extraído do carimbo de data/hora)",
    nomes_originais[2:21]
  ),
  Tipo = c(
    "datetime", "nominal", "ordinal_1_5", "nominal", "ordinal_1_5",
    "textual", "textual", "ordinal_1_5", "nominal", "ordinal",
    "nominal", "ordinal_1_5", "nominal", "ordinal", "ordinal_1_5",
    "ordinal_1_5", "ordinal_1_5", "ordinal_1_5", "ordinal_1_5", "ordinal_1_5",
    "textual", "textual"
  ),
  Bloco_Tematico = c(
    "Metadados", "Metadados",
    "Bloco 1: Percepção Geral e Papel do Gestor",
    "Bloco 1: Percepção Geral e Papel do Gestor",
    "Bloco 1: Percepção Geral e Papel do Gestor",
    "Bloco 1: Percepção Geral e Papel do Gestor",
    "Bloco 1: Percepção Geral e Papel do Gestor",
    "Bloco 2: Acolhimento e Integração Organizacional",
    "Bloco 2: Acolhimento e Integração Organizacional",
    "Bloco 2: Acolhimento e Integração Organizacional",
    "Bloco 2: Acolhimento e Integração Organizacional",
    "Bloco 3: Comunicação e Supervisão de Desempenho",
    "Bloco 3: Comunicação e Supervisão de Desempenho",
    "Bloco 3: Comunicação e Supervisão de Desempenho",
    "Bloco 3: Comunicação e Supervisão de Desempenho",
    "Bloco 4: Perfil Comportamental e Desempenho dos Jovens",
    "Bloco 4: Perfil Comportamental e Desempenho dos Jovens",
    "Bloco 4: Perfil Comportamental e Desempenho dos Jovens",
    "Bloco 4: Perfil Comportamental e Desempenho dos Jovens",
    "Bloco 4: Perfil Comportamental e Desempenho dos Jovens",
    "Bloco 5: Sugestões e Inconformidades Estruturais",
    "Bloco 5: Sugestões e Inconformidades Estruturais"
  )
)

write_csv(codebook_gest, caminho_codebook)
