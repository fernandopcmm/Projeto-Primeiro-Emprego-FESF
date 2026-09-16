# ==============================================================================
# Projeto Primeiro Emprego (PPE) - Análise Comparativa Longitudinal (2025-2026)
# Script 04: 04_q23_faixa_etaria.R - Distribuição por Faixa Etária
# Padrão: Storytelling with Data (Cole Nussbaumer Knaflic) e REGRAS_SCRIPTS_R.md
# ==============================================================================
# Autoria: Coordenação de Pesquisa e Avaliação de Políticas Públicas
# Data: 2026-09-15
# Ambiente: R / ggplot2 / tidyverse / tinytable
# ==============================================================================

# 1. Carregamento de Bibliotecas -----------------------------------------------
suppressPackageStartupMessages({
  library(tidyverse)
  library(scales)
  library(tinytable)
})

# 2. Definição da Paleta de Cores (Padrão Storytelling with Data - SWD) ---------
COR_DESTAQUE <- "#174A7E"  # Azul escuro institucional (2026 / Foco analítico)
COR_CONTEXTO <- "#929497"  # Cinza neutro (2025 / Referência longitudinal)
COR_TEXTO    <- "#231F20"  # Cinza escuro / grafite para títulos
COR_SUBTEXTO <- "#555655"  # Cinza médio para subtítulos e rótulos
COR_GRID     <- "#E5E5E5"  # Cinza claro para linhas de grade sutis

# 3. Importação da Base Consolidada --------------------------------------------
caminho_csv <- "/home/nando/Documentos/gemini/ATUAL/OUTPUT/PPE_beneficiarios_2025_2026.csv"
df <- read_csv(caminho_csv, show_col_types = FALSE)

# 4. Preparação e Harmonização da Idade e Faixas Etárias -----------------------
# Tratamento higienizado de q23 conforme diretrizes do ciclo 2025 e script 02
df_idade <- df |>
  mutate(
    AnoCol = factor(AnoCol, levels = c("2025", "2026")),
    q23_num = case_when(
      AnoCol == "2026" & q23 == "01/10/1977" ~ 2026 - 1977,
      AnoCol == "2026" & q23 == "1976.0" ~ 2026 - 1976,
      AnoCol == "2026" & q23 == "5.008031976E9" ~ 2026 - 1976,
      AnoCol == "2026" & q23 == "1.624924565E9" ~ 2026 - 1999,
      AnoCol == "2026" & q23 == "3.3" ~ 2026 - 1994,
      grepl("^[0-9]{2}", q23) ~ as.numeric(str_extract(q23, "^[0-9]{2}")),
      TRUE ~ suppressWarnings(as.numeric(q23))
    ),
    faixa_etaria = case_when(
      q23_num >= 16 & q23_num <= 21 ~ "16 a 21",
      q23_num >= 22 & q23_num <= 25 ~ "22 a 25",
      q23_num >= 26 & q23_num <= 35 ~ "26 a 35",
      q23_num > 35 ~ "Acima de 35",
      TRUE ~ NA_character_
    ),
    faixa_etaria = factor(
      faixa_etaria,
      levels = c("16 a 21", "22 a 25", "26 a 35", "Acima de 35"),
      ordered = TRUE
    )
  ) |>
  filter(!is.na(faixa_etaria))

# Agregação por Ano e Faixa Etária
df_resumo_idade <- df_idade |>
  count(AnoCol, faixa_etaria, name = "Respondentes") |>
  group_by(AnoCol) |>
  mutate(
    Total_Ano = sum(Respondentes),
    Percentual = Respondentes / Total_Ano,
    Rotulo_Curto = paste0(
      format(Respondentes, big.mark = ".", decimal.mark = ","),
      "\n(", percent(Percentual, accuracy = 0.1, decimal.mark = ","), ")"
    )
  ) |>
  ungroup()

# 5. Estruturação da Tabela Acadêmica (t23_faixa_etaria) ------------------------
# Colunas: Faixa Etária, 2025 (N), 2025 (%), 2026 (N), 2026 (%), Variação (p.p.)
n_25_tot <- sum(df_resumo_idade$Respondentes[df_resumo_idade$AnoCol == "2025"])
n_26_tot <- sum(df_resumo_idade$Respondentes[df_resumo_idade$AnoCol == "2026"])

tab_25 <- df_resumo_idade |>
  filter(AnoCol == "2025") |>
  select(faixa_etaria, n_25 = Respondentes, pct_25 = Percentual)

tab_26 <- df_resumo_idade |>
  filter(AnoCol == "2026") |>
  select(faixa_etaria, n_26 = Respondentes, pct_26 = Percentual)

tabela_idade_dados <- left_join(tab_25, tab_26, by = "faixa_etaria") |>
  mutate(
    var_pp = (pct_26 - pct_25) * 100,
    `Faixa Etária` = as.character(faixa_etaria),
    `2025 (N)` = format(n_25, big.mark = ".", decimal.mark = ","),
    `2025 (%)` = percent(pct_25, accuracy = 0.1, decimal.mark = ","),
    `2026 (N)` = format(n_26, big.mark = ".", decimal.mark = ","),
    `2026 (%)` = percent(pct_26, accuracy = 0.1, decimal.mark = ","),
    `Variação (p.p.)` = sub("\\.", ",", sprintf("%+.1f", var_pp))
  ) |>
  select(`Faixa Etária`, `2025 (N)`, `2025 (%)`, `2026 (N)`, `2026 (%)`, `Variação (p.p.)`)

# Linha de totalização
linha_total_idade <- tibble(
  `Faixa Etária` = "Total",
  `2025 (N)` = format(n_25_tot, big.mark = ".", decimal.mark = ","),
  `2025 (%)` = "100,0%",
  `2026 (N)` = format(n_26_tot, big.mark = ".", decimal.mark = ","),
  `2026 (%)` = "100,0%",
  `Variação (p.p.)` = "—"
)

t23_faixa_etaria <- bind_rows(tabela_idade_dados, linha_total_idade) |>
  tt() |>
  format_tt(escape = TRUE)

# 6. Construção do Gráfico com Storytelling (g_faixa_etaria) -------------------
# Padrão SWD: barras agrupadas por faixa etária com cores pré-atentivas por ano
limite_y_idade <- max(df_resumo_idade$Respondentes, na.rm = TRUE) * 1.25

g_faixa_etaria <- ggplot(df_resumo_idade, aes(x = faixa_etaria, y = Respondentes, fill = AnoCol)) +
  geom_col(
    position = position_dodge(width = 0.75),
    width = 0.65,
    alpha = 0.95
  ) +
  geom_text(
    aes(label = Rotulo_Curto, color = AnoCol),
    position = position_dodge(width = 0.75),
    vjust = -0.3,
    size = 3.3,
    fontface = "bold",
    lineheight = 0.9
  ) +
  scale_fill_manual(
    name = "Ano de Coleta",
    values = c("2025" = COR_CONTEXTO, "2026" = COR_DESTAQUE)
  ) +
  scale_color_manual(
    name = "Ano de Coleta",
    values = c("2025" = COR_CONTEXTO, "2026" = COR_DESTAQUE)
  ) +
  scale_y_continuous(
    limits = c(0, limite_y_idade),
    labels = label_number(big.mark = ".", decimal.mark = ","),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    title = "Avaliação PPE: Distribuição de Beneficiários por Faixa Etária",
    subtitle = "Comparação da frequência absoluta e relativa entre os ciclos avaliativos de 2025 e 2026\n(Cinza: 2025 | Azul: 2026)",
    x = "Faixa Etária (anos)",
    y = "Número de Respondentes",
    caption = "Fonte: Dados consolidados das pesquisas de avaliação do Projeto Primeiro Emprego (2025 - 2026)."
  ) +
  theme_minimal(base_size = 11, base_family = "sans") +
  theme(
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.title = element_text(color = COR_TEXTO, size = 13, face = "bold", margin = margin(b = 6)),
    plot.subtitle = element_text(color = COR_SUBTEXTO, size = 10, margin = margin(b = 16), lineheight = 1.15),
    plot.caption = element_text(color = COR_CONTEXTO, size = 8, hjust = 0, margin = margin(t = 12), lineheight = 1.1),
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = COR_GRID, linewidth = 0.4),
    axis.line.x = element_line(color = COR_CONTEXTO, linewidth = 0.5),
    axis.line.y = element_blank(),
    axis.ticks.x = element_line(color = COR_CONTEXTO, linewidth = 0.5),
    axis.ticks.y = element_blank(),
    axis.text.x = element_text(color = COR_TEXTO, size = 10.5, face = "bold", margin = margin(t = 6)),
    axis.text.y = element_text(color = COR_SUBTEXTO, size = 9.5),
    axis.title.x = element_text(color = COR_SUBTEXTO, size = 10, margin = margin(t = 8)),
    axis.title.y = element_text(color = COR_SUBTEXTO, size = 10, margin = margin(r = 10)),
    plot.margin = margin(t = 12, r = 15, b = 12, l = 15)
  )
