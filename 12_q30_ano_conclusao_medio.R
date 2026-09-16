# ==============================================================================
# Projeto Primeiro Emprego (PPE) - Análise Comparativa Longitudinal (2025-2026)
# Script 12: 12_q30_ano_conclusao_medio.R - Ano de Conclusão do Ensino Médio
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
COR_TEXTO    <- "#231F20"  # Grafite escuro para títulos e rótulos
COR_SUBTEXTO <- "#555655"  # Cinza médio para subtítulos e anotações
COR_GRID     <- "#E5E5E5"  # Cinza claro para linhas de grade sutis

# 3. Importação da Base Consolidada --------------------------------------------
caminho_csv <- "/home/nando/Documentos/gemini/ATUAL/OUTPUT/PPE_beneficiarios_2025_2026.csv"
df <- read_csv(caminho_csv, show_col_types = FALSE)

# 4. Tratamento e Categorização Histórica do Ano de Conclusão (q30) ------------
# Limpeza de inconsistências textuais e agrupamento em faixas temporais solicitadas:
# "Até 2015", "2016 a 2020", "2021 a 2023", "2024 em diante"
df_q30 <- df |>
  mutate(
    AnoCol = factor(AnoCol, levels = c("2025", "2026")),
    q30_num = suppressWarnings(case_when(
      grepl("^[0-9]{4}$", q30) ~ as.numeric(q30),
      grepl("^[0-9]{4}\\.0$", q30) ~ as.numeric(sub("\\.0$", "", q30)),
      q30 %in% c("2020/2021", "2010-2021", "2009/ 2022", "2011 e 2024") ~ as.numeric(str_extract(q30, "[0-9]{4}$")),
      q30 == "20/12/2022" ~ 2022,
      q30 == "20209.0" ~ 2020,
      q30 == "20215.0" ~ 2021,
      q30 == "20222.0" ~ 2022,
      q30 %in% c("202.0", "207.0", "219.0") ~ as.numeric(sub("^20", "200", sub("\\.0$", "", q30))),
      q30 %in% c("22.0", "23.0", "24.0") ~ 2000 + as.numeric(sub("\\.0$", "", q30)),
      q30 == "95.0" ~ 1995,
      q30 == "20000.0" ~ 2000,
      TRUE ~ as.numeric(q30)
    )),
    grupo_ano_conclusao = case_when(
      q30_num <= 2015 ~ "Até 2015",
      q30_num >= 2016 & q30_num <= 2020 ~ "2016 a 2020",
      q30_num >= 2021 & q30_num <= 2023 ~ "2021 a 2023",
      q30_num >= 2024 ~ "2024 em diante",
      TRUE ~ NA_character_
    ),
    grupo_ano_conclusao = factor(
      grupo_ano_conclusao,
      levels = c("Até 2015", "2016 a 2020", "2021 a 2023", "2024 em diante"),
      ordered = TRUE
    )
  ) |>
  filter(!is.na(grupo_ano_conclusao))

df_resumo_q30 <- df_q30 |>
  count(AnoCol, grupo_ano_conclusao, name = "Respondentes") |>
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

# 5. Estruturação da Tabela Acadêmica (t30_ano_conclusao) ----------------------
# Colunas: Período de Conclusão, 2025 (N), 2025 (%), 2026 (N), 2026 (%), Variação (p.p.)
n_25_tot <- sum(df_resumo_q30$Respondentes[df_resumo_q30$AnoCol == "2025"])
n_26_tot <- sum(df_resumo_q30$Respondentes[df_resumo_q30$AnoCol == "2026"])

tab_q30_25 <- df_resumo_q30 |>
  filter(AnoCol == "2025") |>
  select(grupo_ano_conclusao, n_25 = Respondentes, pct_25 = Percentual)

tab_q30_26 <- df_resumo_q30 |>
  filter(AnoCol == "2026") |>
  select(grupo_ano_conclusao, n_26 = Respondentes, pct_26 = Percentual)

tabela_q30_dados <- left_join(tab_q30_25, tab_q30_26, by = "grupo_ano_conclusao") |>
  mutate(
    var_pp = (pct_26 - pct_25) * 100,
    `Período de Conclusão` = as.character(grupo_ano_conclusao),
    `2025 (N)` = format(n_25, big.mark = ".", decimal.mark = ","),
    `2025 (%)` = percent(pct_25, accuracy = 0.1, decimal.mark = ","),
    `2026 (N)` = format(n_26, big.mark = ".", decimal.mark = ","),
    `2026 (%)` = percent(pct_26, accuracy = 0.1, decimal.mark = ","),
    `Variação (p.p.)` = sub("\\.", ",", sprintf("%+.1f", var_pp))
  ) |>
  select(`Período de Conclusão`, `2025 (N)`, `2025 (%)`, `2026 (N)`, `2026 (%)`, `Variação (p.p.)`)

linha_total_q30 <- tibble(
  `Período de Conclusão` = "Total Válido",
  `2025 (N)` = format(n_25_tot, big.mark = ".", decimal.mark = ","),
  `2025 (%)` = "100,0%",
  `2026 (N)` = format(n_26_tot, big.mark = ".", decimal.mark = ","),
  `2026 (%)` = "100,0%",
  `Variação (p.p.)` = "—"
)

t30_ano_conclusao <- bind_rows(tabela_q30_dados, linha_total_q30) |>
  tt() |>
  format_tt(escape = TRUE)

# 6. Construção do Gráfico com Storytelling (g_ano_conclusao) ------------------
limite_y_q30 <- max(df_resumo_q30$Respondentes, na.rm = TRUE) * 1.25

g_ano_conclusao <- ggplot(df_resumo_q30, aes(x = grupo_ano_conclusao, y = Respondentes, fill = AnoCol)) +
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
    limits = c(0, limite_y_q30),
    labels = label_number(big.mark = ".", decimal.mark = ","),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    title = "Avaliação PPE: Período de Conclusão do Ensino Médio",
    subtitle = "Comparação da temporalidade de formação escolar entre os ciclos de 2025 e 2026\n(Cinza: 2025 | Azul: 2026)",
    x = "Período Histórico de Conclusão",
    y = "Número de Beneficiários",
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
