# ==============================================================================
# Projeto Primeiro Emprego (PPE) - Análise Comparativa Longitudinal (2025-2026)
# Script 28: 28_q41_situacao_financeira.R - Papel no Sustento da Família
# ==============================================================================
suppressPackageStartupMessages({
  library(tidyverse)
  library(scales)
  library(tinytable)
})

COR_DESTAQUE <- "#174A7E"
COR_CONTEXTO <- "#929497"
COR_TEXTO    <- "#231F20"
COR_SUBTEXTO <- "#555655"
COR_GRID     <- "#E5E5E5"

caminho_csv <- "/home/nando/Documentos/gemini/ATUAL/OUTPUT/PPE_beneficiarios_2025_2026.csv"
df <- read_csv(caminho_csv, show_col_types = FALSE)

df_q41 <- df |>
  mutate(
    AnoCol = factor(AnoCol, levels = c("2025", "2026")),
    papel_renda = case_when(
      grepl("principal responsável", q41, ignore.case = TRUE) ~ "Principal responsável pelo sustento",
      grepl("contribuo com sustento", q41, ignore.case = TRUE) ~ "Contribui com o sustento familiar",
      grepl("me sustento totalmente", q41, ignore.case = TRUE) ~ "Sustenta-se de forma autônoma",
      grepl("recebo ajuda", q41, ignore.case = TRUE) ~ "Tem renda, mas necessita de ajuda",
      grepl("financiados pela família", q41, ignore.case = TRUE) ~ "Gastos financiados pela família",
      TRUE ~ "Outra situação"
    ),
    papel_renda = factor(papel_renda, levels = c(
      "Contribui com o sustento familiar",
      "Sustenta-se de forma autônoma",
      "Principal responsável pelo sustento",
      "Tem renda, mas necessita de ajuda",
      "Gastos financiados pela família"
    ))
  )

df_resumo_q41 <- df_q41 |>
  count(AnoCol, papel_renda) |>
  pivot_wider(names_from = AnoCol, values_from = n, values_fill = 0) |>
  mutate(
    pct_2025 = `2025` / sum(`2025`),
    pct_2026 = `2026` / sum(`2026`),
    dif_pp = (pct_2026 - pct_2025) * 100
  )

tabela_q41_dados <- tibble(
  `Papel Financeiro no Domicílio` = c(as.character(df_resumo_q41$papel_renda), "Total"),
  `2025 (N)` = c(as.character(df_resumo_q41$`2025`), "711"),
  `2025 (%)` = c(percent(df_resumo_q41$pct_2025, accuracy = 0.1, decimal.mark = ","), "100,0%"),
  `2026 (N)` = c(as.character(df_resumo_q41$`2026`), "879"),
  `2026 (%)` = c(percent(df_resumo_q41$pct_2026, accuracy = 0.1, decimal.mark = ","), "100,0%"),
  `Variação (p.p.)` = c(sprintf("%+.1f", df_resumo_q41$dif_pp), "—")
)

t41_financeira <- tt(tabela_q41_dados) |>
  format_tt(escape = TRUE)

df_plot_q41 <- df_q41 |>
  count(AnoCol, papel_renda, name = "Respondentes") |>
  group_by(AnoCol) |>
  mutate(
    Total = sum(Respondentes),
    Percentual = Respondentes / Total,
    Rotulo = percent(Percentual, accuracy = 0.1, decimal.mark = ",")
  ) |>
  ungroup()

g_financeira <- ggplot(df_plot_q41, aes(x = fct_rev(papel_renda), y = Percentual, fill = AnoCol)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.65, alpha = 0.95) +
  geom_text(
    aes(label = Rotulo),
    position = position_dodge(width = 0.75),
    hjust = -0.15,
    size = 3.1,
    fontface = "bold",
    color = COR_TEXTO
  ) +
  coord_flip() +
  scale_fill_manual(
    name = "Ciclo Avaliativo",
    values = c("2025" = COR_CONTEXTO, "2026" = COR_DESTAQUE)
  ) +
  scale_y_continuous(
    labels = label_percent(accuracy = 1),
    limits = c(0, 0.55),
    expand = expansion(mult = c(0, 0.08))
  ) +
  labs(
    title = "Suficiência Financeira: Papel da Renda do PPE no Sustento Domiciliar",
    subtitle = "Quase 70% são esteio ou coprovedores essenciais da família; ~20% têm autonomia plena
(Cinza: 2025 | Azul: 2026)",
    x = "Papel Financeiro Declarado",
    y = "Proporção de Beneficiários (%)",
    caption = "Fonte: Dados consolidados das pesquisas de avaliação do Projeto Primeiro Emprego (2025 - 2026)."
  ) +
  theme_minimal(base_size = 11, base_family = "sans") +
  theme(
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.title = element_text(color = COR_TEXTO, size = 13, face = "bold", margin = margin(b = 6)),
    plot.subtitle = element_text(color = COR_SUBTEXTO, size = 10, margin = margin(b = 16), lineheight = 1.15),
    plot.caption = element_text(color = COR_CONTEXTO, size = 8, hjust = 0, margin = margin(t = 12)),
    legend.position = "top",
    legend.justification = "left",
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = COR_GRID, linewidth = 0.4),
    axis.line.y = element_line(color = COR_CONTEXTO, linewidth = 0.5),
    axis.text.y = element_text(color = COR_TEXTO, size = 10, face = "bold"),
    axis.text.x = element_text(color = COR_SUBTEXTO, size = 9.5),
    plot.margin = margin(t = 12, r = 15, b = 12, l = 15)
  )
