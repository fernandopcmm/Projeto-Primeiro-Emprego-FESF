# ==============================================================================
# Projeto Primeiro Emprego (PPE) - Análise Comparativa Longitudinal (2025-2026)
# Script 47: 47_q61_capital_psicologico_confianca.R - Autoeficácia e Confiança
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

df_q61 <- df |>
  mutate(
    AnoCol = factor(AnoCol, levels = c("2025", "2026")),
    confianca = factor(q61, levels = c(
      "Mais confiante e melhor preparado",
      "Mais confiante e com preparo similar",
      "Menos confiante",
      "Confiança e preparo não tiveram alteração"
    ))
  )

df_resumo_q61 <- df_q61 |>
  count(AnoCol, confianca) |>
  pivot_wider(names_from = AnoCol, values_from = n, values_fill = 0) |>
  mutate(
    pct_2025 = `2025` / sum(`2025`),
    pct_2026 = `2026` / sum(`2026`),
    dif_pp = (pct_2026 - pct_2025) * 100
  )

tabela_q61_dados <- tibble(
  `Sentimento em Relação ao Mercado de Trabalho` = c(as.character(df_resumo_q61$confianca), "Total"),
  `2025 (N)` = c(as.character(df_resumo_q61$`2025`), "711"),
  `2025 (%)` = c(percent(df_resumo_q61$pct_2025, accuracy = 0.1, decimal.mark = ","), "100,0%"),
  `2026 (N)` = c(as.character(df_resumo_q61$`2026`), "879"),
  `2026 (%)` = c(percent(df_resumo_q61$pct_2026, accuracy = 0.1, decimal.mark = ","), "100,0%"),
  `Variação (p.p.)` = c(sprintf("%+.1f", df_resumo_q61$dif_pp), "—")
)

t61_confianca <- tt(tabela_q61_dados) |>
  format_tt(escape = TRUE)

df_plot_q61 <- df_q61 |>
  count(AnoCol, confianca, name = "Respondentes") |>
  group_by(AnoCol) |>
  mutate(
    Total = sum(Respondentes),
    Percentual = Respondentes / Total,
    Rotulo = percent(Percentual, accuracy = 0.1, decimal.mark = ",")
  ) |>
  ungroup()

g_confianca <- ggplot(df_plot_q61, aes(x = fct_rev(confianca), y = Percentual, fill = AnoCol)) +
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
    limits = c(0, 0.85),
    expand = expansion(mult = c(0, 0.08))
  ) +
  labs(
    title = "Capital Psicológico: Sentimento de Autoeficácia Frente ao Mercado",
    subtitle = "Salto de autoconfiança profissional: mais de 73% sentem-se mais confiantes e mais preparados
(Cinza: 2025 | Azul: 2026)",
    x = "Autoavaliação de Confiança e Preparo",
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
    axis.text.y = element_text(color = COR_TEXTO, size = 9.5, face = "bold"),
    axis.text.x = element_text(color = COR_SUBTEXTO, size = 9.5),
    plot.margin = margin(t = 12, r = 15, b = 12, l = 15)
  )
