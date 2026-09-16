# ==============================================================================
# Projeto Primeiro Emprego (PPE) - Avaliação dos Gestores / Pontos Focais
# Script: gest_10_q8_contribuicao_integracao.R - Contribuição para Integração
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

caminho_csv <- "/home/nando/Documentos/gemini/ATUAL/OUTPUT/gest_PPE_tratado_2025_2026.csv"
df <- read_csv(caminho_csv, show_col_types = FALSE)

df_q8 <- df |>
  mutate(
    AnoCol = factor(AnoCol, levels = c("2025", "2026")),
    contribuicao = factor(q8_ord, levels = c(
      "É fundamental",
      "Contribui significativamente",
      "Contribui em alguma medida"
    ))
  )

tabela_q8_dados <- tibble(
  `Contribuição do Acolhimento para a Integração` = c(
    "É fundamental",
    "Contribui significativamente",
    "Contribui em alguma medida",
    "Total"
  ),
  `2025 (N)` = c("41", "20", "6", "67"),
  `2025 (%)` = c("61,2%", "29,9%", "9,0%", "100,0%"),
  `2026 (N)` = c("51", "26", "0", "77"),
  `2026 (%)` = c("66,2%", "33,8%", "0,0%", "100,0%"),
  `Variação (p.p.)` = c("+5,0", "+3,9", "-9,0", "—")
)

t08_contribuicao_acolhimento <- tt(tabela_q8_dados) |>
  format_tt(escape = TRUE)

df_plot_q8 <- df_q8 |>
  count(AnoCol, contribuicao, name = "Respondentes") |>
  complete(AnoCol, contribuicao, fill = list(Respondentes = 0)) |>
  group_by(AnoCol) |>
  mutate(
    Total = sum(Respondentes),
    Percentual = Respondentes / Total,
    Rotulo = if_else(Respondentes > 0, percent(Percentual, accuracy = 0.1, decimal.mark = ","), "")
  ) |>
  ungroup()

g08_contribuicao_acolhimento <- ggplot(df_plot_q8, aes(x = contribuicao, y = Percentual, fill = AnoCol)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.65, alpha = 0.95) +
  geom_text(
    aes(label = Rotulo),
    position = position_dodge(width = 0.75),
    vjust = -0.4,
    size = 3.2,
    fontface = "bold",
    color = COR_TEXTO
  ) +
  scale_fill_manual(
    name = "Ciclo Avaliativo",
    values = c("2025" = COR_CONTEXTO, "2026" = COR_DESTAQUE)
  ) +
  scale_y_continuous(
    labels = label_percent(accuracy = 1),
    limits = c(0, 0.78),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    title = "Eficácia Socializadora: Contribuição do Acolhimento para a Integração",
    subtitle = "Unanimidade em 2026: 100% dos gestores consideram o acolhimento fundamental (66,2%) ou muito significativo (33,8%)
(Cinza: 2025 | Azul: 2026)",
    x = "Grau de Contribuição Atribuído",
    y = "Proporção de Gestores (%)",
    caption = "Fonte: Dados da pesquisa com Pontos Focais do PPE (2025 - 2026)."
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
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = COR_GRID, linewidth = 0.4),
    axis.line.x = element_line(color = COR_CONTEXTO, linewidth = 0.5),
    axis.text.x = element_text(color = COR_TEXTO, size = 9.5, face = "bold"),
    axis.text.y = element_text(color = COR_SUBTEXTO, size = 9.5),
    plot.margin = margin(t = 12, r = 15, b = 12, l = 15)
  )
