# ==============================================================================
# Projeto Primeiro Emprego (PPE) - Avaliação dos Gestores / Pontos Focais
# Script: gest_03_q1_relevancia.R - Relevância Global do PPE
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

df_q1 <- df |>
  mutate(
    AnoCol = factor(AnoCol, levels = c("2025", "2026")),
    escala = factor(q1, levels = 1:5, labels = c(
      "1 - Nada relevante", "2 - Pouco relevante", "3 - Moderadamente",
      "4 - Muito relevante", "5 - Extremamente relevante"
    ))
  )

tabela_q1_dados <- tibble(
  `Nível de Relevância Declarada` = c(
    "1 - Nada relevante", "2 - Pouco relevante", "3 - Moderadamente",
    "4 - Muito relevante", "5 - Extremamente relevante", "Total"
  ),
  `2025 (N)` = c("0", "1", "4", "10", "52", "67"),
  `2025 (%)` = c("0,0%", "1,5%", "6,0%", "14,9%", "77,6%", "100,0%"),
  `2026 (N)` = c("0", "0", "1", "9", "67", "77"),
  `2026 (%)` = c("0,0%", "0,0%", "1,3%", "11,7%", "87,0%", "100,0%"),
  `Variação (p.p.)` = c("0,0", "-1,5", "-4,7", "-3,2", "+9,4", "—")
)

t01_relevancia <- tt(tabela_q1_dados) |>
  format_tt(escape = TRUE)

df_plot_q1 <- df_q1 |>
  count(AnoCol, escala, name = "Respondentes") |>
  complete(AnoCol, escala, fill = list(Respondentes = 0)) |>
  group_by(AnoCol) |>
  mutate(
    Total = sum(Respondentes),
    Percentual = Respondentes / Total,
    Rotulo = if_else(Respondentes > 0, percent(Percentual, accuracy = 0.1, decimal.mark = ","), "")
  ) |>
  ungroup()

g01_relevancia <- ggplot(df_plot_q1, aes(x = escala, y = Percentual, fill = AnoCol)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.65, alpha = 0.95) +
  geom_text(
    aes(label = Rotulo),
    position = position_dodge(width = 0.75),
    vjust = -0.4,
    size = 3.1,
    fontface = "bold",
    color = COR_TEXTO
  ) +
  scale_fill_manual(
    name = "Ciclo Avaliativo",
    values = c("2025" = COR_CONTEXTO, "2026" = COR_DESTAQUE)
  ) +
  scale_y_continuous(
    labels = label_percent(accuracy = 1),
    limits = c(0, 0.98),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    title = "Relevância Estratégica: Validação Social do PPE pelos Pontos Focais",
    subtitle = "Reconhecimento quase unânime: 92,5% (2025) e 98,7% (2026) consideram muito ou extremamente relevante
(Cinza: 2025 | Azul: 2026)",
    x = "Escala de Relevância Social do PPE",
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
