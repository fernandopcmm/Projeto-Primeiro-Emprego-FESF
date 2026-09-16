# ==============================================================================
# Projeto Primeiro Emprego (PPE) - Avaliação dos Gestores / Pontos Focais
# Script: gest_08_q6_avaliacao_acolhimento.R - Avaliação do Processo de Acolhimento
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

df_q6 <- df |>
  mutate(
    AnoCol = factor(AnoCol, levels = c("2025", "2026")),
    escala = factor(q6, levels = 1:5, labels = c(
      "1 - Muito insatisfatório", "2 - Insatisfatório", "3 - Regular / Neutro",
      "4 - Satisfatório", "5 - Muito satisfatório"
    ))
  )

tabela_q6_dados <- tibble(
  `Avaliação do Processo de Acolhimento` = c(
    "1 - Muito insatisfatório", "2 - Insatisfatório", "3 - Regular / Neutro",
    "4 - Satisfatório", "5 - Muito satisfatório", "Total"
  ),
  `2025 (N)` = c("0", "1", "4", "25", "37", "67"),
  `2025 (%)` = c("0,0%", "1,5%", "6,0%", "37,3%", "55,2%", "100,0%"),
  `2026 (N)` = c("1", "1", "12", "26", "37", "77"),
  `2026 (%)` = c("1,3%", "1,3%", "15,6%", "33,8%", "48,1%", "100,0%"),
  `Variação (p.p.)` = c("+1,3", "-0,2", "+9,6", "-3,5", "-7,1", "—")
)

t06_acolhimento <- tt(tabela_q6_dados) |>
  format_tt(escape = TRUE)

df_plot_q6 <- df_q6 |>
  count(AnoCol, escala, name = "Respondentes") |>
  complete(AnoCol, escala, fill = list(Respondentes = 0)) |>
  group_by(AnoCol) |>
  mutate(
    Total = sum(Respondentes),
    Percentual = Respondentes / Total,
    Rotulo = if_else(Respondentes > 0, percent(Percentual, accuracy = 0.1, decimal.mark = ","), "")
  ) |>
  ungroup()

g06_acolhimento <- ggplot(df_plot_q6, aes(x = escala, y = Percentual, fill = AnoCol)) +
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
    limits = c(0, 0.68),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    title = "Recepção Institucional: Avaliação do Processo de Acolhimento dos Jovens",
    subtitle = "Forte satisfação gerencial: 92,5% (2025) e 81,9% (2026) avaliam como satisfatório ou muito satisfatório
(Cinza: 2025 | Azul: 2026)",
    x = "Escala de Avaliação do Acolhimento",
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
