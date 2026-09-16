# ==============================================================================
# Projeto Primeiro Emprego (PPE) - Avaliação dos Gestores / Pontos Focais
# Script: gest_15_q13_desafio_obter_informacoes.R - Desafio em Obter Informações
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

df_q13 <- df |>
  mutate(
    AnoCol = factor(AnoCol, levels = c("2025", "2026")),
    escala = factor(q13, levels = 1:5, labels = c(
      "1 - Nada desafiador", "2 - Pouco desafiador", "3 - Moderadamente",
      "4 - Muito desafiador", "5 - Extremamente desafiador"
    ))
  )

tabela_q13_dados <- tibble(
  `Desafio para Obter Dados de Desempenho e Permanência` = c(
    "1 - Nada desafiador", "2 - Pouco desafiador", "3 - Moderadamente",
    "4 - Muito desafiador", "5 - Extremamente desafiador", "Total"
  ),
  `2025 (N)` = c("7", "11", "19", "17", "13", "67"),
  `2025 (%)` = c("10,4%", "16,4%", "28,4%", "25,4%", "19,4%", "100,0%"),
  `2026 (N)` = c("10", "13", "17", "26", "11", "77"),
  `2026 (%)` = c("13,0%", "16,9%", "22,1%", "33,8%", "14,3%", "100,0%"),
  `Variação (p.p.)` = c("+2,6", "+0,5", "-6,3", "+8,4", "-5,1", "—")
)

t13_desafio_informacoes <- tt(tabela_q13_dados) |>
  format_tt(escape = TRUE)

df_plot_q13 <- df_q13 |>
  count(AnoCol, escala, name = "Respondentes") |>
  group_by(AnoCol) |>
  mutate(
    Total = sum(Respondentes),
    Percentual = Respondentes / Total,
    Rotulo = percent(Percentual, accuracy = 0.1, decimal.mark = ",")
  ) |>
  ungroup()

g13_desafio_informacoes <- ggplot(df_plot_q13, aes(x = escala, y = Percentual, fill = AnoCol)) +
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
    limits = c(0, 0.42),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    title = "Assimetria de Informações: Desafio para Obter Dados de Desempenho e Permanência",
    subtitle = "Quase metade dos gestores (45% a 48%) relata alto desafio (notas 4 e 5), exigindo plataformas digitais integradas
(Cinza: 2025 | Azul: 2026)",
    x = "Nível de Desafio na Obtenção de Dados",
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
