# ==============================================================================
# Projeto Primeiro Emprego (PPE) - Avaliação dos Gestores / Pontos Focais
# Script: gest_07_q5_principais_facilidades.R - Principais Facilidades e Aspectos Positivos
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

tabela_q5_dados <- tibble(
  `Eixo Temático das Facilidades e Aspectos Positivos` = c(
    "Apoio, acolhimento e engajamento da equipe de trabalho",
    "Diálogo aberto, escuta ativa e empatia na relação",
    "Orientação formativa, mentoria e transmissão de conhecimentos",
    "Satisfação em acompanhar o crescimento e evolução do jovem",
    "Total de Respondentes com Menções Qualitativas"
  ),
  `2025 (N)` = c("16", "19", "17", "8", "67"),
  `2025 (%)` = c("23,9%", "28,4%", "25,4%", "11,9%", "100,0%"),
  `2026 (N)` = c("28", "23", "21", "17", "77"),
  `2026 (%)` = c("36,4%", "29,9%", "27,3%", "22,1%", "100,0%"),
  `Variação (p.p.)` = c("+12,5", "+1,5", "+1,9", "+10,2", "—")
)

t05_facilidades_categorias <- tt(tabela_q5_dados) |>
  format_tt(escape = TRUE)

df_plot_q5 <- tibble(
  Eixo = factor(rep(c(
    "Apoio e acolhimento da equipe",
    "Diálogo aberto e empatia",
    "Orientação e mentoria",
    "Acompanhar evolução do jovem"
  ), each = 2), levels = c(
    "Apoio e acolhimento da equipe",
    "Diálogo aberto e empatia",
    "Orientação e mentoria",
    "Acompanhar evolução do jovem"
  )),
  AnoCol = factor(rep(c("2025", "2026"), 4), levels = c("2025", "2026")),
  Percentual = c(0.239, 0.364, 0.284, 0.299, 0.254, 0.273, 0.119, 0.221)
) |>
  mutate(Rotulo = percent(Percentual, accuracy = 0.1, decimal.mark = ","))

g05_facilidades_frequencia <- ggplot(df_plot_q5, aes(x = fct_rev(Eixo), y = Percentual, fill = AnoCol)) +
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
    limits = c(0, 0.45),
    expand = expansion(mult = c(0, 0.08))
  ) +
  labs(
    title = "Potencialidades do PPE: Facilidades e Melhores Aspectos da Tutoria",
    subtitle = "Forte expansão no apoio da equipe (+12,5 p.p.) e na satisfação com a evolução do jovem (+10,2 p.p.)
(Cinza: 2025 | Azul: 2026)",
    x = "Eixo Temático de Facilidade",
    y = "Proporção de Gestores Citantes (%)",
    caption = "Fonte: Dados qualitativos categorizados da pesquisa com Pontos Focais do PPE (2025 - 2026)."
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
