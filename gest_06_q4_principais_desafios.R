# ==============================================================================
# Projeto Primeiro Emprego (PPE) - Avaliação dos Gestores / Pontos Focais
# Script: gest_06_q4_principais_desafios.R - Principais Desafios Cotidianos
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

# Categorização semântica consolidada dos 144 gestores
tabela_q4_dados <- tibble(
  `Eixo Temático dos Desafios Relatados` = c(
    "Falta de tempo e sobrecarga das rotinas habituais",
    "Imaturidade, disciplina e postura profissional juvenil",
    "Falhas de comunicação e suporte lento da entidade parceira",
    "Infraestrutura deficitária e desvio/ausência de pessoal permanente",
    "Total de Respondentes com Menções Qualitativas"
  ),
  `2025 (N)` = c("15", "11", "11", "5", "67"),
  `2025 (%)` = c("22,4%", "16,4%", "16,4%", "7,5%", "100,0%"),
  `2026 (N)` = c("15", "17", "14", "2", "77"),
  `2026 (%)` = c("19,5%", "22,1%", "18,2%", "2,6%", "100,0%"),
  `Variação (p.p.)` = c("-2,9", "+5,7", "+1,8", "-4,9", "—")
)

t04_desafios_categorias <- tt(tabela_q4_dados) |>
  format_tt(escape = TRUE)

df_plot_q4 <- tibble(
  Eixo = factor(rep(c(
    "Falta de tempo / sobrecarga",
    "Imaturidade / postura juvenil",
    "Comunicação / suporte parceira",
    "Infraestrutura / desvio de função"
  ), each = 2), levels = c(
    "Falta de tempo / sobrecarga",
    "Imaturidade / postura juvenil",
    "Comunicação / suporte parceira",
    "Infraestrutura / desvio de função"
  )),
  AnoCol = factor(rep(c("2025", "2026"), 4), levels = c("2025", "2026")),
  Percentual = c(0.224, 0.195, 0.164, 0.221, 0.164, 0.182, 0.075, 0.026)
) |>
  mutate(Rotulo = percent(Percentual, accuracy = 0.1, decimal.mark = ","))

g04_desafios_frequencia <- ggplot(df_plot_q4, aes(x = fct_rev(Eixo), y = Percentual, fill = AnoCol)) +
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
    limits = c(0, 0.30),
    expand = expansion(mult = c(0, 0.08))
  ) +
  labs(
    title = "Gargalos da Gestão: Principais Desafios Enfrentados pelos Pontos Focais",
    subtitle = "Sobrecarga de tempo, manejo da imaturidade juvenil e ruídos com a entidade parceira
(Cinza: 2025 | Azul: 2026)",
    x = "Eixo Temático de Desafio",
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
