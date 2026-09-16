# ==============================================================================
# Projeto Primeiro Emprego (PPE) - Avaliação dos Gestores / Pontos Focais
# Script: gest_11_q9_insercao_cultura_orgao.R - Inserção na Cultura Organizacional
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

df_q9 <- df |>
  mutate(
    AnoCol = factor(AnoCol, levels = c("2025", "2026")),
    insercao = factor(q9_cat, levels = c(
      "Acompanhamento por servidor mais experiente",
      "Múltiplas formas / Outras práticas",
      "Apresentação formal à equipe"
    ))
  )

tabela_q9_dados <- tibble(
  `Forma de Inserção na Cultura do Órgão` = c(
    "Acompanhamento por servidor mais experiente (mentoria)",
    "Múltiplas práticas integradas (reuniões, eventos, manuais)",
    "Apresentação formal inicial à equipe",
    "Total"
  ),
  `2025 (N)` = c("38", "19", "10", "67"),
  `2025 (%)` = c("56,7%", "28,4%", "14,9%", "100,0%"),
  `2026 (N)` = c("40", "25", "12", "77"),
  `2026 (%)` = c("51,9%", "32,5%", "15,6%", "100,0%"),
  `Variação (p.p.)` = c("-4,8", "+4,1", "+0,7", "—")
)

t09_insercao_cultura <- tt(tabela_q9_dados) |>
  format_tt(escape = TRUE)

df_plot_q9 <- df_q9 |>
  count(AnoCol, insercao, name = "Respondentes") |>
  group_by(AnoCol) |>
  mutate(
    Total = sum(Respondentes),
    Percentual = Respondentes / Total,
    Rotulo = percent(Percentual, accuracy = 0.1, decimal.mark = ",")
  ) |>
  ungroup()

g09_insercao_cultura <- ggplot(df_plot_q9, aes(x = fct_rev(insercao), y = Percentual, fill = AnoCol)) +
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
    limits = c(0, 0.65),
    expand = expansion(mult = c(0, 0.08))
  ) +
  labs(
    title = "Socialização Organizacional: Modos de Inserção dos Jovens na Cultura do Serviço Público",
    subtitle = "Hegemonia do apadrinhamento funcional por servidor sênior (~52-57%) e práticas multifacetadas
(Cinza: 2025 | Azul: 2026)",
    x = "Prática de Socialização",
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
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = COR_GRID, linewidth = 0.4),
    axis.line.y = element_line(color = COR_CONTEXTO, linewidth = 0.5),
    axis.text.y = element_text(color = COR_TEXTO, size = 9.5, face = "bold"),
    axis.text.x = element_text(color = COR_SUBTEXTO, size = 9.5),
    plot.margin = margin(t = 12, r = 15, b = 12, l = 15)
  )
