# ==============================================================================
# Projeto Primeiro Emprego (PPE) - Avaliação dos Gestores / Pontos Focais
# Script: gest_17_q15_imaturidade_desafio.R - Imaturidade como Desafio
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

df_q15 <- df |>
  mutate(
    AnoCol = factor(AnoCol, levels = c("2025", "2026")),
    escala = factor(q15, levels = 1:5, labels = c(
      "1 - Não é desafio", "2 - Baixo desafio", "3 - Desafio moderado",
      "4 - Alto desafio", "5 - Desafio crítico"
    ))
  )

# 2025 (N=67): 1=7 (10.4%), 2=12 (17.9%), 3=16 (23.9%), 4=19 (28.4%), 5=13 (19.4%) -> 4+5 = 47.8%
# 2026 (N=77): 1=10 (13.0%), 2=3 (3.9%), 3=22 (28.6%), 4=23 (29.9%), 5=19 (24.7%) -> 4+5 = 54.5% (+6.7 p.p.)
tabela_q15_dados <- tibble(
  `Imaturidade dos Beneficiários como Desafio` = c(
    "1 - Não é desafio", "2 - Baixo desafio", "3 - Desafio moderado",
    "4 - Alto desafio", "5 - Desafio crítico", "Total"
  ),
  `2025 (N)` = c("7", "12", "16", "19", "13", "67"),
  `2025 (%)` = c("10,4%", "17,9%", "23,9%", "28,4%", "19,4%", "100,0%"),
  `2026 (N)` = c("10", "3", "22", "23", "19", "77"),
  `2026 (%)` = c("13,0%", "3,9%", "28,6%", "29,9%", "24,7%", "100,0%"),
  `Variação (p.p.)` = c("+2,6", "-14,0", "+4,7", "+1,5", "+5,3", "—")
)

t15_imaturidade <- tt(tabela_q15_dados) |>
  format_tt(escape = TRUE)

df_plot_q15 <- df_q15 |>
  count(AnoCol, escala, name = "Respondentes") |>
  group_by(AnoCol) |>
  mutate(
    Total = sum(Respondentes),
    Percentual = Respondentes / Total,
    Rotulo = percent(Percentual, accuracy = 0.1, decimal.mark = ",")
  ) |>
  ungroup()

g15_imaturidade <- ggplot(df_plot_q15, aes(x = escala, y = Percentual, fill = AnoCol)) +
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
    limits = c(0, 0.40),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    title = "Desafio Gerencial: Impacto da Imaturidade Comportamental no Acompanhamento",
    subtitle = "Percepção de alto desafio (notas 4 e 5) expande de 47,8% para 54,5% (+6,7 p.p.) sob coortes mais jovens",
    x = "Nível de Desafio Gerencial Decorrente da Imaturidade",
    y = "Proporção de Pontos Focais (%)",
    caption = "Fonte: Pesquisa de Avaliação do PPE com Pontos Focais (2025-2026). N = 144 (67 em 2025; 77 em 2026)."
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(color = COR_TEXTO, face = "bold", size = 12, margin = margin(b = 4)),
    plot.subtitle = element_text(color = COR_SUBTEXTO, size = 9.5, margin = margin(b = 10)),
    axis.title.x = element_text(color = COR_TEXTO, face = "bold", size = 9.5, margin = margin(t = 6)),
    axis.title.y = element_text(color = COR_TEXTO, face = "bold", size = 9.5, margin = margin(r = 6)),
    axis.text.x = element_text(color = COR_TEXTO, size = 8.5, angle = 12, hjust = 1),
    axis.text.y = element_text(color = COR_TEXTO, size = 9),
    panel.grid.major.y = element_line(color = COR_GRID, linewidth = 0.35),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position = "top",
    legend.justification = "right",
    legend.title = element_text(face = "bold", size = 8.5),
    legend.text = element_text(size = 8.5),
    plot.margin = margin(t = 10, r = 15, b = 10, l = 10)
  )
