# ==============================================================================
# Projeto Primeiro Emprego (PPE) - Avaliação dos Gestores / Pontos Focais
# Script: gest_18_q16_clareza_objetivos_profissionais.R - Clareza de Objetivos
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

df_q16 <- df |>
  mutate(
    AnoCol = factor(AnoCol, levels = c("2025", "2026")),
    escala = factor(q16, levels = 1:5, labels = c(
      "1 - Discordo totalmente", "2 - Discordo em parte", "3 - Neutro / Indeciso",
      "4 - Concordo em parte", "5 - Concordo totalmente"
    ))
  )

# 2025 (N=67): 1=3 (4.5%), 2=4 (6.0%), 3=22 (32.8%), 4=26 (38.8%), 5=12 (17.9%) -> 4+5 = 56.7%
# 2026 (N=77): 1=2 (2.6%), 2=5 (6.5%), 3=29 (37.7%), 4=23 (29.9%), 5=18 (23.4%) -> 4+5 = 53.2% (-3.5 p.p.)
tabela_q16_dados <- tibble(
  `Clareza sobre Objetivos Profissionais Futuros` = c(
    "1 - Discordo totalmente", "2 - Discordo em parte", "3 - Neutro / Indeciso",
    "4 - Concordo em parte", "5 - Concordo totalmente", "Total"
  ),
  `2025 (N)` = c("3", "4", "22", "26", "12", "67"),
  `2025 (%)` = c("4,5%", "6,0%", "32,8%", "38,8%", "17,9%", "100,0%"),
  `2026 (N)` = c("2", "5", "29", "23", "18", "77"),
  `2026 (%)` = c("2,6%", "6,5%", "37,7%", "29,9%", "23,4%", "100,0%"),
  `Variação (p.p.)` = c("-1,9", "+0,5", "+4,9", "-8,9", "+5,5", "—")
)

t16_clareza_objetivos <- tt(tabela_q16_dados) |>
  format_tt(escape = TRUE)

df_plot_q16 <- df_q16 |>
  count(AnoCol, escala, name = "Respondentes") |>
  group_by(AnoCol) |>
  mutate(
    Total = sum(Respondentes),
    Percentual = Respondentes / Total,
    Rotulo = percent(Percentual, accuracy = 0.1, decimal.mark = ",")
  ) |>
  ungroup()

g16_clareza_objetivos <- ggplot(df_plot_q16, aes(x = escala, y = Percentual, fill = AnoCol)) +
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
    limits = c(0, 0.45),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    title = "Perspectiva de Carreira: Clareza dos Beneficiários sobre Objetivos Profissionais",
    subtitle = "Predomínio de concordância (~53-57%), acompanhado de expressiva zona de indecisão (37,7% em 2026)",
    x = "Grau de Concordância com a Clareza de Objetivos",
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
