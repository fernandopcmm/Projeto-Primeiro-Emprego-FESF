# ==============================================================================
# Projeto Primeiro Emprego (PPE) - Avaliação dos Gestores / Pontos Focais
# Script: gest_16_q14_proatividade_jovens.R - Proatividade dos Beneficiários
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

df_q14 <- df |>
  mutate(
    AnoCol = factor(AnoCol, levels = c("2025", "2026")),
    escala = factor(q14, levels = 1:5, labels = c(
      "1 - Totalmente passivos", "2 - Pouco proativos", "3 - Medianamente proativos",
      "4 - Muito proativos", "5 - Altamente proativos"
    ))
  )

# 2025 (N=67): 1=1 (1.5%), 2=2 (3.0%), 3=14 (20.9%), 4=16 (23.9%), 5=34 (50.7%) -> 4+5 = 74.6%
# 2026 (N=77): 1=0 (0.0%), 2=2 (2.6%), 3=11 (14.3%), 4=35 (45.5%), 5=29 (37.7%) -> 4+5 = 83.1% (+8.5 p.p.)
tabela_q14_dados <- tibble(
  `Proatividade Geral dos Beneficiários` = c(
    "1 - Totalmente passivos", "2 - Pouco proativos", "3 - Medianamente proativos",
    "4 - Muito proativos", "5 - Altamente proativos", "Total"
  ),
  `2025 (N)` = c("1", "2", "14", "16", "34", "67"),
  `2025 (%)` = c("1,5%", "3,0%", "20,9%", "23,9%", "50,7%", "100,0%"),
  `2026 (N)` = c("0", "2", "11", "35", "29", "77"),
  `2026 (%)` = c("0,0%", "2,6%", "14,3%", "45,5%", "37,7%", "100,0%"),
  `Variação (p.p.)` = c("-1,5", "-0,4", "-6,6", "+21,6", "-13,0", "—")
)

t14_proatividade <- tt(tabela_q14_dados) |>
  format_tt(escape = TRUE)

df_plot_q14 <- df_q14 |>
  count(AnoCol, escala, name = "Respondentes") |>
  group_by(AnoCol) |>
  mutate(
    Total = sum(Respondentes),
    Percentual = Respondentes / Total,
    Rotulo = percent(Percentual, accuracy = 0.1, decimal.mark = ",")
  ) |>
  ungroup()

# Assegurar nivel 1 presente para 2026 mesmo sendo 0
if (!any(df_plot_q14$AnoCol == "2026" & df_plot_q14$escala == "1 - Totalmente passivos")) {
  df_plot_q14 <- df_plot_q14 |>
    add_row(AnoCol = factor("2026", levels = c("2025", "2026")),
            escala = factor("1 - Totalmente passivos", levels = levels(df_q14$escala)),
            Respondentes = 0, Total = 77, Percentual = 0, Rotulo = "0,0%")
}

g14_proatividade <- ggplot(df_plot_q14, aes(x = escala, y = Percentual, fill = AnoCol)) +
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
    limits = c(0, 0.60),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    title = "Avaliação de Desempenho: Proatividade dos Beneficiários no Trabalho",
    subtitle = "Avaliação positiva acumulada (notas 4 e 5) avança de 74,6% para expressivos 83,1% em 2026 (+8,5 p.p.)",
    x = "Grau de Proatividade e Iniciativa Percebida",
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
