# ==============================================================================
# Projeto Primeiro Emprego (PPE) - Avaliação dos Gestores / Pontos Focais
# Script: gest_20_q18_relacionamento_equipe.R - Convivência e Relação com Equipe
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

df_q18 <- df |>
  mutate(
    AnoCol = factor(AnoCol, levels = c("2025", "2026")),
    escala = factor(q18, levels = 1:5, labels = c(
      "1 - Relação muito ruim", "2 - Relação ruim", "3 - Relação regular",
      "4 - Boa relação", "5 - Excelente relação"
    ))
  )

# 2025 (N=67): 1=0 (0.0%), 2=0 (0.0%), 3=7 (10.4%), 4=32 (47.8%), 5=28 (41.8%) -> 4+5 = 89.6%
# 2026 (N=77): 1=0 (0.0%), 2=1 (1.3%), 3=2 (2.6%), 4=33 (42.9%), 5=41 (53.2%) -> 4+5 = 96.1% (+6.5 p.p.)
tabela_q18_dados <- tibble(
  `Relacionamento Interpessoal com a Equipe de Trabalho` = c(
    "1 - Relação muito ruim", "2 - Relação ruim", "3 - Relação regular",
    "4 - Boa relação", "5 - Excelente relação", "Total"
  ),
  `2025 (N)` = c("0", "0", "7", "32", "28", "67"),
  `2025 (%)` = c("0,0%", "0,0%", "10,4%", "47,8%", "41,8%", "100,0%"),
  `2026 (N)` = c("0", "1", "2", "33", "41", "77"),
  `2026 (%)` = c("0,0%", "1,3%", "2,6%", "42,9%", "53,2%", "100,0%"),
  `Variação (p.p.)` = c("0,0", "+1,3", "-7,8", "-4,9", "+11,4", "—")
)

t18_relacionamento_equipe <- tt(tabela_q18_dados) |>
  format_tt(escape = TRUE)

df_plot_q18 <- df_q18 |>
  count(AnoCol, escala, name = "Respondentes") |>
  group_by(AnoCol) |>
  mutate(
    Total = sum(Respondentes),
    Percentual = Respondentes / Total,
    Rotulo = percent(Percentual, accuracy = 0.1, decimal.mark = ",")
  ) |>
  ungroup()

if (!any(df_plot_q18$escala == "1 - Relação muito ruim")) {
  df_plot_q18 <- df_plot_q18 |>
    add_row(AnoCol = factor("2025", levels = c("2025", "2026")),
            escala = factor("1 - Relação muito ruim", levels = levels(df_q18$escala)),
            Respondentes = 0, Total = 67, Percentual = 0, Rotulo = "0,0%") |>
    add_row(AnoCol = factor("2026", levels = c("2025", "2026")),
            escala = factor("1 - Relação muito ruim", levels = levels(df_q18$escala)),
            Respondentes = 0, Total = 77, Percentual = 0, Rotulo = "0,0%")
}
if (!any(df_plot_q18$AnoCol == "2025" & df_plot_q18$escala == "2 - Relação ruim")) {
  df_plot_q18 <- df_plot_q18 |>
    add_row(AnoCol = factor("2025", levels = c("2025", "2026")),
            escala = factor("2 - Relação ruim", levels = levels(df_q18$escala)),
            Respondentes = 0, Total = 67, Percentual = 0, Rotulo = "0,0%")
}

g18_relacionamento_equipe <- ggplot(df_plot_q18, aes(x = escala, y = Percentual, fill = AnoCol)) +
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
    limits = c(0, 0.62),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    title = "Convivência Socioprofissional: Relacionamento dos Jovens com a Equipe",
    subtitle = "Consagração relacional: 96,1% de avaliações positivas em 2026, com nota máxima atingindo 53,2% (+11,4 p.p.)",
    x = "Qualidade da Convivência Interpessoal no Trabalho",
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
