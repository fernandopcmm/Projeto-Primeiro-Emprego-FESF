# ==============================================================================
# Projeto Primeiro Emprego (PPE) - Avaliação dos Gestores / Pontos Focais
# Script: gest_19_q17_adaptacao_servico_publico.R - Adaptação ao Serviço Público
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

df_q17 <- df |>
  mutate(
    AnoCol = factor(AnoCol, levels = c("2025", "2026")),
    escala = factor(q17, levels = 1:5, labels = c(
      "1 - Não se adaptam", "2 - Baixa adaptação", "3 - Adaptação moderada",
      "4 - Boa adaptação", "5 - Excelente adaptação"
    ))
  )

# 2025 (N=67): 1=0 (0.0%), 2=1 (1.5%), 3=13 (19.4%), 4=29 (43.3%), 5=24 (35.8%) -> 4+5 = 79.1%
# 2026 (N=77): 1=0 (0.0%), 2=1 (1.3%), 3=18 (23.4%), 4=32 (41.6%), 5=26 (33.8%) -> 4+5 = 75.3% (-3.8 p.p.)
tabela_q17_dados <- tibble(
  `Adaptação à Cultura e Rotinas do Serviço Público` = c(
    "1 - Não se adaptam", "2 - Baixa adaptação", "3 - Adaptação moderada",
    "4 - Boa adaptação", "5 - Excelente adaptação", "Total"
  ),
  `2025 (N)` = c("0", "1", "13", "29", "24", "67"),
  `2025 (%)` = c("0,0%", "1,5%", "19,4%", "43,3%", "35,8%", "100,0%"),
  `2026 (N)` = c("0", "1", "18", "32", "26", "77"),
  `2026 (%)` = c("0,0%", "1,3%", "23,4%", "41,6%", "33,8%", "100,0%"),
  `Variação (p.p.)` = c("0,0", "-0,2", "+4,0", "-1,7", "-2,0", "—")
)

t17_adaptacao_publica <- tt(tabela_q17_dados) |>
  format_tt(escape = TRUE)

df_plot_q17 <- df_q17 |>
  count(AnoCol, escala, name = "Respondentes") |>
  group_by(AnoCol) |>
  mutate(
    Total = sum(Respondentes),
    Percentual = Respondentes / Total,
    Rotulo = percent(Percentual, accuracy = 0.1, decimal.mark = ",")
  ) |>
  ungroup()

if (!any(df_plot_q17$escala == "1 - Não se adaptam")) {
  df_plot_q17 <- df_plot_q17 |>
    add_row(AnoCol = factor("2025", levels = c("2025", "2026")),
            escala = factor("1 - Não se adaptam", levels = levels(df_q17$escala)),
            Respondentes = 0, Total = 67, Percentual = 0, Rotulo = "0,0%") |>
    add_row(AnoCol = factor("2026", levels = c("2025", "2026")),
            escala = factor("1 - Não se adaptam", levels = levels(df_q17$escala)),
            Respondentes = 0, Total = 77, Percentual = 0, Rotulo = "0,0%")
}

g17_adaptacao_publica <- ggplot(df_plot_q17, aes(x = escala, y = Percentual, fill = AnoCol)) +
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
    limits = c(0, 0.50),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    title = "Socialização Institucional: Adaptação à Cultura e Rotinas do Setor Público",
    subtitle = "Consistente aprovação gerencial (75% a 79% em notas 4 e 5) e rejeição residual quase nula (<2%)",
    x = "Grau de Adaptação às Normas e Processos Públicos",
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
