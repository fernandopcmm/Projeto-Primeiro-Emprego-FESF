# ==============================================================================
# Projeto Primeiro Emprego (PPE) - Análise Comparativa Longitudinal (2025-2026)
# Script 38: 38_q49_instalacoes_fisicas.R - Adequação das Instalações Físicas
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

caminho_csv <- "/home/nando/Documentos/gemini/ATUAL/OUTPUT/PPE_beneficiarios_2025_2026.csv"
df <- read_csv(caminho_csv, show_col_types = FALSE)

df_q49 <- df |>
  mutate(
    AnoCol = factor(AnoCol, levels = c("2025", "2026")),
    adequacao = factor(q49, levels = c("Sim, todas", "Sim, a maior parte", "Somente algumas", "Nenhuma"))
  )

df_resumo_q49 <- df_q49 |>
  count(AnoCol, adequacao) |>
  pivot_wider(names_from = AnoCol, values_from = n, values_fill = 0) |>
  mutate(
    pct_2025 = `2025` / sum(`2025`),
    pct_2026 = `2026` / sum(`2026`),
    dif_pp = (pct_2026 - pct_2025) * 100
  )

tabela_q49_dados <- tibble(
  `Adequação das Instalações Físicas` = c(as.character(df_resumo_q49$adequacao), "Total"),
  `2025 (N)` = c(as.character(df_resumo_q49$`2025`), "711"),
  `2025 (%)` = c(percent(df_resumo_q49$pct_2025, accuracy = 0.1, decimal.mark = ","), "100,0%"),
  `2026 (N)` = c(as.character(df_resumo_q49$`2026`), "879"),
  `2026 (%)` = c(percent(df_resumo_q49$pct_2026, accuracy = 0.1, decimal.mark = ","), "100,0%"),
  `Variação (p.p.)` = c(sprintf("%+.1f", df_resumo_q49$dif_pp), "—")
)

t49_instalacoes <- tt(tabela_q49_dados) |>
  format_tt(escape = TRUE)

df_plot_q49 <- df_q49 |>
  count(AnoCol, adequacao, name = "Respondentes") |>
  group_by(AnoCol) |>
  mutate(
    Total = sum(Respondentes),
    Percentual = Respondentes / Total,
    Rotulo = percent(Percentual, accuracy = 0.1, decimal.mark = ",")
  ) |>
  ungroup()

g_instalacoes <- ggplot(df_plot_q49, aes(x = adequacao, y = Percentual, fill = AnoCol)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.65, alpha = 0.95) +
  geom_text(
    aes(label = Rotulo),
    position = position_dodge(width = 0.75),
    vjust = -0.4,
    size = 3.2,
    fontface = "bold",
    color = COR_TEXTO
  ) +
  scale_fill_manual(
    name = "Ciclo Avaliativo",
    values = c("2025" = COR_CONTEXTO, "2026" = COR_DESTAQUE)
  ) +
  scale_y_continuous(
    labels = label_percent(accuracy = 1),
    limits = c(0, 0.55),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    title = "Ambiente Laboral: Adequação das Instalações Físicas no Posto de Atuação",
    subtitle = "Mais de 85% atestam que as instalações são totalmente ou majoritariamente adequadas
(Cinza: 2025 | Azul: 2026)",
    x = "Grau de Adequação da Infraestrutura",
    y = "Proporção de Beneficiários (%)",
    caption = "Fonte: Dados consolidados das pesquisas de avaliação do Projeto Primeiro Emprego (2025 - 2026)."
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
    axis.text.x = element_text(color = COR_TEXTO, size = 10, face = "bold"),
    axis.text.y = element_text(color = COR_SUBTEXTO, size = 9.5),
    plot.margin = margin(t = 12, r = 15, b = 12, l = 15)
  )
