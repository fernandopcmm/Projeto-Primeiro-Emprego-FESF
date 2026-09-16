# ==============================================================================
# Projeto Primeiro Emprego (PPE) - Análise Comparativa Longitudinal (2025-2026)
# Script 35: 35_q29_trabalho_anterior_formal.R - Trabalho Anterior (Visão Formal)
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

df_q29 <- df |>
  mutate(
    AnoCol = factor(AnoCol, levels = c("2025", "2026")),
    trabalhou_ant = factor(q29, levels = c("Não", "Sim"))
  )

tabela_q29_dados <- tibble(
  `Já Trabalhou Antes do PPE?` = c("Não (Primeiro Emprego Declarado)", "Sim (Experiência Anterior)", "Total"),
  `2025 (N)` = c("454", "257", "711"),
  `2025 (%)` = c("63,9%", "36,1%", "100,0%"),
  `2026 (N)` = c("552", "327", "879"),
  `2026 (%)` = c("62,8%", "37,2%", "100,0%"),
  `Variação (p.p.)` = c("-1,1", "+1,1", "—")
)

t29_trabalho_formal <- tt(tabela_q29_dados) |>
  format_tt(escape = TRUE)

df_plot_q29 <- df_q29 |>
  count(AnoCol, trabalhou_ant, name = "Respondentes") |>
  group_by(AnoCol) |>
  mutate(
    Total = sum(Respondentes),
    Percentual = Respondentes / Total,
    Rotulo = paste0(
      format(Respondentes, big.mark = ".", decimal.mark = ","), " (",
      percent(Percentual, accuracy = 0.1, decimal.mark = ","), ")"
    )
  ) |>
  ungroup()

g_trabalho_formal <- ggplot(df_plot_q29, aes(x = trabalhou_ant, y = Percentual, fill = AnoCol)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.65, alpha = 0.95) +
  geom_text(
    aes(label = Rotulo),
    position = position_dodge(width = 0.75),
    vjust = -0.4,
    size = 3.3,
    fontface = "bold",
    color = COR_TEXTO
  ) +
  scale_fill_manual(
    name = "Ciclo Avaliativo",
    values = c("2025" = COR_CONTEXTO, "2026" = COR_DESTAQUE)
  ) +
  scale_y_continuous(
    labels = label_percent(accuracy = 1),
    limits = c(0, 0.75),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    title = "Histórico Ocupacional: Já Trabalhou Antes de Ingressar no PPE?",
    subtitle = "Quase dois terços (~63-64%) declaram estar em seu primeiro emprego ao ingressar no PPE
(Cinza: 2025 | Azul: 2026)",
    x = "Declaração de Trabalho Prévio",
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
