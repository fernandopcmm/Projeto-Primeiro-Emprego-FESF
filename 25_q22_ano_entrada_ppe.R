# ==============================================================================
# Projeto Primeiro Emprego (PPE) - Análise Comparativa Longitudinal (2025-2026)
# Script 25: 25_q22_ano_entrada_ppe.R - Ano de Entrada no PPE e Maturação da Coorte
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

df_q22 <- df |>
  mutate(
    AnoCol = factor(AnoCol, levels = c("2025", "2026")),
    ano_ent = str_replace(q22, "\\.0$", ""),
    ano_ent = case_when(
      ano_ent %in% c("Antes de 2024", "2024", "2025", "2026") ~ ano_ent,
      TRUE ~ "Antes de 2024"
    ),
    ano_ent = factor(ano_ent, levels = c("Antes de 2024", "2024", "2025", "2026"))
  )

tabela_q22_dados <- tibble(
  `Ano de Entrada no PPE` = c("Antes de 2024", "2024", "2025", "2026", "Total"),
  `2025 (N)` = c("0", "537", "174", "0", "711"),
  `2025 (%)` = c("0,0%", "75,5%", "24,5%", "0,0%", "100,0%"),
  `2026 (N)` = c("25", "415", "271", "168", "879"),
  `2026 (%)` = c("2,8%", "47,2%", "30,8%", "19,1%", "100,0%"),
  `Variação (p.p.)` = c("+2,8", "-28,3", "+6,3", "+19,1", "—")
)

t22_ano_entrada <- tt(tabela_q22_dados) |>
  format_tt(escape = TRUE)

df_plot_q22 <- df_q22 |>
  count(AnoCol, ano_ent, name = "Respondentes") |>
  group_by(AnoCol) |>
  mutate(
    Total = sum(Respondentes),
    Percentual = Respondentes / Total,
    Rotulo = paste0(
      format(Respondentes, big.mark = ".", decimal.mark = ","), "
(",
      percent(Percentual, accuracy = 0.1, decimal.mark = ","), ")"
    )
  ) |>
  ungroup()

g_ano_entrada <- ggplot(df_plot_q22, aes(x = ano_ent, y = Percentual, fill = AnoCol)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.65, alpha = 0.95) +
  geom_text(
    aes(label = Rotulo),
    position = position_dodge(width = 0.75),
    vjust = -0.3,
    size = 3.2,
    fontface = "bold",
    color = COR_TEXTO,
    lineheight = 0.9
  ) +
  scale_fill_manual(
    name = "Ciclo Avaliativo",
    values = c("2025" = COR_CONTEXTO, "2026" = COR_DESTAQUE)
  ) +
  scale_y_continuous(
    labels = label_percent(accuracy = 1),
    limits = c(0, 0.85),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    title = "Composição das Coortes: Ano de Ingresso no Projeto Primeiro Emprego",
    subtitle = "Rotatividade e convivência simultânea de veteranos e recém-ingressos no programa
(Cinza: 2025 | Azul: 2026)",
    x = "Ano de Admissão no Programa",
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
