# ==============================================================================
# Projeto Primeiro Emprego (PPE) - Análise Comparativa Longitudinal (2025-2026)
# Script 30: 30_q43_renda_antes_ppe.R - Renda Familiar Anterior ao Ingresso no PPE
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

padroniza_renda <- function(x) {
  case_when(
    grepl("nenhum", x, ignore.case = TRUE) ~ "Nenhuma renda",
    grepl("at[ée] 1,5", x, ignore.case = TRUE) ~ "Até 1,5 Salários Mínimos",
    grepl("1,5 a 3", x, ignore.case = TRUE) ~ "De 1,5 a 3 Salários Mínimos",
    grepl("3 a 4,5", x, ignore.case = TRUE) ~ "De 3 a 4,5 Salários Mínimos",
    grepl("4,5 a 6|6 a 10|10 a 30", x, ignore.case = TRUE) ~ "Acima de 4,5 Salários Mínimos",
    TRUE ~ "Outra faixa"
  )
}

df_q43 <- df |>
  mutate(
    AnoCol = factor(AnoCol, levels = c("2025", "2026")),
    renda_antes = factor(padroniza_renda(q43), levels = c(
      "Nenhuma renda",
      "Até 1,5 Salários Mínimos",
      "De 1,5 a 3 Salários Mínimos",
      "De 3 a 4,5 Salários Mínimos",
      "Acima de 4,5 Salários Mínimos"
    ))
  )

df_resumo_q43 <- df_q43 |>
  count(AnoCol, renda_antes) |>
  pivot_wider(names_from = AnoCol, values_from = n, values_fill = 0) |>
  mutate(
    pct_2025 = `2025` / sum(`2025`),
    pct_2026 = `2026` / sum(`2026`),
    dif_pp = (pct_2026 - pct_2025) * 100
  )

tabela_q43_dados <- tibble(
  `Faixa de Renda Familiar Pré-PPE` = c(as.character(df_resumo_q43$renda_antes), "Total"),
  `2025 (N)` = c(as.character(df_resumo_q43$`2025`), "711"),
  `2025 (%)` = c(percent(df_resumo_q43$pct_2025, accuracy = 0.1, decimal.mark = ","), "100,0%"),
  `2026 (N)` = c(as.character(df_resumo_q43$`2026`), "879"),
  `2026 (%)` = c(percent(df_resumo_q43$pct_2026, accuracy = 0.1, decimal.mark = ","), "100,0%"),
  `Variação (p.p.)` = c(sprintf("%+.1f", df_resumo_q43$dif_pp), "—")
)

t43_renda_antes <- tt(tabela_q43_dados) |>
  format_tt(escape = TRUE)

df_plot_q43 <- df_q43 |>
  count(AnoCol, renda_antes, name = "Respondentes") |>
  group_by(AnoCol) |>
  mutate(
    Total = sum(Respondentes),
    Percentual = Respondentes / Total,
    Rotulo = percent(Percentual, accuracy = 0.1, decimal.mark = ",")
  ) |>
  ungroup()

g_renda_antes <- ggplot(df_plot_q43, aes(x = renda_antes, y = Percentual, fill = AnoCol)) +
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
    limits = c(0, 0.70),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    title = "Renda de Origem: Situação Econômica da Família Antes do Ingresso no PPE",
    subtitle = "Vulnerabilidade profunda: ~81-82% sobreviviam sem renda ou com até 1,5 salário mínimo
(Cinza: 2025 | Azul: 2026)",
    x = "Faixa de Renda Familiar Prévia",
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
    axis.text.x = element_text(color = COR_TEXTO, size = 9.5, face = "bold"),
    axis.text.y = element_text(color = COR_SUBTEXTO, size = 9.5),
    plot.margin = margin(t = 12, r = 15, b = 12, l = 15)
  )
