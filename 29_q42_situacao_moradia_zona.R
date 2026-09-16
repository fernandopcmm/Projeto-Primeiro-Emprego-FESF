# ==============================================================================
# Projeto Primeiro Emprego (PPE) - Análise Comparativa Longitudinal (2025-2026)
# Script 29: 29_q42_situacao_moradia_zona.R - Zona de Moradia e Acesso ao Trabalho
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

df_q42 <- df |>
  mutate(
    AnoCol = factor(AnoCol, levels = c("2025", "2026")),
    zona_moradia = case_when(
      grepl("zona urbana", q42, ignore.case = TRUE) ~ "Zona Urbana",
      grepl("longe do trabalho", q42, ignore.case = TRUE) ~ "Zona Rural (Deslocamento longo ≥2h)",
      grepl("zona rural", q42, ignore.case = TRUE) ~ "Zona Rural (Deslocamento regular)",
      TRUE ~ "Zona Urbana"
    ),
    zona_moradia = factor(zona_moradia, levels = c(
      "Zona Urbana",
      "Zona Rural (Deslocamento regular)",
      "Zona Rural (Deslocamento longo ≥2h)"
    ))
  )

df_resumo_q42 <- df_q42 |>
  count(AnoCol, zona_moradia) |>
  pivot_wider(names_from = AnoCol, values_from = n, values_fill = 0) |>
  mutate(
    pct_2025 = `2025` / sum(`2025`),
    pct_2026 = `2026` / sum(`2026`),
    dif_pp = (pct_2026 - pct_2025) * 100
  )

tabela_q42_dados <- tibble(
  `Zona de Moradia e Condição de Acesso` = c(as.character(df_resumo_q42$zona_moradia), "Total"),
  `2025 (N)` = c(as.character(df_resumo_q42$`2025`), "711"),
  `2025 (%)` = c(percent(df_resumo_q42$pct_2025, accuracy = 0.1, decimal.mark = ","), "100,0%"),
  `2026 (N)` = c(as.character(df_resumo_q42$`2026`), "879"),
  `2026 (%)` = c(percent(df_resumo_q42$pct_2026, accuracy = 0.1, decimal.mark = ","), "100,0%"),
  `Variação (p.p.)` = c(sprintf("%+.1f", df_resumo_q42$dif_pp), "—")
)

t42_moradia <- tt(tabela_q42_dados) |>
  format_tt(escape = TRUE)

df_plot_q42 <- df_q42 |>
  count(AnoCol, zona_moradia, name = "Respondentes") |>
  group_by(AnoCol) |>
  mutate(
    Total = sum(Respondentes),
    Percentual = Respondentes / Total,
    Rotulo = percent(Percentual, accuracy = 0.1, decimal.mark = ",")
  ) |>
  ungroup()

g_moradia <- ggplot(df_plot_q42, aes(x = zona_moradia, y = Percentual, fill = AnoCol)) +
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
    limits = c(0, 1.0),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    title = "Ambiente de Moradia: Inserção Urbana vs. Rural dos Beneficiários",
    subtitle = "Predominância urbana (~83-89%) e avanço da capilaridade rural em 2026 (17,2% somadas)
(Cinza: 2025 | Azul: 2026)",
    x = "Localização e Condição de Acesso",
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
