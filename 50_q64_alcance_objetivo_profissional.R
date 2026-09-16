# ==============================================================================
# Projeto Primeiro Emprego (PPE) - Análise Comparativa Longitudinal (2025-2026)
# Script 50: 50_q64_alcance_objetivo_profissional.R - Concretização de Metas
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

df_q64 <- df |>
  mutate(
    AnoCol = factor(AnoCol, levels = c("2025", "2026")),
    alcance = case_when(
      grepl("almejo objetivo maior", q64) ~ "Alcançou e almeja objetivo maior",
      grepl("estou satisfeito", q64) ~ "Alcançou e está satisfeito(a)",
      grepl("mais pr[óo]ximo", q64) ~ "Não alcançou, mas está mais próximo",
      grepl("agora tenho", q64) ~ "Não tinha objetivo e agora tem",
      grepl("não sinto que estou mais próximo", q64) ~ "Não alcançou nem está próximo",
      TRUE ~ "Em dúvida / Não definido"
    ),
    alcance = factor(alcance, levels = c(
      "Alcançou e almeja objetivo maior",
      "Não alcançou, mas está mais próximo",
      "Alcançou e está satisfeito(a)",
      "Não tinha objetivo e agora tem",
      "Não alcançou nem está próximo",
      "Em dúvida / Não definido"
    ))
  )

df_resumo_q64 <- df_q64 |>
  count(AnoCol, alcance) |>
  pivot_wider(names_from = AnoCol, values_from = n, values_fill = 0) |>
  mutate(
    pct_2025 = `2025` / sum(`2025`),
    pct_2026 = `2026` / sum(`2026`),
    dif_pp = (pct_2026 - pct_2025) * 100
  )

tabela_q64_dados <- tibble(
  `Situação em Relação ao Objetivo Profissional Hoje` = c(as.character(df_resumo_q64$alcance), "Total"),
  `2025 (N)` = c(as.character(df_resumo_q64$`2025`), "711"),
  `2025 (%)` = c(percent(df_resumo_q64$pct_2025, accuracy = 0.1, decimal.mark = ","), "100,0%"),
  `2026 (N)` = c(as.character(df_resumo_q64$`2026`), "879"),
  `2026 (%)` = c(percent(df_resumo_q64$pct_2026, accuracy = 0.1, decimal.mark = ","), "100,0%"),
  `Variação (p.p.)` = c(sprintf("%+.1f", df_resumo_q64$dif_pp), "—")
)

t64_alcance_objetivo <- tt(tabela_q64_dados) |>
  format_tt(escape = TRUE)

df_plot_q64 <- df_q64 |>
  count(AnoCol, alcance, name = "Respondentes") |>
  group_by(AnoCol) |>
  mutate(
    Total = sum(Respondentes),
    Percentual = Respondentes / Total,
    Rotulo = percent(Percentual, accuracy = 0.1, decimal.mark = ",")
  ) |>
  ungroup()

g_alcance_objetivo <- ggplot(df_plot_q64, aes(x = fct_rev(alcance), y = Percentual, fill = AnoCol)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.65, alpha = 0.95) +
  geom_text(
    aes(label = Rotulo),
    position = position_dodge(width = 0.75),
    hjust = -0.15,
    size = 3.1,
    fontface = "bold",
    color = COR_TEXTO
  ) +
  coord_flip() +
  scale_fill_manual(
    name = "Ciclo Avaliativo",
    values = c("2025" = COR_CONTEXTO, "2026" = COR_DESTAQUE)
  ) +
  scale_y_continuous(
    labels = label_percent(accuracy = 1),
    limits = c(0, 0.75),
    expand = expansion(mult = c(0, 0.08))
  ) +
  labs(
    title = "Eficácia Biográfica: Alcance e Expansão das Metas Profissionais",
    subtitle = "Quase 80% atingiram suas metas originais e 64-66% almejam voos profissionais maiores
(Cinza: 2025 | Azul: 2026)",
    x = "Avaliação da Concretização de Metas",
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
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = COR_GRID, linewidth = 0.4),
    axis.line.y = element_line(color = COR_CONTEXTO, linewidth = 0.5),
    axis.text.y = element_text(color = COR_TEXTO, size = 9.5, face = "bold"),
    axis.text.x = element_text(color = COR_SUBTEXTO, size = 9.5),
    plot.margin = margin(t = 12, r = 15, b = 12, l = 15)
  )
