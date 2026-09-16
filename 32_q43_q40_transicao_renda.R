# ==============================================================================
# Projeto Primeiro Emprego (PPE) - Análise Comparativa Longitudinal (2025-2026)
# Script 32: 32_q43_q40_transicao_renda.R - Transição e Alavancagem de Renda
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

df_trans_long <- bind_rows(
  df |> mutate(Momento = "Antes do PPE", Faixa = padroniza_renda(q43)),
  df |> mutate(Momento = "Atual (Com PPE)", Faixa = padroniza_renda(q40))
) |>
  mutate(
    AnoCol = factor(AnoCol, levels = c("2025", "2026")),
    Momento = factor(Momento, levels = c("Antes do PPE", "Atual (Com PPE)")),
    Faixa = factor(Faixa, levels = c(
      "Nenhuma renda",
      "Até 1,5 Salários Mínimos",
      "De 1,5 a 3 Salários Mínimos",
      "De 3 a 4,5 Salários Mínimos",
      "Acima de 4,5 Salários Mínimos"
    ))
  )

# Tabela sintética da transição (Pré vs Pós PPE consolidado)
tabela_trans_dados <- tibble(
  `Faixa de Renda Familiar` = c(
    "Nenhuma renda",
    "Até 1,5 Salários Mínimos",
    "De 1,5 a 3 Salários Mínimos",
    "De 3 a 4,5 Salários Mínimos",
    "Acima de 4,5 Salários Mínimos",
    "Total"
  ),
  `2025 Antes (%)` = c("23,6%", "57,7%", "16,2%", "2,3%", "0,3%", "100,0%"),
  `2025 Atual (%)` = c("6,8%", "49,1%", "39,4%", "3,8%", "1,0%", "100,0%"),
  `2025 Variação (p.p.)` = c("-16,9", "-8,6", "+23,2", "+1,5", "+0,7", "—"),
  `2026 Antes (%)` = c("24,5%", "57,3%", "16,2%", "1,8%", "0,2%", "100,0%"),
  `2026 Atual (%)` = c("7,3%", "49,7%", "37,7%", "4,7%", "0,7%", "100,0%"),
  `2026 Variação (p.p.)` = c("-17,2", "-7,7", "+21,5", "+2,9", "+0,4", "—")
)

t43_40_transicao <- tt(tabela_trans_dados) |>
  format_tt(escape = TRUE)

df_plot_trans <- df_trans_long |>
  count(AnoCol, Momento, Faixa, name = "Respondentes") |>
  group_by(AnoCol, Momento) |>
  mutate(
    Total = sum(Respondentes),
    Percentual = Respondentes / Total,
    Rotulo = percent(Percentual, accuracy = 0.1, decimal.mark = ",")
  ) |>
  ungroup()

g_transicao_renda <- ggplot(df_plot_trans, aes(x = Faixa, y = Percentual, fill = Momento)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.65, alpha = 0.95) +
  geom_text(
    aes(label = Rotulo),
    position = position_dodge(width = 0.75),
    vjust = -0.4,
    size = 2.9,
    fontface = "bold",
    color = COR_TEXTO
  ) +
  facet_wrap(~ AnoCol) +
  scale_fill_manual(
    name = "Momento Analítico",
    values = c("Antes do PPE" = COR_CONTEXTO, "Atual (Com PPE)" = COR_DESTAQUE)
  ) +
  scale_y_continuous(
    labels = label_percent(accuracy = 1),
    limits = c(0, 0.68),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    title = "Transição e Alavancagem de Renda: Impacto Socioeconômico do PPE",
    subtitle = "Retração maciça de lares sem renda (-17 p.p.) e expansão de lares com 1,5 a 3 salários mínimos (+22 p.p.)
(Cinza: Antes do PPE | Azul: Atual com remuneração do programa)",
    x = "Faixa de Renda Familiar Mensal",
    y = "Proporção de Famílias (%)",
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
    strip.text = element_text(color = COR_TEXTO, face = "bold", size = 11),
    axis.line.x = element_line(color = COR_CONTEXTO, linewidth = 0.5),
    axis.text.x = element_text(color = COR_TEXTO, size = 8.5, face = "bold", angle = 15, hjust = 1),
    axis.text.y = element_text(color = COR_SUBTEXTO, size = 9.5),
    plot.margin = margin(t = 12, r = 15, b = 12, l = 15)
  )
