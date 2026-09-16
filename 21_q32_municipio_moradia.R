# ==============================================================================
# Projeto Primeiro Emprego (PPE) - Análise Comparativa Longitudinal (2025-2026)
# Script 21: 21_q32_municipio_moradia.R - Município de Residência Atual
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

mun_padrao <- function(x) {
  x <- str_to_title(str_trim(x))
  case_when(
    grepl("salvador", x, ignore.case = TRUE) ~ "Salvador",
    grepl("jequi", x, ignore.case = TRUE) ~ "Jequié",
    grepl("feira", x, ignore.case = TRUE) ~ "Feira de Santana",
    grepl("conquista", x, ignore.case = TRUE) ~ "Vitória da Conquista",
    grepl("guanambi", x, ignore.case = TRUE) ~ "Guanambi",
    grepl("juazeiro", x, ignore.case = TRUE) ~ "Juazeiro",
    grepl("cama[çc]ari", x, ignore.case = TRUE) ~ "Camaçari",
    grepl("serrinha", x, ignore.case = TRUE) ~ "Serrinha",
    TRUE ~ x
  )
}

df_q32 <- df |>
  mutate(
    AnoCol = factor(AnoCol, levels = c("2025", "2026")),
    mun = mun_padrao(q32),
    mun_cat = case_when(
      mun %in% c("Salvador", "Jequié", "Feira de Santana", "Vitória da Conquista", "Guanambi", "Juazeiro", "Serrinha", "Camaçari") ~ mun,
      TRUE ~ "Demais Municípios"
    ),
    mun_cat = factor(mun_cat, levels = c(
      "Salvador", "Jequié", "Feira de Santana", "Vitória da Conquista", 
      "Guanambi", "Juazeiro", "Serrinha", "Camaçari", "Demais Municípios"
    ))
  )

df_resumo_q32 <- df_q32 |>
  count(AnoCol, mun_cat) |>
  pivot_wider(names_from = AnoCol, values_from = n, values_fill = 0) |>
  mutate(
    pct_2025 = `2025` / sum(`2025`),
    pct_2026 = `2026` / sum(`2026`),
    dif_pp = (pct_2026 - pct_2025) * 100
  )

tabela_q32_dados <- tibble(
  `Município de Moradia` = c(
    as.character(df_resumo_q32$mun_cat), "Total"
  ),
  `2025 (N)` = c(as.character(df_resumo_q32$`2025`), "711"),
  `2025 (%)` = c(percent(df_resumo_q32$pct_2025, accuracy = 0.1, decimal.mark = ","), "100,0%"),
  `2026 (N)` = c(as.character(df_resumo_q32$`2026`), "879"),
  `2026 (%)` = c(percent(df_resumo_q32$pct_2026, accuracy = 0.1, decimal.mark = ","), "100,0%"),
  `Variação (p.p.)` = c(sprintf("%+.1f", df_resumo_q32$dif_pp), "—")
)

t32_municipio_moradia <- tt(tabela_q32_dados) |>
  format_tt(escape = TRUE)

df_plot_q32 <- df_q32 |>
  count(AnoCol, mun_cat, name = "Respondentes") |>
  group_by(AnoCol) |>
  mutate(
    Total = sum(Respondentes),
    Percentual = Respondentes / Total,
    Rotulo = percent(Percentual, accuracy = 0.1, decimal.mark = ",")
  ) |>
  ungroup()

g_municipio_moradia <- ggplot(df_plot_q32, aes(x = fct_rev(mun_cat), y = Percentual, fill = AnoCol)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.65, alpha = 0.95) +
  geom_text(
    aes(label = Rotulo),
    position = position_dodge(width = 0.75),
    hjust = -0.15,
    size = 3.0,
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
    limits = c(0, 0.45),
    expand = expansion(mult = c(0, 0.08))
  ) +
  labs(
    title = "Residência Habitual: Município de Moradia Atual dos Beneficiários",
    subtitle = "Distribuição residencial com expansão em Feira de Santana e Região Metropolitana
(Cinza: 2025 | Azul: 2026)",
    x = "Município de Residência",
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
    axis.text.y = element_text(color = COR_TEXTO, size = 10, face = "bold"),
    axis.text.x = element_text(color = COR_SUBTEXTO, size = 9.5),
    plot.margin = margin(t = 12, r = 15, b = 12, l = 15)
  )
