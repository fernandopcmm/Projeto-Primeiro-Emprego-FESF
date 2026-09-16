# ==============================================================================
# Projeto Primeiro Emprego (PPE) - Análise Comparativa Longitudinal (2025-2026)
# Script 23: 23_q32_q33_mobilidade_pendular.R - Mobilidade Pendular (Comutação)
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
  x <- str_to_lower(str_trim(x))
  case_when(
    grepl("salvador", x) ~ "salvador",
    grepl("jequi", x) ~ "jequie",
    grepl("feira", x) ~ "feira de santana",
    grepl("conquista", x) ~ "vitoria da conquista",
    grepl("guanambi", x) ~ "guanambi",
    grepl("juazeiro", x) ~ "juazeiro",
    grepl("serrinha", x) ~ "serrinha",
    grepl("ipia", x) ~ "ipiau",
    grepl("cama[çc]ari", x) ~ "camacari",
    TRUE ~ x
  )
}

df_mobi <- df |>
  mutate(
    AnoCol = factor(AnoCol, levels = c("2025", "2026")),
    mora = mun_padrao(q32),
    trab = mun_padrao(q33),
    mobi_cat = if_else(
      mora == trab, 
      "Trabalha no mesmo município de residência", 
      "Mobilidade pendular (intermunicipal)"
    ),
    mobi_cat = factor(mobi_cat, levels = c(
      "Trabalha no mesmo município de residência", 
      "Mobilidade pendular (intermunicipal)"
    ))
  )

tabela_mobi_dados <- tibble(
  `Padrão de Deslocamento Laboral` = c(
    "Trabalha no mesmo município de residência",
    "Mobilidade pendular (intermunicipal)",
    "Total"
  ),
  `2025 (N)` = c("642", "69", "711"),
  `2025 (%)` = c("90,3%", "9,7%", "100,0%"),
  `2026 (N)` = c("793", "86", "879"),
  `2026 (%)` = c("90,2%", "9,8%", "100,0%"),
  `Variação (p.p.)` = c("-0,1", "+0,1", "—")
)

t32_33_mobilidade <- tt(tabela_mobi_dados) |>
  format_tt(escape = TRUE)

df_plot_mobi <- df_mobi |>
  count(AnoCol, mobi_cat, name = "Respondentes") |>
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

g_mobilidade <- ggplot(df_plot_mobi, aes(x = mobi_cat, y = Percentual, fill = AnoCol)) +
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
    limits = c(0, 1.05),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    title = "Mobilidade Pendular: Coincidência entre Município de Moradia e Trabalho",
    subtitle = "Aproximadamente 90% dos jovens trabalham no seu próprio município de residência
(Cinza: 2025 | Azul: 2026)",
    x = "Padrão de Deslocamento",
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
