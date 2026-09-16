# ==============================================================================
# Projeto Primeiro Emprego (PPE) - Análise Comparativa Longitudinal (2025-2026)
# Script 20: 20_q28_municipio_nascimento.R - Município de Nascimento
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
    grepl("ipia", x, ignore.case = TRUE) ~ "Ipiaú",
    grepl("serrinha", x, ignore.case = TRUE) ~ "Serrinha",
    TRUE ~ x
  )
}

df_q28 <- df |>
  mutate(
    AnoCol = factor(AnoCol, levels = c("2025", "2026")),
    mun = mun_padrao(q28),
    mun_cat = case_when(
      mun %in% c("Salvador", "Jequié", "Feira de Santana", "Vitória da Conquista", "Guanambi", "Juazeiro", "Ipiaú", "Serrinha") ~ mun,
      TRUE ~ "Demais Municípios"
    ),
    mun_cat = factor(mun_cat, levels = c(
      "Salvador", "Jequié", "Feira de Santana", "Vitória da Conquista", 
      "Guanambi", "Juazeiro", "Ipiaú", "Serrinha", "Demais Municípios"
    ))
  )

tabela_q28_dados <- tibble(
  `Município de Nascimento` = c(
    "Salvador", "Jequié", "Feira de Santana", "Vitória da Conquista", 
    "Guanambi", "Juazeiro", "Ipiaú", "Serrinha", "Demais Municípios", "Total"
  ),
  `2025 (N)` = c("257", "100", "27", "42", "21", "8", "19", "13", "224", "711"),
  `2025 (%)` = c("36,1%", "14,1%", "3,8%", "5,9%", "3,0%", "1,1%", "2,7%", "1,8%", "31,5%", "100,0%"),
  `2026 (N)` = c("329", "78", "72", "24", "32", "23", "15", "22", "284", "879"),
  `2026 (%)` = c("37,4%", "8,9%", "8,2%", "2,7%", "3,6%", "2,6%", "1,7%", "2,5%", "32,3%", "100,0%"),
  `Variação (p.p.)` = c("+1,3", "-5,2", "+4,4", "-3,2", "+0,6", "+1,5", "-1,0", "+0,7", "+0,8", "—")
)

t28_municipio_nascimento <- tt(tabela_q28_dados) |>
  format_tt(escape = TRUE)

df_plot_q28 <- df_q28 |>
  count(AnoCol, mun_cat, name = "Respondentes") |>
  group_by(AnoCol) |>
  mutate(
    Total = sum(Respondentes),
    Percentual = Respondentes / Total,
    Rotulo = percent(Percentual, accuracy = 0.1, decimal.mark = ",")
  ) |>
  ungroup()

g_municipio_nascimento <- ggplot(df_plot_q28, aes(x = fct_rev(mun_cat), y = Percentual, fill = AnoCol)) +
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
    title = "Capilaridade Territorial: Municípios de Origem dos Beneficiários",
    subtitle = "Concentração na Capital e presença expressiva em polos do interior da Bahia
(Cinza: 2025 | Azul: 2026)",
    x = "Município de Nascimento",
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
