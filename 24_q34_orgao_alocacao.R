# ==============================================================================
# Projeto Primeiro Emprego (PPE) - Análise Comparativa Longitudinal (2025-2026)
# Script 24: 24_q34_orgao_alocacao.R - Unidade e Órgão de Lotação no PPE
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

df_q34 <- df |>
  mutate(
    AnoCol = factor(AnoCol, levels = c("2025", "2026")),
    orgao_cat = case_when(
      grepl("prado valadares|hgpv", q34, ignore.case = TRUE) ~ "HGPV (Hospital Prado Valadares)",
      grepl("roberto santos|hgrs", q34, ignore.case = TRUE) ~ "HGRS (Hospital Roberto Santos)",
      grepl("sesab|secretaria da sa[úu]de", q34, ignore.case = TRUE) ~ "SESAB (Administração Central)",
      grepl("maternidade|tsylla|mmcj|albert sabin", q34, ignore.case = TRUE) ~ "Maternidades Estaduais",
      grepl("hospital|hgi|hgca|hgvc|hge|hgesf|hepr|icom|lopes rodrigues|menandro|dantas bi[ãa]o", q34, ignore.case = TRUE) ~ "Hospitais Gerais e Especializados",
      grepl("base regional|nrs|direc|policl[íi]nica", q34, ignore.case = TRUE) ~ "Bases Regionais / Policlínicas",
      TRUE ~ "Outras Unidades de Saúde e Educação"
    ),
    orgao_cat = factor(orgao_cat, levels = c(
      "Outras Unidades de Saúde e Educação",
      "Hospitais Gerais e Especializados",
      "Bases Regionais / Policlínicas",
      "Maternidades Estaduais",
      "SESAB (Administração Central)",
      "HGPV (Hospital Prado Valadares)",
      "HGRS (Hospital Roberto Santos)"
    ))
  )

tabela_q34_dados <- tibble(
  `Tipo de Unidade / Órgão de Lotação` = c(
    "Outras Unidades de Saúde e Educação",
    "Hospitais Gerais e Especializados",
    "Bases Regionais / Policlínicas",
    "Maternidades Estaduais",
    "SESAB (Administração Central)",
    "HGPV (Hospital Prado Valadares)",
    "HGRS (Hospital Roberto Santos)",
    "Total"
  ),
  `2025 (N)` = c("247", "145", "82", "72", "60", "102", "3", "711"),
  `2025 (%)` = c("34,7%", "20,4%", "11,5%", "10,1%", "8,4%", "14,3%", "0,4%", "100,0%"),
  `2026 (N)` = c("278", "209", "133", "82", "70", "64", "43", "879"),
  `2026 (%)` = c("31,6%", "23,8%", "15,1%", "9,3%", "8,0%", "7,3%", "4,9%", "100,0%"),
  `Variação (p.p.)` = c("-3,1", "+3,4", "+3,6", "-0,8", "-0,4", "-7,0", "+4,5", "—")
)

t34_orgao <- tt(tabela_q34_dados) |>
  format_tt(escape = TRUE)

df_plot_q34 <- df_q34 |>
  count(AnoCol, orgao_cat, name = "Respondentes") |>
  group_by(AnoCol) |>
  mutate(
    Total = sum(Respondentes),
    Percentual = Respondentes / Total,
    Rotulo = percent(Percentual, accuracy = 0.1, decimal.mark = ",")
  ) |>
  ungroup()

g_orgao <- ggplot(df_plot_q34, aes(x = orgao_cat, y = Percentual, fill = AnoCol)) +
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
    limits = c(0, 0.40),
    expand = expansion(mult = c(0, 0.08))
  ) +
  labs(
    title = "Alocação Institucional: Órgãos e Unidades de Atuação no PPE",
    subtitle = "Inserção primordial na rede hospitalar estadual, policlínicas e administração da SESAB
(Cinza: 2025 | Azul: 2026)",
    x = "Unidade / Órgão de Atuação",
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
