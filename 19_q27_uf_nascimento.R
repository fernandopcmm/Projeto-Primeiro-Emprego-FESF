# ==============================================================================
# Projeto Primeiro Emprego (PPE) - Análise Comparativa Longitudinal (2025-2026)
# Script 19: 19_q27_uf_nascimento.R - Estado de Nascimento (UF) e Migração
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

df_q27 <- df |>
  mutate(
    AnoCol = factor(AnoCol, levels = c("2025", "2026")),
    q27_padrao = case_when(
      grepl("ba|bahia|salvador|jequié|feira", q27, ignore.case = TRUE) ~ "Bahia (BA)",
      grepl("sp|são paulo", q27, ignore.case = TRUE) ~ "São Paulo (SP)",
      grepl("rj|rio de janeiro", q27, ignore.case = TRUE) ~ "Rio de Janeiro (RJ)",
      grepl("^[0-9]{2}/[0-9]{2}/[0-9]{4}$", q27) ~ "Bahia (BA)",
      TRUE ~ "Outros Estados"
    ),
    q27_padrao = factor(q27_padrao, levels = c("Bahia (BA)", "São Paulo (SP)", "Rio de Janeiro (RJ)", "Outros Estados"))
  )

tabela_q27_dados <- tibble(
  `Estado de Nascimento (UF)` = c("Bahia (BA)", "São Paulo (SP)", "Rio de Janeiro (RJ)", "Outros Estados", "Total"),
  `2025 (N)` = c("655", "21", "0", "35", "711"),
  `2025 (%)` = c("92,1%", "3,0%", "0,0%", "4,9%", "100,0%"),
  `2026 (N)` = c("836", "18", "8", "17", "879"),
  `2026 (%)` = c("95,1%", "2,0%", "0,9%", "1,9%", "100,0%"),
  `Variação (p.p.)` = c("+3,0", "-1,0", "+0,9", "-3,0", "—")
)

t27_uf_nascimento <- tt(tabela_q27_dados) |>
  format_tt(escape = TRUE)

df_plot_q27 <- df_q27 |>
  count(AnoCol, q27_padrao, name = "Respondentes") |>
  group_by(AnoCol) |>
  mutate(
    Total = sum(Respondentes),
    Percentual = Respondentes / Total,
    Rotulo = percent(Percentual, accuracy = 0.1, decimal.mark = ",")
  ) |>
  ungroup()

g_uf_nascimento <- ggplot(df_plot_q27, aes(x = q27_padrao, y = Percentual, fill = AnoCol)) +
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
    limits = c(0, 1.05),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    title = "Origem Geográfica: Estado de Nascimento dos Beneficiários",
    subtitle = "Predominância absoluta de baianos natos e baixa migração interestadual (2025 vs. 2026)
(Cinza: 2025 | Azul: 2026)",
    x = "Unidade da Federação de Nascimento",
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
