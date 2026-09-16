# ==============================================================================
# Projeto Primeiro Emprego (PPE) - Avaliação dos Gestores / Pontos Focais
# Script: gest_09_q7_protocolos_acolhimento.R - Protocolos de Acolhimento
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

caminho_csv <- "/home/nando/Documentos/gemini/ATUAL/OUTPUT/gest_PPE_tratado_2025_2026.csv"
df <- read_csv(caminho_csv, show_col_types = FALSE)

df_q7 <- df |>
  mutate(
    AnoCol = factor(AnoCol, levels = c("2025", "2026")),
    protocolo = factor(q7_cat, levels = c(
      "Sim (Possui protocolos)",
      "Não (Não possui)",
      "Não tenho certeza / Talvez"
    ))
  )

tabela_q7_dados <- tibble(
  `Órgão Possui Protocolos de Acolhimento?` = c(
    "Sim (Possui protocolos padronizados)",
    "Não (Não possui diretrizes formais)",
    "Não tenho certeza / Talvez",
    "Total"
  ),
  `2025 (N)` = c("43", "12", "12", "67"),
  `2025 (%)` = c("64,2%", "17,9%", "17,9%", "100,0%"),
  `2026 (N)` = c("54", "6", "17", "77"),
  `2026 (%)` = c("70,1%", "7,8%", "22,1%", "100,0%"),
  `Variação (p.p.)` = c("+5,9", "-10,1", "+4,2", "—")
)

t07_protocolos <- tt(tabela_q7_dados) |>
  format_tt(escape = TRUE)

df_plot_q7 <- df_q7 |>
  count(AnoCol, protocolo, name = "Respondentes") |>
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

g07_protocolos <- ggplot(df_plot_q7, aes(x = protocolo, y = Percentual, fill = AnoCol)) +
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
    limits = c(0, 0.85),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    title = "Institucionalização Normativa: Presença de Protocolos Formais de Acolhimento",
    subtitle = "Avanço na formalização: 70,1% dos órgãos contam com diretrizes padronizadas em 2026
(Cinza: 2025 | Azul: 2026)",
    x = "Existência de Protocolos Declarada",
    y = "Proporção de Gestores (%)",
    caption = "Fonte: Dados da pesquisa com Pontos Focais do PPE (2025 - 2026)."
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
