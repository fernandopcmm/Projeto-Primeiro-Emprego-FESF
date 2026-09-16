# ==============================================================================
# Projeto Primeiro Emprego (PPE) - Avaliação dos Gestores / Pontos Focais
# Script: gest_13_q11_canais_comunicacao.R - Canais de Comunicação Utilizados
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

df_q11 <- df |>
  mutate(
    AnoCol = factor(AnoCol, levels = c("2025", "2026")),
    canal = factor(q11_cat, levels = c(
      "WhatsApp exclusivo",
      "E-mail exclusivo",
      "Outros canais",
      "Telefone / Contato direto",
      "WhatsApp e E-mail integrados"
    ))
  )

tabela_q11_dados <- tibble(
  `Canal de Comunicação com a Entidade Parceira` = c(
    "WhatsApp exclusivo",
    "E-mail exclusivo",
    "Outros canais (Plataformas / Presencial)",
    "Telefone / Contato direto",
    "WhatsApp e E-mail integrados",
    "Total"
  ),
  `2025 (N)` = c("37", "20", "8", "1", "1", "67"),
  `2025 (%)` = c("55,2%", "29,9%", "11,9%", "1,5%", "1,5%", "100,0%"),
  `2026 (N)` = c("40", "25", "10", "2", "0", "77"),
  `2026 (%)` = c("51,9%", "32,5%", "13,0%", "2,6%", "0,0%", "100,0%"),
  `Variação (p.p.)` = c("-3,3", "+2,6", "+1,1", "+1,1", "-1,5", "—")
)

t11_canais_comunicacao <- tt(tabela_q11_dados) |>
  format_tt(escape = TRUE)

df_plot_q11 <- df_q11 |>
  count(AnoCol, canal, name = "Respondentes") |>
  complete(AnoCol, canal, fill = list(Respondentes = 0)) |>
  group_by(AnoCol) |>
  mutate(
    Total = sum(Respondentes),
    Percentual = Respondentes / Total,
    Rotulo = if_else(Respondentes > 0, percent(Percentual, accuracy = 0.1, decimal.mark = ","), "")
  ) |>
  ungroup()

g11_canais_comunicacao <- ggplot(df_plot_q11, aes(x = fct_rev(canal), y = Percentual, fill = AnoCol)) +
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
    limits = c(0, 0.65),
    expand = expansion(mult = c(0, 0.08))
  ) +
  labs(
    title = "Meios de Interlocução: Canais de Comunicação Utilizados com a Parceira",
    subtitle = "Hegemonia do WhatsApp (~52-55%) e estabilidade do E-mail (~30-33%) na mediação institucional
(Cinza: 2025 | Azul: 2026)",
    x = "Canal de Comunicação",
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
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = COR_GRID, linewidth = 0.4),
    axis.line.y = element_line(color = COR_CONTEXTO, linewidth = 0.5),
    axis.text.y = element_text(color = COR_TEXTO, size = 9.5, face = "bold"),
    axis.text.x = element_text(color = COR_SUBTEXTO, size = 9.5),
    plot.margin = margin(t = 12, r = 15, b = 12, l = 15)
  )
