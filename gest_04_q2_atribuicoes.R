# ==============================================================================
# Projeto Primeiro Emprego (PPE) - Avaliação dos Gestores / Pontos Focais
# Script: gest_04_q2_atribuicoes.R - Atribuições do Ponto Focal
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

df_q2 <- df |>
  mutate(
    AnoCol = factor(AnoCol, levels = c("2025", "2026")),
    atribuicao = factor(q2_cat, levels = c(
      "Orientação e supervisão direta",
      "Acompanhamento de desempenho e frequência",
      "Outras atribuições"
    ))
  )

tabela_q2_dados <- tibble(
  `Principal Atribuição como Ponto Focal` = c(
    "Orientação e supervisão direta",
    "Acompanhamento de desempenho e frequência",
    "Outras atribuições (Mediação e rotinas)",
    "Total"
  ),
  `2025 (N)` = c("24", "20", "23", "67"),
  `2025 (%)` = c("35,8%", "29,9%", "34,3%", "100,0%"),
  `2026 (N)` = c("30", "24", "23", "77"),
  `2026 (%)` = c("39,0%", "31,2%", "29,9%", "100,0%"),
  `Variação (p.p.)` = c("+3,2", "+1,3", "-4,4", "—")
)

t02_atribuicoes <- tt(tabela_q2_dados) |>
  format_tt(escape = TRUE)

df_plot_q2 <- df_q2 |>
  count(AnoCol, atribuicao, name = "Respondentes") |>
  group_by(AnoCol) |>
  mutate(
    Total = sum(Respondentes),
    Percentual = Respondentes / Total,
    Rotulo = percent(Percentual, accuracy = 0.1, decimal.mark = ",")
  ) |>
  ungroup()

g02_atribuicoes <- ggplot(df_plot_q2, aes(x = fct_rev(atribuicao), y = Percentual, fill = AnoCol)) +
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
    limits = c(0, 0.48),
    expand = expansion(mult = c(0, 0.08))
  ) +
  labs(
    title = "Papel Gerencial: Principal Atribuição Prática como Ponto Focal",
    subtitle = "Predomínio de funções de supervisão direta e acompanhamento de frequência/desempenho (~70%)
(Cinza: 2025 | Azul: 2026)",
    x = "Atribuição Principal",
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
    axis.text.y = element_text(color = COR_TEXTO, size = 10, face = "bold"),
    axis.text.x = element_text(color = COR_SUBTEXTO, size = 9.5),
    plot.margin = margin(t = 12, r = 15, b = 12, l = 15)
  )
