# ==============================================================================
# Projeto Primeiro Emprego (PPE) - Avaliação dos Gestores / Pontos Focais
# Script: gest_05_q3_nivel_desafio.R - Nível de Desafio do Papel
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

df_q3 <- df |>
  mutate(
    AnoCol = factor(AnoCol, levels = c("2025", "2026")),
    desafio = factor(q3, levels = 1:5, labels = c(
      "1 - Nada desafiador", "2 - Pouco desafiador", "3 - Moderadamente",
      "4 - Muito desafiador", "5 - Extremamente desafiador"
    ))
  )

tabela_q3_dados <- tibble(
  `Nível Geral de Desafio da Função` = c(
    "1 - Nada desafiador", "2 - Pouco desafiador", "3 - Moderadamente",
    "4 - Muito desafiador", "5 - Extremamente desafiador", "Total"
  ),
  `2025 (N)` = c("3", "5", "12", "27", "20", "67"),
  `2025 (%)` = c("4,5%", "7,5%", "17,9%", "40,3%", "29,9%", "100,0%"),
  `2026 (N)` = c("1", "5", "18", "32", "21", "77"),
  `2026 (%)` = c("1,3%", "6,5%", "23,4%", "41,6%", "27,3%", "100,0%"),
  `Variação (p.p.)` = c("-3,2", "-1,0", "+5,5", "+1,3", "-2,6", "—")
)

t03_nivel_desafio <- tt(tabela_q3_dados) |>
  format_tt(escape = TRUE)

df_plot_q3 <- df_q3 |>
  count(AnoCol, desafio, name = "Respondentes") |>
  group_by(AnoCol) |>
  mutate(
    Total = sum(Respondentes),
    Percentual = Respondentes / Total,
    Rotulo = percent(Percentual, accuracy = 0.1, decimal.mark = ",")
  ) |>
  ungroup()

g03_nivel_desafio <- ggplot(df_plot_q3, aes(x = desafio, y = Percentual, fill = AnoCol)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.65, alpha = 0.95) +
  geom_text(
    aes(label = Rotulo),
    position = position_dodge(width = 0.75),
    vjust = -0.4,
    size = 3.1,
    fontface = "bold",
    color = COR_TEXTO
  ) +
  scale_fill_manual(
    name = "Ciclo Avaliativo",
    values = c("2025" = COR_CONTEXTO, "2026" = COR_DESTAQUE)
  ) +
  scale_y_continuous(
    labels = label_percent(accuracy = 1),
    limits = c(0, 0.50),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    title = "Complexidade da Tutoria: Nível Geral de Desafio do Ponto Focal",
    subtitle = "Aproximadamente 70% consideram a função altamente desafiadora (notas 4 e 5)
(Cinza: 2025 | Azul: 2026)",
    x = "Escala de Desafio Percebido",
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
