# ==============================================================================
# Projeto Primeiro Emprego (PPE) - Avaliação dos Gestores / Pontos Focais
# Script: gest_21_q19_sugestoes_aprimoramento.R - Sugestões de Aprimoramento
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

# Categorização semântica das 144 respostas discursivas de q19:
# 2025 (N=67 respondentes, 66 com sugestões substantivas):
# Capacitação técnica/atitudinal prévia e continuada: 14 (20,9%)
# Comunicação ágil, canais dedicados e retornos da FESF: 9 (13,4%)
# Suporte psicossocial, escuta qualificada e acolhimento: 8 (11,9%)
# Alinhamento de perfil de lotação e critérios de substituição: 10 (14,9%)
# Outras proposições operacionais / Manutenção do modelo: 25 (37,3%)
# Total = 66 menções (98,5% de preenchimento)

# 2026 (N=77 respondentes, 77 com sugestões substantivas):
# Capacitação técnica/atitudinal prévia e continuada: 23 (29,9%)
# Comunicação ágil, canais dedicados e retornos da FESF: 28 (36,4%)
# Suporte psicossocial, escuta qualificada e acolhimento: 9 (11,7%)
# Alinhamento de perfil de lotação e critérios de substituição: 15 (19,5%)
# Outras proposições operacionais / Manutenção do modelo: 2 (2,6%)

tabela_q19_dados <- tibble(
  `Eixo Propositivo para Aprimoramento do PPE` = c(
    "Comunicação ágil, canais dedicados e retornos da coordenação",
    "Capacitação técnica e comportamental prévia e continuada",
    "Alinhamento de perfil de lotação e regras de substituição",
    "Suporte psicossocial, escuta e acolhimento emocional",
    "Outras sugestões operacionais / Manutenção do formato atual",
    "Total de Respondentes com Proposições Substantivas"
  ),
  `2025 (N)` = c("9", "14", "10", "8", "25", "66"),
  `2025 (%)` = c("13,4%", "20,9%", "14,9%", "11,9%", "37,3%", "98,5%"),
  `2026 (N)` = c("28", "23", "15", "9", "2", "77"),
  `2026 (%)` = c("36,4%", "29,9%", "19,5%", "11,7%", "2,6%", "100,0%"),
  `Variação (p.p.)` = c("+23,0", "+9,0", "+4,6", "-0,2", "-34,7", "—")
)

t19_sugestoes <- tt(tabela_q19_dados) |>
  format_tt(escape = TRUE)

df_plot_q19 <- tibble(
  Eixo = factor(rep(c(
    "Canais ágeis e comunicação",
    "Capacitação técnica / soft skills",
    "Alinhamento de perfil / lotação",
    "Apoio psicossocial / escuta"
  ), each = 2), levels = c(
    "Canais ágeis e comunicação",
    "Capacitação técnica / soft skills",
    "Alinhamento de perfil / lotação",
    "Apoio psicossocial / escuta"
  )),
  AnoCol = factor(rep(c("2025", "2026"), 4), levels = c("2025", "2026")),
  Percentual = c(0.134, 0.364, 0.209, 0.299, 0.149, 0.195, 0.119, 0.117)
) |>
  mutate(Rotulo = percent(Percentual, accuracy = 0.1, decimal.mark = ","))

g19_sugestoes <- ggplot(df_plot_q19, aes(x = fct_rev(Eixo), y = Percentual, fill = AnoCol)) +
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
    limits = c(0, 0.45),
    expand = expansion(mult = c(0, 0.08))
  ) +
  labs(
    title = "Diretrizes de Aperfeiçoamento: Eixos Propositivos dos Pontos Focais",
    subtitle = "Forte demanda por canais dedicados (+23,0 p.p.) e capacitação preparatória contínua (+9,0 p.p.)",
    x = "Eixo Temático de Aprimoramento",
    y = "Proporção de Gestores que Citaram o Tema (%)",
    caption = "Fonte: Análise semântica das respostas discursivas da questão q19. N = 144 (67 em 2025; 77 em 2026)."
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(color = COR_TEXTO, face = "bold", size = 12, margin = margin(b = 4)),
    plot.subtitle = element_text(color = COR_SUBTEXTO, size = 9.5, margin = margin(b = 10)),
    axis.title.x = element_text(color = COR_TEXTO, face = "bold", size = 9.5, margin = margin(t = 6)),
    axis.title.y = element_text(color = COR_TEXTO, face = "bold", size = 9.5, margin = margin(r = 6)),
    axis.text.x = element_text(color = COR_TEXTO, size = 9),
    axis.text.y = element_text(color = COR_TEXTO, size = 9),
    panel.grid.major.x = element_line(color = COR_GRID, linewidth = 0.35),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.position = "top",
    legend.justification = "right",
    legend.title = element_text(face = "bold", size = 8.5),
    legend.text = element_text(size = 8.5),
    plot.margin = margin(t = 10, r = 15, b = 10, l = 10)
  )
