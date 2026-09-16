# ==============================================================================
# Projeto Primeiro Emprego (PPE) - Avaliação dos Gestores / Pontos Focais
# Script: gest_22_q20_inconformidades_riscos.R - Inconformidades e Riscos Críticos
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

# Categorização semântica das 144 respostas discursivas de q20:
# 2025 (N=67 respondentes):
# Sem inconformidades / Nada a relatar / Satisfeito: 42 (62,7%)
# Gestão de expectativas da vigência contratual (1 ano vs 2 anos): 5 (7,5%)
# Suporte administrativo e benefícios (transporte, crachá, RH parceira): 6 (9,0%)
# Comunicação lenta / Falta de retorno da coordenação externa: 5 (7,5%)
# Riscos de desvio de atribuição / Plantões desregulamentados: 2 (3,0%)
# Menções pontuais diversas: 7 (10,4%)

# 2026 (N=77 respondentes):
# Sem inconformidades / Nada a relatar / Satisfeito: 47 (61,0%)
# Gestão de expectativas da vigência contratual (1 ano vs 2 anos): 7 (9,1%)
# Suporte administrativo e benefícios (transporte, crachá, RH parceira): 3 (3,9%)
# Comunicação lenta / Falta de retorno da coordenação externa: 7 (9,1%)
# Riscos de desvio de atribuição / Plantões desregulamentados: 5 (6,5%)
# Menções pontuais diversas: 8 (10,4%)

tabela_q20_dados <- tibble(
  `Inconformidades Críticas e Riscos Identificados` = c(
    "Ausência de inconformidades / Funcionamento satisfatório",
    "Expectativas contratuais: incerteza sobre vigência de 1 vs 2 anos",
    "Comunicação lenta e morosidade de suporte da entidade parceira",
    "Desvio de função, substituição de servidores e escalas de plantão",
    "Suporte administrativo a benefícios, crachás e auxílio-transporte",
    "Outras manifestações pontuais de conformidade operacional",
    "Total"
  ),
  `2025 (N)` = c("42", "5", "5", "2", "6", "7", "67"),
  `2025 (%)` = c("62,7%", "7,5%", "7,5%", "3,0%", "9,0%", "10,4%", "100,0%"),
  `2026 (N)` = c("47", "7", "7", "5", "3", "8", "77"),
  `2026 (%)` = c("61,0%", "9,1%", "9,1%", "6,5%", "3,9%", "10,4%", "100,0%"),
  `Variação (p.p.)` = c("-1,7", "+1,6", "+1,6", "+3,5", "-5,1", "0,0", "—")
)

t20_inconformidades <- tt(tabela_q20_dados) |>
  format_tt(escape = TRUE)

# Foco nas inconformidades ativas substantivas relatadas
df_plot_q20 <- tibble(
  Risco = factor(rep(c(
    "Incerteza contratual (1 vs 2 anos)",
    "Morosidade no retorno da parceira",
    "Desvio de função / plantões",
    "Benefícios / auxílio-transporte"
  ), each = 2), levels = c(
    "Incerteza contratual (1 vs 2 anos)",
    "Morosidade no retorno da parceira",
    "Desvio de função / plantões",
    "Benefícios / auxílio-transporte"
  )),
  AnoCol = factor(rep(c("2025", "2026"), 4), levels = c("2025", "2026")),
  Percentual = c(0.075, 0.091, 0.075, 0.091, 0.030, 0.065, 0.090, 0.039)
) |>
  mutate(Rotulo = percent(Percentual, accuracy = 0.1, decimal.mark = ","))

g20_inconformidades <- ggplot(df_plot_q20, aes(x = fct_rev(Risco), y = Percentual, fill = AnoCol)) +
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
    limits = c(0, 0.15),
    expand = expansion(mult = c(0, 0.08))
  ) +
  labs(
    title = "Matriz de Alerta: Principais Inconformidades e Riscos Operacionais",
    subtitle = "Predomínio de conformidade (>61%); alertas para indefinição de prazos contratuais e desvio funcional",
    x = "Tipologia de Inconformidade / Ponto de Atenção",
    y = "Proporção de Pontos Focais que Relataram (%)",
    caption = "Fonte: Análise semântica das respostas discursivas da questão q20. N = 144 (67 em 2025; 77 em 2026)."
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
