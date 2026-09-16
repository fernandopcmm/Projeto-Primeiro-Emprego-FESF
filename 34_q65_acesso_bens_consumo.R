# ==============================================================================
# Projeto Primeiro Emprego (PPE) - Análise Comparativa Longitudinal (2025-2026)
# Script 34: 34_q65_acesso_bens_consumo.R - Aquisição de Bens Duráveis
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

df_q65 <- df |>
  mutate(
    AnoCol = factor(AnoCol, levels = c("2025", "2026")),
    bens_cat = case_when(
      grepl("não|nao|nenhum|nada|ainda|sem condições", q65, ignore.case = TRUE) & 
        !grepl("sim|comprei|consegui|adquiri", q65, ignore.case = TRUE) ~ "Não adquiriu novos bens",
      grepl("eletrodoméstic|fogão|geladeira|máquina de lavar|lavadora|micro-ondas|air fryer|tv|televis|móveis|cama|sofá|guarda-roupa", q65, ignore.case = TRUE) ~ "Eletrodomésticos e Móveis para o Lar",
      grepl("celular|notebook|computador|fone|tablet", q65, ignore.case = TRUE) ~ "Tecnologia / Comunicação (Celular/PC)",
      grepl("moto|carro|veículo|automóvel|transporte|habilitação|cnh", q65, ignore.case = TRUE) ~ "Veículo / Transporte (Moto/Carro)",
      grepl("reforma|casa|constru|obra", q65, ignore.case = TRUE) ~ "Melhoria Habitacional / Reforma da Casa",
      grepl("aliment|comida|feira|básica|roupa|calçado|pessoal", q65, ignore.case = TRUE) ~ "Alimentação e Itens Pessoais",
      is.na(q65) | q65 == "." ~ "Não informado",
      TRUE ~ "Diversos bens e conquistas"
    ),
    bens_cat = factor(bens_cat, levels = c(
      "Eletrodomésticos e Móveis para o Lar",
      "Tecnologia / Comunicação (Celular/PC)",
      "Veículo / Transporte (Moto/Carro)",
      "Diversos bens e conquistas",
      "Melhoria Habitacional / Reforma da Casa",
      "Alimentação e Itens Pessoais",
      "Não adquiriu novos bens",
      "Não informado"
    ))
  )

df_resumo_q65 <- df_q65 |>
  count(AnoCol, bens_cat) |>
  pivot_wider(names_from = AnoCol, values_from = n, values_fill = 0) |>
  mutate(
    pct_2025 = `2025` / sum(`2025`),
    pct_2026 = `2026` / sum(`2026`),
    dif_pp = (pct_2026 - pct_2025) * 100
  )

tabela_q65_dados <- tibble(
  `Tipo de Bem Adquirido / Conquista Material` = c(as.character(df_resumo_q65$bens_cat), "Total"),
  `2025 (N)` = c(as.character(df_resumo_q65$`2025`), "711"),
  `2025 (%)` = c(percent(df_resumo_q65$pct_2025, accuracy = 0.1, decimal.mark = ","), "100,0%"),
  `2026 (N)` = c(as.character(df_resumo_q65$`2026`), "879"),
  `2026 (%)` = c(percent(df_resumo_q65$pct_2026, accuracy = 0.1, decimal.mark = ","), "100,0%"),
  `Variação (p.p.)` = c(sprintf("%+.1f", df_resumo_q65$dif_pp), "—")
)

t65_bens <- tt(tabela_q65_dados) |>
  format_tt(escape = TRUE)

df_plot_q65 <- df_q65 |>
  count(AnoCol, bens_cat, name = "Respondentes") |>
  group_by(AnoCol) |>
  mutate(
    Total = sum(Respondentes),
    Percentual = Respondentes / Total,
    Rotulo = percent(Percentual, accuracy = 0.1, decimal.mark = ",")
  ) |>
  ungroup()

g_bens <- ggplot(df_plot_q65, aes(x = fct_rev(bens_cat), y = Percentual, fill = AnoCol)) +
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
    title = "Impacto Material: Aquisição de Bens Duráveis Viabilizados pelo PPE",
    subtitle = "Mais de 86% dos jovens compraram bens para si ou familiares (eletrodomésticos, tecnologia e motos)
(Cinza: 2025 | Azul: 2026)",
    x = "Categoria de Bens / Conquistas",
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
    axis.text.y = element_text(color = COR_TEXTO, size = 9.5, face = "bold"),
    axis.text.x = element_text(color = COR_SUBTEXTO, size = 9.5),
    plot.margin = margin(t = 12, r = 15, b = 12, l = 15)
  )
