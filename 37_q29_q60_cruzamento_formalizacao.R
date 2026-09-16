# ==============================================================================
# Projeto Primeiro Emprego (PPE) - Análise Comparativa Longitudinal (2025-2026)
# Script 37: 37_q29_q60_cruzamento_formalizacao.R - Taxonomia da Inserção Laboral
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

df_cruz <- df |>
  mutate(
    AnoCol = factor(AnoCol, levels = c("2025", "2026")),
    perfil_insercao = case_when(
      q29 == "Não" & q60 == "Sim" ~ "Primeiro emprego formal (com informalidade prévia)",
      q29 == "Sim" & q60 == "Sim" ~ "Inserção laboral prévia (formal e informal)",
      q29 == "Não" & q60 == "Não" ~ "Inexperiência absoluta (nunca trabalhou)",
      q29 == "Sim" & q60 == "Não" ~ "Histórico formal exclusivo",
      TRUE ~ "Outros perfis"
    ),
    perfil_insercao = factor(perfil_insercao, levels = c(
      "Primeiro emprego formal (com informalidade prévia)",
      "Inserção laboral prévia (formal e informal)",
      "Inexperiência absoluta (nunca trabalhou)",
      "Histórico formal exclusivo"
    ))
  )

df_resumo_cruz <- df_cruz |>
  count(AnoCol, perfil_insercao) |>
  pivot_wider(names_from = AnoCol, values_from = n, values_fill = 0) |>
  mutate(
    pct_2025 = `2025` / sum(`2025`),
    pct_2026 = `2026` / sum(`2026`),
    dif_pp = (pct_2026 - pct_2025) * 100
  )

tabela_cruz_dados <- tibble(
  `Taxonomia da Trajetória Laboral Pré-PPE` = c(as.character(df_resumo_cruz$perfil_insercao), "Total"),
  `2025 (N)` = c(as.character(df_resumo_cruz$`2025`), "711"),
  `2025 (%)` = c(percent(df_resumo_cruz$pct_2025, accuracy = 0.1, decimal.mark = ","), "100,0%"),
  `2026 (N)` = c(as.character(df_resumo_cruz$`2026`), "879"),
  `2026 (%)` = c(percent(df_resumo_cruz$pct_2026, accuracy = 0.1, decimal.mark = ","), "100,0%"),
  `Variação (p.p.)` = c(sprintf("%+.1f", df_resumo_cruz$dif_pp), "—")
)

t29_60_formalizacao <- tt(tabela_cruz_dados) |>
  format_tt(escape = TRUE)

df_plot_cruz <- df_cruz |>
  count(AnoCol, perfil_insercao, name = "Respondentes") |>
  group_by(AnoCol) |>
  mutate(
    Total = sum(Respondentes),
    Percentual = Respondentes / Total,
    Rotulo = percent(Percentual, accuracy = 0.1, decimal.mark = ",")
  ) |>
  ungroup()

g_formalizacao <- ggplot(df_plot_cruz, aes(x = fct_rev(perfil_insercao), y = Percentual, fill = AnoCol)) +
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
    limits = c(0, 0.55),
    expand = expansion(mult = c(0, 0.08))
  ) +
  labs(
    title = "Transição para a Formalidade: Taxonomia Ocupacional Pré-Ingresso",
    subtitle = "O PPE atua precipuamente como ponte da informalidade para o emprego formal protegido
(Cinza: 2025 | Azul: 2026)",
    x = "Condição Ocupacional Anterior",
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
