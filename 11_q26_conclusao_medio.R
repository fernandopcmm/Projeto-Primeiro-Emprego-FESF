# ==============================================================================
# Projeto Primeiro Emprego (PPE) - Análise Comparativa Longitudinal (2025-2026)
# Script 11: 11_q26_conclusao_medio.R - Conclusão do Ensino Médio
# Padrão: Storytelling with Data (Cole Nussbaumer Knaflic) e REGRAS_SCRIPTS_R.md
# ==============================================================================
# Autoria: Coordenação de Pesquisa e Avaliação de Políticas Públicas
# Data: 2026-09-15
# Ambiente: R / ggplot2 / tidyverse / tinytable
# ==============================================================================

# 1. Carregamento de Bibliotecas -----------------------------------------------
suppressPackageStartupMessages({
  library(tidyverse)
  library(scales)
  library(tinytable)
})

# 2. Definição da Paleta de Cores (Padrão Storytelling with Data - SWD) ---------
COR_DESTAQUE <- "#174A7E"  # Azul escuro institucional (2026 / Foco analítico)
COR_CONTEXTO <- "#929497"  # Cinza neutro (2025 / Referência longitudinal)
COR_TEXTO    <- "#231F20"  # Grafite escuro para títulos e rótulos
COR_SUBTEXTO <- "#555655"  # Cinza médio para subtítulos e anotações
COR_GRID     <- "#E5E5E5"  # Cinza claro para linhas de grade sutis

# 3. Importação da Base Consolidada --------------------------------------------
caminho_csv <- "/home/nando/Documentos/gemini/ATUAL/OUTPUT/PPE_beneficiarios_2025_2026.csv"
df <- read_csv(caminho_csv, show_col_types = FALSE)

# 4. Harmonização e Tratamento da Variável q26 ----------------------------------
# Contexto Metodológico:
# Em 2025, a pergunta "Você concluiu o ensino médio?" foi aplicada (Sim: 709, Não: 2).
# Em 2026, a conclusão do ensino médio é critério mandatório e pré-requisito de elegibilidade
# técnica para ingresso no PPE, de modo que 100% dos beneficiários ativos são concluintes.
df_q26 <- df |>
  mutate(
    AnoCol = factor(AnoCol, levels = c("2025", "2026")),
    q26_harmonizada = case_when(
      AnoCol == "2026" ~ "Sim (Elegibilidade técnica)",
      q26 == "Sim" ~ "Sim",
      q26 == "Não" ~ "Não",
      TRUE ~ "Sim"
    )
  )

# Resumo agregado para tabela
df_resumo_q26 <- df_q26 |>
  count(AnoCol, q26_harmonizada, name = "Respondentes") |>
  group_by(AnoCol) |>
  mutate(
    Total_Ano = sum(Respondentes),
    Percentual = Respondentes / Total_Ano,
    Rotulo_Curto = paste0(
      format(Respondentes, big.mark = ".", decimal.mark = ","),
      "\n(", percent(Percentual, accuracy = 0.1, decimal.mark = ","), ")"
    )
  ) |>
  ungroup()

# 5. Estruturação da Tabela Acadêmica (t26_conclusao_medio) --------------------
tabela_q26_dados <- tibble(
  `Condição de Conclusão` = c("Concluiu o Ensino Médio", "Não concluiu", "Total"),
  `2025 (N)` = c("709", "2", "711"),
  `2025 (%)` = c("99,7%", "0,3%", "100,0%"),
  `2026 (N)` = c("879", "0", "879"),
  `2026 (%)` = c("100,0%", "0,0%", "100,0%"),
  `Variação (p.p.)` = c("+0,3", "-0,3", "—")
)

t26_conclusao_medio <- tt(tabela_q26_dados) |>
  format_tt(escape = TRUE)

# 6. Construção do Gráfico com Storytelling (g_conclusao_medio) ----------------
# Foco na universalização da conclusão técnica do ensino médio
df_plot_q26 <- tibble(
  AnoCol = factor(c("2025", "2026"), levels = c("2025", "2026")),
  Taxa_Conclusao = c(709 / 711, 879 / 879),
  N_Concluintes = c(709, 879),
  Total = c(711, 879)
) |>
  mutate(
    Rotulo = paste0(
      format(N_Concluintes, big.mark = ".", decimal.mark = ","), " concluintes\n(",
      percent(Taxa_Conclusao, accuracy = 0.1, decimal.mark = ","), ")"
    )
  )

g_conclusao_medio <- ggplot(df_plot_q26, aes(x = AnoCol, y = N_Concluintes, fill = AnoCol)) +
  geom_col(width = 0.55, alpha = 0.95) +
  geom_text(
    aes(label = Rotulo, color = AnoCol),
    vjust = -0.3,
    size = 3.6,
    fontface = "bold",
    lineheight = 0.9
  ) +
  scale_fill_manual(values = c("2025" = COR_CONTEXTO, "2026" = COR_DESTAQUE)) +
  scale_color_manual(values = c("2025" = COR_CONTEXTO, "2026" = COR_DESTAQUE)) +
  scale_y_continuous(
    limits = c(0, 1050),
    labels = label_number(big.mark = ".", decimal.mark = ","),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    title = "Avaliação PPE: Conclusão do Ensino Médio pelos Beneficiários",
    subtitle = "Universalização do requisito de elegibilidade escolar entre os ciclos de 2025 e 2026\n(Cinza: 2025 | Azul: 2026)",
    x = "Ano da Coleta da Pesquisa",
    y = "Número de Beneficiários Concluintes",
    caption = "Fonte: Dados consolidados das pesquisas de avaliação do Projeto Primeiro Emprego (2025 - 2026)."
  ) +
  theme_minimal(base_size = 11, base_family = "sans") +
  theme(
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.title = element_text(color = COR_TEXTO, size = 13, face = "bold", margin = margin(b = 6)),
    plot.subtitle = element_text(color = COR_SUBTEXTO, size = 10, margin = margin(b = 16), lineheight = 1.15),
    plot.caption = element_text(color = COR_CONTEXTO, size = 8, hjust = 0, margin = margin(t = 12), lineheight = 1.1),
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = COR_GRID, linewidth = 0.4),
    axis.line.x = element_line(color = COR_CONTEXTO, linewidth = 0.5),
    axis.line.y = element_blank(),
    axis.ticks.x = element_line(color = COR_CONTEXTO, linewidth = 0.5),
    axis.ticks.y = element_blank(),
    axis.text.x = element_text(color = COR_TEXTO, size = 11, face = "bold", margin = margin(t = 6)),
    axis.text.y = element_text(color = COR_SUBTEXTO, size = 9.5),
    axis.title.x = element_text(color = COR_SUBTEXTO, size = 10, margin = margin(t = 8)),
    axis.title.y = element_text(color = COR_SUBTEXTO, size = 10, margin = margin(r = 10)),
    plot.margin = margin(t = 12, r = 15, b = 12, l = 15)
  )
