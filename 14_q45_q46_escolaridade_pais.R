# ==============================================================================
# Projeto Primeiro Emprego (PPE) - Análise Comparativa Longitudinal (2025-2026)
# Script 14: 14_q45_q46_escolaridade_pais.R - Escolaridade do Pai e da Mãe
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
COR_DESTAQUE <- "#174A7E"  # Azul escuro institucional (Mãe / Destaque de escolaridade)
COR_CONTEXTO <- "#929497"  # Cinza neutro (Pai / Contexto comparativo)
COR_TEXTO    <- "#231F20"  # Grafite escuro para títulos e rótulos
COR_SUBTEXTO <- "#555655"  # Cinza médio para subtítulos e anotações
COR_GRID     <- "#E5E5E5"  # Cinza claro para linhas de grade sutis

# 3. Importação da Base Consolidada --------------------------------------------
caminho_csv <- "/home/nando/Documentos/gemini/ATUAL/OUTPUT/PPE_beneficiarios_2025_2026.csv"
df <- read_csv(caminho_csv, show_col_types = FALSE)

# 4. Harmonização e Preparação das Variáveis de Escolaridade (q45 e q46) -------
niveis_escolaridade <- c(
  "Nenhuma escolaridade",
  "Ensino fundamental 1º ao 5º ano",
  "Ensino fundamental 6º ao 9º ano",
  "Ensino médio",
  "Ensino superior",
  "Pós-graduação"
)

# Resumo para o Pai (q45)
df_pai <- df |>
  filter(!is.na(q45)) |>
  count(AnoCol, q45, name = "N") |>
  group_by(AnoCol) |>
  mutate(
    Total = sum(N),
    Pct = N / Total,
    Familiar = "Pai",
    Escolaridade = factor(q45, levels = niveis_escolaridade)
  ) |>
  ungroup()

# Resumo para a Mãe (q46)
df_mae <- df |>
  filter(!is.na(q46)) |>
  count(AnoCol, q46, name = "N") |>
  group_by(AnoCol) |>
  mutate(
    Total = sum(N),
    Pct = N / Total,
    Familiar = "Mãe",
    Escolaridade = factor(q46, levels = niveis_escolaridade)
  ) |>
  ungroup()

# Base empilhada Pai e Mãe
df_pais_comb <- bind_rows(df_pai, df_mae) |>
  mutate(
    AnoCol = factor(AnoCol, levels = c("2025", "2026")),
    Familiar = factor(Familiar, levels = c("Pai", "Mãe"))
  )

# 5. Estruturação da Tabela Acadêmica (t45_46_pais) ----------------------------
# Formato: Escolaridade | 2025 Pai (N / %) | 2025 Mãe (N / %) | 2026 Pai (N / %) | 2026 Mãe (N / %)
tabela_pais_25_pai <- df_pai |> filter(AnoCol == "2025") |> select(Escolaridade, n_pai_25 = N, pct_pai_25 = Pct)
tabela_pais_25_mae <- df_mae |> filter(AnoCol == "2025") |> select(Escolaridade, n_mae_25 = N, pct_mae_25 = Pct)
tabela_pais_26_pai <- df_pai |> filter(AnoCol == "2026") |> select(Escolaridade, n_pai_26 = N, pct_pai_26 = Pct)
tabela_pais_26_mae <- df_mae |> filter(AnoCol == "2026") |> select(Escolaridade, n_mae_26 = N, pct_mae_26 = Pct)

tabela_pais_dados <- tibble(Escolaridade = factor(niveis_escolaridade, levels = niveis_escolaridade)) |>
  left_join(tabela_pais_25_pai, by = "Escolaridade") |>
  left_join(tabela_pais_25_mae, by = "Escolaridade") |>
  left_join(tabela_pais_26_pai, by = "Escolaridade") |>
  left_join(tabela_pais_26_mae, by = "Escolaridade") |>
  mutate(
    `Nível de Escolaridade` = as.character(Escolaridade),
    `2025 Pai (N)` = format(n_pai_25, big.mark = ".", decimal.mark = ","),
    `2025 Pai (%)` = percent(pct_pai_25, accuracy = 0.1, decimal.mark = ","),
    `2025 Mãe (N)` = format(n_mae_25, big.mark = ".", decimal.mark = ","),
    `2025 Mãe (%)` = percent(pct_mae_25, accuracy = 0.1, decimal.mark = ","),
    `2026 Pai (N)` = format(n_pai_26, big.mark = ".", decimal.mark = ","),
    `2026 Pai (%)` = percent(pct_pai_26, accuracy = 0.1, decimal.mark = ","),
    `2026 Mãe (N)` = format(n_mae_26, big.mark = ".", decimal.mark = ","),
    `2026 Mãe (%)` = percent(pct_mae_26, accuracy = 0.1, decimal.mark = ",")
  ) |>
  select(
    `Nível de Escolaridade`,
    `2025 Pai (N)`, `2025 Pai (%)`,
    `2025 Mãe (N)`, `2025 Mãe (%)`,
    `2026 Pai (N)`, `2026 Pai (%)`,
    `2026 Mãe (N)`, `2026 Mãe (%)`
  )

linha_total_pais <- tibble(
  `Nível de Escolaridade` = "Total",
  `2025 Pai (N)` = format(sum(df_pai$N[df_pai$AnoCol == "2025"]), big.mark = ".", decimal.mark = ","),
  `2025 Pai (%)` = "100,0%",
  `2025 Mãe (N)` = format(sum(df_mae$N[df_mae$AnoCol == "2025"]), big.mark = ".", decimal.mark = ","),
  `2025 Mãe (%)` = "100,0%",
  `2026 Pai (N)` = format(sum(df_pai$N[df_pai$AnoCol == "2026"]), big.mark = ".", decimal.mark = ","),
  `2026 Pai (%)` = "100,0%",
  `2026 Mãe (N)` = format(sum(df_mae$N[df_mae$AnoCol == "2026"]), big.mark = ".", decimal.mark = ","),
  `2026 Mãe (%)` = "100,0%"
)

t45_46_pais <- bind_rows(tabela_pais_dados, linha_total_pais) |>
  tt() |>
  format_tt(escape = TRUE)

# 6. Construção do Gráfico com Storytelling (g_escolaridade_pais) ---------------
# Comparativo visual multifacetado: Escolaridade x Proporção por Pai e Mãe
df_pais_plot <- df_pais_comb |>
  mutate(
    Escolaridade_rev = factor(Escolaridade, levels = rev(niveis_escolaridade)),
    Rotulo_Linha = percent(Pct, accuracy = 0.1, decimal.mark = ",")
  )

g_escolaridade_pais <- ggplot(df_pais_plot, aes(x = Escolaridade_rev, y = Pct, fill = Familiar)) +
  geom_col(
    position = position_dodge(width = 0.75),
    width = 0.65,
    alpha = 0.95
  ) +
  geom_text(
    aes(label = Rotulo_Linha, color = Familiar),
    position = position_dodge(width = 0.75),
    hjust = -0.1,
    size = 3.0,
    fontface = "bold"
  ) +
  scale_fill_manual(
    name = "Familiar",
    values = c("Pai" = COR_CONTEXTO, "Mãe" = COR_DESTAQUE)
  ) +
  scale_color_manual(
    name = "Familiar",
    values = c("Pai" = COR_CONTEXTO, "Mãe" = COR_DESTAQUE)
  ) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    limits = c(0, 0.52),
    expand = expansion(mult = c(0, 0.08))
  ) +
  coord_flip() +
  facet_wrap(~ AnoCol) +
  labs(
    title = "Avaliação PPE: Nível de Escolaridade dos Pais dos Beneficiários",
    subtitle = "Comparação da proporção da escolaridade paterna e materna nos ciclos de 2025 e 2026\n(Cinza: Pai | Azul: Mãe)",
    x = NULL,
    y = "Proporção de Respostas no Ano",
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
    strip.text = element_text(size = 11, face = "bold", color = COR_TEXTO, margin = margin(b = 8)),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = COR_GRID, linewidth = 0.4),
    axis.line.y = element_line(color = COR_CONTEXTO, linewidth = 0.5),
    axis.line.x = element_blank(),
    axis.ticks.y = element_line(color = COR_CONTEXTO, linewidth = 0.5),
    axis.ticks.x = element_blank(),
    axis.text.y = element_text(color = COR_TEXTO, size = 9.5, face = "bold", margin = margin(r = 6)),
    axis.text.x = element_text(color = COR_SUBTEXTO, size = 9),
    axis.title.x = element_text(color = COR_SUBTEXTO, size = 10, margin = margin(t = 8)),
    plot.margin = margin(t = 12, r = 16, b = 12, l = 16)
  )
