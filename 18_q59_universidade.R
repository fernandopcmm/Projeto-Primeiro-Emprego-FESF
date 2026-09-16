# ==============================================================================
# Projeto Primeiro Emprego (PPE) - Análise Comparativa Longitudinal (2025-2026)
# Script 18: 18_q59_universidade.R - Ingresso / Matrícula no Ensino Superior
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
COR_DESTAQUE <- "#174A7E"  # Azul escuro institucional (Sim / Foco analítico)
COR_CONTEXTO <- "#929497"  # Cinza neutro (Não / Contexto de referência)
COR_TEXTO    <- "#231F20"  # Grafite escuro para títulos e rótulos
COR_SUBTEXTO <- "#555655"  # Cinza médio para subtítulos e anotações
COR_GRID     <- "#E5E5E5"  # Cinza claro para linhas de grade sutis

# 3. Importação da Base Consolidada --------------------------------------------
caminho_csv <- "/home/nando/Documentos/gemini/ATUAL/OUTPUT/PPE_beneficiarios_2025_2026.csv"
df <- read_csv(caminho_csv, show_col_types = FALSE)

# 4. Harmonização e Preparação da Variável q59 ----------------------------------
# Está em alguma universidade / faculdade ("Sim" vs "Não")
df_uni <- df |>
  filter(q59 %in% c("Sim", "Não")) |>
  mutate(
    AnoCol = factor(AnoCol, levels = c("2025", "2026")),
    q59 = factor(q59, levels = c("Sim", "Não"))
  )

df_resumo_uni <- df_uni |>
  count(AnoCol, q59, name = "Respondentes") |>
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

# 5. Estruturação da Tabela Acadêmica (t59_universidade) ------------------------
# Colunas: Matrícula no Ensino Superior, 2025 (N), 2025 (%), 2026 (N), 2026 (%), Variação (p.p.)
n_25_tot <- sum(df_resumo_uni$Respondentes[df_resumo_uni$AnoCol == "2025"])
n_26_tot <- sum(df_resumo_uni$Respondentes[df_resumo_uni$AnoCol == "2026"])

tab_uni_25 <- df_resumo_uni |>
  filter(AnoCol == "2025") |>
  select(q59, n_25 = Respondentes, pct_25 = Percentual)

tab_uni_26 <- df_resumo_uni |>
  filter(AnoCol == "2026") |>
  select(q59, n_26 = Respondentes, pct_26 = Percentual)

tabela_uni_dados <- left_join(tab_uni_25, tab_uni_26, by = "q59") |>
  mutate(
    var_pp = (pct_26 - pct_25) * 100,
    `Matrícula Universitária` = if_else(q59 == "Sim", "Matriculado(a) no Ensino Superior", "Não matriculado(a)"),
    `2025 (N)` = format(n_25, big.mark = ".", decimal.mark = ","),
    `2025 (%)` = percent(pct_25, accuracy = 0.1, decimal.mark = ","),
    `2026 (N)` = format(n_26, big.mark = ".", decimal.mark = ","),
    `2026 (%)` = percent(pct_26, accuracy = 0.1, decimal.mark = ","),
    `Variação (p.p.)` = sub("\\.", ",", sprintf("%+.1f", var_pp))
  ) |>
  select(`Matrícula Universitária`, `2025 (N)`, `2025 (%)`, `2026 (N)`, `2026 (%)`, `Variação (p.p.)`)

linha_total_uni <- tibble(
  `Matrícula Universitária` = "Total",
  `2025 (N)` = format(n_25_tot, big.mark = ".", decimal.mark = ","),
  `2025 (%)` = "100,0%",
  `2026 (N)` = format(n_26_tot, big.mark = ".", decimal.mark = ","),
  `2026 (%)` = "100,0%",
  `Variação (p.p.)` = "—"
)

t59_universidade <- bind_rows(tabela_uni_dados, linha_total_uni) |>
  tt() |>
  format_tt(escape = TRUE)

# 6. Construção do Gráfico com Storytelling (g_universidade) -------------------
limite_y_uni <- max(df_resumo_uni$Respondentes, na.rm = TRUE) * 1.25

g_universidade <- ggplot(df_resumo_uni, aes(x = AnoCol, y = Respondentes, fill = q59)) +
  geom_col(
    position = position_dodge(width = 0.72),
    width = 0.62,
    alpha = 0.95
  ) +
  geom_text(
    aes(label = Rotulo_Curto, color = q59),
    position = position_dodge(width = 0.72),
    vjust = -0.35,
    size = 3.5,
    fontface = "bold",
    lineheight = 0.9
  ) +
  scale_fill_manual(values = c("Sim" = COR_DESTAQUE, "Não" = COR_CONTEXTO)) +
  scale_color_manual(values = c("Sim" = COR_DESTAQUE, "Não" = COR_CONTEXTO)) +
  scale_y_continuous(
    limits = c(0, limite_y_uni),
    labels = label_number(big.mark = ".", decimal.mark = ","),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    title = "Avaliação PPE: Ingressantes e Estudantes no Ensino Superior",
    subtitle = "Comparação da quantidade e proporção de beneficiários cursando faculdade/universidade\n(Azul: Sim | Cinza: Não)",
    x = "Ano da Coleta da Pesquisa",
    y = "Número de Beneficiários",
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
