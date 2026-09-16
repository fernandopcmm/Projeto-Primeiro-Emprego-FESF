# ==============================================================================
# Projeto Primeiro Emprego (PPE) - Análise Comparativa Longitudinal (2025-2026)
# Script 05: 05_q36_raca_cor.R - Distribuição de Raça/Cor Autodeclarada
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
COR_TEXTO    <- "#231F20"  # Cinza escuro / grafite para títulos
COR_SUBTEXTO <- "#555655"  # Cinza médio para subtítulos e rótulos
COR_GRID     <- "#E5E5E5"  # Cinza claro para linhas de grade sutis

# 3. Importação da Base Consolidada --------------------------------------------
caminho_csv <- "/home/nando/Documentos/gemini/ATUAL/OUTPUT/PPE_beneficiarios_2025_2026.csv"
df <- read_csv(caminho_csv, show_col_types = FALSE)

# 4. Preparação e Agregação dos Dados de Raça/Cor ------------------------------
# Categorias oficiais do IBGE adotadas no questionário do PPE:
# Pardo(a), Preto(a), Branco(a), Amarelo(a), Indígena ou de origem indígena
categorias_raca <- c(
  "Pardo(a)",
  "Preto(a)",
  "Branco(a)",
  "Amarelo(a)",
  "Indígena ou de origem indígena"
)

df_raca <- df |>
  filter(!is.na(q36)) |>
  mutate(
    AnoCol = factor(AnoCol, levels = c("2025", "2026")),
    q36 = factor(q36, levels = categorias_raca)
  )

df_resumo_raca <- df_raca |>
  count(AnoCol, q36, name = "Respondentes") |>
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

# 5. Estruturação da Tabela Acadêmica (t36_raca_cor) ---------------------------
# Colunas: Raça/Cor, 2025 (N), 2025 (%), 2026 (N), 2026 (%), Variação (p.p.)
n_25_tot <- sum(df_resumo_raca$Respondentes[df_resumo_raca$AnoCol == "2025"])
n_26_tot <- sum(df_resumo_raca$Respondentes[df_resumo_raca$AnoCol == "2026"])

tab_raca_25 <- df_resumo_raca |>
  filter(AnoCol == "2025") |>
  select(q36, n_25 = Respondentes, pct_25 = Percentual)

tab_raca_26 <- df_resumo_raca |>
  filter(AnoCol == "2026") |>
  select(q36, n_26 = Respondentes, pct_26 = Percentual)

tabela_raca_dados <- left_join(tab_raca_25, tab_raca_26, by = "q36") |>
  mutate(
    var_pp = (pct_26 - pct_25) * 100,
    `Raça/Cor` = as.character(q36),
    `2025 (N)` = format(n_25, big.mark = ".", decimal.mark = ","),
    `2025 (%)` = percent(pct_25, accuracy = 0.1, decimal.mark = ","),
    `2026 (N)` = format(n_26, big.mark = ".", decimal.mark = ","),
    `2026 (%)` = percent(pct_26, accuracy = 0.1, decimal.mark = ","),
    `Variação (p.p.)` = sub("\\.", ",", sprintf("%+.1f", var_pp))
  ) |>
  select(`Raça/Cor`, `2025 (N)`, `2025 (%)`, `2026 (N)`, `2026 (%)`, `Variação (p.p.)`)

linha_total_raca <- tibble(
  `Raça/Cor` = "Total",
  `2025 (N)` = format(n_25_tot, big.mark = ".", decimal.mark = ","),
  `2025 (%)` = "100,0%",
  `2026 (N)` = format(n_26_tot, big.mark = ".", decimal.mark = ","),
  `2026 (%)` = "100,0%",
  `Variação (p.p.)` = "—"
)

t36_raca_cor <- bind_rows(tabela_raca_dados, linha_total_raca) |>
  tt() |>
  format_tt(escape = TRUE)

# 6. Construção do Gráfico com Storytelling (g_raca_cor) -----------------------
# Gráfico horizontal para acomodar os rótulos de raça/cor com clareza tipográfica
limite_y_raca <- max(df_resumo_raca$Respondentes, na.rm = TRUE) * 1.25

# Ordenação para barras horizontais: mais frequente no topo
df_resumo_raca_plot <- df_resumo_raca |>
  mutate(
    q36_rev = factor(q36, levels = rev(categorias_raca)),
    Rotulo_Linha = paste0(
      format(Respondentes, big.mark = ".", decimal.mark = ","),
      " (", percent(Percentual, accuracy = 0.1, decimal.mark = ","), ")"
    )
  )

g_raca_cor <- ggplot(df_resumo_raca_plot, aes(x = q36_rev, y = Respondentes, fill = AnoCol)) +
  geom_col(
    position = position_dodge(width = 0.75),
    width = 0.65,
    alpha = 0.95
  ) +
  geom_text(
    aes(label = Rotulo_Linha, color = AnoCol),
    position = position_dodge(width = 0.75),
    hjust = -0.1,
    size = 3.3,
    fontface = "bold"
  ) +
  scale_fill_manual(
    name = "Ano de Coleta",
    values = c("2025" = COR_CONTEXTO, "2026" = COR_DESTAQUE)
  ) +
  scale_color_manual(
    name = "Ano de Coleta",
    values = c("2025" = COR_CONTEXTO, "2026" = COR_DESTAQUE)
  ) +
  scale_y_continuous(
    limits = c(0, limite_y_raca),
    labels = label_number(big.mark = ".", decimal.mark = ","),
    expand = expansion(mult = c(0, 0.08))
  ) +
  coord_flip() +
  labs(
    title = "Avaliação PPE: Distribuição por Raça/Cor Autodeclarada",
    subtitle = "Comparação da frequência absoluta e relativa entre os ciclos avaliativos de 2025 e 2026\n(Cinza: 2025 | Azul: 2026)",
    x = NULL,
    y = "Número de Respondentes",
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
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = COR_GRID, linewidth = 0.4),
    axis.line.y = element_line(color = COR_CONTEXTO, linewidth = 0.5),
    axis.line.x = element_blank(),
    axis.ticks.y = element_line(color = COR_CONTEXTO, linewidth = 0.5),
    axis.ticks.x = element_blank(),
    axis.text.y = element_text(color = COR_TEXTO, size = 10, face = "bold", margin = margin(r = 6)),
    axis.text.x = element_text(color = COR_SUBTEXTO, size = 9.5),
    axis.title.x = element_text(color = COR_SUBTEXTO, size = 10, margin = margin(t = 8)),
    plot.margin = margin(t = 12, r = 20, b = 12, l = 15)
  )
