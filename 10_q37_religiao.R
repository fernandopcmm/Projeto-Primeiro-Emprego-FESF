# ==============================================================================
# Projeto Primeiro Emprego (PPE) - Análise Comparativa Longitudinal (2025-2026)
# Script 10: 10_q37_religiao.R - Distribuição por Afiliação Religiosa
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

# 4. Harmonização e Preparação dos Dados de Religião (q37) ---------------------
# Categorias harmonizadas solicitadas:
# Evangélica / Cristã, Católica, Sem religião, Matriz Africana (Candomblé/Umbanda),
# Testemunha de Jeová, Espírita, Outras religiões
categorias_religiao <- c(
  "Evangélica / Cristã",
  "Católica",
  "Sem religião",
  "Matriz Africana (Candomblé/Umbanda)",
  "Testemunha de Jeová",
  "Espírita",
  "Outras religiões"
)

df_religiao <- df |>
  mutate(
    AnoCol = factor(AnoCol, levels = c("2025", "2026")),
    q37_limpa = case_when(
      grepl("evengélica|evangélica|evangelica|cristã|cristão|crista|crente|protestante|adventista|presbiteriana|batista|assembleia|congregação", q37, ignore.case = TRUE) ~ "Evangélica / Cristã",
      grepl("católica|catolica", q37, ignore.case = TRUE) ~ "Católica",
      grepl("não tem|sem religião|ateu|agnóstico|agnostico|nenhuma", q37, ignore.case = TRUE) ~ "Sem religião",
      grepl("candomblé|candomble|umbanda|matriz africana|afro|orixá", q37, ignore.case = TRUE) ~ "Matriz Africana (Candomblé/Umbanda)",
      grepl("testemunha", q37, ignore.case = TRUE) ~ "Testemunha de Jeová",
      grepl("espírita|espirita|kardec", q37, ignore.case = TRUE) ~ "Espírita",
      TRUE ~ "Outras religiões"
    ),
    q37_limpa = factor(q37_limpa, levels = categorias_religiao)
  )

df_resumo_religiao <- df_religiao |>
  count(AnoCol, q37_limpa, name = "Respondentes") |>
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

# 5. Estruturação da Tabela Acadêmica (t37_religiao) ---------------------------
# Colunas: Afiliação Religiosa, 2025 (N), 2025 (%), 2026 (N), 2026 (%), Variação (p.p.)
n_25_tot <- sum(df_resumo_religiao$Respondentes[df_resumo_religiao$AnoCol == "2025"])
n_26_tot <- sum(df_resumo_religiao$Respondentes[df_resumo_religiao$AnoCol == "2026"])

tab_rel_25 <- df_resumo_religiao |>
  filter(AnoCol == "2025") |>
  select(q37_limpa, n_25 = Respondentes, pct_25 = Percentual)

tab_rel_26 <- df_resumo_religiao |>
  filter(AnoCol == "2026") |>
  select(q37_limpa, n_26 = Respondentes, pct_26 = Percentual)

tabela_rel_dados <- left_join(tab_rel_25, tab_rel_26, by = "q37_limpa") |>
  mutate(
    var_pp = (pct_26 - pct_25) * 100,
    `Afiliação Religiosa` = as.character(q37_limpa),
    `2025 (N)` = format(n_25, big.mark = ".", decimal.mark = ","),
    `2025 (%)` = percent(pct_25, accuracy = 0.1, decimal.mark = ","),
    `2026 (N)` = format(n_26, big.mark = ".", decimal.mark = ","),
    `2026 (%)` = percent(pct_26, accuracy = 0.1, decimal.mark = ","),
    `Variação (p.p.)` = sub("\\.", ",", sprintf("%+.1f", var_pp))
  ) |>
  select(`Afiliação Religiosa`, `2025 (N)`, `2025 (%)`, `2026 (N)`, `2026 (%)`, `Variação (p.p.)`)

linha_total_rel <- tibble(
  `Afiliação Religiosa` = "Total",
  `2025 (N)` = format(n_25_tot, big.mark = ".", decimal.mark = ","),
  `2025 (%)` = "100,0%",
  `2026 (N)` = format(n_26_tot, big.mark = ".", decimal.mark = ","),
  `2026 (%)` = "100,0%",
  `Variação (p.p.)` = "—"
)

t37_religiao <- bind_rows(tabela_rel_dados, linha_total_rel) |>
  tt() |>
  format_tt(escape = TRUE)

# 6. Construção do Gráfico com Storytelling (g_religiao) -----------------------
# Gráfico horizontal ordenado para perfeita leitura das descrições confessionais
limite_y_rel <- max(df_resumo_religiao$Respondentes, na.rm = TRUE) * 1.25

df_resumo_rel_plot <- df_resumo_religiao |>
  mutate(
    q37_rev = factor(q37_limpa, levels = rev(categorias_religiao)),
    Rotulo_Linha = paste0(
      format(Respondentes, big.mark = ".", decimal.mark = ","),
      " (", percent(Percentual, accuracy = 0.1, decimal.mark = ","), ")"
    )
  )

g_religiao <- ggplot(df_resumo_rel_plot, aes(x = q37_rev, y = Respondentes, fill = AnoCol)) +
  geom_col(
    position = position_dodge(width = 0.75),
    width = 0.65,
    alpha = 0.95
  ) +
  geom_text(
    aes(label = Rotulo_Linha, color = AnoCol),
    position = position_dodge(width = 0.75),
    hjust = -0.1,
    size = 3.2,
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
    limits = c(0, limite_y_rel),
    labels = label_number(big.mark = ".", decimal.mark = ","),
    expand = expansion(mult = c(0, 0.08))
  ) +
  coord_flip() +
  labs(
    title = "Avaliação PPE: Distribuição por Afiliação Religiosa",
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
