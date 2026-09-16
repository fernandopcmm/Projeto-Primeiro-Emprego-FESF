# ==============================================================================
# Projeto Primeiro Emprego (PPE) - Análise Comparativa Longitudinal (2025-2026)
# Script 09: 09_q21_redes_sociais.R - Redes Sociais e Canais de Comunicação
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

# 4. Harmonização e Preparação dos Dados (q21) ----------------------------------
# Categorias harmonizadas solicitadas:
# Instagram, Não participa de rede social, LinkedIn, Tik Tok, WhatsApp, Facebook, Outras / Múltiplas redes
categorias_redes <- c(
  "Instagram",
  "Não participa de rede social",
  "LinkedIn",
  "Tik Tok",
  "WhatsApp",
  "Facebook",
  "Outras / Múltiplas redes"
)

df_redes <- df |>
  mutate(
    AnoCol = factor(AnoCol, levels = c("2025", "2026")),
    q21_limpa = case_when(
      grepl("não|nenhum|nao|utilizo", q21, ignore.case = TRUE) ~ "Não participa de rede social",
      grepl("todas|todas as redes|todas acima|todas a cima|múltiplas|multiplas", q21, ignore.case = TRUE) ~ "Outras / Múltiplas redes",
      grepl("e |,| e |/|&|mais de", q21, ignore.case = TRUE) & !grepl("não|nenhum|nao", q21, ignore.case = TRUE) ~ "Outras / Múltiplas redes",
      grepl("instagram|insta", q21, ignore.case = TRUE) ~ "Instagram",
      grepl("linkedin|linkid", q21, ignore.case = TRUE) ~ "LinkedIn",
      grepl("tik tok|tiktok", q21, ignore.case = TRUE) ~ "Tik Tok",
      grepl("whatsapp|watsaap|zap|watsap|wattsapp|whatsaap", q21, ignore.case = TRUE) ~ "WhatsApp",
      grepl("facebook|facbook", q21, ignore.case = TRUE) ~ "Facebook",
      TRUE ~ "Outras / Múltiplas redes"
    ),
    q21_limpa = factor(q21_limpa, levels = categorias_redes)
  )

df_resumo_redes <- df_redes |>
  count(AnoCol, q21_limpa, name = "Respondentes") |>
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

# 5. Estruturação da Tabela Acadêmica (t21_redes_sociais) -----------------------
# Colunas: Rede Social / Canal, 2025 (N), 2025 (%), 2026 (N), 2026 (%), Variação (p.p.)
n_25_tot <- sum(df_resumo_redes$Respondentes[df_resumo_redes$AnoCol == "2025"])
n_26_tot <- sum(df_resumo_redes$Respondentes[df_resumo_redes$AnoCol == "2026"])

tab_redes_25 <- df_resumo_redes |>
  filter(AnoCol == "2025") |>
  select(q21_limpa, n_25 = Respondentes, pct_25 = Percentual)

tab_redes_26 <- df_resumo_redes |>
  filter(AnoCol == "2026") |>
  select(q21_limpa, n_26 = Respondentes, pct_26 = Percentual)

tabela_redes_dados <- left_join(tab_redes_25, tab_redes_26, by = "q21_limpa") |>
  mutate(
    var_pp = (pct_26 - pct_25) * 100,
    `Rede Social / Canal` = as.character(q21_limpa),
    `2025 (N)` = format(n_25, big.mark = ".", decimal.mark = ","),
    `2025 (%)` = percent(pct_25, accuracy = 0.1, decimal.mark = ","),
    `2026 (N)` = format(n_26, big.mark = ".", decimal.mark = ","),
    `2026 (%)` = percent(pct_26, accuracy = 0.1, decimal.mark = ","),
    `Variação (p.p.)` = sub("\\.", ",", sprintf("%+.1f", var_pp))
  ) |>
  select(`Rede Social / Canal`, `2025 (N)`, `2025 (%)`, `2026 (N)`, `2026 (%)`, `Variação (p.p.)`)

linha_total_redes <- tibble(
  `Rede Social / Canal` = "Total",
  `2025 (N)` = format(n_25_tot, big.mark = ".", decimal.mark = ","),
  `2025 (%)` = "100,0%",
  `2026 (N)` = format(n_26_tot, big.mark = ".", decimal.mark = ","),
  `2026 (%)` = "100,0%",
  `Variação (p.p.)` = "—"
)

t21_redes_sociais <- bind_rows(tabela_redes_dados, linha_total_redes) |>
  tt() |>
  format_tt(escape = TRUE)

# 6. Construção do Gráfico com Storytelling (g_redes_sociais) -------------------
# Gráfico horizontal minimalista, ordenado com as redes mais representativas no topo
limite_y_redes <- max(df_resumo_redes$Respondentes, na.rm = TRUE) * 1.25

df_resumo_redes_plot <- df_resumo_redes |>
  mutate(
    q21_rev = factor(q21_limpa, levels = rev(categorias_redes)),
    Rotulo_Linha = paste0(
      format(Respondentes, big.mark = ".", decimal.mark = ","),
      " (", percent(Percentual, accuracy = 0.1, decimal.mark = ","), ")"
    )
  )

g_redes_sociais <- ggplot(df_resumo_redes_plot, aes(x = q21_rev, y = Respondentes, fill = AnoCol)) +
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
    limits = c(0, limite_y_redes),
    labels = label_number(big.mark = ".", decimal.mark = ","),
    expand = expansion(mult = c(0, 0.08))
  ) +
  coord_flip() +
  labs(
    title = "Avaliação PPE: Redes Sociais e Canais de Comunicação Utilizados",
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
