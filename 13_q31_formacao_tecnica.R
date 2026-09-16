# ==============================================================================
# Projeto Primeiro Emprego (PPE) - Análise Comparativa Longitudinal (2025-2026)
# Script 13: 13_q31_formacao_tecnica.R - Cursos de Formação Técnica
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

# 4. Harmonização dos Cursos Técnicos (q31) -------------------------------------
# Normalização textual e agregação nas grandes áreas técnicas do PPE
df_q31 <- df |>
  filter(!is.na(q31)) |>
  mutate(
    AnoCol = factor(AnoCol, levels = c("2025", "2026")),
    q31_norm = str_to_lower(q31),
    curso = case_when(
      grepl("log[ií]stica", q31_norm) ~ "Logística",
      grepl("administra|adm", q31_norm) ~ "Administração",
      grepl("enfermagem", q31_norm) ~ "Enfermagem",
      grepl("inform[aá]tica|desenvolvimento de sistemas|redes de computadores|t\\.i|ti", q31_norm) ~ "Informática / TI",
      grepl("an[aá]lise|laborat[oó]rio", q31_norm) ~ "Análises Clínicas",
      grepl("seguran[cç]a do trabalho", q31_norm) ~ "Segurança do Trabalho",
      grepl("ger[eê]ncia em sa[uú]de|sa[uú]de bucal|farm[aá]cia|radiologia", q31_norm) ~ "Outras áreas da Saúde",
      grepl("nutri[cç][aã]o|diet[eé]tica", q31_norm) ~ "Nutrição e Dietética",
      grepl("secretariado", q31_norm) ~ "Secretariado",
      grepl("contabilidade|finan[cç]as", q31_norm) ~ "Contabilidade",
      grepl("recursos humanos|rh", q31_norm) ~ "Recursos Humanos",
      grepl("meio ambiente", q31_norm) ~ "Meio Ambiente",
      TRUE ~ "Outros cursos técnicos"
    )
  )

# Identificação do Top 10 de cursos no consolidado geral
top10_cursos <- df_q31 |>
  count(curso, sort = TRUE) |>
  slice_head(n = 10) |>
  pull(curso)

df_q31_top10 <- df_q31 |>
  mutate(
    curso_top = if_else(curso %in% top10_cursos, curso, "Outros cursos técnicos"),
    curso_top = factor(curso_top, levels = top10_cursos)
  )

# Resumo agregado por ano
df_resumo_q31 <- df_q31_top10 |>
  count(AnoCol, curso_top, name = "Respondentes") |>
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

# 5. Estruturação da Tabela Acadêmica (t31_formacao_tecnica) -------------------
# Colunas: Curso / Área Técnica, 2025 (N), 2025 (%), 2026 (N), 2026 (%), Variação (p.p.)
n_25_tot <- sum(df_resumo_q31$Respondentes[df_resumo_q31$AnoCol == "2025"])
n_26_tot <- sum(df_resumo_q31$Respondentes[df_resumo_q31$AnoCol == "2026"])

tab_q31_25 <- df_resumo_q31 |>
  filter(AnoCol == "2025") |>
  select(curso_top, n_25 = Respondentes, pct_25 = Percentual)

tab_q31_26 <- df_resumo_q31 |>
  filter(AnoCol == "2026") |>
  select(curso_top, n_26 = Respondentes, pct_26 = Percentual)

tabela_q31_dados <- left_join(tab_q31_25, tab_q31_26, by = "curso_top") |>
  mutate(
    n_25 = replace_na(n_25, 0),
    pct_25 = replace_na(pct_25, 0),
    n_26 = replace_na(n_26, 0),
    pct_26 = replace_na(pct_26, 0),
    var_pp = (pct_26 - pct_25) * 100,
    `Curso / Área Técnica` = as.character(curso_top),
    `2025 (N)` = format(n_25, big.mark = ".", decimal.mark = ","),
    `2025 (%)` = percent(pct_25, accuracy = 0.1, decimal.mark = ","),
    `2026 (N)` = format(n_26, big.mark = ".", decimal.mark = ","),
    `2026 (%)` = percent(pct_26, accuracy = 0.1, decimal.mark = ","),
    `Variação (p.p.)` = sub("\\.", ",", sprintf("%+.1f", var_pp))
  ) |>
  select(`Curso / Área Técnica`, `2025 (N)`, `2025 (%)`, `2026 (N)`, `2026 (%)`, `Variação (p.p.)`)

linha_total_q31 <- tibble(
  `Curso / Área Técnica` = "Total Amostral",
  `2025 (N)` = format(n_25_tot, big.mark = ".", decimal.mark = ","),
  `2025 (%)` = "100,0%",
  `2026 (N)` = format(n_26_tot, big.mark = ".", decimal.mark = ","),
  `2026 (%)` = "100,0%",
  `Variação (p.p.)` = "—"
)

t31_formacao_tecnica <- bind_rows(tabela_q31_dados, linha_total_q31) |>
  tt() |>
  format_tt(escape = TRUE)

# 6. Construção do Gráfico com Storytelling (g_formacao_tecnica) ----------------
# Gráfico horizontal para legibilidade dos nomes dos cursos
limite_y_q31 <- max(df_resumo_q31$Respondentes, na.rm = TRUE) * 1.25

df_resumo_q31_plot <- df_resumo_q31 |>
  mutate(
    curso_rev = factor(curso_top, levels = rev(top10_cursos)),
    Rotulo_Linha = paste0(
      format(Respondentes, big.mark = ".", decimal.mark = ","),
      " (", percent(Percentual, accuracy = 0.1, decimal.mark = ","), ")"
    )
  )

g_formacao_tecnica <- ggplot(df_resumo_q31_plot, aes(x = curso_rev, y = Respondentes, fill = AnoCol)) +
  geom_col(
    position = position_dodge(width = 0.75),
    width = 0.65,
    alpha = 0.95
  ) +
  geom_text(
    aes(label = Rotulo_Linha, color = AnoCol),
    position = position_dodge(width = 0.75),
    hjust = -0.1,
    size = 3.1,
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
    limits = c(0, limite_y_q31),
    labels = label_number(big.mark = ".", decimal.mark = ","),
    expand = expansion(mult = c(0, 0.08))
  ) +
  coord_flip() +
  labs(
    title = "Avaliação PPE: Principais Cursos de Formação Técnica",
    subtitle = "Comparação da frequência absoluta e relativa entre os ciclos avaliativos de 2025 e 2026\n(Cinza: 2025 | Azul: 2026)",
    x = NULL,
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
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = COR_GRID, linewidth = 0.4),
    axis.line.y = element_line(color = COR_CONTEXTO, linewidth = 0.5),
    axis.line.x = element_blank(),
    axis.ticks.y = element_line(color = COR_CONTEXTO, linewidth = 0.5),
    axis.ticks.x = element_blank(),
    axis.text.y = element_text(color = COR_TEXTO, size = 9.5, face = "bold", margin = margin(r = 6)),
    axis.text.x = element_text(color = COR_SUBTEXTO, size = 9.5),
    axis.title.x = element_text(color = COR_SUBTEXTO, size = 10, margin = margin(t = 8)),
    plot.margin = margin(t = 12, r = 20, b = 12, l = 15)
  )
