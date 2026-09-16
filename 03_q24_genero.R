# ==============================================================================
# Projeto Primeiro Emprego (PPE) - Análise Comparativa Longitudinal (2025-2026)
# Script 03: 03_q24_genero.R - Comparação de Gênero por Ano de Coleta
# Inspirado em Cole Nussbaumer Knaflic (Storytelling with Data)
# ==============================================================================
# Autoria: Coordenação de Pesquisa e Avaliação de Políticas Públicas
# Data: 2026-09-15
# Ambiente: R / ggplot2 / tidyverse
# ==============================================================================

# 1. Carregamento de Bibliotecas -----------------------------------------------
suppressPackageStartupMessages({
  library(tidyverse)
  library(scales)
  library(tinytable)
})

# 2. Definição da Paleta de Cores (Padrão Storytelling with Data - SWD) ---------
# Uso deliberado de cor com foco pré-atentivo:
# Destaque em tom de azul institucional para a categoria focal (Feminino)
# Tons neutros de cinza para contexto (Masculino e elementos visuais de apoio)
COR_DESTAQUE <- "#174A7E"  # Azul escuro institucional (Feminino)
COR_CONTEXTO <- "#929497"  # Cinza neutro (Masculino)
COR_TEXTO    <- "#231F20"  # Cinza escuro / preto suave para títulos
COR_SUBTEXTO <- "#555655"  # Cinza médio para subtítulos e anotações
COR_GRID     <- "#E5E5E5"  # Cinza claro para linhas de referência

# 3. Importação da Base Consolidada --------------------------------------------
caminho_csv <- "/home/nando/Documentos/gemini/ATUAL/OUTPUT/PPE_beneficiarios_2025_2026.csv"
df <- read_csv(caminho_csv, show_col_types = FALSE)

# 4. Preparação e Agregação dos Dados ------------------------------------------
# Filtro para as categorias binárias principais de gênero (q24) e garantia do fator AnoCol
df_genero <- df |>
  filter(q24 %in% c("Feminino", "Masculino")) |>
  mutate(
    AnoCol = factor(AnoCol, levels = c("2025", "2026")),
    q24 = factor(q24, levels = c("Feminino", "Masculino"))
  )

# Tabela resumida com totais anuais, frequências e percentuais
df_resumo <- df_genero |>
  count(AnoCol, q24, name = "Respondentes") |>
  group_by(AnoCol) |>
  mutate(
    Total_Ano = sum(Respondentes),
    Percentual = Respondentes / Total_Ano,
    Rotulo_Curto = paste0(
      format(Respondentes, big.mark = ".", decimal.mark = ","),
      "\n(", percent(Percentual, accuracy = 0.1, decimal.mark = ","), ")"
    ),
    Cor_Barra = if_else(q24 == "Feminino", COR_DESTAQUE, COR_CONTEXTO)
  ) |>
  ungroup()

# Estruturação da Tabela Acadêmica Comparativa (Padrão Booktabs / tinytable)
# Colunas: Gênero, 2025 (N), 2025 (%), 2026 (N), 2026 (%), Variação (p.p.)
n_fem_25 <- df_resumo$Respondentes[df_resumo$AnoCol == "2025" & df_resumo$q24 == "Feminino"]
n_mas_25 <- df_resumo$Respondentes[df_resumo$AnoCol == "2025" & df_resumo$q24 == "Masculino"]
tot_25   <- sum(df_resumo$Respondentes[df_resumo$AnoCol == "2025"])

n_fem_26 <- df_resumo$Respondentes[df_resumo$AnoCol == "2026" & df_resumo$q24 == "Feminino"]
n_mas_26 <- df_resumo$Respondentes[df_resumo$AnoCol == "2026" & df_resumo$q24 == "Masculino"]
tot_26   <- sum(df_resumo$Respondentes[df_resumo$AnoCol == "2026"])

pct_fem_25 <- (n_fem_25 / tot_25) * 100
pct_mas_25 <- (n_mas_25 / tot_25) * 100

pct_fem_26 <- (n_fem_26 / tot_26) * 100
pct_mas_26 <- (n_mas_26 / tot_26) * 100

var_fem_pp <- pct_fem_26 - pct_fem_25
var_mas_pp <- pct_mas_26 - pct_mas_25

tabela_genero_dados <- tibble(
  `Gênero` = c("Feminino", "Masculino", "Total"),
  `2025 (N)` = c(format(n_fem_25, big.mark = ".", decimal.mark = ","),
                 format(n_mas_25, big.mark = ".", decimal.mark = ","),
                 format(tot_25, big.mark = ".", decimal.mark = ",")),
  `2025 (%)` = c(sub("\\.", ",", sprintf("%.1f%%", pct_fem_25)),
                 sub("\\.", ",", sprintf("%.1f%%", pct_mas_25)),
                 "100,0%"),
  `2026 (N)` = c(format(n_fem_26, big.mark = ".", decimal.mark = ","),
                 format(n_mas_26, big.mark = ".", decimal.mark = ","),
                 format(tot_26, big.mark = ".", decimal.mark = ",")),
  `2026 (%)` = c(sub("\\.", ",", sprintf("%.1f%%", pct_fem_26)),
                 sub("\\.", ",", sprintf("%.1f%%", pct_mas_26)),
                 "100,0%"),
  `Variação (p.p.)` = c(sub("\\.", ",", sprintf("%+.1f", var_fem_pp)),
                        sub("\\.", ",", sprintf("%+.1f", var_mas_pp)),
                        "—")
)

# Renderizador de tabela acadêmica tinytable (sem linhas verticais, com linhas horizontais limpas)
t24_genero <- tt(tabela_genero_dados) |>
  format_tt(escape = TRUE)

# 5. Construção do Gráfico com Storytelling (Barras Agrupadas com Destaque) -----
# Princípios SWD aplicados:
# - Remoção de poluição visual (grid vertical, bordas, linhas pesadas);
# - Rótulos diretos sobre as barras (dispensa legenda convencional e reduz carga cognitiva);
# - Alinhamento à esquerda de título e subtítulo explicativo com a conclusão-chave;
# - Anotação contextual destacando o avanço na inclusão feminina.

g_comparativo <- ggplot(df_resumo, aes(x = AnoCol, y = Respondentes, fill = q24)) +
  # Barras emparelhadas com largura adequada para respiração visual
  geom_col(
    position = position_dodge(width = 0.72),
    width = 0.62,
    alpha = 0.95
  ) +
  # Rótulos com quantitativo e percentual diretamente nas barras
  geom_text(
    aes(label = Rotulo_Curto, color = q24),
    position = position_dodge(width = 0.72),
    vjust = -0.35,
    size = 3.6,
    fontface = "bold",
    lineheight = 0.9
  ) +
  # Mapeamento manual de cores com foco na categoria feminina
  scale_fill_manual(values = c("Feminino" = COR_DESTAQUE, "Masculino" = COR_CONTEXTO)) +
  scale_color_manual(values = c("Feminino" = COR_DESTAQUE, "Masculino" = COR_CONTEXTO)) +
  # Expansão do eixo vertical para acomodar os rótulos de dados
  scale_y_continuous(
    limits = c(0, 850),
    breaks = seq(0, 800, by = 200),
    labels = label_number(big.mark = ".", decimal.mark = ","),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    title = "Avaliação PPE: Distribuição de Respondentes por Gênero",
    subtitle = "Comparação do quantitativo absoluto e proporção entre as coletas de 2025 e 2026\n(Azul: Feminino | Cinza: Masculino)",
    x = "Ano da Coleta da Pesquisa",
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

# 6. Objeto do Gráfico ---------------------------------------------------------
# O objeto g_comparativo fica armazenado no ambiente para chamada direta pelo chunk Quarto
