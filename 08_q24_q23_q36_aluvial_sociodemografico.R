# ==============================================================================
# Projeto Primeiro Emprego (PPE) - Análise Comparativa Longitudinal (2025-2026)
# Script 08: 08_q24_q23_q36_aluvial_sociodemografico.R
# Diagrama Aluvial Interseccional: Gênero x Faixa Etária x Raça/Cor
# Padrão: Storytelling with Data (Cole Nussbaumer Knaflic) e REGRAS_SCRIPTS_R.md
# ==============================================================================
# Autoria: Coordenação de Pesquisa e Avaliação de Políticas Públicas
# Data: 2026-09-15
# Ambiente: R / ggplot2 / ggalluvial / tidyverse / tinytable
# ==============================================================================

# 1. Carregamento de Bibliotecas -----------------------------------------------
suppressPackageStartupMessages({
  library(tidyverse)
  library(ggalluvial)
  library(scales)
  library(tinytable)
})

# 2. Definição da Paleta de Cores (Padrão Storytelling with Data - SWD) ---------
COR_DESTAQUE <- "#174A7E"  # Azul escuro institucional (Feminino)
COR_CONTEXTO <- "#929497"  # Cinza neutro (Masculino)
COR_TEXTO    <- "#231F20"  # Grafite escuro para títulos e rótulos
COR_SUBTEXTO <- "#555655"  # Cinza médio para subtítulos e anotações
COR_STRATUM  <- "#F4F5F7"  # Fundo sutil dos blocos/estratos
COR_BORDA    <- "#BDC3C7"  # Borda suave dos blocos

# 3. Importação e Tratamento Integrado dos Dados --------------------------------
caminho_csv <- "/home/nando/Documentos/gemini/ATUAL/OUTPUT/PPE_beneficiarios_2025_2026.csv"
df <- read_csv(caminho_csv, show_col_types = FALSE)

# Harmonização de Gênero (q24), Idade/Faixa Etária (q23) e Raça/Cor (q36)
df_aluvial_prep <- df |>
  filter(q24 %in% c("Feminino", "Masculino")) |>
  mutate(
    AnoCol = factor(AnoCol, levels = c("2025", "2026")),
    
    # Tratamento higienizado de idade
    q23_num = case_when(
      AnoCol == "2026" & q23 == "01/10/1977" ~ 2026 - 1977,
      AnoCol == "2026" & q23 == "1976.0" ~ 2026 - 1976,
      AnoCol == "2026" & q23 == "5.008031976E9" ~ 2026 - 1976,
      AnoCol == "2026" & q23 == "1.624924565E9" ~ 2026 - 1999,
      AnoCol == "2026" & q23 == "3.3" ~ 2026 - 1994,
      grepl("^[0-9]{2}", q23) ~ as.numeric(str_extract(q23, "^[0-9]{2}")),
      TRUE ~ suppressWarnings(as.numeric(q23))
    ),
    
    # Categorização etária
    faixa_etaria = case_when(
      q23_num >= 16 & q23_num <= 21 ~ "16 a 21",
      q23_num >= 22 & q23_num <= 25 ~ "22 a 25",
      q23_num >= 26 & q23_num <= 35 ~ "26 a 35",
      q23_num > 35 ~ "Acima de 35",
      TRUE ~ NA_character_
    ),
    
    # Harmonização de raça/cor conforme requisitos
    raca_cor = case_when(
      q36 == "Pardo(a)" ~ "Pardo(a)",
      q36 == "Preto(a)" ~ "Preto(a)",
      q36 == "Branco(a)" ~ "Branco(a)",
      !is.na(q36) ~ "Outros",
      TRUE ~ NA_character_
    ),
    
    Sexo = factor(q24, levels = c("Feminino", "Masculino")),
    `Faixa Etária` = factor(faixa_etaria, levels = c("16 a 21", "22 a 25", "26 a 35", "Acima de 35")),
    `Raça/Cor` = factor(raca_cor, levels = c("Pardo(a)", "Preto(a)", "Branco(a)", "Outros"))
  ) |>
  filter(!is.na(`Faixa Etária`), !is.na(`Raça/Cor`))

# Agregação de fluxos por ano
df_fluxos <- df_aluvial_prep |>
  count(AnoCol, Sexo, `Faixa Etária`, `Raça/Cor`, name = "Freq")

# 4. Estruturação da Tabela Acadêmica de Perfis (t08_aluvial) -------------------
# Identificação e consolidação longitudinal dos 10 principais fluxos interseccionais
n_tot_2025 <- sum(df_fluxos$Freq[df_fluxos$AnoCol == "2025"])
n_tot_2026 <- sum(df_fluxos$Freq[df_fluxos$AnoCol == "2026"])

tabela_fluxos_dados <- df_fluxos |>
  pivot_wider(names_from = AnoCol, values_from = Freq, values_fill = 0) |>
  mutate(
    Total_Geral = `2025` + `2026`,
    pct_25 = `2025` / n_tot_2025,
    pct_26 = `2026` / n_tot_2026,
    var_pp = (pct_26 - pct_25) * 100
  ) |>
  arrange(desc(Total_Geral)) |>
  slice_head(n = 10) |>
  mutate(
    `Sexo` = as.character(Sexo),
    `Faixa Etária` = as.character(`Faixa Etária`),
    `Raça/Cor` = as.character(`Raça/Cor`),
    `2025 (N)` = format(`2025`, big.mark = ".", decimal.mark = ","),
    `2025 (%)` = percent(pct_25, accuracy = 0.1, decimal.mark = ","),
    `2026 (N)` = format(`2026`, big.mark = ".", decimal.mark = ","),
    `2026 (%)` = percent(pct_26, accuracy = 0.1, decimal.mark = ","),
    `Variação (p.p.)` = sub("\\.", ",", sprintf("%+.1f", var_pp))
  ) |>
  select(
    `Sexo`, `Faixa Etária`, `Raça/Cor`,
    `2025 (N)`, `2025 (%)`,
    `2026 (N)`, `2026 (%)`,
    `Variação (p.p.)`
  )

# Totalização de referência dos respondentes válidos
linha_total_aluvial <- tibble(
  `Sexo` = "Total Amostral Válido",
  `Faixa Etária` = "—",
  `Raça/Cor` = "—",
  `2025 (N)` = format(n_tot_2025, big.mark = ".", decimal.mark = ","),
  `2025 (%)` = "100,0%",
  `2026 (N)` = format(n_tot_2026, big.mark = ".", decimal.mark = ","),
  `2026 (%)` = "100,0%",
  `Variação (p.p.)` = "—"
)

t08_aluvial <- bind_rows(tabela_fluxos_dados, linha_total_aluvial) |>
  tt() |>
  format_tt(escape = TRUE)

# 5. Construção do Gráfico Aluvial com Storytelling (g_aluvial_sociodemografico)
# Composição com ggalluvial:
# - axis1: Sexo
# - axis2: Faixa Etária
# - axis3: Raça/Cor
# - facet_wrap(~ AnoCol, scales = "free_y")
# - Destaque cromático por Sexo (Feminino: Azul Institucional, Masculino: Cinza)
g_aluvial_sociodemografico <- ggplot(
  df_fluxos,
  aes(y = Freq, axis1 = Sexo, axis2 = `Faixa Etária`, axis3 = `Raça/Cor`)
) +
  # Linhas de fluxo com curvatura e transparência ideais para redução de ruído visual
  geom_alluvium(
    aes(fill = Sexo),
    width = 1/8,
    alpha = 0.72,
    knot.pos = 0.4,
    color = "white",
    linewidth = 0.2
  ) +
  # Estratos de nós com acabamento limpo
  geom_stratum(
    width = 1/5,
    fill = COR_STRATUM,
    color = COR_BORDA,
    linewidth = 0.4
  ) +
  # Rótulos dos nós com tipografia legível
  geom_text(
    stat = "stratum",
    aes(label = after_stat(stratum)),
    size = 3.2,
    fontface = "bold",
    color = COR_TEXTO
  ) +
  # Configuração dos eixos categóricos
  scale_x_discrete(
    limits = c("Gênero", "Faixa Etária", "Raça/Cor"),
    expand = c(0.12, 0.05)
  ) +
  scale_y_continuous(
    labels = label_number(big.mark = ".", decimal.mark = ","),
    expand = expansion(mult = c(0.01, 0.05))
  ) +
  # Destaque pré-atentivo institucional
  scale_fill_manual(
    name = "Gênero",
    values = c("Feminino" = COR_DESTAQUE, "Masculino" = COR_CONTEXTO)
  ) +
  # Comparação longitudinal lado a lado
  facet_wrap(~ AnoCol, scales = "free_y") +
  labs(
    title = "Avaliação PPE: Fluxo Interseccional Sociodemográfico dos Beneficiários",
    subtitle = "Mapeamento comparativo entre os ciclos de 2025 e 2026: Gênero → Faixa Etária → Raça/Cor\n(Azul: Feminino | Cinza: Masculino)",
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
    strip.text = element_text(size = 11, face = "bold", color = COR_TEXTO, margin = margin(b = 8)),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "#EBEBEB", linewidth = 0.35),
    axis.text.x = element_text(size = 10.5, face = "bold", color = COR_TEXTO, margin = margin(t = 6)),
    axis.text.y = element_text(size = 9, color = COR_SUBTEXTO),
    axis.title.y = element_text(size = 10, color = COR_SUBTEXTO, margin = margin(r = 10)),
    plot.margin = margin(t = 12, r = 16, b = 12, l = 16)
  )
