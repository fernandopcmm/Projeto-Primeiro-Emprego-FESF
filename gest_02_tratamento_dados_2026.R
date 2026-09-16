# ==============================================================================
# Projeto Primeiro Emprego (PPE) - Avaliação dos Pontos Focais (Gestores)
# Script: gest_02_tratamento_dados_2026.R
# ==============================================================================
# Autoria: Fernando Antonio de Melo Pereira Lhamas (UFRN / NPGA-UFBA)
# Finalidade: Higienizar, tipificar e harmonizar os fatores ordinais e nominais
#             das 20 questões da base de gestores/pontos focais (2025 vs 2026),
#             gerando a base analítica consolidada.
# ==============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
})

caminho_csv_entrada <- "/home/nando/Documentos/gemini/ATUAL/OUTPUT/gest_PPE_pontos_focais_2025_2026.csv"
caminho_csv_saida   <- "/home/nando/Documentos/gemini/ATUAL/OUTPUT/gest_PPE_tratado_2025_2026.csv"

df <- read_csv(caminho_csv_entrada, show_col_types = FALSE)

df_tratado <- df |>
  mutate(
    # Metadados
    AnoCol = factor(AnoCol, levels = c("2025", "2026")),
    
    # Bloco 1: Percepção Geral e Papel do Gestor
    q1_num = as.numeric(q1),
    q1_ord = factor(q1, levels = 1:5, ordered = TRUE),
    q1_sentimento = factor(case_when(
      q1 %in% c(1, 2) ~ "Negativo (1-2)",
      q1 == 3         ~ "Neutro (3)",
      q1 %in% c(4, 5) ~ "Positivo (4-5)"
    ), levels = c("Negativo (1-2)", "Neutro (3)", "Positivo (4-5)")),
    
    q2_cat = factor(case_when(
      grepl("Orienta", q2, ignore.case = TRUE) ~ "Orientação e supervisão direta",
      grepl("desempenho e frequ", q2, ignore.case = TRUE) ~ "Acompanhamento de desempenho e frequência",
      grepl("Media", q2, ignore.case = TRUE) ~ "Mediação institucional",
      TRUE ~ "Outras atribuições"
    ), levels = c(
      "Orientação e supervisão direta",
      "Acompanhamento de desempenho e frequência",
      "Mediação institucional",
      "Outras atribuições"
    )),
    
    q3_num = as.numeric(q3),
    q3_ord = factor(q3, levels = 1:5, ordered = TRUE),
    q3_nivel = factor(case_when(
      q3 %in% c(1, 2) ~ "Baixo desafio (1-2)",
      q3 == 3         ~ "Desafio moderado (3)",
      q3 %in% c(4, 5) ~ "Alto desafio (4-5)"
    ), levels = c("Baixo desafio (1-2)", "Desafio moderado (3)", "Alto desafio (4-5)")),
    
    # Textos abertos dos desafios e facilidades
    q4_texto = str_squish(q4),
    q5_texto = str_squish(q5),
    
    # Bloco 2: Acolhimento e Integração Organizacional
    q6_num = as.numeric(q6),
    q6_ord = factor(q6, levels = 1:5, ordered = TRUE),
    
    q7_cat = factor(case_when(
      grepl("^Sim", q7, ignore.case = TRUE) ~ "Sim (Possui protocolos)",
      grepl("^N[ãa]o$", q7, ignore.case = TRUE) ~ "Não (Não possui)",
      TRUE ~ "Não tenho certeza / Talvez"
    ), levels = c("Sim (Possui protocolos)", "Não (Não possui)", "Não tenho certeza / Talvez")),
    
    q8_ord = factor(case_when(
      grepl("fundamental", q8, ignore.case = TRUE) ~ "É fundamental",
      grepl("significativamente", q8, ignore.case = TRUE) ~ "Contribui significativamente",
      TRUE ~ "Contribui em alguma medida"
    ), levels = c("É fundamental", "Contribui significativamente", "Contribui em alguma medida"), ordered = TRUE),
    
    q9_cat = factor(case_when(
      grepl("servidor mais experiente|profissional", q9, ignore.case = TRUE) ~ "Acompanhamento por servidor mais experiente",
      grepl("Apresentação formal", q9, ignore.case = TRUE) ~ "Apresentação formal à equipe",
      TRUE ~ "Múltiplas formas / Outras práticas"
    ), levels = c(
      "Acompanhamento por servidor mais experiente",
      "Apresentação formal à equipe",
      "Múltiplas formas / Outras práticas"
    )),
    
    # Bloco 3: Comunicação e Supervisão de Desempenho
    q10_num = as.numeric(q10),
    q10_ord = factor(q10, levels = 1:5, ordered = TRUE),
    
    q11_cat = factor(case_when(
      grepl("whatsapp", q11, ignore.case = TRUE) & !grepl("e-mail", q11, ignore.case = TRUE) ~ "WhatsApp exclusivo",
      grepl("e-mail", q11, ignore.case = TRUE) & !grepl("whatsapp", q11, ignore.case = TRUE) ~ "E-mail exclusivo",
      grepl("whatsapp", q11, ignore.case = TRUE) & grepl("e-mail", q11, ignore.case = TRUE) ~ "WhatsApp e E-mail integrados",
      grepl("telefone", q11, ignore.case = TRUE) ~ "Telefone / Contato direto",
      TRUE ~ "Outros canais"
    ), levels = c("WhatsApp exclusivo", "E-mail exclusivo", "WhatsApp e E-mail integrados", "Telefone / Contato direto", "Outros canais")),
    
    q12_cat = factor(case_when(
      grepl("Diariamente", q12, ignore.case = TRUE) ~ "Diariamente",
      grepl("Semanalmente", q12, ignore.case = TRUE) ~ "Semanalmente",
      grepl("Quinzenalmente|Mensalmente", q12, ignore.case = TRUE) ~ "Quinzenalmente ou Mensalmente",
      TRUE ~ "Raramente / Eventual"
    ), levels = c("Diariamente", "Semanalmente", "Quinzenalmente ou Mensalmente", "Raramente / Eventual")),
    
    q13_num = as.numeric(q13),
    q13_ord = factor(q13, levels = 1:5, ordered = TRUE),
    
    # Bloco 4: Perfil Comportamental e Desempenho dos Jovens (Likert 1-5)
    q14_num = as.numeric(q14),
    q14_ord = factor(q14, levels = 1:5, ordered = TRUE),
    
    q15_num = as.numeric(q15),
    q15_ord = factor(q15, levels = 1:5, ordered = TRUE),
    
    q16_num = as.numeric(q16),
    q16_ord = factor(q16, levels = 1:5, ordered = TRUE),
    
    q17_num = as.numeric(q17),
    q17_ord = factor(q17, levels = 1:5, ordered = TRUE),
    
    q18_num = as.numeric(q18),
    q18_ord = factor(q18, levels = 1:5, ordered = TRUE),
    
    # Bloco 5: Sugestões e Inconformidades (Textual)
    q19_texto = str_squish(q19),
    q20_texto = str_squish(q20)
  )

write_csv(df_tratado, caminho_csv_saida)
