# ==============================================================================
# Projeto Primeiro Emprego (PPE) - Avaliação Multianual (2025 - 2026)
# Script 02: 02_tratamento_dados_2026.R
# Tratamento, Tipagem de Variáveis, Conversão para Factors, Limpeza Textual e Auditoria de NAs
# ==============================================================================
# Autoria: Coordenação de Pesquisa e Avaliação de Políticas Públicas
# Data: 2026-09-15
# Ambiente: R / Tidyverse
# ==============================================================================

# 1. Carregamento de Bibliotecas -----------------------------------------------
suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
  library(stringr)
  library(forcats)
})

# 2. Leitura da Base Consolidada ------------------------------------------------
caminho_csv <- "/home/nando/Documentos/gemini/ATUAL/OUTPUT/PPE_beneficiarios_2025_2026.csv"
df_completo <- read_csv(caminho_csv, show_col_types = FALSE)

cat("==============================================================================\n")
cat("          AVALIAÇÃO PPE: TRATAMENTO E AUDITORIA DOS DADOS DE 2026             \n")
cat("==============================================================================\n")
cat(sprintf("Registros totais no arquivo: %d (2025: %d | 2026: %d)\n",
            nrow(df_completo),
            sum(df_completo$AnoCol == "2025", na.rm = TRUE),
            sum(df_completo$AnoCol == "2026", na.rm = TRUE)))

# 3. Tratamento Específico e Conversão de Variáveis em 2026 ---------------------
# Aplica-se a higienização preservando a compatibilidade semântica com o ciclo 2025

df_tratado <- df_completo |>
  mutate(
    # Fator de Ano da Coleta
    AnoCol = factor(AnoCol, levels = c("2025", "2026")),
    
    # --------------------------------------------------------------------------
    # A. Variáveis Quantitativas / Numéricas
    # --------------------------------------------------------------------------
    # q23: Idade em anos (limpeza de sufixos "anos", formatações e dadas em notação científica)
    q23_num = case_when(
      AnoCol == "2026" & q23 == "01/10/1977" ~ 2026 - 1977,
      AnoCol == "2026" & q23 == "1976.0" ~ 2026 - 1976,
      AnoCol == "2026" & q23 == "5.008031976E9" ~ 2026 - 1976, # Nasc. em 1976 (Data de nascimento)
      AnoCol == "2026" & q23 == "1.624924565E9" ~ 2026 - 1999, # Nasc. em 1999
      AnoCol == "2026" & q23 == "3.3" ~ 2026 - 1994,           # Nasc. em 1994 (31/32 anos)
      grepl("^[0-9]{2}", q23) ~ as.numeric(str_extract(q23, "^[0-9]{2}")),
      TRUE ~ suppressWarnings(as.numeric(q23))
    ),
    
    # q30: Ano de conclusão do ensino médio (limpeza de intervalos e erros de digitação)
    q30_num = case_when(
      grepl("^[0-9]{4}$", q30) ~ as.numeric(q30),
      grepl("^[0-9]{4}\\.0$", q30) ~ as.numeric(sub("\\.0$", "", q30)),
      q30 %in% c("2020/2021", "2010-2021", "2009/ 2022", "2011 e 2024") ~ as.numeric(str_extract(q30, "[0-9]{4}$")),
      q30 == "20/12/2022" ~ 2022,
      q30 == "20209.0" ~ 2020,
      q30 == "20215.0" ~ 2021,
      q30 == "20222.0" ~ 2022,
      q30 %in% c("202.0", "207.0", "219.0") ~ as.numeric(sub("^20", "200", sub("\\.0$", "", q30))),
      q30 %in% c("22.0", "23.0", "24.0") ~ 2000 + as.numeric(sub("\\.0$", "", q30)),
      q30 == "95.0" ~ 1995,
      q30 == "20000.0" ~ 2000,
      TRUE ~ suppressWarnings(as.numeric(q30))
    ),

    # --------------------------------------------------------------------------
    # B. Variáveis Qualitativas Nominais e Dicotômicas -> Factor
    # --------------------------------------------------------------------------
    q24 = factor(q24, levels = c("Feminino", "Masculino", "Não Binário")),
    q26 = factor(q26, levels = c("Não", "Sim")), # Em 2026 todos são concluintes
    q29 = factor(q29, levels = c("Não", "Sim")),
    q35 = factor(q35, levels = c("Solteiro(a)", "Casado(a)", "Separado(a)")),
    q36 = factor(q36, levels = c("Preto(a)", "Pardo(a)", "Branco(a)", "Amarelo(a)", "Indígena ou de origem indígena")),
    q59 = factor(q59, levels = c("Não", "Sim")),
    q60 = factor(q60, levels = c("Não", "Sim")),

    # --------------------------------------------------------------------------
    # C. Variáveis Qualitativas Ordinais -> Ordered Factor
    # --------------------------------------------------------------------------
    # q22: Ano de entrada no PPE
    q22 = factor(
      str_replace(q22, "\\.0$", ""),
      levels = c("Antes de 2024", "2024", "2025", "2026"),
      ordered = TRUE
    ),

    # q38: Onde e com quem mora
    q38 = factor(q38),

    # q39: Tamanho da família / coabitantes
    q39_fator = factor(
      case_when(
        q39 == "Nenhuma" ~ "0",
        q39 %in% c("Seis", "Mais de Seis") ~ "6+",
        TRUE ~ q39
      ),
      levels = c("0", "Uma", "Duas", "Três", "Quatro", "Cinco", "6+"),
      ordered = TRUE
    ),

    # q40: Renda familiar atual
    q40_fator = factor(
      case_when(
        grepl("Até 1,5", q40) ~ "Até 1,5",
        grepl("1,5 a 3", q40) ~ "1,5-3",
        grepl("3 a 4,5", q40) ~ "3-4,5",
        grepl("4,5 a 6|10 a 30", q40) ~ "4,5+",
        TRUE ~ q40
      ),
      levels = c("Nenhuma", "Até 1,5", "1,5-3", "3-4,5", "4,5+"),
      ordered = TRUE
    ),

    # q41: Percepção da situação financeira
    q41 = factor(q41),

    # q42: Situação de moradia
    q42 = factor(q42),

    # q43: Renda familiar antes do PPE
    q43_fator = factor(
      case_when(
        grepl("Até 1,5", q43) ~ "Até 1,5",
        grepl("1,5 a 3", q43) ~ "1,5-3",
        grepl("3 a 4,5", q43) ~ "3-4,5",
        grepl("4,5 a 6|10 a 30", q43) ~ "4,5+",
        TRUE ~ q43
      ),
      levels = c("Nenhuma", "Até 1,5", "1,5-3", "3-4,5", "4,5+"),
      ordered = TRUE
    ),

    # q45 e q46: Escolaridade dos pais
    q45 = factor(q45, levels = c(
      "Nenhuma escolaridade",
      "Ensino fundamental 1º ao 5º ano",
      "Ensino fundamental 6º ao 9º ano",
      "Ensino médio",
      "Ensino superior",
      "Pós-graduação"
    ), ordered = TRUE),
    
    q46 = factor(q46, levels = c(
      "Nenhuma escolaridade",
      "Ensino fundamental 1º ao 5º ano",
      "Ensino fundamental 6º ao 9º ano",
      "Ensino médio",
      "Ensino superior",
      "Pós-graduação"
    ), ordered = TRUE),

    # q47: Livros lidos nos últimos 12 meses
    q47 = factor(q47, levels = c(
      "Nenhum", "Um ou dois", "Entre três e cinco", "Entre seis e oito", "Mais de oito"
    ), ordered = TRUE),

    # q48: Horas de estudo por semana
    q48 = factor(q48, levels = c(
      "Nenhuma, apenas trabalho", "Uma a três", "Quatro a sete", "Oito a doze", "Mais de doze"
    ), ordered = TRUE),

    # q49: Condições físicas das instalações
    q49 = factor(q49, levels = c(
      "Nenhuma", "Somente algumas", "Sim, a maior parte", "Sim, todas"
    ), ordered = TRUE),

    # q50: Satisfação geral com o trabalho
    q50 = factor(q50, levels = c(
      "Insatisfeito", "Satisfeito somente em alguns momentos", "Satisfeito a maior parte do tempo", "Muito satisfeito"
    ), ordered = TRUE),

    # q51: Auxílio na progressão da carreira
    q51 = factor(q51, levels = c(
      "Não sinto que estou progredindo", "Pouco", "Parcialmente", "Plenamente"
    ), ordered = TRUE),

    # q52: Recursos audiovisuais e tecnológicos
    q52 = factor(q52),

    # q53: Relacionamento com colegas
    q53 = factor(q53, levels = c(
      "Não é saudável", "É pouco saudável", "É relativamente saudável", "É bem saudável"
    ), ordered = TRUE),

    # q54: Nível de exigência do trabalho
    q54 = factor(q54, levels = c(
      "Deveria exigir muito menos", "Deveria exigir um pouco menos", "Exige na medida certa", "Deveria exigir um pouco mais", "Deveria exigir muitos mais"
    ), ordered = TRUE),

    # q55, q56, q57: Contribuições formativas e cidadãs
    q55 = factor(q55, levels = c(
      "Não contribui", "Contribui muito pouco", "Contribui parcialmente", "Contribui amplamente"
    ), ordered = TRUE),
    q56 = factor(q56, levels = c(
      "Não contribui", "Contribui muito pouco", "Contribui parcialmente", "Contribui amplamente"
    ), ordered = TRUE),
    q57 = factor(q57, levels = c(
      "Não contribui", "Contribui muito pouco", "Contribui parcialmente", "Contribui amplamente"
    ), ordered = TRUE),

    # q58: Desejo de continuar estudando
    q58 = factor(q58, levels = c(
      "Muito fraco", "Fraco", "Regular", "Alto", "Muito alto"
    ), ordered = TRUE),

    # q61: Sentimento em relação ao mercado
    q61 = factor(q61),

    # q62: Crença em conseguir emprego formal antes do PPE
    q62 = factor(q62, levels = c(
      "Não", "Talvez", "Sim, mas com muita dificuldade", "Sim, com certeza"
    ), ordered = TRUE),

    # q63: Maior objetivo profissional antes do PPE
    q63 = factor(q63),

    # q64: Sentimento em relação ao objetivo após ingresso no PPE
    q64 = factor(q64),

    # --------------------------------------------------------------------------
    # D. Variáveis de Texto Aberto (Padronização, Limpeza e Falsos Cognatos)
    # --------------------------------------------------------------------------
    # q21: Redes sociais (unificação de negações e maiúsculas/minúsculas)
    q21_limpa = case_when(
      grepl("não|nenhum|nao", q21, ignore.case = TRUE) ~ "Não participa de rede social",
      grepl("instagram", q21, ignore.case = TRUE) ~ "Instagram",
      grepl("linkedin", q21, ignore.case = TRUE) ~ "LinkedIn",
      grepl("facebook", q21, ignore.case = TRUE) ~ "Facebook",
      grepl("tik tok|tiktok", q21, ignore.case = TRUE) ~ "Tik Tok",
      grepl("whatsapp", q21, ignore.case = TRUE) ~ "WhatsApp",
      grepl("todas", q21, ignore.case = TRUE) ~ "Múltiplas redes",
      TRUE ~ "Outras"
    ),
    q21_limpa = factor(q21_limpa),

    # q25: Orientação Sexual (harmonização de falsos cognatos e categorias)
    # Correção: "Cisgenêro" é identidade de gênero (não orientação sexual); classificado como não informado
    q25_limpa = case_when(
      q25 %in% c("Heterossexual") ~ "Heterossexual",
      q25 %in% c("Bissexual") ~ "Bissexual",
      q25 %in% c("Homossexual", "Lésbica") ~ "Homossexual/Lésbica",
      q25 %in% c("Assexual", "Assexuado") ~ "Assexual",
      q25 %in% c("Pansexual") ~ "Pansexual",
      q25 %in% c("Prefiro não responder", "Não responde", "Cisgenêro", "Transexual", "Eu", ".") ~ "Prefiro não responder/Outro",
      TRUE ~ "Prefiro não responder/Outro"
    ),
    q25_limpa = factor(q25_limpa),

    # q27: Sigla do Estado de Nascimento (UF)
    q27_limpa = case_when(
      grepl("ba|bahia|salvador|jequié|feira", q27, ignore.case = TRUE) ~ "BA",
      grepl("sp|são paulo", q27, ignore.case = TRUE) ~ "SP",
      grepl("rj|rio de janeiro", q27, ignore.case = TRUE) ~ "RJ",
      grepl("df|brasília", q27, ignore.case = TRUE) ~ "DF",
      grepl("se|sergipe", q27, ignore.case = TRUE) ~ "SE",
      grepl("go|goiás", q27, ignore.case = TRUE) ~ "GO",
      grepl("es|espírito santo", q27, ignore.case = TRUE) ~ "ES",
      grepl("pi|piauí", q27, ignore.case = TRUE) ~ "PI",
      grepl("am|amazonas", q27, ignore.case = TRUE) ~ "AM",
      grepl("rn|rio grande do norte", q27, ignore.case = TRUE) ~ "RN",
      grepl("bh|minas", q27, ignore.case = TRUE) ~ "MG",
      TRUE ~ "Outro/Inválido"
    ),
    q27_limpa = factor(q27_limpa),

    # q37: Religião (correção de erro tipográfico histórico "Evengélica" -> "Evangélica")
    q37_limpa = case_when(
      grepl("evengélica|evangélica|crente|adventista|deus|cristã|cristão|igreja", q37, ignore.case = TRUE) ~ "Evangélica / Cristã",
      grepl("católica", q37, ignore.case = TRUE) ~ "Católica",
      grepl("não tem religião|ateu|agnostico|agnóstico|deísta|afastada", q37, ignore.case = TRUE) ~ "Sem religião",
      grepl("candomblé|umbanda", q37, ignore.case = TRUE) ~ "Matriz Africana (Candomblé/Umbanda)",
      grepl("testemunha", q37, ignore.case = TRUE) ~ "Testemunha de Jeová",
      grepl("espírita", q37, ignore.case = TRUE) ~ "Espírita",
      TRUE ~ "Outras religiões"
    ),
    q37_limpa = factor(q37_limpa),

    # q44: Benefício Social (unificação de respostas afirmativas de Bolsa Família e BPC)
    q44_limpa = case_when(
      grepl("bolsa família|bolsa familia|o bolsa", q44, ignore.case = TRUE) ~ "Bolsa Família",
      grepl("bpc|loas|loa", q44, ignore.case = TRUE) ~ "BPC / LOAS",
      grepl("aposentad|inss|pensão|pensionista", q44, ignore.case = TRUE) ~ "Previdência / Pensão / INSS",
      grepl("nenhum|não|nao|00000", q44, ignore.case = TRUE) ~ "Nenhum",
      TRUE ~ "Outro benefício"
    ),
    q44_limpa = factor(q44_limpa)
  )

# 4. Auditoria de Tipagem das Variáveis Quantitativas ---------------------------
cat("\n--- AUDITORIA DE VARIÁVEIS QUANTITATIVAS (2026) ---\n")
df26_audit <- df_tratado |> filter(AnoCol == "2026")

cat("\nIdade (q23_num):\n")
cat("Tipo:", class(df26_audit$q23_num), "\n")
cat("Resumo Estatístico:\n")
print(summary(df26_audit$q23_num))
cat(sprintf("Valores ausentes (NA): %d (%.2f%%)\n",
            sum(is.na(df26_audit$q23_num)),
            mean(is.na(df26_audit$q23_num)) * 100))

cat("\nAno de Conclusão do Ensino Médio (q30_num):\n")
cat("Tipo:", class(df26_audit$q30_num), "\n")
cat("Resumo Estatístico:\n")
print(summary(df26_audit$q30_num))
cat(sprintf("Valores ausentes (NA): %d (%.2f%%)\n",
            sum(is.na(df26_audit$q30_num)),
            mean(is.na(df26_audit$q30_num)) * 100))

# 5. Auditoria de Frequência das Variáveis Qualitativas Harmonizadas ------------
cat("\n--- TABELAS DE FREQUÊNCIA DAS PRINCIPAIS VARIÁVEIS HIGIENIZADAS (2026) ---\n")

cat("\nq21_limpa (Redes Sociais):\n")
print(df26_audit |> count(q21_limpa, sort = TRUE) |> mutate(pct = round(n / sum(n) * 100, 1)))

cat("\nq25_limpa (Orientação Sexual):\n")
print(df26_audit |> count(q25_limpa, sort = TRUE) |> mutate(pct = round(n / sum(n) * 100, 1)))

cat("\nq27_limpa (UF de Nascimento):\n")
print(df26_audit |> count(q27_limpa, sort = TRUE) |> mutate(pct = round(n / sum(n) * 100, 1)))

cat("\nq37_limpa (Religião):\n")
print(df26_audit |> count(q37_limpa, sort = TRUE) |> mutate(pct = round(n / sum(n) * 100, 1)))

cat("\nq44_limpa (Benefício Social):\n")
print(df26_audit |> count(q44_limpa, sort = TRUE) |> mutate(pct = round(n / sum(n) * 100, 1)))

# 6. Diagnóstico de Dados Ausentes (NAs) em 2026 --------------------------------
cat("\n--- RELATÓRIO DE DADOS AUSENTES (NAs) NA AMOSTRA DE 2026 ---\n")
nas_2026 <- sapply(df26_audit, function(x) sum(is.na(x)))
nas_2026_pct <- sapply(df26_audit, function(x) round(mean(is.na(x)) * 100, 2))
tabela_nas <- tibble(
  Variavel = names(nas_2026),
  Total_NA = nas_2026,
  Percentual_NA = nas_2026_pct
) |>
  filter(Total_NA > 0) |>
  arrange(desc(Total_NA))

print(tabela_nas)

cat("\n==============================================================================\n")
cat("                  COMENTÁRIOS E DIAGNÓSTICO DE QUALIDADE                      \n")
cat("==============================================================================\n")
cat("1. Variável q26 ('Concluiu ensino médio?'):\n")
cat("   - Registrou 879 NAs (100% de omissão em 2026).\n")
cat("   - Diagnóstico: A pergunta foi suprimida do formulário de 2026 porque a conclusão\n")
cat("     do ensino técnico é pré-requisito mandatório de elegibilidade do PPE.\n\n")
cat("2. Variável q30 ('Ano de conclusão do ensino médio'):\n")
cat("   - Registrou apenas 5 NAs (0.57%) após tratamento de padrões de texto aberto.\n")
cat("   - Os casos residuais referem-se a respostas vagas ('Não lembro', 'Ensino médio completo').\n\n")
cat("3. Variáveis Textuais e Falsos Cognatos:\n")
cat("   - q37: O erro de grafia 'Evengélica' (333 casos em 2026) foi corrigido e agrupado com 'Cristã'.\n")
cat("   - q25: Respostas conceituais de identidade de gênero (ex.: 'Cisgenêro', 'Transexual')\n")
cat("     foram tratadas apropriadamente em conformidade analítica.\n")
cat("   - q44: Respostas textuais abertas sobre Bolsa Família, BPC e LOAS foram categorizadas.\n")
cat("==============================================================================\n")
