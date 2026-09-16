# ==============================================================================
# Projeto Primeiro Emprego (PPE) - Avaliação Multianual (2025 - 2026)
# Script 01: Importação, Conversão para CSV, Criação do Fator AnoCol e Codebook
# ==============================================================================
# Autoria: Fernando Lhamas
# Data: 2026-09-15
# Ambiente: R / Tidyverse
# ==============================================================================

# 1. Carregamento de Bibliotecas -----------------------------------------------
suppressPackageStartupMessages({
  library(readxl)
  library(tidyverse)
  library(lubridate)
})

# 2. Definição de Parâmetros e Caminhos Relativos ------------------------------
caminho_input_excel <- "/home/nando/Documentos/gemini/ATUAL/INPUT/Questionário - Avaliação PPE (respostas) (1).xlsx"
caminho_output_csv  <- "/home/nando/Documentos/gemini/ATUAL/OUTPUT/PPE_beneficiarios_2025_2026.csv"
caminho_codebook    <- "/home/nando/Documentos/gemini/ATUAL/OUTPUT/codebook_beneficiarios.csv"

# 3. Importação dos Dados Brutos do Excel --------------------------------------
cat("Importando planilha bruta do questionário...\n")
df_raw <- read_excel(caminho_input_excel, sheet = 1)
cat(sprintf("Dimensões originais da base importada: %d linhas e %d colunas.\n", nrow(df_raw), ncol(df_raw)))

# 4. Extração e Criação do Vetor de Fator 'AnoCol' ------------------------------
# Separação baseada na data registrada no 'Carimbo de data/hora'
# Gera um vetor do tipo factor estrito com os levels "2025" e "2026"
df_proc <- df_raw |>
  mutate(
    Data_Hora = ymd_hms(`Carimbo de data/hora`),
    Ano_Extraido = as.character(year(Data_Hora)),
    AnoCol = factor(Ano_Extraido, levels = c("2025", "2026"))
  )

cat("\nDistribuição de observações por Ano de Coleta (AnoCol):\n")
print(table(df_proc$AnoCol, useNA = "ifany"))

# 5. Estruturação e Aplicação do Codebook Aderente ao Projeto de 2025 -----------
# No ciclo de 2025 (Relatório_beneficiários.qmd), foram descartadas as colunas 1 a 5
# e a coluna 7 (link de rede social), resultando nas questões q21 até q65.
# Aqui preservamos o Carimbo de data/hora e criamos identificadores canônicos e harmonizados.

codebook <- tribble(
  ~codigo_2025, ~codigo_canonico, ~rotulo_original, ~tipo_dado, ~descricao,
  "data_coleta", "MET_DATA_COLETA", "Carimbo de data/hora", "datetime", "Momento exato de envio do formulário",
  "anocol", "MET_ANO_COLETA", "AnoCol", "factor", "Ano da rodada de coleta (Levels: 2025, 2026)",
  "q21", "RED_SOCIAL_PARTICIPA", "Participa de alguma rede social?", "nominal", "Adesão a redes sociais e canais virtuais",
  "q22", "PPE_ANO_ENTRADA", "Ano de entrada no PPE", "ordinal", "Ano em que o jovem iniciou suas atividades no PPE",
  "q23", "DEM_IDADE", "Idade (somente números - formato \"99\")", "numerica", "Idade do jovem beneficiário em anos",
  "q24", "DEM_SEXO", "Sexo", "nominal", "Sexo biológico / declarado do respondente",
  "q25", "DEM_ORIENTACAO_SEXUAL", "Qual sua orientação sexual?", "nominal", "Orientação sexual autodeclarada",
  "q26", "EDU_CONCLUIU_MEDIO", "Concluiu ensino médio?", "nominal", "Status de conclusão do ensino médio regular/técnico",
  "q27", "DEM_UF_NASCIMENTO", "Estado de nascimento (somente sigla - Ex: \"BA\")", "nominal", "UF de nascimento do beneficiário",
  "q28", "DEM_CIDADE_NASCIMENTO", "Cidade de nascimento (por extenso, somente o nome da cidade, sem o estado - Ex: \"Salvador\")", "nominal", "Município de naturalidade do beneficiário",
  "q29", "TRA_TRABALHOU_ANTES", "Você já trabalhou antes do PPE?", "nominal", "Histórico de inserção laboral prévia geral",
  "q30", "EDU_ANO_CONCLUSAO_MEDIO", "Ano de conclusão do ensino médio?", "numerica", "Ano de formatura no ensino médio",
  "q31", "EDU_FORMACAO_TECNICA", "Qual sua formação técnica?", "textual", "Curso técnico profissionalizante concluído",
  "q32", "LOC_MUNICIPIO_MORADIA", "Município de moradia? (por extenso, somente o nome da cidade, sem o estado - Ex: \"Salvador\")", "nominal", "Município de residência atual",
  "q33", "LOC_MUNICIPIO_TRABALHO", "Município onde trabalha pelo PPE? (por extenso, somente o nome da cidade, sem o estado - Ex: \"Salvador\")", "nominal", "Município onde a vaga do PPE está alocada",
  "q34", "LOC_UNIDADE_LOTACAO", "Unidade/Local de trabalho onde atua no PPE", "textual", "Órgão, hospital ou secretaria de lotação",
  "q35", "DEM_ESTADO_CIVIL", "Qual o seu estado civil?", "nominal", "Estado civil declarado",
  "q36", "DEM_RACA_COR", "Como você se considera? (raça/cor)", "nominal", "Autodeclaração étnico-racial (IBGE)",
  "q37", "DEM_RELIGIAO", "Qual a sua religião?", "nominal", "Afiliação ou crença religiosa",
  "q38", "SOC_ONDE_COM_QUEM_MORA", "Onde e com quem você mora atualmente?", "nominal", "Arranjo domiciliar e coabitação",
  "q39", "SOC_TAMANHO_FAMILIA", "Quantas pessoas, da sua família, moram com você na mesma casa?", "numerica", "Número de coabitantes da família",
  "q40", "SOC_RENDA_FAMILIAR_ATUAL", "Somando a sua renda com a renda dos seus familiares que moram com você, quanto é, aproximadamente a renda familiar?", "ordinal", "Faixa de renda familiar atual (com bolsa PPE)",
  "q41", "SOC_SITUACAO_FINANCEIRA", "Assinale a situação financeira que melhor descreve seu caso (considerando o PPE como renda)", "nominal", "Percepção de suficiência financeira",
  "q42", "SOC_CONDICAO_MORADIA", "Indique a resposta que melhor descreve sua atual situação de moradia", "nominal", "Condição de posse/aluguel da residência",
  "q43", "SOC_RENDA_FAMILIAR_ANTES", "Antes de ingressar no PPE, qual era a renda média mensal da sua família?", "ordinal", "Faixa de renda familiar anterior ao programa",
  "q44", "SOC_BENEFICIO_SOCIAL", "Sua família recebe algum benefício social?", "nominal", "Acesso a programas de transferência de renda",
  "q45", "EDU_ESCOLARIDADE_PAI", "Até que nível seu pai estudou?", "ordinal", "Nível de instrução paterno",
  "q46", "EDU_ESCOLARIDADE_MAE", "Até que nível sua mãe estudou?", "ordinal", "Nível de instrução materno",
  "q47", "CUL_LIVROS_LIDOS", "Quantos livros você leu nos últimos 12 meses?", "ordinal", "Consumo e hábitos de leitura anual",
  "q48", "EDU_HORAS_ESTUDO_SEMANA", "Quantas horas por semana, aproximadamente, você dedica a estudos?", "ordinal", "Carga horária semanal dedicada aos estudos",
  "q49", "AMB_CONDICOES_FISICAS", "As condições gerais das instalações físicas dos eu trabalho são adequadas?", "ordinal", "Adequação da infraestrutura do posto de trabalho",
  "q50", "SAT_SATISFACAO_GERAL", "De forma geral, como se sente em relação ao trabalho oriundo do PPE?", "ordinal", "Nível de satisfação com as atribuições no PPE",
  "q51", "TRA_AUXILIO_CARREIRA", "A sua instituição empregadora te auxilia a progredir na carreira?", "ordinal", "Suporte institucional ao desenvolvimento profissional",
  "q52", "HAB_USO_TECNOLOGIA", "Como você caracteriza sua experiência no uso de recursos audiovisuais e tecnológicos no trabalho?", "ordinal", "Uso e domínio de tecnologias no posto",
  "q53", "CLI_RELACIONAMENTO_COLEGAS", "Como você avalia o relacionamento interpessoal com os colegas de trabalho?", "ordinal", "Clima organizacional e integração interpessoal",
  "q54", "TRA_EXIGENCIA_TRABALHO", "Como você avalia o nível de exigência do trabalho?", "ordinal", "Percepção do grau de cobrança e complexidade",
  "q55", "DES_CULTURA_GERAL", "Você considera que seu trabalho contribui para aquisição de cultura geral?", "ordinal", "Percepção de ampliação de repertório cultural",
  "q56", "DES_CIDADAO_EXEMPLAR", "Você considera que seu trabalho te ajuda a ser um cidadão exemplar?", "ordinal", "Desenvolvimento de competências cívicas e cidadãs",
  "q57", "DES_EXERCICIO_PROFISSIONAL", "Você considera que seu trabalho contribui na preparação para o exercício profissional?", "ordinal", "Aderência entre prática laboral e formação técnica",
  "q58", "EDU_DESEJO_ESTUDAR", "Como você avalia o desejo de continuar estudando?", "ordinal", "Propensão à educação continuada",
  "q59", "EDU_ESTA_UNIVERSIDADE", "Está em alguma universidade?", "nominal", "Ingresso no ensino superior",
  "q60", "TRA_TRABALHO_INFORMAL_PREV", "Antes do PPE, você já havia trabalhado, mesmo que informalmente?", "nominal", "Experiência prévia no mercado informal",
  "q61", "PSI_SENTIMENTO_MERCADO", "Como você se sente em relação ao mercado de trabalho comparado a antes do PPE?", "ordinal", "Autoeficácia e capital psicológico profissional",
  "q62", "PSI_CRENCA_EMPREGO_FORMAL", "Você acreditava que conseguiria um emprego formal antes do PPE?", "ordinal", "Expectativa prévia de obtenção de emprego formal",
  "q63", "CAR_OBJETIVO_ANTES_PPE", "Antes do PPE, qual era seu maior objetivo profissional?", "textual", "Aspiração de carreira antes de ingressar no PPE",
  "q64", "CAR_SENTIMENTO_OBJETIVO_AGORA", "Considerando o objetivo profissional de antes do PPE, como se sente agora?", "ordinal", "Avaliação retrospectiva do objetivo profissional",
  "q65", "BEN_ACESSO_BENS_CONSUMO", "Sua família tem acesso a bens que antes do PPE não possuíam ou teve condições de comprar mais itens?", "nominal", "Impacto material na aquisição de bens duráveis"
)

# Exportação do Codebook para auditoria
write_csv(codebook, caminho_codebook)
cat(sprintf("\nCodebook exportado com sucesso para: %s\n", caminho_codebook))

# 6. Harmonização dos Dados Conforme os Padrões de 2025 -------------------------
# Seleciona AnoCol, Carimbo de data/hora e as 45 variáveis analíticas (q21 a q65),
# removendo dados de identificação pessoal em conformidade estrita com a LGPD.

colunas_interesse <- c(
  "AnoCol",
  "Carimbo de data/hora",
  codebook$rotulo_original[3:nrow(codebook)]
)

df_harmonizado <- df_proc |>
  select(all_of(colunas_interesse))

# Renomeia para q21 a q65, mantendo total aderência aos scripts individuais do projeto
novos_nomes <- c("AnoCol", "Data_Hora", paste0("q", 21:65))
colnames(df_harmonizado) <- novos_nomes

cat(sprintf("\nBase harmonizada construída: %d registros e %d colunas.\n", nrow(df_harmonizado), ncol(df_harmonizado)))

# 7. Linha de Código para Exportação em CSV -------------------------------------
# Conforme diretriz solicitada, a exportação definitiva permanece parametrizada e documentada:

write_csv(df_harmonizado, file = "/home/nando/Documentos/gemini/ATUAL/OUTPUT/PPE_beneficiarios_2025_2026.csv")
