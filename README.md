Este repositório reúne os dados, rotinas de processamento estatístico em **R** e relatórios técnicos em
  **Quarto (.qmd / .html)** referentes à avaliação do Projeto Primeiro Emprego (PPE) na administração pública
  estadual da Bahia. 
    
    A pesquisa investiga o impacto do programa a partir de duas perspectivas complementares: a trajetória
  socioeconômica e profissional dos **beneficiários** e a visão dos **gestores/pontos focais** sobre a
  inserção e conformidade operacional.
    
    ---
    
    ## 📂 Estrutura do Repositório
    
    ### 1. Análise dos Beneficiários
    Investiga o perfil sociodemográfico, mobilidade educacional/territorial, transição de renda, satisfação e
  expectativas profissionais dos jovens inseridos.
    * **Relatório:** `relatorio_beneficiarios_2025_2026.html` (código-fonte em
  `relatorio_beneficiarios_2025_2026.qmd`).
    * **Base de Dados Harmonizada:** `PPE_beneficiarios_2025_2026.csv`.
    * **Dicionário de Variáveis:** `codebook_beneficiarios.csv` e `CLASSIFICACAO_VARIAVEIS_TOPICOS.md`.
    * **Scripts R:** `01_importar_harmonizar_dados.R` a `50_q64_alcance_objetivo_profissional.R`.

    ### 2. Análise dos Gestores e Pontos Focais
    Examina os processos de acolhimento institucional, rotinas de trabalho, comunicação intersetorial e
  avaliação de desempenho dos beneficiários.
    * **Relatório:** `gest_relatorio_pontos_focais_2025_2026.html` (código-fonte em
  `gest_relatorio_pontos_focais_2025_2026.qmd`).
    * **Bases de Dados:** `gest_PPE_pontos_focais_2025_2026.csv` e `gest_PPE_tratado_2025_2026.csv`.
    * **Dicionário de Variáveis:** `gest_codebook_pontos_focais.csv` e `gest_CLASSIFICACAO_VARIAVEIS_TOPICOS.
  md`.
    * **Scripts R:** `gest_01_importar_harmonizar_dados.R` a `gest_22_q20_inconformidades_riscos.R`.
