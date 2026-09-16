# Projeto Primeiro Emprego (PPE) – FESF-SUS
### Avaliação Longitudinal Comparativa (2025 – 2026)

Este repositório reúne os dados, rotinas de processamento estatístico em **R** e relatórios técnicos em **Quarto (.qmd / .html)** referentes à avaliação do Projeto Primeiro Emprego (PPE) na administração pública estadual da Bahia.

A pesquisa investiga o impacto do programa a partir de duas perspectivas complementares: a trajetória socioeconômica e profissional dos **beneficiários** e a visão dos **gestores/pontos focais** sobre a inserção, acolhimento e conformidade operacional.

---

## 🌐 Acesso aos Relatórios Online (GitHub Pages)

Os relatórios interativos compilados e a página principal podem ser acessados diretamente pelos links abaixo:

* 🏠 **[Página Principal do Projeto (Landing Page)](https://fernandopcmm.github.io/Projeto-Primeiro-Emprego-FESF/)**
* 📊 **[Relatório de Avaliação dos Beneficiários (2025–2026)](https://fernandopcmm.github.io/Projeto-Primeiro-Emprego-FESF/relatorio_beneficiarios_2025_2026.html)**
* 📋 **[Relatório dos Gestores e Pontos Focais (2025–2026)](https://fernandopcmm.github.io/Projeto-Primeiro-Emprego-FESF/gest_relatorio_pontos_focais_2025_2026.html)**

---

## 📂 Estrutura do Repositório

### 1. Análise dos Beneficiários
Investiga o perfil sociodemográfico, mobilidade educacional/territorial, transição de renda, satisfação e expectativas profissionais dos jovens inseridos.
* **Relatório:** [`relatorio_beneficiarios_2025_2026.html`](relatorio_beneficiarios_2025_2026.html) (código-fonte em [`relatorio_beneficiarios_2025_2026.qmd`](relatorio_beneficiarios_2025_2026.qmd)).
* **Base de Dados Harmonizada:** [`PPE_beneficiarios_2025_2026.csv`](PPE_beneficiarios_2025_2026.csv).
* **Dicionário de Variáveis:** [`codebook_beneficiarios.csv`](codebook_beneficiarios.csv) e [`CLASSIFICACAO_VARIAVEIS_TOPICOS.md`](CLASSIFICACAO_VARIAVEIS_TOPICOS.md).
* **Scripts R:** `01_importar_harmonizar_dados.R` a `50_q64_alcance_objetivo_profissional.R`.

### 2. Análise dos Gestores e Pontos Focais
Examina os processos de acolhimento institucional, rotinas de trabalho, comunicação intersetorial e conformidade operacional.
* **Relatório:** [`gest_relatorio_pontos_focais_2025_2026.html`](gest_relatorio_pontos_focais_2025_2026.html) (código-fonte em [`gest_relatorio_pontos_focais_2025_2026.qmd`](gest_relatorio_pontos_focais_2026.qmd)).
* **Bases de Dados:** [`gest_PPE_pontos_focais_2025_2026.csv`](gest_PPE_pontos_focais_2025_2026.csv) e [`gest_PPE_tratado_2025_2026.csv`](gest_PPE_tratado_2025_2026.csv).
* **Dicionário de Variáveis:** [`gest_codebook_pontos_focais.csv`](gest_codebook_pontos_focais.csv) e [`gest_CLASSIFICACAO_VARIAVEIS_TOPICOS.md`](gest_CLASSIFICACAO_VARIAVEIS_TOPICOS.md).
* **Scripts R:** `gest_01_importar_harmonizar_dados.R` a `gest_22_q20_inconformidades_riscos.R`.

---

## 🛠️ Tecnologias e Reprodutibilidade
* **Linguagem:** [R](https://www.r-project.org/) (tidyverse, ggplot2, ggalluvial, knitr, kableExtra).
* **Documentação Científica:** [Quarto CLI](https://quarto.org/) com normas ABNT via [`referencias.bib`](referencias.bib).
* As regras de padronização dos scripts analíticos estão documentadas em [`REGRAS_SCRIPTS_R.md`](REGRAS_SCRIPTS_R.md).

---

## 👤 Autoria
**Fernando Antonio de Melo Pereira Lhamas**  
*Universidade Federal do Rio Grande do Norte (UFRN) | Núcleo de Pós-Graduação em Administração (NPGA/UFBA)*
