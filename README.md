# Projeto Primeiro Emprego (PPE) – FESF-SUS
### Avaliação Longitudinal Comparativa (2025 – 2026)

Este repositório reúne as rotinas de processamento estatístico em **R**, documentação analítica e relatórios técnicos em **Quarto (.qmd / .html)** referentes à avaliação do Projeto Primeiro Emprego (PPE) na administração pública estadual da Bahia.

A pesquisa investiga o impacto do programa a partir de duas perspectivas complementares: a trajetória socioeconômica e profissional dos **beneficiários** e a visão dos **gestores/pontos focais** sobre a inserção, acolhimento e conformidade operacional.

---

## 🌐 Acesso aos Relatórios Online (GitHub Pages)

Os relatórios interativos compilados e a página principal podem ser acessados diretamente pelos links abaixo:

* 🏠 **[Página Principal do Projeto (Landing Page)](https://fernandopcmm.github.io/Projeto-Primeiro-Emprego-FESF/)**
* 📊 **[Relatório de Avaliação dos Beneficiários (2025–2026)](https://fernandopcmm.github.io/Projeto-Primeiro-Emprego-FESF/relatorio_beneficiarios_2025_2026.html)**
* 📋 **[Relatório dos Gestores e Pontos Focais (2025–2026)](https://fernandopcmm.github.io/Projeto-Primeiro-Emprego-FESF/gest_relatorio_pontos_focais_2025_2026.html)**

---

## 🔒 Acesso às Bases de Dados e Privacidade (LGPD)

Em conformidade com a **Lei Geral de Proteção de Dados (LGPD - Lei nº 13.709/2018)** e os protocolos de ética em pesquisa institucional, os bancos de dados tabulares (`.csv`) não se encontram abertos publicamente neste repositório.

Pesquisadores, gestores públicos ou acadêmicos que tenham interesse no acesso às bases para fins de reprodutibilidade, auditoria científica ou novas investigações podem solicitá-las diretamente ao pesquisador responsável:

* **Fernando Antonio de Melo Pereira Lhamas**
  * E-mail institucional UFRN: [fernando.lhamas@ufrn.br](mailto:fernando.lhamas@ufrn.br)
  * E-mail institucional UFBA: [fernando.melo@ufba.br](mailto:fernando.melo@ufba.br)

---

## 📂 Estrutura do Repositório

### 1. Análise dos Beneficiários
Investiga o perfil sociodemográfico, mobilidade educacional/territorial, transição de renda, satisfação e expectativas profissionais dos jovens inseridos.
* **Relatório:** [`relatorio_beneficiarios_2025_2026.html`](relatorio_beneficiarios_2025_2026.html) (código-fonte em [`relatorio_beneficiarios_2025_2026.qmd`](relatorio_beneficiarios_2025_2026.qmd)).
* **Classificação Temática:** [`CLASSIFICACAO_VARIAVEIS_TOPICOS.md`](CLASSIFICACAO_VARIAVEIS_TOPICOS.md).
* **Scripts R:** `01_importar_harmonizar_dados.R` a `50_q64_alcance_objetivo_profissional.R`.

### 2. Análise dos Gestores e Pontos Focais
Examina os processos de acolhimento institucional, rotinas de trabalho, comunicação intersetorial e conformidade operacional.
* **Relatório:** [`gest_relatorio_pontos_focais_2025_2026.html`](gest_relatorio_pontos_focais_2025_2026.html) (código-fonte em [`gest_relatorio_pontos_focais_2025_2026.qmd`](gest_relatorio_pontos_focais_2026.qmd)).
* **Classificação Temática:** [`gest_CLASSIFICACAO_VARIAVEIS_TOPICOS.md`](gest_CLASSIFICACAO_VARIAVEIS_TOPICOS.md).
* **Scripts R:** `gest_01_importar_harmonizar_dados.R` a `gest_22_q20_inconformidades_riscos.R`.

---

## 🛠️ Tecnologias e Reprodutibilidade
* **Linguagem:** [R](https://www.r-project.org/) (tidyverse, ggplot2, ggalluvial, knitr, kableExtra, tinytable).
* **Documentação Científica:** [Quarto CLI](https://quarto.org/) com normas ABNT via [`referencias.bib`](referencias.bib).
* As regras de padronização dos scripts analíticos estão documentadas em [`REGRAS_SCRIPTS_R.md`](REGRAS_SCRIPTS_R.md).

---

## 👤 Autoria e Contato
**Fernando Antonio de Melo Pereira Lhamas**  
*Universidade Federal do Rio Grande do Norte (UFRN) | Núcleo de Pós-Graduação em Administração (NPGA/UFBA)*  
Contatos: [fernando.lhamas@ufrn.br](mailto:fernando.lhamas@ufrn.br) | [fernando.melo@ufba.br](mailto:fernando.melo@ufba.br)
