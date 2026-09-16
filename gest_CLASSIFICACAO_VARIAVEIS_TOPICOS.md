# Planejamento Diretor de Classificação Temática das Variáveis dos Gestores (Pontos Focais)
## Avaliação Longitudinal do Projeto Primeiro Emprego (2025–2026)

**Autor:** Fernando Antonio de Melo Pereira Lhamas  
*Universidade Federal do Rio Grande do Norte (UFRN) | Núcleo de Pós-Graduação em Administração (NPGA/UFBA)*  
**Data:** Setembro de 2026  
**Documento Normativo de Referência:** `gest_CLASSIFICACAO_VARIAVEIS_TOPICOS.md`

---

## 1. Justificativa Metodológica e Escopo do Estudo

A presente pesquisa avaliativa investiga a percepção dos **Pontos Focais** — gestores, diretores e supervisores dos órgãos públicos e unidades de saúde do Estado da Bahia que acolhem, orientam e acompanham diariamente os beneficiários do **Projeto Primeiro Emprego (PPE)**.

A base empírica longitudinal congrega **144 gestores públicos respondentes**, distribuídos em dois ciclos avaliativos anuais:
* **Ciclo 2025:** 67 respondentes (coleta realizada entre setembro e outubro de 2025);
* **Ciclo 2026:** 77 respondentes (coleta realizada entre maio e junho de 2026).

Todas as 20 variáveis investigadas (`q1` a `q20`) foram mapeadas e organizadas sistematicamente em **cinco blocos temáticos analíticos**, além de um bloco conclusivo de recomendações gerenciais, garantindo a cobertura exaustiva de todo o instrumento de coleta.

---

## 2. Estrutura dos Blocos Temáticos e Mapeamento dos Scripts R

### Bloco I: Percepção Geral do Projeto e o Papel do Ponto Focal
*Foco: Validação da relevância social da política pública, definição das atribuições práticas e mensuração dos desafios e facilidades da tutoria cotidiana.*

* `gest_03_q1_relevancia.R`:
  * **Variável:** `q1` — *Relevância do PPE na inserção de jovens no mundo do trabalho na Bahia* (Escala Likert 1 a 5).
  * **Objetos:** `t01_relevancia` (Tabela Tinytable) e `g01_relevancia` (Gráfico ggplot2 SWD).
* `gest_04_q2_atribuicoes.R`:
  * **Variável:** `q2` — *Principal atribuição prática desempenhada como Ponto Focal*.
  * **Objetos:** `t02_atribuicoes` e `g02_atribuicoes`.
* `gest_05_q3_nivel_desafio.R`:
  * **Variável:** `q3` — *Nível geral de desafio do papel de Ponto Focal* (Escala Likert 1 a 5).
  * **Objetos:** `t03_nivel_desafio` e `g03_nivel_desafio`.
* `gest_06_q4_principais_desafios.R`:
  * **Variável:** `q4` — *Três principais desafios enfrentados no cotidiano da gestão* (Análise de conteúdo, termos-chave e categorização semântica).
  * **Objetos:** `t04_desafios_categorias` e `g04_desafios_frequencia`.
* `gest_07_q5_principais_facilidades.R`:
  * **Variável:** `q5` — *Três principais facilidades e melhores aspectos vivenciados como Ponto Focal* (Humanização, mentoria e evolução dos jovens).
  * **Objetos:** `t05_facilidades_categorias` e `g05_facilidades_frequencia`.

---

### Bloco II: Acolhimento, Protocolos e Inserção Organizacional
*Foco: Procedimentos de recepção inicial dos beneficiários, grau de padronização normativa e modelos de socialização na cultura do setor público.*

* `gest_08_q6_avaliacao_acolhimento.R`:
  * **Variável:** `q6` — *Avaliação global do processo de acolhimento dos jovens no órgão* (Escala Likert 1 a 5).
  * **Objetos:** `t06_acolhimento` e `g06_acolhimento`.
* `gest_09_q7_protocolos_acolhimento.R`:
  * **Variável:** `q7` — *Existência de protocolos ou diretrizes padronizadas de acolhimento* (Sim, Não, Não tenho certeza).
  * **Objetos:** `t07_protocolos` e `g07_protocolos`.
* `gest_10_q8_contribuicao_integracao.R`:
  * **Variável:** `q8` — *Contribuição do acolhimento para a integração dos jovens na equipe* (Fundamental, Significativa, Em alguma medida).
  * **Objetos:** `t08_contribuicao_acolhimento` e `g08_contribuicao_acolhimento`.
* `gest_11_q9_insercao_cultura_orgao.R`:
  * **Variável:** `q9` — *Formas de inserção do jovem na cultura organizacional* (Acompanhamento por servidor sênior, apresentação formal, integração mista).
  * **Objetos:** `t09_insercao_cultura` e `g09_insercao_cultura`.

---

### Bloco III: Comunicação Interinstitucional, Canais e Supervisão do Desempenho
*Foco: Relação entre os órgãos de ponta e a entidade parceira gestora (FESF-SUS / FLEM), eficácia dos canais de diálogo e assiduidade do acompanhamento laboral.*

* `gest_12_q10_qualidade_comunicacao_parceira.R`:
  * **Variável:** `q10` — *Clareza e frequência da comunicação entre a entidade parceira e o Ponto Focal* (Escala Likert 1 a 5).
  * **Objetos:** `t10_qualidade_comunicacao` e `g10_qualidade_comunicacao`.
* `gest_13_q11_canais_comunicacao.R`:
  * **Variável:** `q11` — *Canais de comunicação mais utilizados para interlocução* (WhatsApp exclusivo, E-mail exclusivo, Canais integrados).
  * **Objetos:** `t11_canais_comunicacao` e `g11_canais_comunicacao`.
* `gest_14_q12_frequencia_acompanhamento.R`:
  * **Variável:** `q12` — *Frequência de acompanhamento das atividades e do desempenho dos jovens* (Diariamente, Semanalmente, Quinzenal/Mensal).
  * **Objetos:** `t12_frequencia_acompanhamento` e `g12_frequencia_acompanhamento`.
* `gest_15_q13_desafio_obter_informacoes.R`:
  * **Variável:** `q13` — *Grau de desafio para obter informações sobre desempenho e permanência dos jovens* (Escala Likert 1 a 5).
  * **Objetos:** `t13_desafio_informacoes` e `g13_desafio_informacoes`.

---

### Bloco IV: Perfil Comportamental, Atitudes e Integração dos Beneficiários
*Foco: Desempenho atitudinal, maturidade profissional, proatividade, adaptação às rotinas públicas e sociabilidade na equipe.*

* `gest_16_q14_proatividade_jovens.R`:
  * **Variável:** `q14` — *Proatividade geral dos jovens beneficiários em suas atribuições* (Escala Likert 1 a 5).
  * **Objetos:** `t14_proatividade` e `g14_proatividade`.
* `gest_17_q15_imaturidade_desafio.R`:
  * **Variável:** `q15` — *Imaturidade dos jovens como desafio para o acompanhamento gerencial* (Escala Likert 1 a 5).
  * **Objetos:** `t15_imaturidade` e `g15_imaturidade`.
* `gest_18_q16_clareza_objetivos_profissionais.R`:
  * **Variável:** `q16` — *Clareza dos beneficiários quanto aos seus objetivos profissionais futuros* (Escala Likert 1 a 5).
  * **Objetos:** `t16_clareza_objetivos` e `g16_clareza_objetivos`.
* `gest_19_q17_adaptacao_servico_publico.R`:
  * **Variável:** `q17` — *Adaptação dos jovens à cultura e às rotinas do serviço público* (Escala Likert 1 a 5).
  * **Objetos:** `t17_adaptacao_publica` e `g17_adaptacao_publica`.
* `gest_20_q18_relacionamento_equipe.R`:
  * **Variável:** `q18` — *Relacionamento interpessoal com os demais trabalhadores e servidores* (Escala Likert 1 a 5).
  * **Objetos:** `t18_relacionamento_equipe` e `g18_relacionamento_equipe`.

---

### Bloco V: Sugestões de Aprimoramento, Inconformidades e Riscos de Gestão
*Foco: Diagnóstico qualitativo crítico, propostas de aprimoramento da governança e identificação de riscos operacionais (confusão de prazos contratuais e desvio de finalidade).*

* `gest_21_q19_sugestoes_aprimoramento.R`:
  * **Variável:** `q19` — *Sugestões dos gestores para aprimoramento do PPE* (Articulação institucional, capacitação, canais dedicados e manuais de orientação).
  * **Objetos:** `t19_sugestoes` e `g19_sugestoes`.
* `gest_22_q20_inconformidades_riscos.R`:
  * **Variável:** `q20` — *Identificação de inconformidades críticas ou aspectos de atenção urgente* (Substituição indevida de servidores permanentes e gestão de expectativas contratuais).
  * **Objetos:** `t20_inconformidades` e `g20_inconformidades`.

---

### Bloco VI: Considerações Finais e Recomendações Estratégicas de Gestão
*Foco: Matriz conclusiva de forças e fraquezas, avaliação longitudinal da maturação gerencial e proposições práticas para a FESF-SUS e Governo do Estado.*

---

## 3. Diretrizes e Padrões Rígidos de Execução

1. **Prefixo Obrigatório:** Todos os arquivos gerados (scripts `.R`, dados consolidados, tabelas, gráficos e relatório Quarto) devem conter obrigatoriamente o prefixo `gest_`.
2. **Zero Poluição de Console:** Proibição estrita de chamadas como `print()` ou `cat()` dentro dos scripts `.R`. Os scripts apenas carregam dados, computam métricas e instanciam objetos em memória (`tXX_...` e `gXX_...`).
3. **Zero Poluição em Disco:** Proibição terminante de geração de arquivos `.png`, `.pdf` ou `.jpeg` em disco (`ggsave` vetado).
4. **Padrão Acadêmico Booktabs (`tinytable`):** Uso do pacote `tinytable::tt() |> format_tt(escape = TRUE)` com totais e variações percentuais em pontos percentuais (`p.p.`).
5. **Storytelling with Data (`ggplot2`):** Gráficos minimalistas e elegantes com paleta `#174A7E` (Destaque 2026), `#929497` (Contexto 2025), `#231F20` (Títulos) e `#555655` (Subtítulos).
