# MAPEAMENTO E CLASSIFICAÇÃO DAS VARIÁVEIS POR TÓPICO ANALÍTICO
**Projeto:** Avaliação do Projeto Primeiro Emprego (PPE) — Análise Longitudinal 2025–2026  
**Documento Mestre do Relatório:** `/home/nando/Documentos/gemini/ATUAL/OUTPUT/relatorio_beneficiarios_2025_2026.qmd`  
**Data da Matriz:** 2026-09-15  

> **Objetivo Metodológico:** Este documento fixa o plano diretor temático de todas as 45 variáveis analíticas (`q21` a `q65`) do questionário de beneficiários. Nenhuma variável será deixada para trás e nenhuma seção necessitará de retrabalho ou retorno posterior.

---

## Estrutura Geral das Seções Analíticas

| Seção no Relatório | Tópico Temático | Variáveis Contempladas | Status de Execução |
| :---: | :--- | :---: | :---: |
| **Seção 1** | Apresentação e Contexto da Política Pública | — | **Concluída** |
| **Seção 2** | Nota Técnica, Coleta (FESF-SUS) e Metodologia | — | **Concluída** |
| **Seção 3** | Perfil Sociodemográfico, Religioso e Comunicação | `q21`, `q24`, `q23`, `q36`, `q35`, `q25`, `q37` | **Em finalização** |
| **Seção 4** | Trajetória Educacional e Formação Profissional | `q26`, `q30`, `q31`, `q45`, `q46`, `q47`, `q48`, `q58`, `q59` | **A iniciar** |
| **Seção 5** | Distribuição Territorial e Alocação no Programa | `q27`, `q28`, `q32`, `q33`, `q34`, `q22` | **A iniciar** |
| **Seção 6** | Contexto Socioeconômico, Moradia e Mobilidade de Renda | `q38`, `q39`, `q40`, `q41`, `q42`, `q43`, `q44`, `q65` | **A iniciar** |
| **Seção 7** | Inserção Laboral Prévia, Trabalho Atual e Clima Organizacional | `q29`, `q60`, `q49`, `q51`, `q52`, `q53`, `q54`, `q55`, `q56`, `q57` | **A iniciar** |
| **Seção 8** | Satisfação, Autoeficácia e Aspirações Profissionais | `q50`, `q61`, `q62`, `q63`, `q64` | **A iniciar** |

---

## Detalhamento das Variáveis por Seção

### Seção 3: Perfil Sociodemográfico, Religioso e Canais de Comunicação
*Foco: Identidade, composição demográfica, diversidade, pertencimento cultural/religioso e adesão a redes virtuais.*

* `q21`: **Participa de alguma rede social?** (Instagram, LinkedIn, Facebook, WhatsApp, etc.);
* `q24`: **Sexo / Gênero** (Feminino, Masculino, Não Binário) *(Já implementado no Script 03)*;
* `q23`: **Idade em anos / Faixa Etária** (16-21, 22-25, 26-35, 35+) *(Já implementado no Script 04)*;
* `q36`: **Raça/Cor** (Pardo, Preto, Branco, Amarelo, Indígena) *(Já implementado no Script 05)*;
* `q35`: **Estado Civil** (Solteiro, Casado, Separado) *(Já implementado no Script 06)*;
* `q25`: **Orientação Sexual** (Heterossexual, Bissexual, Homossexual/Lésbica, etc.) *(Já implementado no Script 07)*;
* `q37`: **Afiliação Religiosa** (Evangélica/Cristã, Católica, Sem religião, Matriz Africana, etc.);
* **Síntese Interseccional:** Diagrama Aluvial (`q24` × `q23` × `q36`) *(Já implementado no Script 08)*.

---

### Seção 4: Trajetória Educacional e Formação Profissional
*Foco: Nível de instrução formal, lapso temporal até o emprego, perfil dos cursos técnicos e hábitos de estudo.*

* `q26`: **Concluiu ensino médio?** (Auditoria de elegibilidade mandante do PPE);
* `q30`: **Ano de conclusão do ensino médio** (Intervalo entre formação e inserção laboral);
* `q31`: **Qual sua formação técnica?** (Cursos técnicos de origem: Enfermagem, Administração, TI, etc. — nuvem de palavras / frequência);
* `q45`: **Escolaridade do Pai** (Origem educacional e capital cultural familiar);
* `q46`: **Escolaridade da Mãe** (Origem educacional e efeito materno);
* `q47`: **Livros lidos nos últimos 12 meses** (Capital cultural e consumo de leitura);
* `q48`: **Horas semanais dedicadas ao estudo** (Investimento educacional concomitante ao trabalho);
* `q58`: **Desejo de continuar estudando** (Propensão à educação continuada);
* `q59`: **Está em alguma universidade?** (Acesso ao ensino superior).

---

### Seção 5: Distribuição Territorial, Mobilidade e Alocação no Programa
*Foco: Origem geográfica dos jovens, fluxos pendulares moradia-trabalho e órgãos da administração alocadores.*

* `q27`: **Estado de nascimento (UF)** (Naturalidade e migração interestadual);
* `q28`: **Município de nascimento** (Capilaridade nos territórios de identidade da Bahia);
* `q32`: **Município de moradia atual** (Residência do beneficiário);
* `q33`: **Município onde trabalha pelo PPE** (Polo de atuação no programa);
* `q32` × `q33`: **Mobilidade pendular** (Reside no mesmo município em que trabalha?);
* `q34`: **Unidade / Local de trabalho onde atua** (Órgãos estaduais, hospitais, escolas, secretarias);
* `q22`: **Ano de entrada no PPE** (Tempo de permanência e maturação da experiência: Antes de 2024, 2024, 2025, 2026).

---

### Seção 6: Contexto Socioeconômico, Condições de Moradia e Transição de Renda
*Foco: Vulnerabilidade social prévia, impacto econômico distributivo, arrefecimento da pobreza e bens duráveis.*

* `q38`: **Onde e com quem você mora atualmente?** (Arranjo domiciliar e coabitação);
* `q39`: **Tamanho da família / Coabitantes** (Número de familiares residentes no mesmo domicílio);
* `q40`: **Renda familiar atual aproximada** (Renda total do domicílio com a bolsa do PPE);
* `q41`: **Percepção da situação financeira** (Suficiência orçamentária e papel do jovem no sustento da casa);
* `q42`: **Condição e situação de moradia** (Zona urbana vs. rural, proximidade do trabalho e posse do imóvel);
* `q43`: **Renda familiar média antes de ingressar no PPE** (Renda base pré-programa);
* `q40` × `q43`: **Transição de Renda Familiar** (Cruzamento de mobilidade social pré vs. pós PPE);
* `q44`: **Acesso a programas de transferência de renda** (Bolsa Família, BPC, LOAS);
* `q65`: **Acesso e aquisição de bens duráveis** (Impacto material direto na aquisição de bens de consumo).

---

### Seção 7: Inserção Laboral Prévia, Condições de Trabalho e Clima Organizacional
*Foco: Quebra de barreiras de entrada, transição informalidade-formalidade, ambiente e aderência profissional.*

* `q29`: **Trabalhou antes do PPE?** (Inserção no mercado formal prévia);
* `q60`: **Trabalhou antes, mesmo que informalmente?** (Experiência no mercado informal e contradição reflexiva);
* `q29` × `q60`: **Análise da Formalização do Primeiro Emprego**;
* `q49`: **Adequação das instalações físicas do trabalho** (Infraestrutura e segurança ocupacional);
* `q51`: **Auxílio da instituição na progressão de carreira** (Suporte institucional ao desenvolvimento);
* `q52`: **Experiência com recursos audiovisuais e tecnologias** (Apropriação digital no posto);
* `q53`: **Relacionamento interpessoal com colegas** (Clima organizacional e integração interpessoal);
* `q54`: **Nível de exigência do trabalho** (Cobrança e complexidade das atribuições);
* `q55`: **Contribuição do trabalho para aquisição de cultura geral**;
* `q56`: **Contribuição para a formação de cidadão exemplar** (Competências cívicas);
* `q57`: **Aderência entre trabalho desempenhado e formação técnica** (Alocação correta da qualificação).

---

### Seção 8: Satisfação Geral, Capital Psicológico e Aspirações Profissionais
*Foco: Avaliação global do beneficiário, ganho de autoeficácia profissional e perspectivas futuras.*

* `q50`: **Nível de satisfação geral com o trabalho no PPE** (Escala avaliativa do programa);
* `q61`: **Sentimento em relação ao mercado comparado a antes do PPE** (Capital psicológico e autoconfiança);
* `q62`: **Crença em conseguir emprego formal antes do PPE** (Expectativa prévia de inserção laboral);
* `q63`: **Maior objetivo profissional antes de entrar no PPE** (Aspiração de carreira prévia);
* `q64`: **Sentimento em relação ao objetivo profissional hoje** (Alcance retrospectivo de metas de carreira).

---

## Protocolo de Execução Sistemática
1. Nenhuma seção subsequente será iniciada antes de esgotar integralmente todas as variáveis listadas em seu bloco;
2. Cada variável receberá script dedicado com tabela acadêmica (`tinytable`) e visualização `ggplot2` minimalista;
3. O relatório Quarto (`.qmd`) manterá a numeração estrita e a redação interpretativa de cada tabela e figura.
