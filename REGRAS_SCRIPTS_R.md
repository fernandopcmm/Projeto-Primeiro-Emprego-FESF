# LIVRO DE REGRAS: PADRONIZAÇÃO DE SCRIPTS .R PARA RELATÓRIOS QUARTO (.QMD)
**Projeto:** Avaliação do Projeto Primeiro Emprego (PPE) — Análise Comparativa Longitudinal 2025–2026  
**Localização:** `/home/nando/Documentos/gemini/ATUAL/OUTPUT/`

---

### 1. Política Estrita de Não-Poluição de Diretório (Zero-File Pollution)
- **Nenhum arquivo auxiliar de imagem ou documento:** É expressamente proibido salvar imagens (`.png`, `.jpg`, `.jpeg`, `.svg`), documentos (`.pdf`, `.html`, `.docx`) ou relatórios intermediários a partir dos scripts `.R` de gráficos ou tabelas.
- **Chamada direta no Quarto e Proibição de `print()` / `cat()`:** Os scripts `.R` NÃO devem utilizar as funções `print()`, `cat()` ou expressões livres para imprimir saídas no console. As tabelas e gráficos devem ser apenas atribuídos a variáveis (ex.: `t24_genero <- ...` e `g_comparativo <- ...`). A renderização é feita exclusivamente pelo chunk Quarto que chama o nome da variável após o `source()`, evitando duplicações indesejadas e mensagens brutas no relatório.
- **Proibição de `ggsave()` e `save_plot()`:** Não utilizar comandos de persistência em disco nos scripts analíticos de visualização.

---

### 2. Convenção Rigorosa de Nomenclatura dos Scripts `.R`
A nomenclatura deve seguir a estrutura padronizada:
```text
<NUM_ORDEM>_<CODIGOS_VARIAVEIS>_<TITULO_ABREVIADO>.R
```
- `<NUM_ORDEM>`: Sequencial numérico de 2 dígitos referente à ordem da saída no relatório (ex.: `01`, `02`, `03`, ...).
- `<CODIGOS_VARIAVEIS>`: Código(s) da(s) variável(is) analisada(s) separados por sublinhado (ex.: `q24`, `q23_q50`, `q36_q31`).
- `<TITULO_ABREVIADO>`: Título temático conciso (completo se < 30 caracteres, ou abreviado em snake_case).
- **Exemplo Real Aplicado:** O script comparativo de gênero e tabela de contingência é:
  `03_q24_genero.R`

---

### 3. Estilo Moderno de Tabelas Acadêmicas (`tinytable` / `kableExtra`)
Para relatórios e artigos acadêmicos modernos em Quarto (especialmente com saída em Word `.docx` e PDF via Typst/LaTeX):
- **Pacotes recomendados:**
  1. **`tinytable` (Recomendado/Moderno):** O ecossistema mais recente e flexível para Quarto e Rmarkdown, suportando formatação fluida, alinhamentos, bordas horizontais limpas (*booktabs-style*) e integração nativa em HTML, PDF, Typst e Word.
  2. **`kableExtra` / `knitr::kable`:** Alternativa tradicional para personalização com classes tabulares acadêmicas e `booktabs = TRUE`.
- **Diretrizes de Diagramação de Tabelas Acadêmicas (Padrão APA / ABNT / Booktabs):**
  - **Proibição de linhas verticais:** Tabelas acadêmicas nunca utilizam grades verticais.
  - **Linhas horizontais comedidas:** Apenas linha superior de topo, linha delimitadora do cabeçalho e linha inferior de fechamento total.
  - **Alinhamento semântico:** Texto alinhado à esquerda (`l`), números e quantidades alinhados ao centro ou à direita (`c` ou `r`).
  - **Rótulos formais:** Cabeçalhos claros com indicação de unidades e totais (ex.: `2025 (N)`, `2025 (%)`, `Variação (p.p.)`).

---

### 4. Ecossistema Técnico: Prioridade `tidyverse` e `tidyplots`
- **Manipulação de dados:** Sempre estruturada com pipes nativos (`|>`) e verbos do `tidyverse` (`dplyr`, `tidyr`, `forcats`, `scales`).
- **Consultar scripts de 2025 (`INPUT/Projeto_Primeiro_Emprego/`):**
  - Para toda variável `qXX`, verificar primeiramente o script de referência correspondente de 2025 (ex.: `INPUT/Projeto_Primeiro_Emprego/qXX.R`).
  - Identificar a estrutura de dados, ordenação de níveis de fatores e rótulos adotados no ciclo anterior para garantir continuidade semântica.
- **Uso do `tidyplots`:**
  - Priorizar a sintaxe limpa e declarativa da biblioteca `tidyplots` (conforme padrão do ciclo 2025) quando a visualização atender perfeitamente aos requisitos comparativos.
  - Onde for necessária maior personalização longitudinal (barras agrupadas com percentuais diretos por ano de coleta), utilizar `ggplot2` estendendo o tema minimalista.

---

### 5. Gestão de Padrões Visuais e Arquétipos Gráficos
#### A. Tipos de Gráficos Inéditos no Projeto
Quando a saída exigir uma tipologia gráfica nova no projeto (ex.: primeira nuvem de palavras, primeira correlação quantitativa, primeiro dispersão/scatter ou heatmap):
1. **Consultar o repositório de estilos SWD:** Acessar os exemplos e templates em `/home/nando/Documentos/gemini/ATUAL/INPUT/storytelling-with-data-ggplot/`.
2. **Aplicar os 6 princípios de Cole Nussbaumer Knaflic:**
   - Redução drástica da carga cognitiva (decluttering).
   - Eliminação de grids pesados, eixos redundantes e bordas de painel.
   - Foco pré-atentivo deliberado (cinza neutro para contexto + cor primária institucional para destaque analítico).
   - Alinhamento à esquerda de títulos e subtítulos informativos.

#### B. Tipos de Gráficos Repetidos (Consistência de Família Gráfica)
- Quando o par de tipos de variáveis já tiver um padrão estabelecido (ex.: variável categórica univariada ou fator × fator no comparativo 2025 vs. 2026):
  - **Replicar exatamente a estrutura visual** adotada nos scripts antecessores (ex.: paleta `COR_DESTAQUE = "#174A7E"`, `COR_CONTEXTO = "#929497"`, larguras de barra, tipografia e tema).
  - Manter coerência cromática e hierarquia tipográfica em todo o relatório.

---

### 6. Área Interna do Gráfico Limpa para Diagramação Editorial
- **Sem anotações explicativas internas:** Não inserir caixas de texto com parágrafos, conclusões ou setas explicativas dentro da área de plotagem (`annotate("text", ...)`).
- **Destino dos textos interpretativos:** Toda a narrativa detalhada, inferências causais e discussão dos dados devem constar no corpo do relatório Quarto (`.qmd`) ou na legenda do documento, preservando o gráfico limpo e legível.
- **Rótulos numéricos objetivos:** Permitidos apenas rótulos diretos nas barras/pontos (valores absolutos e percentuais), eliminando a necessidade de legendas poluídas.
