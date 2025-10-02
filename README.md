<h1 align="center">📊 Análise de Homicídios Dolosos no Estado do Ceará: perfil das vítimas, tendências e diferenças regionais (2009-2024)</h1>

<p align="center">
  <img alt="R" src="https://img.shields.io/badge/R-4.x-276DC3?logo=r&logoColor=white">
  <img alt="License" src="https://img.shields.io/badge/license-MIT-green">
  <img alt="Status" src="https://img.shields.io/badge/status-ativo-blue">
</p>

<p align="center">
  Repositório dedicado ao projeto desenvolvido durante minha bolsa de iniciação científica enquanto graduando do curso de Estatística da UFC. A análise é focada nos registros de <strong>homicídios dolosos</strong> no Ceará entre 2009 e 2024 por Região Integrada de Segurança (RIS) e por Área Integrada de Segurança Publica (AIS). 
</p>

<hr>

<h2>🎯 Objetivos</h2>
<ul>
  <li>Consolidar séries temporais de homicídios dolosos por município/AIS/ano.</li>
  <li>Calcular taxas por 100 mil habitantes e variações anuais.</li>
  <li>Produzir visualizações claras (tendência, sazonalidade e comparação territorial).</li>
  <li>Gerar tabelas em <em>LaTeX</em> e relatórios (<em>R Markdown/Quarto</em>).</li>
</ul>

<h2>📂 Estrutura do repositório</h2>
<pre><code>.
├── data/
│   ├── raw/            # dados originais (CSV/Excel)
│   ├── interim/        # dados intermediários (limpos)
│   └── processed/      # dados finais prontos para análise
├── R/
│   ├── 00_utils.R      # funções auxiliares (leitura, checagens, formatação)
│   ├── 01_load.R       # importação e padronização
│   ├── 02_clean.R      # tratamento, joins e criação de métricas
│   ├── 03_eda.R        # exploração e gráficos
│   ├── 04_models.R     # (opcional) modelos estatísticos
│   └── 05_export.R     # tabelas LaTeX, CSVs e figuras
├── reports/
│   ├── figs/           # figuras exportadas (PNG/SVG)
│   └── tables/         # tabelas (CSV/LaTeX)
├── README.md (ou README.html)
└── renv/ (opcional)    # isolamento de dependências do R
</code></pre>

<h2>🧰 Requisitos</h2>
<ul>
  <li><strong>R</strong> 4.x</li>
  <li>Pacotes: <code>tidyverse</code>, <code>readxl</code>, <code>janitor</code>, <code>lubridate</code>, <code>scales</code>, <code>sf</code> (se houver mapas), <code>gt</code> ou <code>kableExtra</code> para tabelas.</li>
  <li>(Opcional) <code>renv</code> para reprodutibilidade.</li>
</ul>

<details>
<summary><strong>Instalação rápida dos pacotes</strong></summary>
<pre><code class="language-r">install.packages(c(
  "tidyverse","readxl","janitor","lubridate","scales","sf","gt","kableExtra"
))

# Reprodutibilidade (opcional)
# install.packages("renv"); renv::init()
</code></pre>
</details>

<h2>⚙️ Execução (pipeline sugerido)</h2>
<ol>
  <li>Coloque os dados brutos em <code>data/raw/</code> (ex.: ocorrências por município/ano e população IBGE).</li>
  <li>Rode os scripts na ordem:
    <ul>
      <li><code>R/01_load.R</code> → importa e padroniza colunas (datas, nomes, códigos IBGE).</li>
      <li><code>R/02_clean.R</code> → remove duplicidades, ajusta categorias, cria métricas.</li>
      <li><code>R/03_eda.R</code> → EDA, gráficos e estatísticas descritivas.</li>
      <li><code>R/05_export.R</code> → exporta tabelas/figuras para <code>reports/</code>.</li>
    </ul>
  </li>
</ol>

<h2>📈 Métricas principais</h2>
<ul>
  <li><strong>Contagem</strong> de homicídios dolosos por município/AIS/ano/mês.</li>
  <li><strong>Taxa</strong> por 100.000 habitantes: <code>taxa = (ocorrências / população) * 100000</code>.</li>
  <li><strong>Variação anual</strong> (Δ%): <code>(valor_ano - valor_ano_anterior) / valor_ano_anterior</code>.</li>
</ul>

<h2>🧪 Exemplo de código em R</h2>
<pre><code class="language-r">library(tidyverse)
library(readxl)
library(janitor)
library(lubridate)
library(scales)

# 1) Leitura e padronização -----------------------------------------------
homi &lt;- read_csv("data/raw/homicidios_ce.csv") |&gt;
  clean_names() |&gt;
  mutate(data = ymd(data),
         ano  = year(data),
         mes  = month(data, label = TRUE, abbr = TRUE))

pop  &lt;- read_csv("data/raw/pop_ibge_ce.csv") |&gt;
  clean_names() |&gt;
  select(cod_ibge, municipio, ano, populacao)

# 2) Agregação e taxa por 100k --------------------------------------------
base_anual &lt;- homi |&gt;
  filter(tipo_crime == "Homicídio doloso") |&gt;
  group_by(cod_ibge, municipio, ano) |&gt;
  summarise(ocorrencias = n(), .groups = "drop") |&gt;
  left_join(pop, by = c("cod_ibge","municipio","ano")) |&gt;
  mutate(taxa_100k = (ocorrencias / populacao) * 100000)

# 3) Variação ano-a-ano ----------------------------------------------------
base_anual &lt;- base_anual |&gt;
  arrange(municipio, ano) |&gt;
  group_by(municipio) |&gt;
  mutate(var_yoy = (ocorrencias - lag(ocorrencias)) / lag(ocorrencias)) |&gt;
  ungroup()

# 4) Gráfico de tendência (estado) ----------------------------------------
serie_ce &lt;- base_anual |&gt;
  group_by(ano) |&gt;
  summarise(ocorrencias = sum(ocorrencias),
            populacao  = sum(populacao),
            taxa_100k  = (ocorrencias/populacao)*100000, .groups = "drop")

ggplot(serie_ce, aes(ano, taxa_100k)) +
  geom_line(size = 1) +
  geom_point() +
  scale_x_continuous(breaks = unique(serie_ce$ano)) +
  scale_y_continuous(labels = label_number(accuracy = 0.1, big.mark = ".", decimal.mark = ",")) +
  labs(title = "Ceará — Taxa de Homicídios Dolosos (por 100 mil hab.)",
       x = "Ano", y = "Taxa (por 100 mil)") +
  theme_minimal()

# 5) Exportar resultados ---------------------------------------------------
readr::write_csv(base_anual, "data/processed/homicidios_ce_anual.csv")
ggplot2::ggsave("reports/figs/taxa_ce.png", width = 9, height = 5, dpi = 300)
</code></pre>

<h3>Observação sobre formatação numérica</h3>
<p>Para usar vírgula como separador decimal no R, ajuste os <em>labels</em> no <code>ggplot2</code> (como no exemplo) e, se desejar, defina:</p>
<pre><code class="language-r">options(OutDec = ",")  # afeta impressão em R base
</code></pre>

<h2>🗺️ (Opcional) Mapas temáticos com <code>sf</code></h2>
<p>Se houver <em>shape</em> dos municípios do Ceará, é possível unir por <code>cod_ibge</code> e mapear taxas por 100k:</p>
<pre><code class="language-r">library(sf)
muni &lt;- sf::st_read("data/raw/ce_municipios.geojson") |&gt; clean_names()
mapa &lt;- muni |&gt; left_join(base_anual |&gt; filter(ano == max(ano)), by = "cod_ibge")

ggplot(mapa) +
  geom_sf(aes(fill = taxa_100k)) +
  scale_fill_continuous(labels = label_number(accuracy = 0.1, decimal.mark = ",")) +
  labs(title = "Taxa de Homicídios Dolosos por Município (por 100 mil hab.)",
       fill = "Taxa") +
  theme_minimal()
</code></pre>

<h2>🧾 Boas práticas adotadas</h2>
<ul>
  <li><strong>Tidy data</strong>: variáveis em colunas, observações em linhas.</li>
  <li><strong>Scripts modulares</strong>: carregar → limpar → analisar → exportar.</li>
  <li><strong>Reprodutibilidade</strong>: versão do R e pacotes documentados (opção com <code>renv</code>).</li>
  <li><strong>Documentação</strong>: comentários claros e convenções de nomes.</li>
</ul>

<h2>📜 Licença</h2>
<p>Este projeto é distribuído sob a licença <strong>MIT</strong>. Consulte o arquivo <code>LICENSE</code>.</p>

<h2>🙏 Agradecimentos &amp; Fonte de dados</h2>
<ul>
  <li>Órgãos de segurança pública e estatística que disponibilizam dados administrativos e populacionais.</li>
  <li>Comunidade R por pacotes e exemplos.</li>
</ul>

<h2>📬 Contato</h2>
<p>Em caso de dúvidas, sugestões ou colaborações, abra uma <em>issue</em> ou envie um PR.</p>
<h1 align="center">📈 Análise de Homicídios Dolosos no Estado do Ceará: perfil das vítimas, tendências temporais e diferenças regionais (2009-2024)</h1>

<p align="center">
  <img alt="R" src="https://img.shields.io/badge/R-4.x-276DC3?logo=r&logoColor=white">
  <img alt="License" src="https://img.shields.io/badge/license-MIT-green">
  <img alt="Status" src="https://img.shields.io/badge/status-ativo-blue">
</p>

<p align="center">
  Repositório dedicado ao projeto desenvolvido durante minha bolsa de iniciação científica enquanto graduando do curso de Estatística da UFC. A análise é focada nos registros de <strong>homicídios dolosos</strong> no Ceará entre 2009 e 2024 por Região Integrada de Segurança (RIS) e por Área Integrada de Segurança Publica (AIS). 
</p>

<hr>

<h2>🎯 Objetivos</h2>
<ul>
  <li>Consolidar séries temporais de homicídios dolosos por Região Integrada de Segurança/AIS.</li>
  <li>Produzir visualizações claras (tendência, sazonalidade e comparação territorial).</li>
</ul>

<h2>📂 Estrutura do repositório</h2>
<pre><code>.
├── data/
│   ├── raw/            # dados originais (CSV/Excel)
│   ├── interim/        # dados intermediários (limpos)
│   └── processed/      # dados finais prontos para análise
├── R/
│   ├── 00_utils.R      # funções auxiliares (leitura, checagens, formatação)
│   ├── 01_load.R       # importação e padronização
│   ├── 02_clean.R      # tratamento, joins e criação de métricas
│   ├── 03_eda.R        # exploração e gráficos
│   ├── 04_models.R     # (opcional) modelos estatísticos
│   └── 05_export.R     # tabelas LaTeX, CSVs e figuras
├── reports/
│   ├── figs/           # figuras exportadas (PNG/SVG)
│   └── tables/         # tabelas (CSV/LaTeX)
├── README.md (ou README.html)
└── renv/ (opcional)    # isolamento de dependências do R
</code></pre>

<h2>🧰 Requisitos</h2>
<ul>
  <li><strong>R</strong> 4.x</li>
  <li>Pacotes: <code>tidyverse</code>, <code>readxl</code>, <code>janitor</code>, <code>lubridate</code>, <code>scales</code>, <code>sf</code>, <code>gt</code> ou <code>kableExtra</code> para tabelas.</li>
  <li>(Opcional) <code>renv</code> para reprodutibilidade.</li>
</ul>

<details>
<summary><strong>Instalação rápida dos pacotes</strong></summary>
<pre><code class="language-r">install.packages(c(
  "tidyverse","readxl","janitor","lubridate","scales","sf","gt","kableExtra"
))

# Reprodutibilidade (opcional)
# install.packages("renv"); renv::init()
</code></pre>
</details>

<h2>⚙️ Execução (pipeline sugerido)</h2>
<ol>
  <li>Coloque os dados brutos em <code>data/raw/</code> (ex.: ocorrências por município/ano e população IBGE).</li>
  <li>Rode os scripts na ordem:
    <ul>
      <li><code>R/01_load.R</code> → importa e padroniza colunas (datas, nomes, códigos IBGE).</li>
      <li><code>R/02_clean.R</code> → remove duplicidades, ajusta categorias, cria métricas.</li>
      <li><code>R/03_eda.R</code> → EDA, gráficos e estatísticas descritivas.</li>
      <li><code>R/05_export.R</code> → exporta tabelas/figuras para <code>reports/</code>.</li>
    </ul>
  </li>
</ol>

<h2>📈 Métricas principais</h2>
<ul>
  <li><strong>Contagem</strong> de homicídios dolosos por município/AIS/ano/mês.</li>
  <li><strong>Taxa</strong> por 100.000 habitantes: <code>taxa = (ocorrências / população) * 100000</code>.</li>
  <li><strong>Variação anual</strong> (Δ%): <code>(valor_ano - valor_ano_anterior) / valor_ano_anterior</code>.</li>
</ul>

<h2>🧪 Exemplo de código em R</h2>
<pre><code class="language-r">library(tidyverse)
library(readxl)
library(janitor)
library(lubridate)
library(scales)

# 1) Leitura e padronização -----------------------------------------------
homi &lt;- read_csv("data/raw/homicidios_ce.csv") |&gt;
  clean_names() |&gt;
  mutate(data = ymd(data),
         ano  = year(data),
         mes  = month(data, label = TRUE, abbr = TRUE))

pop  &lt;- read_csv("data/raw/pop_ibge_ce.csv") |&gt;
  clean_names() |&gt;
  select(cod_ibge, municipio, ano, populacao)

# 2) Agregação e taxa por 100k --------------------------------------------
base_anual &lt;- homi |&gt;
  filter(tipo_crime == "Homicídio doloso") |&gt;
  group_by(cod_ibge, municipio, ano) |&gt;
  summarise(ocorrencias = n(), .groups = "drop") |&gt;
  left_join(pop, by = c("cod_ibge","municipio","ano")) |&gt;
  mutate(taxa_100k = (ocorrencias / populacao) * 100000)

# 3) Variação ano-a-ano ----------------------------------------------------
base_anual &lt;- base_anual |&gt;
  arrange(municipio, ano) |&gt;
  group_by(municipio) |&gt;
  mutate(var_yoy = (ocorrencias - lag(ocorrencias)) / lag(ocorrencias)) |&gt;
  ungroup()

# 4) Gráfico de tendência (estado) ----------------------------------------
serie_ce &lt;- base_anual |&gt;
  group_by(ano) |&gt;
  summarise(ocorrencias = sum(ocorrencias),
            populacao  = sum(populacao),
            taxa_100k  = (ocorrencias/populacao)*100000, .groups = "drop")

ggplot(serie_ce, aes(ano, taxa_100k)) +
  geom_line(size = 1) +
  geom_point() +
  scale_x_continuous(breaks = unique(serie_ce$ano)) +
  scale_y_continuous(labels = label_number(accuracy = 0.1, big.mark = ".", decimal.mark = ",")) +
  labs(title = "Ceará — Taxa de Homicídios Dolosos (por 100 mil hab.)",
       x = "Ano", y = "Taxa (por 100 mil)") +
  theme_minimal()

# 5) Exportar resultados ---------------------------------------------------
readr::write_csv(base_anual, "data/processed/homicidios_ce_anual.csv")
ggplot2::ggsave("reports/figs/taxa_ce.png", width = 9, height = 5, dpi = 300)
</code></pre>

<h3>Observação sobre formatação numérica</h3>
<p>Para usar vírgula como separador decimal no R, ajuste os <em>labels</em> no <code>ggplot2</code> (como no exemplo) e, se desejar, defina:</p>
<pre><code class="language-r">options(OutDec = ",")  # afeta impressão em R base
</code></pre>

<h2>🗺️ (Opcional) Mapas temáticos com <code>sf</code></h2>
<p>Se houver <em>shape</em> dos municípios do Ceará, é possível unir por <code>cod_ibge</code> e mapear taxas por 100k:</p>
<pre><code class="language-r">library(sf)
muni &lt;- sf::st_read("data/raw/ce_municipios.geojson") |&gt; clean_names()
mapa &lt;- muni |&gt; left_join(base_anual |&gt; filter(ano == max(ano)), by = "cod_ibge")

ggplot(mapa) +
  geom_sf(aes(fill = taxa_100k)) +
  scale_fill_continuous(labels = label_number(accuracy = 0.1, decimal.mark = ",")) +
  labs(title = "Taxa de Homicídios Dolosos por Município (por 100 mil hab.)",
       fill = "Taxa") +
  theme_minimal()
</code></pre>

<h2>🧾 Boas práticas adotadas</h2>
<ul>
  <li><strong>Tidy data</strong>: variáveis em colunas, observações em linhas.</li>
  <li><strong>Scripts modulares</strong>: carregar → limpar → analisar → exportar.</li>
  <li><strong>Reprodutibilidade</strong>: versão do R e pacotes documentados (opção com <code>renv</code>).</li>
  <li><strong>Documentação</strong>: comentários claros e convenções de nomes.</li>
</ul>

<h2>📜 Licença</h2>
<p>Este projeto é distribuído sob a licença <strong>MIT</strong>. Consulte o arquivo <code>LICENSE</code>.</p>

<h2>🙏 Agradecimentos &amp; Fonte de dados</h2>
<ul>
  <li>Órgãos de segurança pública e estatística que disponibilizam dados administrativos e populacionais.</li>
  <li>Comunidade R por pacotes e exemplos.</li>
</ul>

<h2>📬 Contato</h2>
<p>Em caso de dúvidas, sugestões ou colaborações, abra uma <em>issue</em> ou envie um PR.</p>
