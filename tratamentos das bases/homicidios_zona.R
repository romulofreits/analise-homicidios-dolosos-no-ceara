# Análise das taxas por zona

library(tidyverse)
library(readxl)
library(ggstatsplot)
library(xtable)
library(kableExtra)


homicidios = read_excel('BASES TRATADAS/homicidios_ceara_2009_2024.xlsx')

# Base de homicidios por Região Integrada de Segurança
homicidios_zona = homicidios |> 
  group_by(zona, ano) |> 
  summarise(total_homicidios = sum(total_homicidios, na.rm = TRUE),
            populacao_zona = if(first(zona) == 'Capital') {
              first(populacao)
    } else {
      sum(populacao, na.rm = TRUE)
    },
    .groups = 'drop'
  ) |> 
  mutate(taxa_homicidios = round((total_homicidios / populacao_zona) * 100000, 4)) |> 
  arrange(zona, ano)

homicidios_zona = homicidios_zona |> 
  filter(zona != 'ais nao identificada (fortaleza)')

# salvando a base de dados em formato .xlsx
write_xlsx(homicidios_zona, 'homicidios_zona_2009_2024.xlsx')


tabela_descritiva = homicidios_zona |> 
  group_by(zona) |> 
  summarise(Média = mean(total_homicidios, na.rm = TRUE) |> round(4),
            Mediana = median(total_homicidios, na.rm = TRUE),
            Mínimo = min(total_homicidios, na.rm = TRUE),
            Máximo = max(total_homicidios, na.rm = TRUE),
            Desvio_Padrão = sd(total_homicidios, na.rm = TRUE) |> round(4),
            Assimetria = e1071::skewness(total_homicidios),
            Curtose = e1071::kurtosis(total_homicidios) |> round(4),
            CV = (sd(total_homicidios, na.rm = TRUE) / mean(total_homicidios, na.rm = TRUE)) |> round(4),
            IQR = IQR(total_homicidios, na.rm = TRUE)) |> 
            arrange(desc(Média)) 


violino_zona = homicidios_zona |>
  ggbetweenstats(
    x = zona,
    y = total_homicidios,
    
    type = 'nonparametric',              # Kruskal-Wallis para >2 grupos
    pairwise.comparisons = TRUE,         # ativa comparações par-a-par (Mann-Whitney)
    pairwise.display = 'significant',    # mostra apenas comparações significativas
    bf.message = FALSE,
    messages = FALSE,
    results.subtitle = TRUE,             # exibe resultado do teste global (KW)
    
    centrality.point.args = list(size = 3, color = 'red4'),
    
    plot.type = 'violin',
    violin.args = list(
      fill = 'gray80',
      alpha = 0.7,
      trim = FALSE
    ),
    point.args = list(
      position = position_jitter(width = 0.1),
      alpha = 0.5,
      color = '#440154'
    ),
    centrality.plotting = TRUE,
    centrality.color = 'red',
    
    title = '',
    xlab = '',
    ylab = '',
    caption = ''
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = 'bold', hjust = 0.5)
  )

# Também pode salvar como PDF para usar vetorialmente no LaTeX
ggsave('grafico_violin.pdf', violino_zona,
       width = 12, height = 10, units = 'in')


# Teste não-paramétrico (Kruskal-Wallis se não atender pressupostos)
kruskal.test(total_homicidios ~ zona, data = homicidios_por_zona)








