# Análise das taxas por zona

library(tidyverse)
library(readxl)
library(ggstatsplot)
library(xtable)
library(kableExtra)
library(FSA)
library(trend)


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


# Analisando suposições de normalidade
by(homicidios_zona$total_homicidios, homicidios_zona$zona, shapiro.test)

ggplot(homicidios_zona, aes(sample = total_homicidios)) +
  stat_qq() +
  stat_qq_line() +
  facet_wrap(~ zona, scales = 'free')

# Teste de Levene para homogeneidade de variâncias
car::leveneTest(total_homicidios ~ zona, data = homicidios_zona)
bartlett.test(total_homicidios ~ zona, data = homicidios_zona)

# Verificando independencia das observações
par(mfrow(c(2,2)))
acf(ts(homicidios_zona$total_homicidios[homicidios_zona$zona == 'Capital']))
acf(ts(homicidios_zona$total_homicidios[homicidios_zona$zona == 'Interior Sul']))
acf(ts(homicidios_zona$total_homicidios[homicidios_zona$zona == 'Interior Norte']))
acf(ts(homicidios_zona$total_homicidios[homicidios_zona$zona == 'Região Metropolitana']))

# Teste não-paramétrico (Kruskal-Wallis se não atender pressupostos)
kruskal.test(total_homicidios ~ zona, data = homicidios_zona)
kruskal.test(taxa_homicidios ~ zona, data = homicidios_zona)

FSA::dunnTest(total_homicidios ~ zona, data = homicidios_zona, method = 'bonferroni')
FSA::dunnTest(taxa_homicidios ~ zona, data = homicidios_zona, method = 'bonferroni')

# ANALISANDO A TENDÊNCIA DE CADA RISP

# # VPA (%) para cada ano
# homicidios_zona |> 
#   arrange(zona, ano) |> 
#   group_by(zona) |> 
#   mutate(variacao_percentual = (total_homicidios / lag(total_homicidios) - 1) * 100) |> View()
# 
# # VPA (%) 2009 - 2024
# homicidios_zona |> 
#   group_by(zona) |> 
#   summarise(ano_inicial = first(ano[order(ano)]),
#             ano_final   = last(ano[order(ano)]),
#             valor_inicial = first(total_homicidios[order(ano)]),
#             valor_final   = last(total_homicidios[order(ano)]),
#             variacao_percentual = ((valor_final - valor_inicial) / valor_inicial) * 100)

# Teste de não-paramétrico de Mann-Kendall

homicidios_zona |> 
  group_by(zona) |> 
  group_map(~ mk.test(.x$total_homicidios), .keep = TRUE)


# Opção paramétrico por meio de regressao linear
library(broom)
modelo_capital = lm(total_homicidios ~ as.numeric(ano), data = homicidios_zona |>
                      filter(zona == 'Capital'))
summary(modelo_capital)

homicidios_zona |> 
  mutate(ano = as.numeric(as.character(ano))) |> 
  group_by(zona) |> 
  do(tidy(lm(total_homicidios ~ ano, data = .))) |> 
  filter(term == 'ano') |> 
  mutate(tendencia = case_when(
    p.value < 0.05 & estimate > 0 ~ 'Crescente',
    p.value < 0.05 & estimate < 0 ~ 'Decrescente',
    TRUE ~ 'Estacionária'
  ))

