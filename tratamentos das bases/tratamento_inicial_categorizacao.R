# Bibliotecas -------------------------------------------------------------
library(tidyverse)
library(readxl)
library(lubridate)
library(janitor)
library(tidyterra)
library(xtable)
library(kableExtra)
library(writexl)
library(RColorBrewer)
library(patchwork)
library(forcats)
library(ggstatsplot)
library(insight)

# Tratamento inicial da base de CVLI
CVLI_2009_2024 = read_excel("CVLI_2009-2024.xlsx")
CVLI_2009_2024 = CVLI_2009_2024 |> 
  mutate(mes = month(Data, label = TRUE, abbr = FALSE),
         ano = year(Data)) |> janitor::clean_names()

# Categorizando a variável idade_da_vitima de acordo com o atlas da violencia
CVLI_2009_2024 = CVLI_2009_2024 |> 
  mutate(faixa_etaria = case_when(
    idade_da_vitima == 'Não Informada' ~ 'Não Informada',
    as.numeric(idade_da_vitima) >= 0 & as.numeric(idade_da_vitima) <= 9 ~ '0 a 9',
    as.numeric(idade_da_vitima) >= 10 & as.numeric(idade_da_vitima) <= 14 ~ '10 a 14',
    as.numeric(idade_da_vitima) >= 15 & as.numeric(idade_da_vitima) <= 19 ~ '15 a 19',
    as.numeric(idade_da_vitima) >= 20 & as.numeric(idade_da_vitima) <= 29 ~ '20 a 29',
    as.numeric(idade_da_vitima) >= 30 & as.numeric(idade_da_vitima) <= 39 ~ '30 a 39',
    as.numeric(idade_da_vitima) >= 40 & as.numeric(idade_da_vitima) <= 49 ~ '40 a 49',
    as.numeric(idade_da_vitima) >= 50 & as.numeric(idade_da_vitima) <= 59 ~ '50 a 59',
    as.numeric(idade_da_vitima) >= 60 ~ "60 ou mais",
    TRUE ~ NA_character_))



# Categorizando as AIS de acordo com a 'zona' correspondente
CVLI_2009_2024 = CVLI_2009_2024 |> 
  mutate(
    risp = case_when(
      ais %in% c('AIS 01', 'AIS 02', 'AIS 03', 'AIS 04', 'AIS 05', 
                 'AIS 06', 'AIS 07', 'AIS 08', 'AIS 09', 'AIS 10') ~ "Capital",
      ais %in% c('AIS 11', 'AIS 12', 'AIS 13', 'AIS 23', 'AIS 24', 'AIS 25') ~ "Região Metropolitana",
      ais %in% c('AIS 14', 'AIS 15', 'AIS 16', 'AIS 17') ~ "Interior Norte",
      ais %in% c('AIS 18', 'AIS 19', 'AIS 20', 'AIS 21', 'AIS 22') ~ "Interior Sul",
      ais == 'AIS Não Identificada (Fortaleza)' ~ 'ais nao identificada (fortaleza)',
      TRUE ~ NA_character_))

# salvando a base de dados em formato .xlsx
write_xlsx(CVLI_2009_2024, 'CVLI_2009_2024_tratada_2009_2024.xlsx')



# Base homicidios agrupada por zona
homicidios_zona = CVLI_2009_2024 |> 
  filter(natureza == 'HOMICIDIO DOLOSO') |> 
  group_by(zona, ano)



# Perfil médio (Homicídio Doloso) por zona
CVLI_2009_2024 |> 
  filter(zona != 'ais nao identificada (fortaleza)', 
         natureza == 'HOMICIDIO DOLOSO') |> 
  group_by(zona, ano, ais)  |>  
  summarise(total_registros = n(), .groups = 'drop') |> 
  group_by(zona, ano) |>  
  summarise(media_registros = mean(total_registros), .groups = 'drop') |>  
  ggplot(aes(x = ano, y = media_registros, color = zona)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = seq(min(CVLI_2009_2024$ano), 
                                  max(CVLI_2009_2024$ano), by = 1), 
                     guide = guide_axis(angle = 45)) +
  scale_y_continuous(limits = c(0, NA),
                     expand = expansion(mult = c(0, 0.1))) +
  labs(x = '',y = '', color = '') +
  theme_minimal() +
  theme(legend.position = 'top',
        plot.title = element_text(hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size = 8)) +
  scale_color_brewer(palette = 'Set1')


# Boxplot de mes e zona
CVLI_2009_2024 |>
  filter(natureza == 'HOMICIDIO DOLOSO') |> 
  group_by(mes, ais) |>
  summarise(n = n(), .groups = 'drop') |>
  ggplot(aes(x = factor(mes), y = n)) +
  geom_boxplot(fill = 'gray', alpha = 0.7) +
  geom_jitter() +
  labs(x = '', y = '') +
  theme_minimal()

# Gráficos de violinos para as distribuições mensais e anuais por AIS

# CVLI_2009_2024 |>
#   filter(zona != 'ais nao identificada (fortaleza)',
#          natureza == 'HOMICIDIO DOLOSO') |>
#   group_by(mes, ais) |>
#   summarise(total_homicidios = n(), .groups = 'drop') |>
#   #mutate(mes = factor(mes, levels = month.name)) |>
#   ggbetweenstats(
#     x = mes,
#     y = total_homicidios,
#     
#     # Desativa o teste estatístico
#     type = "nonparametric",              # evita ANOVA
#     pairwise.comparisons = FALSE,        # sem comparações par-a-par
#     bf.message = FALSE,                  # remove aviso de Bayes
#     messages = FALSE,                    # remove mensagens do pacote
#     results.subtitle = FALSE,            # remove subtítulo com estatísticas
#     
#     # Gráfico de violino
#     plot.type = "violin",
#     violin.args = list(
#       fill = "gray80",
#       alpha = 0.7,
#       trim = FALSE
#     ),
#     point.args = list(
#       position = position_jitter(width = 0.1),
#       alpha = 0.5,
#       color = "#440154"
#     ),
#     centrality.plotting = TRUE,
#     centrality.color = "red",
#     
#     # Títulos
#     title = '',
#     xlab = '',
#     ylab = '',
#     caption = ''
#   ) +
#   theme_minimal() +
#   theme(
#     axis.text.x = element_text(angle = 45, hjust = 1),
#     plot.title = element_text(face = "bold", hjust = 0.5)
#   )

# CVLI_2009_2024 |>
#   filter(zona != 'ais nao identificada (fortaleza)',
#          natureza == 'HOMICIDIO DOLOSO') |>
#   group_by(mes, ais) |>
#   summarise(total_homicidios = n(), .groups = 'drop') |>
#   ggbetweenstats(
#     x = mes,
#     y = total_homicidios,
#     
#     # Desativa o teste estatístico
#     type = "none",              # evita ANOVA
#     pairwise.comparisons = FALSE,        # sem comparações par-a-par
#     bf.message = FALSE,                  # remove aviso de Bayes
#     messages = FALSE,                    # remove mensagens do pacote
#     results.subtitle = FALSE, 
#     pairwise.display = 'none',
#     
#     centrality.point.args = list(size = 3, color = 'red4'),  # Ponto de média
#     
#     # Gráfico de violino
#     plot.type = "violin",
#     violin.args = list(
#       fill = "gray80",
#       alpha = 0.7,
#       trim = FALSE
#     ),
#     point.args = list(
#       position = position_jitter(width = 0.1),
#       alpha = 0.5,
#       color = "#440154"
#     ),
#     centrality.plotting = TRUE,
#     centrality.color = "red",
#     
#     # Títulos
#     title = '',
#     xlab = '',
#     ylab = '',
#     caption = ''
#   ) +
#   theme_minimal() +
#   theme(
#     axis.text.x = element_text(angle = 45, hjust = 1),
#     plot.title = element_text(face = "bold", hjust = 0.5)
#   )


# Boxplot com a distribuição do total de homicidios anual por AIS
cores_ais = c(
  "#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00",
  "#FFFF33", "#A65628", "#F781BF", "#999999", "#66C2A5",
  "#FC8D62", "#8DA0CB", "#E78AC3", "#A6D854", "#FFD92F",
  "#E5C494", "#B3B3B3", "#1B9E77", "#D95F02", "#7570B3",
  "#E7298A", "#66A61E", "#E6AB02", "#A6761D", "#666666"
)


boxplot_ano_ais = CVLI_2009_2024 |>
  filter(zona != 'ais nao identificada (fortaleza)',
         natureza == 'HOMICIDIO DOLOSO') |>
  group_by(ano, ais) |>
  summarise(total_homicidios = n(), .groups = 'drop') |>
  ggplot(aes(x = as.factor(ano), y = total_homicidios)) + 
  geom_boxplot(outlier.shape = NA, fill = 'white', alpha = 0.7) +
  geom_jitter(aes(color = ais),
              width = 0.15,
              alpha = 0.6,
              size = 2,
              show.legend = TRUE) +
  scale_color_manual(values = cores_ais, name = "AIS") +  # cores manuais
  labs(x = '', y = '', title = '', caption = '') +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(face = 'bold', hjust = 0.5)) +
  theme(legend.position = 'right', legend.justification = 'center')


ggsave('boxplot_ano_ais.pdf', boxplot_ano_ais,
       width = 14, height = 10, units = 'in')




