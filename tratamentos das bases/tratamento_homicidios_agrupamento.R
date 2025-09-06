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

# base com as estimativas populacionais por municipio
base_populacao_2009_2024 = read_excel('estimativas_populacao_municipios_ceara_2009_2024.xlsx')

CVLI_2009_2024 = read_excel('BASES TRATADAS/CVLI_2009_2024_tratada_2009_2024.xlsx')


# ANALISANDO OS HOMICIDIOS DOLOSOS

# verificando os nomes dos municípios que aparecem em ambas as bases
identical(sort(unique(CVLI_2009_2024$municipio)), 
          sort(unique(base_populacao_2009_2024$municipio)))

CVLI_2009_2024 = CVLI_2009_2024 |> 
  mutate(municipio = tolower(municipio),
         municipio = iconv(municipio, to = "ASCII//TRANSLIT"))

base_populacao_2009_2024 = base_populacao_2009_2024 |> 
  mutate(municipio = tolower(municipio),
         municipio = iconv(municipio, to = "ASCII//TRANSLIT"))

setdiff(unique(CVLI_2009_2024$municipio), 
        unique(base_populacao_2009_2024$municipio))

setdiff(unique(base_populacao_2009_2024$municipio), 
        unique(CVLI_2009_2024$municipio))

base_populacao_2009_2024 = base_populacao_2009_2024 |> 
  mutate(municipio = case_when(municipio == 'itapage' ~ 'itapaje', TRUE ~ municipio))


# BASE DE HOMICIDIOS DOLOSOS -----

# pivotando a base populacao
pivot_base_pop = base_populacao_2009_2024 |> 
  pivot_longer(cols = starts_with('populacao_'), 
               names_to = 'ano',values_to = 'populacao') |> 
  mutate(ano = str_remove(ano, 'populacao_')) |> 
  select(municipio, ano, populacao, cod_municipio)


relacao_municipio_ais_zona = CVLI_2009_2024 |> 
  distinct(municipio, ais, zona)

relacao_municipio_ais_zona |> 
  count(municipio) |> 
  filter(n > 1)


# criando a base com todos os municipios mesmo que sem registros 
grade_completa = expand_grid(
  municipio = unique(base_populacao_2009_2024$municipio),
  mes = unique(CVLI_2009_2024$mes),                        
  ano = 2009:2024) |> 
  left_join(relacao_municipio_ais_zona, by = 'municipio')

base_homicidio_doloso = CVLI_2009_2024 |> 
  filter(natureza == 'HOMICIDIO DOLOSO') |> 
  group_by(municipio, ais, zona, mes, ano) |>
  summarise(total_homicidios = n(), .groups = 'drop')

base_homicidio_doloso = grade_completa |> 
  left_join(base_homicidio_doloso, 
            by = c('municipio', 'ais', 'zona', 'mes', 'ano'),
            relationship = 'many-to-many') |> 
  replace_na(list(total_homicidios = 0))


base_homicidio_doloso = base_homicidio_doloso |> 
  mutate(ano = as.character(ano))

base_homicidio_doloso = base_homicidio_doloso |> 
  mutate(mes = as.factor(mes))


base_homicidio_doloso = base_homicidio_doloso |> 
  left_join(pivot_base_pop, by = c('municipio', 'ano')) |> 
  group_by(municipio) |> 
  mutate(cod_municipio = first(cod_municipio)) |> 
  ungroup() |> 
  select(cod_municipio, municipio, ais, zona, mes, ano, total_homicidios, populacao)

# salvando a base de dados em formato .xlsx
write_xlsx(base_homicidio_doloso, 'homicidios_ceara_2009_2024.xlsx')


# Caracterização das vítimas de homicídio doloso

# Gênero das vítimas de homicídio doloso
CVLI_2009_2024 |> 
  filter(natureza == 'HOMICIDIO DOLOSO') |> 
  count(genero) |> 
  mutate(percentual = round((n / sum(n)) * 100, 2))


genero = CVLI_2009_2024 |> 
  filter(natureza == 'HOMICIDIO DOLOSO') |> 
  count(genero) |> 
  mutate(percentual = round((n / sum(n)) * 100, 2)) |> 
  ggplot(aes(x = reorder(genero, -percentual), y = percentual)) +
  geom_col(fill = '#0F4659') +
  geom_text(aes(label = paste0(n, ' (', percentual, '%)')), 
            vjust = -0.5, size = 4, fontface = 'bold') +
  labs(x = '', y = '') +
  theme_minimal() +
  theme(
    axis.title = element_text(face = 'bold'),
    axis.text.x = element_text(face = 'bold'),
    axis.text.y = element_blank(),        # remove os valores do eixo y
    axis.ticks.y = element_blank(),       # remove os traços do eixo y
    plot.title = element_text(hjust = 0.5, face = 'bold')
  ) +
  ylim(0, NA)

ggsave('genero.pdf', genero,
       width = 10, height = 6, units = 'in')




# Raça das vítimas de homicídio doloso
CVLI_2009_2024 |> 
  filter(natureza == 'HOMICIDIO DOLOSO') |> 
  count(raca_da_vitima) |> 
  mutate(percentual = round((n / sum(n)) * 100, 2)) |> 
  arrange(desc(n))

raca = CVLI_2009_2024 |> 
  filter(natureza == 'HOMICIDIO DOLOSO') |> 
  count(raca_da_vitima) |> 
  mutate(percentual = round((n / sum(n)) * 100, 2)) |> 
  ggplot(aes(x = reorder(raca_da_vitima, -percentual), y = percentual)) +
  geom_col(fill = '#0F4659') +
  geom_text(aes(label = paste0(n, ' (', percentual, '%)')), 
            vjust = -0.5, size = 4, fontface = 'bold') +
  labs(x = '', y = '') +
  theme_minimal() +
  theme(
    axis.title = element_text(face = 'bold'),
    axis.text.x = element_text(face = 'bold'),
    axis.text.y = element_blank(),        # remove os valores do eixo y
    axis.ticks.y = element_blank(),       # remove os traços do eixo y
    plot.title = element_text(hjust = 0.5, face = 'bold')
  ) +
  ylim(0, NA)

ggsave('raca.pdf', raca,
       width = 10, height = 6, units = 'in')

# Meio utilizado para o crime 
CVLI_2009_2024 |> 
  filter(natureza == 'HOMICIDIO DOLOSO') |> 
  count(meio_empregado) |> 
  mutate(percentual = round((n / sum(n)) * 100, 2)) |> 
  arrange(desc(n))

meio = CVLI_2009_2024 |> 
  filter(natureza == 'HOMICIDIO DOLOSO') |> 
  count(meio_empregado) |> 
  mutate(percentual = round((n / sum(n)) * 100, 2)) |> 
  ggplot(aes(x = reorder(meio_empregado, -percentual), y = percentual)) +
  geom_col(fill = '#0F4659') +
  geom_text(aes(label = paste0(n, ' (', percentual, '%)')), 
            vjust = -0.5, size = 4, fontface = 'bold') +
  labs(x = '', y = '') +
  theme_minimal() +
  theme(
    axis.title = element_text(face = 'bold'),
    axis.text.x = element_text(face = 'bold'),
    axis.text.y = element_blank(),        # remove os valores do eixo y
    axis.ticks.y = element_blank(),       # remove os traços do eixo y
    plot.title = element_text(hjust = 0.5, face = 'bold')
  ) +
  ylim(0, NA)

ggsave('meio.pdf', meio,
       width = 10, height = 6, units = 'in')

# Dia da semana do crime 
CVLI_2009_2024 |> 
  filter(natureza == 'HOMICIDIO DOLOSO') |> 
  count(dia_da_semana) |> 
  mutate(percentual = round((n / sum(n)) * 100, 2)) |> 
  arrange(desc(n))


semana = CVLI_2009_2024 |> 
  filter(natureza == 'HOMICIDIO DOLOSO') |> 
  count(dia_da_semana) |> 
  mutate(percentual = round((n / sum(n)) * 100, 2)) |> 
  ggplot(aes(x = reorder(dia_da_semana, -percentual), y = percentual)) +
  geom_col(fill = '#0F4659') +
  geom_text(aes(label = paste0(n, ' (', percentual, '%)')), 
            vjust = -0.5, size = 4, fontface = 'bold') +
  labs(x = '', y = '') +
  theme_minimal() +
  theme(
    axis.title = element_text(face = 'bold'),
    axis.text.x = element_text(face = 'bold'),
    axis.text.y = element_blank(),        # remove os valores do eixo y
    axis.ticks.y = element_blank(),       # remove os traços do eixo y
    plot.title = element_text(hjust = 0.5, face = 'bold')
  ) +
  ylim(0, NA)

ggsave('semana.pdf', semana,
       width = 10, height = 6, units = 'in')


# Faixa-Etária das vítimas  
CVLI_2009_2024 |> 
  filter(natureza == 'HOMICIDIO DOLOSO') |> 
  count(faixa_etaria) |> 
  mutate(percentual = round((n / sum(n)) * 100, 2)) |> 
  arrange(desc(n))

idade = CVLI_2009_2024 |> 
  filter(natureza == 'HOMICIDIO DOLOSO') |> 
  count(faixa_etaria) |> 
  mutate(percentual = round((n / sum(n)) * 100, 2)) |> 
  ggplot(aes(x = reorder(faixa_etaria, -percentual), y = percentual)) +
  geom_col(fill = '#0F4659') +
  geom_text(aes(label = paste0(n, ' (', percentual, '%)')), 
            vjust = -0.5, size = 4, fontface = 'bold') +
  labs(x = '', y = '') +
  theme_minimal() +
  theme(
    axis.title = element_text(face = 'bold'),
    axis.text.x = element_text(face = 'bold'),
    axis.text.y = element_blank(),        # remove os valores do eixo y
    axis.ticks.y = element_blank(),       # remove os traços do eixo y
    plot.title = element_text(hjust = 0.5, face = 'bold')
  ) +
  ylim(0, NA)

ggsave('idade.pdf', idade,
       width = 10, height = 6, units = 'in')

# Escolaridade das vítimas  
CVLI_2009_2024 |> 
  filter(natureza == 'HOMICIDIO DOLOSO') |> 
  count(escolaridade_da_vitima) |> 
  mutate(percentual = round((n / sum(n)) * 100, 2)) |> 
  arrange(desc(n))


escolaridade = CVLI_2009_2024 |> 
  filter(natureza == 'HOMICIDIO DOLOSO') |> 
  count(escolaridade_da_vitima) |> 
  mutate(percentual = round((n / sum(n)) * 100, 2)) |> 
  ggplot(aes(x = reorder(escolaridade_da_vitima, percentual), y = percentual)) +
  geom_col(fill = '#0F4659') +
  geom_text(aes(label = paste0(n, ' (', percentual, '%)')), 
            hjust = -0.1, size = 4, fontface = 'bold') +
  labs(x = '', y = '') +
  theme_minimal() +
  theme(
    axis.title = element_text(face = 'bold'),
    axis.text.y = element_text(face = 'bold'),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    plot.title = element_text(hjust = 0.5, face = 'bold'),
    plot.margin = margin(5, 40, 5, 5),   # margem extra à direita
    ) +
  coord_flip(clip = 'off') +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))


ggsave('escolaridade.pdf', escolaridade,
       width = 12, height = 8, units = 'in')



