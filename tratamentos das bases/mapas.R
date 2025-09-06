library(tidyverse)
library(geobr)
library(sf)
library(ggspatial)
library(viridis)
library(readxl)
library(mapview)

homicidios_municipios = read_excel('BASES TRATADAS/homicidios_ceara_2009_2024.xlsx')

# Número total de homicídios por zona e número por 100 mil habitantes 
df_homicidios_zona = read_excel('BASES TRATADAS/homicidios_zona_2009_2024.xlsx')

municipios_ce = geobr::read_municipality(code_muni = 'CE', year = 2022)

# Baixar shapefile dos municípios do Ceará
ceara_shp = geobr::read_municipality(code_muni = 'CE', year = 2020) |> 
  mutate(code_muni = as.character(code_muni))


coordenadas = sf::st_centroid(municipios_ce) |> 
  sf::st_coordinates() |> 
  as.data.frame() |> 
  cbind(municipios_ce |> 
          select(name_muni, code_muni))

coordenadas = coordenadas |> 
  dplyr::rename(cod_municipio = code_muni,
                municipio = name_muni,
                longitude = X,
                latitude = Y)

coordenadas = coordenadas |> 
  mutate(cod_municipio = as.character(cod_municipio))


# homicídios por município
hom_mun_completa = coordenadas  |>  
  left_join(homicidios_municipios  |>  
              select(cod_municipio, ais, zona, mes, ano, total_homicidios, populacao),
            by = 'cod_municipio')

glimpse(hom_mun_completa)



# Precisamos agrupar esses municípios por zona e ano e fazer a união dos polígonos para formar o polígono de cada zona por ano:
zonas_sf = hom_mun_completa |> 
  filter(zona != 'ais nao identificada (fortaleza)') |> 
  group_by(zona, ano) |> 
  summarise(geometry = st_union(geom), .groups = 'drop') |> 
  st_as_sf()

# Permite visualizar o objeto zonas_sf
mapview(zonas_sf)


homicidios_zona_ano = hom_mun_completa |> 
  group_by(zona, ano) |> 
  summarise(total_homicidios = sum(total_homicidios, na.rm = TRUE), 
            .groups = 'drop') |> 
  filter(zona != 'ais nao identificada (fortaleza)')

zonas_com_dados = zonas_sf |> 
  left_join(homicidios_zona_ano, by = c('zona', 'ano'))


mapa_risp_ano = zonas_com_dados |>
  #filter(ano %in% c(2009, 2012, 2015, 2018, 2021, 2024)) |>
  ggplot() +
  geom_sf(aes(fill = total_homicidios), color = 'white', size = 0.2) +
  facet_wrap(~ ano) +
  scale_fill_viridis_c(
    name = 'Total de\nhomicídios',
    option = 'inferno',  # outras opções: "viridis", "magma", "plasma", "cividis"
    direction = -1,
    limits = c(0, 2000),  # ajusta os limites da escala de cores
    breaks = c(0, 500, 1000, 1500, 2000)  # define os valores desejados para as quebras
  ) +
  labs(
    title = NULL,
    subtitle = NULL,
    x = NULL, y = NULL
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 10, face = "bold"),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8)
  )


ggsave('mapa_risp_ano.pdf', mapa_risp_ano,
       width = 12, height = 10, units = 'in')



zonas_com_dados |>
  filter(ano %in% c(2009, 2012, 2015, 2018, 2021, 2024)) |>
  ggplot() +
  geom_sf(aes(fill = total_homicidios), color = 'white', size = 0.2) +
  facet_wrap(~ ano) +
  scale_fill_gradient(
    name = 'Total de\nhomicídios',
    low = 'gray95', high = 'gray10',  # tons de cinza claros a escuros
    limits = c(0, 2000),
    breaks = c(0, 500, 1000, 1500, 2000)
  ) +
  labs(
    title = NULL,
    subtitle = NULL,
    x = NULL, y = NULL
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 10, face = "bold"),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8)
  )


mapa_risp_ano_bw = zonas_com_dados |>
  filter(ano %in% c(2009, 2012, 2015, 2018, 2021, 2024)) |>
  ggplot() +
  geom_sf(aes(fill = total_homicidios), color = 'white', size = 0.2) +
  facet_wrap(~ ano) +
  scale_fill_gradient(
    name = 'Total de\nhomicídios',
    low = 'gray95', high = 'gray10',
    limits = c(0, 2000),
    breaks = c(0, 500, 1000, 1500, 2000),
    guide = guide_colorbar(
      barheight = 10,  # aumenta a altura da barra
      barwidth = 0.7   # largura da barra
    )
  ) +
  labs(
    title = NULL,
    subtitle = NULL,
    x = NULL, y = NULL
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 10, face = "bold"),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8)
  )

ggsave('mapa_risp_ano_bw.pdf', mapa_risp_ano_bw,
       width = 12, height = 10, units = 'in')


mapa_risp_ano_bw = zonas_com_dados |>
  ggplot() +
  geom_sf(aes(fill = total_homicidios), color = 'white', size = 0.2) +
  facet_wrap(~ ano) +
  scale_fill_gradientn(
    name = 'Total de\nhomicídios',
    colours = c('gray95', 'gray80', 'gray60', 'gray40', 'gray20', 'black'),
    limits = c(0, 2000),
    breaks = c(0, 500, 1000, 1500, 2000)
  ) +
  labs(
    title = NULL,
    subtitle = NULL,
    x = NULL, y = NULL
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 10, face = "bold"),
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 9),
    legend.position = "right"
  )



# mapa_risp_ano_cinza = ggplot(zonas_com_dados) +
#   geom_sf(aes(fill = total_homicidios), color = 'white', size = 0.2) +
#   facet_wrap(~ ano) +
#   scale_fill_gradientn(
#     colours = c("grey90", "grey70", "grey50", "grey30", "black"),
#     na.value = "transparent",
#     name = "Total de\nhomicídios"
#   ) +
#   labs(
#     title = NULL,
#     subtitle = NULL,
#     x = NULL, y = NULL
#   ) +
#   theme_minimal() +
#   theme(
#     strip.text = element_text(size = 10, face = "bold"),
#     legend.title = element_text(size = 10),
#     legend.text = element_text(size = 8)
#   )

