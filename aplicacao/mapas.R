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


mapa_risp_ano = ggplot(zonas_com_dados) +
  geom_sf(aes(fill = total_homicidios), color = 'white', size = 0.2) +
  facet_wrap(~ ano, nrow = 3) +
  scale_fill_viridis_c(
    name = 'Total de\nhomicídios',
    option = 'inferno',  # outras opções: "viridis", "magma", "plasma", "cividis"
    direction = -1,
    limits = c(0, 2000),
    breaks = c(0, 250, 500, 750, 1000, 1250, 1500, 1750, 2000),
    guide = guide_colorbar(
      barwidth = 0.5,
      barheight = 12,
      title.position = 'top'
    )
) +
  labs(
    title = NULL,
    subtitle = NULL,
    x = NULL, y = NULL
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 10, face = 'bold'),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8),
    panel.spacing = unit(0.3, 'lines'),
    plot.margin = margin(0, 0, 0, 0)
  )

# Salvar sem bordas laterais usando grid.draw + pdf
pdf('mapa_risp_ano.pdf', width = 12, height = 10, useDingbats = FALSE)
grid::grid.newpage()
grid::grid.draw(ggplotGrob(mapa_risp_ano))
dev.off()

# ggsave('mapa_risp_ano.pdf', mapa_risp_ano,
#        width = 12, height = 10, units = 'in')
# 
# grob_plot = gridExtra::arrangeGrob(mapa_risp_ano)  # transforma o ggplot em grob
# ggsave("mapa_risp_ano_min_borda.pdf", grob_plot, 
#        width = 12, height = 6, units = "in")

zonas_validas = zonas_com_dados |>
  mutate(geometry = st_make_valid(geometry))

zona_codigos = tibble::tibble(
  zona = c('Capital', 'Região Metropolitana', 'Interior Norte', 'Interior Sul'),
  codigo_zona = c('1', '2', '3', '4')
)


centroides_zonas = zonas_validas |>
  group_by(zona, ano) |>
  summarise(geometry = st_union(geometry), .groups = 'drop') |>
  st_centroid(of_largest_polygon = TRUE)

# Adicionar coordenadas e códigos
coords_zonas = st_coordinates(centroides_zonas)

centroides_zonas = cbind(centroides_zonas, coords_zonas) |>
  left_join(zona_codigos, by = 'zona') |> 
  mutate(Y = ifelse(codigo_zona == '1', Y + 0.4, Y))

  
mapa_risp = ggplot(zonas_com_dados) +
  geom_sf(aes(fill = total_homicidios), color = 'white', size = 0.2) +
  facet_wrap(~ ano) +
  scale_fill_gradientn(
    colours = c('grey90', 'grey70', 'grey50', 'grey30', 'black'),
    na.value = 'transparent',
    name = 'Total de\nhomicídios',
    limits = c(0, 2000),
    breaks = c(0, 250, 500, 750, 1000, 1250, 1500, 1750, 2000),
    guide = guide_colorbar(
      barwidth = 0.5,
      barheight = 12,
      title.position = 'top'
    )
  ) +
  shadowtext::geom_shadowtext(
    data = centroides_zonas,
    aes(x = X, y = Y, label = codigo_zona),
    color = 'white',      # cor do texto
    bg.color = 'black',    # cor do contorno
    bg.r = 0.1,
    size = 3.5,
    fontface = 'bold'
  ) +
  labs(
    title = NULL,
    subtitle = NULL,
    x = NULL, y = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.major.x = element_blank(),  # remove grade vertical
    panel.grid.major.y = element_line(color = 'grey80', size = 0.3),
    panel.grid.minor = element_blank(),
    axis.text.x = element_blank(),       # remove coordenadas X
    axis.text.y = element_text(size = 8),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_line(size = 0.2, color = 'grey70'),
    strip.text = element_text(size = 10, face = 'bold'),
    legend.title = element_text(size = 9, face = 'bold'),
    legend.text = element_text(size = 8),
    legend.position = 'right',
    plot.margin = margin(t = 5, r = 5, b = 5, l = 5)
  )

ggsave(
  filename = 'mapa_risp.pdf',
  plot = mapa_risp,
  width = 12,       # ou maior, ex: 14, 16, dependendo do espaço desejado
  height = 10,      # altura maior para ocupar quase uma página A4 vertical
  units = 'in',
  dpi = 600,        # DPI alto garante boa qualidade para raster (opcional em PDF)
  device = cairo_pdf  # usa Cairo para melhor compatibilidade com curvas e fontes no LaTeX
)






##- Análise do Interior Norte

# 1. Filtrar apenas a zona Interior Norte
interior_norte = hom_mun_completa |> 
  filter(zona == 'Interior Norte')

# 2. Agregar os polígonos por AIS e ano (unir geometria por AIS)
ais_sf = interior_norte |> 
  group_by(ais, ano) |> 
  summarise(geometry = st_union(geom), .groups = 'drop') |> 
  st_as_sf()

# 3. Agregar dados de homicídios por AIS e ano
homicidios_ais_ano = interior_norte |> 
  group_by(ais, ano) |> 
  summarise(total_homicidios = sum(total_homicidios, na.rm = TRUE),
            .groups = 'drop')


# 4. Unir geometria com dados
ais_com_dados = ais_sf |> 
  left_join(homicidios_ais_ano, by = c('ais', 'ano'))


# 1. Corrigir geometria
ais_validos = ais_com_dados |> 
  mutate(geometry = st_make_valid(geometry))

# 2. Agrupar por AIS e ano e calcular os centróides
ais_union = ais_validos |>
  group_by(ais, ano) |>
  summarise(geometry = st_union(geometry), .groups = 'drop')

# 3. Calcular centróide de cada polígono unificado
ais_centroides = st_centroid(ais_union, of_largest_polygon = TRUE)

# 4. Extrair coordenadas
coords = st_coordinates(ais_centroides)

# 5. Unir coordenadas ao `ais_union`
centroides_ais = cbind(ais_union, coords) |> 
  mutate(label = sub('AIS ', '', ais))


# 5. Gerar o mapa
mapa_ais_interior_norte = ggplot(ais_com_dados) +
  geom_sf(aes(fill = total_homicidios), color = 'white', size = 0.2) +
  facet_wrap(~ ano) +
  scale_fill_gradientn(
    colours = c("grey95", "grey80", "grey60", "grey40", "black"),
    na.value = 'transparent',
    name = 'Total de\nhomicídios',
    limits = c(0, 350),
    breaks = c(0, 50, 100, 150, 200, 250, 300, 350),
    guide = guide_colorbar(
      barwidth = 0.5,
      barheight = 12,
      title.position = "top"
    )
  ) +
  labs(
    title = NULL,
    subtitle = NULL,
    caption = NULL,
    x = NULL,
    y = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.major.x = element_blank(),  # sem grade vertical
    panel.grid.major.y = element_line(color = "grey80", size = 0.3),
    panel.grid.minor = element_blank(),
    axis.text.x = element_blank(),  # remove coordenadas X
    axis.text.y = element_text(size = 8),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_line(size = 0.2, color = "grey70"),
    strip.text = element_text(size = 10, face = "bold"),
    plot.title = element_text(face = "bold", size = 12, hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5),
    plot.caption = element_text(size = 8),
    legend.title = element_text(size = 9, face = "bold"),
    legend.text = element_text(size = 8),
    legend.position = "right",
    plot.margin = margin(t = 5, r = 5, b = 5, l = 5)
  ) +
  shadowtext::geom_shadowtext(
    data = centroides_ais,
    aes(x = X, y = Y, label = label),
    color = "white",       # cor do texto
    bg.color = "gray",    # cor do contorno
    bg.r = 0.1,            # raio do contorno (ajuste se quiser mais espesso)
    size = 3.5,
    fontface = "bold"
  )


ggsave(
  filename = 'mapa_ais_interior_norte.pdf',
  plot = mapa_ais_interior_norte,
  width = 12,       # ou maior, ex: 14, 16, dependendo do espaço desejado
  height = 10,      # altura maior para ocupar quase uma página A4 vertical
  units = 'in',
  dpi = 600,        # DPI alto garante boa qualidade para raster (opcional em PDF)
  device = cairo_pdf  # usa Cairo para melhor compatibilidade com curvas e fontes no LaTeX
)



##- Análise do total de homicidios nas AIS
df_interior_norte = homicidios_municipios |> 
  filter(zona == 'Interior Norte')

df_interior_norte |> 
  group_by(ais, ano) |> 
  summarise(total_homicidios = sum(total_homicidios, na.rm = TRUE), .groups = 'drop')



df_interior_norte |> 
  group_by(ais, ano) |> 
  summarise(total_homicidios = sum(total_homicidios, na.rm = TRUE), .groups = 'drop_last') |> 
  arrange(ais, ano) |> 
  mutate(vpa = (total_homicidios - lag(total_homicidios)) / lag(total_homicidios) * 100) |> View()




homicidios_municipios |>
  filter(zona != 'ais nao identificada (fortaleza)') |> 
  group_by(zona, ano) |> 
  summarise(total_homicidios = sum(total_homicidios, na.rm = TRUE), .groups = 'drop') |>
  arrange(zona, ano) |> 
  mutate(vpa = (total_homicidios - lag(total_homicidios)) / lag(total_homicidios) * 100) |> View()
  






mapa_ais_interior_norte_color = ggplot(
  ais_com_dados |> 
    filter(ano %in% c('2009', '2012', '2015', '2018', '2021', '2024'))
) +
  geom_sf(aes(fill = total_homicidios), color = 'white', size = 0.2) +
  facet_wrap(~ ano, nrow = 3) +
  scale_fill_viridis_c(
    option = 'inferno',     # você também pode testar: 'magma', 'plasma', 'cividis'
    direction = -1,         # -1 deixa valores altos mais escuros
    na.value = 'gray90',
    name = 'Total de\nhomicídios',
    limits = c(0, 350),
    breaks = c(0, 50, 100, 150, 200, 250, 300, 350),
    guide = guide_colorbar(
      barwidth = 0.5,
      barheight = 12,
      title.position = 'top'
    )
  ) +
  labs(
    title = NULL,
    subtitle = NULL,
    caption = NULL,
    x = NULL,
    y = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = 'grey80', size = 0.3),
    panel.grid.minor = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 8),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_line(size = 0.2, color = 'grey70'),
    strip.text = element_text(size = 10, face = 'bold'),
    plot.title = element_text(face = 'bold', size = 12, hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5),
    plot.caption = element_text(size = 8),
    legend.title = element_text(size = 9, face = 'bold'),
    legend.text = element_text(size = 8),
    legend.position = 'right',
    plot.margin = margin(t = 5, r = 5, b = 5, l = 5)
  ) +
  shadowtext::geom_shadowtext(
    data = centroides_ais |> filter(ano %in% c('2009', '2012', '2015', '2018', '2021', '2024')),
    aes(x = X, y = Y, label = label),
    color = 'white',
    bg.color = 'black',
    bg.r = 0.1,
    size = 3.5,
    fontface = 'bold'
  )


mapa_ais_interior_norte_color = ggplot(
  ais_com_dados  # Sem filtro de ano
) +
  geom_sf(aes(fill = total_homicidios), color = 'white', size = 0.2) +
  facet_wrap(~ ano, nrow = 3) +
  scale_fill_viridis_c(
    option = 'inferno',
    direction = -1,
    na.value = 'gray90',
    name = 'Total de\nhomicídios',
    limits = c(0, 350),
    breaks = seq(0, 350, 50),
    guide = guide_colorbar(
      barwidth = 0.5,
      barheight = 12,
      title.position = 'top'
    )
  ) +
  labs(
    title = NULL,
    subtitle = NULL,
    caption = NULL,
    x = NULL,
    y = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = 'grey80', size = 0.3),
    panel.grid.minor = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 8),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_line(size = 0.2, color = 'grey70'),
    strip.text = element_text(size = 10, face = 'bold'),
    plot.title = element_text(face = 'bold', size = 12, hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5),
    plot.caption = element_text(size = 8),
    legend.title = element_text(size = 9, face = 'bold'),
    legend.text = element_text(size = 8),
    legend.position = 'right',
    plot.margin = margin(0, 0, 0, 0)
  ) +
  shadowtext::geom_shadowtext(
    data = centroides_ais,  # Sem filtro de ano
    aes(x = X, y = Y, label = label),
    color = 'white',
    bg.color = 'black',
    bg.r = 0.1,
    size = 3.5,
    fontface = 'bold'
  )






ggsave(
  filename = 'mapa_ais_interior_norte_color.pdf',
  plot = mapa_ais_interior_norte_color,
  width = 12,       # ou maior, ex: 14, 16, dependendo do espaço desejado
  height = 10,      # altura maior para ocupar quase uma página A4 vertical
  units = 'in',
  dpi = 600,        # DPI alto garante boa qualidade para raster (opcional em PDF)
  device = cairo_pdf  # usa Cairo para melhor compatibilidade com curvas e fontes no LaTeX
)


mapa_ais_interior_norte_color = gridExtra::arrangeGrob(mapa_ais_interior_norte_color)

# Salvar sem bordas laterais usando grid.draw + pdf
pdf('mapa_ais_interior_norte_color', width = 12, height = 10)








