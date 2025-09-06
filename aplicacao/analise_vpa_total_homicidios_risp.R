library(readxl)
library(tidyverse)
library(prais)

homicidios_risp = read_excel('BASES TRATADAS/homicidios_zona_2009_2024.xlsx') 

# Total de Homicidios por Ano
homicidios_total_risp = homicidios_risp |> 
  mutate(ano = as.numeric(ano)) |> 
  ggplot(aes(x = ano, y = total_homicidios, color = zona)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = seq(min(homicidios_risp$ano |>
                                        as.numeric()), 
                                  max(homicidios_risp$ano |>
                                        as.numeric()), by = 1),
                     guide = guide_axis(angle = 45)) +
  scale_y_continuous(limits = c(0, NA),
                     expand = expansion(mult = c(0, 0.1))) +
  labs(title = NULL, x = '', y = '', color = '') +
  theme_minimal() +
  theme(legend.position = 'top',
        plot.title = element_text(hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size = 8)) +
  scale_color_brewer(palette = 'Set1')


ggsave('homicidios_total_risp.pdf', homicidios_total_risp,
       width = 10, height = 6, units = 'in')



# total_risp = homicidios_risp |>
#   ggplot(aes(x = as.numeric(ano), y = total_homicidios, color = zona)) + 
#   geom_line(size = 1.1) +
#   geom_point(size = 2) + 
#   facet_wrap(~ zona) +
#   labs(title = '', x = '', y = '', color = '') +
#   scale_x_continuous(
#     breaks = seq(min(as.numeric(homicidios_risp$ano)),
#                  max(as.numeric(homicidios_risp$ano)), by = 1),
#     guide = guide_axis(angle = 45)
#   ) +
#   expand_limits(y = 0) +
#   scale_color_brewer(palette = 'Set1') +
#   theme_minimal() +
#   theme(legend.position = 'none')

ggsave('homicidios_total_risp.pdf', total_risp,
       width = 10, height = 6, units = 'in')



# Analisando a Variação Percentual Anual do Número de Homicídios

vpa_homicidios_risp = homicidios_risp |>
  group_by(zona) |>
  arrange(zona, ano) |>
  mutate(vpa = (total_homicidios / lag(total_homicidios) - 1) * 100) |>
  ungroup()


vpa = vpa_homicidios_risp |>
  mutate(sinal = ifelse(vpa >= 0, 'positivo', 'negativo'),
         posicao_texto = ifelse(vpa >= 0, -0.5, 1.2),
         label_texto = paste0(round(vpa, 1), '%')) |>
  ggplot(aes(x = factor(ano), y = vpa, fill = sinal)) +
  geom_col() +
  geom_hline(yintercept = 0, linetype = 'dashed', color = 'gray50') +
  geom_text(aes(label = label_texto, vjust = posicao_texto), 
            size = 3.5, show.legend = FALSE) +
  scale_fill_manual(values = c('positivo' = 'red4', 'negativo' = 'darkgreen'),
                    guide = 'none') +
  facet_wrap(~zona, ncol = 2) +
  labs(x = '', y = '') +
  theme_minimal() +
  theme(strip.text = element_text(face = 'bold'),
        axis.text.x = element_text(angle = 45, hjust = 1))



ggsave('vpa_homicidios_risp.pdf', vpa,
       width = 14, height = 10, units = 'in')


# Teste Não-Paramétrico de Mann-Kendall
library(trend)
library(lmtest)


mk_teste = homicidios_risp |> 
  group_by(zona) |> 
  summarise(mk = list(mk.test(total_homicidios)), .groups = 'drop') |> 
  mutate(S = sapply(mk, function(x) x$estimates['S']),
         tau = sapply(mk, function(x) x$estimates['tau']),
         p_valor = sapply(mk, function(x) x$p.value),
         tendencia = case_when(p_valor < 0.05 & S > 0 ~ 'Crescente',
                          p_valor < 0.05 & S < 0 ~ 'Decrescente',
                          TRUE ~ 'Estacionária')) |> 
  select(zona, S, tau, p_valor, tendencia)


homicidios_risp |> 
  group_by(zona) |> 
  summarise(mk = list(mk.test(taxa_homicidios)), .groups = 'drop') |> 
  mutate(S = sapply(mk, function(x) x$estimates['S']),
         tau = sapply(mk, function(x) x$estimates['tau']),
         p_valor = sapply(mk, function(x) x$p.value),
         tendencia = case_when(p_valor < 0.05 & S > 0 ~ 'Crescente',
                               p_valor < 0.05 & S < 0 ~ 'Decrescente',
                               TRUE ~ 'Estacionária')) |> 
  select(zona, S, tau, p_valor, tendencia)


# Análise de tendência temporal por meio de regressão linear

modelo_capital = lm(taxa_homicidios ~ ano, 
                    data = homicidios_risp |> filter(zona == 'Capital'))

summary(modelo_capital)
round(confint(modelo_capital), 4)
shapiro.test(modelo_capital$residuals)
bptest(modelo_capital)

modelo_interior_norte = lm(taxa_homicidios ~ ano, 
                    data = homicidios_risp |> filter(zona == 'Interior Norte'))

summary(modelo_interior_norte)
round(confint(modelo_interior_norte), 4)
shapiro.test(modelo_interior_norte$residuals)
bptest(modelo_interior_norte)


modelo_interior_sul = lm(taxa_homicidios ~ ano, 
                           data = homicidios_risp |> filter(zona == 'Interior Sul'))

summary(modelo_interior_sul)
round(confint(modelo_interior_sul), 4)
shapiro.test(modelo_interior_sul$residuals)
bptest(modelo_interior_sul)

modelo_regiao_metropolitana = lm(taxa_homicidios ~ ano, 
                         data = homicidios_risp |> filter(zona == 'Região Metropolitana'))

summary(modelo_regiao_metropolitana)
round(confint(modelo_regiao_metropolitana), 4)
shapiro.test(modelo_regiao_metropolitana$residuals)
bptest(modelo_regiao_metropolitana)

# regressao linear de Prais-Winsten

homicidios_risp$ano = as.numeric(as.character(homicidios_risp$ano))

for (i in unique(homicidios_risp$zona)) {
  dados_risp = homicidios_risp |> filter(zona == i)
  modelo_gls = gls(total_homicidios ~ ano, 
                    correlation = corAR1(form = ~ ano), 
                    data = dados_risp)
  cat('Zona:', i, '\n')
  print(summary(modelo_gls))
  cat('\n-----------------------------------\n')
}





