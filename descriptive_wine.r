# Setting my work data directory/ Adcionando meu diretório com os dados
setwd("C:/Users/davia/OneDrive/UFRGS/primeiro_semestre/estatistica_descritiva/trabalho_2/data")

## Calling Libraries / Chamando as bibliotécas
library(readr)
library(knitr)
library(dplyr)
library(labelled)
library(ggplot2)
library(summarytools)
library(tm)
library(SnowballC)
library(wordcloud)

########### CRIANDO A TABELA COM OS TOP 13 MAIORES PREÇOS.
# CREATING A TOP 13 TABLE, BASED ON THE HIGHEST PRICES

# Reading the databse / Lendo a base de dados 
wine_data <- read_csv("winemag-data_first150k.csv")
wine_labels <- colnames(wine_data)

# Adding labels to the columns / Adcionando labels as colunas
rotulos <- c("Id", "País", "Descrição", "Designação", "Pontos", "Preço", 
             "Província", "Região 1", "Região 2", "Variedade", "Vinícola")

wine_data <- set_variable_labels(wine_data,
                                 .labels = rotulos)

# Table without NA's on price and country / Tabela auxiliar sem os NA's em país e preço
wine_data_wout_na <- wine_data[!is.na(wine_data$country) &
                                 !is.na(wine_data$price), ]

wine_data_wout_na <- wine_data_wout_na %>%
  mutate(description = stringr::str_trunc(description, width = 25)) %>% 
  arrange(desc(price))  

# Counting NA's / Contar NA's no dataset
quantidade_na_por_coluna <- apply(wine_data_wout_na, 2, function(x) sum(is.na(x)))

# Resumed table by country / Tabela resumida por país
wine_countries <- wine_data_wout_na %>%
  group_by(country) %>%
  summarize(av_price  = mean(price), 
            av_points = mean(points), 
            n_records = n())

# Resumed table by province / Tabela resumida por província
wine_provinces <- wine_data_wout_na %>%
  group_by(province) %>%
  summarize(av_price  = mean(price), 
            av_points = mean(points), 
            n_records = n(), 
            country   = unique(country))

########### CRIANDO A TABELA COM OS TOP 13 MENORES PREÇOS
# CREATING TABLE WITH TOP 13 HIGHEST PRICES
# Unique and the highest prices wines
precos_unicos_maiores <- unique(wine_data_wout_na$price)
precos_ordenados_maiores <- sort(precos_unicos_maiores, decreasing = TRUE)

# Only the top 13 highest
top_13_precos_maiores <- precos_ordenados_maiores[1:10]
subtabela_precos_maiores <- wine_data_wout_na[wine_data_wout_na$price %in% top_13_precos_maiores, ]

# Table 13 highest / Tabela dos 13 maiores preços
knitr::kable(
  head(subtabela_precos_maiores, 13),
  col.names = rotulos, 
  align = c("c", "c","l","l","c","c","l","l","l","l","l")
)


########### CRIANDO A TABELA COM OS TOP 13 MENORES PREÇOS
# CREATING A TABLE WITH THE TOP 13 SMALLEST PRICES
# Unique and the smallest prices wines
precos_unicos_menores <- unique(wine_data_wout_na$price)
precos_ordenados_menores <- sort(precos_unicos_menores)

# Top 13 smallest prices
top_13_precos_menores <- precos_ordenados_menores[1:10]
subtabela_precos_menores <- wine_data_wout_na[wine_data_wout_na$price %in% top_13_precos_menores, ]
subtabela_precos_menores <- head(subtabela_precos_menores[order(subtabela_precos_menores$price), ], 13)

## Table top 13 smallest
knitr::kable(
  head(subtabela_precos_menores, 13),
  col.names = rotulos, 
  align = c("c", "c","l","l","c","c","l","l","l","l","l")
)


######## FREQUENCY TABLES / TABELAS DE FREQUENCIA (HIGHEST WINES)
# Tabela de frequência dos top 13 maiores preços por país
freq(subtabela_precos_maiores$country)

# Tabela de frequência dos top 13 maiores preços por variedade do vinho
freq(subtabela_precos_maiores$variety)


######## FREQUENCY TABLES / TABELAS DE FREQUENCIA (SMALLEST WINES)
# Tabela de frequência dos top 13 menores preços por país
freq(subtabela_precos_menores$country)

# Tabela de frequência dos top 13 menores preços por variedade do vinho
freq(subtabela_precos_menores$variety)

############ GRÁFICO MÉDIA PREÇOS POR PAÍS ################
# MEAN PRICES BY COUNTRY
# Cálculo de média de preço por país
media_preco_por_pais <- aggregate(wine_data_wout_na$price, by=list(wine_data_wout_na$country), FUN=mean, na.rm=TRUE)

# Renomeando as colunas para tornar o resultado mais claro
colnames(media_preco_por_pais) <- c("País", "Média de Preço")

# Ordenando o resultado em ordem decrescente de média de preço
media_preco_por_pais <- media_preco_por_pais[order(media_preco_por_pais$`Média de Preço`, decreasing=TRUE), ]

# Ordenando os níveis da variável 'País' com base nas médias de preço em ordem decrescente
media_preco_por_pais$País <- factor(media_preco_por_pais$País, levels=media_preco_por_pais$País[order(media_preco_por_pais$`Média de Preço`)])

# Aqui está meu gráfico de média de  preço por país!
grafico_preco_por_pais <- ggplot(media_preco_por_pais, aes(x=`Média de Preço`, y=País, fill=`Média de Preço`)) +
  geom_bar(stat="identity") +
  labs(title="Comparação de Médias de Preço por País", x="Média de Preço", y="País") +
  theme_minimal() +
  scale_fill_gradient(low="maroon", high="#722F37")
print(grafico_preco_por_pais)

############ CALCULO DE VAR E DESVIO PADRÃO DE FORMA TABULAR ################
# CALCULATION OF VARIATION AND STANDARD DEVIATION IN TABULAR FORM
# Calcula  a variância das pontuações por preço
variancia_price_pais <- wine_data_wout_na %>%
  group_by(country) %>%
  summarise(Variância = var(price, na.rm = TRUE))

# Ordene a tabela de variância em ordem decrescente
variancia_price_pais <- variancia_price_pais %>%
  arrange(desc(Variância))

# 
print(variancia_price_pais)

# Calculei o desvio padrão dos preços por país
desvio_padrao_precos <- wine_data_wout_na %>%
  group_by(country) %>%
  summarise(Desvio_Padrao_Precos = sd(price, na.rm = TRUE))
desvio_padrao_precos

# Ploting the variation price by country utilizing histograms
wine_preco_variancia <- ggplot(plot_precos_var, aes(x=price)) +
  labs(title="Variância de Preço por País", x="Preço($)", y="Contagem") +
  geom_histogram(fill = "#722F37", colour = "black") +
  facet_wrap(~country)

print(wine_preco_variancia)

############ CALCULO DE MEDIA DE PONTUAÇÕES POR PAÍS ################
# PONTUATION MEANS BY COUNTRY
# Calculei a média de pontuações por país
media_pontuacao_por_pais <- aggregate(wine_data_wout_na$points, 
                                      by=list(wine_data_wout_na$country), 
                                      FUN=mean, na.rm=TRUE)

# Renomeando as colunas para tornar o resultado mais claro
colnames(media_pontuacao_por_pais) <- c("País", "Média de Pontuação")

# Ordenando o resultado em ordem decrescente de média de pontuação
media_pontuacao_por_pais$País <- factor(media_pontuacao_por_pais$País, 
                                        levels=media_pontuacao_por_pais$País[order(media_pontuacao_por_pais$`Média de Pontuação`)])

# Aqui está o gráfico de média de pontuação por país
grafico_pontuacao_por_pais <- ggplot(media_pontuacao_por_pais, aes(x=`Média de Pontuação`, 
                                                                   y= País, 
                                                                   fill=`Média de Pontuação`)) +
  geom_bar(stat="identity") +
  labs(title="Comparação de Médias de Pontuação por País", x="Média de Pontuação", y="País") +
  theme_minimal() +
  coord_cartesian(xlim = c(0, 90)) +
  scale_fill_gradient(low="maroon", high="#722F37")
print(grafico_pontuacao_por_pais)

############ CALCULO DE VARIÂNCIA DE PONTUAÇÕES POR PAÍS ################
# PONTUATION VARIATION BY COUNTRY
# Calcule a variância das pontuações por pontuação
variancia_pts_pais <- wine_data_wout_na %>%
  group_by(country) %>%
  summarise(Variância = var(points, na.rm = TRUE))

# Ordene a tabela de variância em ordem decrescente
variancia_pts_pais <- variancia_pts_pais %>%
  arrange(desc(Variância))

# Printa a tabela
print(variancia_pts_pais)

# Calcula o desvio padrão das pontuações por país
desvio_padrao_pontuacoes <- wine_data_wout_na %>%
  group_by(country) %>%
  summarise(Desvio_Padrao_Pontuacoes = sd(points, na.rm = TRUE))

desvio_padrao_pontuacoes

################# GRÁFICO VARIÂNCIA PONTUAÇÃO PAÍS ############################
## Estou usando o mesmo código utilizado acima para apresentação de variância
# dos preços dos 4 países mais frequentes. Só estou alterando o eixo do gráfico
# para apresentar a variância da pontuação desta vez

## GGPLOT(Box_Plot) para analisar variância de pontuação por país
wine_pts_variancia <- ggplot(plot_precos_var, aes(country, points)) +
  labs(title="Variância de Pontuação por País", x="Preço($)", y="País") +
  geom_boxplot(fill = "#722F37", colour = "black")
print(wine_pts_variancia)


# Relação entre preço e pontuação
wine_plot_price_points <- ggplot(wine_data_wout_na) +
  aes(points, price) +
  geom_point(color = '#722F37') + 
  theme_minimal() + 
  labs(title="Relação entre Preço e Pontuação",
       x = "Pontos",
       y = "Preço ($)") + 
  scale_x_continuous(breaks = seq(80, 100, 2)) + 
  scale_y_continuous(breaks = seq(0, 3000, 250))
wine_plot_price_points

##
wine_plot_hist_points <- ggplot(wine_data_wout_na, 
                                aes(x = points)) +
  geom_histogram(binwidth = 1, fill = "#722F37", color = "#800020") +  
  theme_minimal() + 
  labs(x = "Pontos",
       y = "Número de registros") + 
  scale_x_continuous(breaks = seq(80, 100, 2)) + 
  scale_y_continuous(breaks = seq(0, 20000, 3000)) 
wine_plot_hist_points

##
wine_counts <- wine_data %>%
  group_by(country) %>%
  summarise(count = n(), avg_price = mean(price, na.rm = TRUE))

###definindo valores que serão utilizados em graficos posteriores.
limiteX<-50

limitesupX<-2000

### grafico com todos os valores demonstrados.
ggplot(wine_counts, aes(x = count, y = avg_price)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Quantidade de Registros",
       y = "Preço Médio",
       title = "Relação entre Quantidade de registros e Preço Médio por País") +
  theme_minimal()

###grafico onde a escala x se encontra em 2000
ggplot(wine_counts, aes(x = count, y = avg_price)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Quantidade de Registros",
       y = "Preço Médio",
       title = "Relação entre Quantidade de registros e Preço Médio por País") +
  theme_minimal()+
  xlim(0, limitesupX)

#grafico onde a escala x se encontra em 50
ggplot(wine_counts, aes(x = count, y = avg_price)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Quantidade de Registros",
       y = "Preço Médio",
       title = "Relação entre Quantidade de registros e Preço Médio por País") +
  theme_minimal()+
  xlim(0, limiteX)
