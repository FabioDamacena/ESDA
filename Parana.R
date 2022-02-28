# Instala??o e Carregamento dos Pacotes Necess?rios

pacotes <- c("rgdal","raster","tmap","maptools","tidyverse","broom","knitr",
             "kableExtra","RColorBrewer", "spdep","gtools","readxl")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

# Carregando shapefile do estado do Paraná disponibilizados pelo IBGE

shp_pr_municipios <- readOGR(dsn = "PR_Municipios_2020", 
                             layer = "PR_Municipios_2020", 
                             encoding = "UTF8",
                             use_iconv = TRUE)

# Caracter?sticas b?sicas dos objetos shp

summary(shp_pr_municipios)

# Classe e tipo do objeto carregado

class(shp_pr_municipios)
typeof(shp_pr_municipios)

# acessando os dados do shapefile carregado

shp_pr_municipios@data %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = TRUE, 
                font_size = 16)

# Para acessar as variáveis

shp_pr_municipios$NM_MUN

# Plotagem básica de um shapefile -----------------------------------------

plot(shp_pr_municipios)


# Plotagem utilizando a tmap:

tm_shape(shp = shp_pr_municipios) +
  tm_fill(col = "NM_MUN", palette = "viridis", legend.show = F)


# ESDA : Exploratory Spatial Data Analysis #####################################

# ESTABELENDO VIZINHANÇA ####################################################### 
# Critérios disponíveis: contiguidade, distância geográfica, k near, distância social
# tipos: queen, rook, bishop

# vizinhança por contiguidade de ordem 1

vizinhos_queen <- poly2nb(pl = shp_pr_municipios,
                          queen = TRUE,
                          row.names = shp_pr_municipios@data$NM_MUN)


# visualiza a vizinhança estabelecida:

plot.new()
plot(vizinhos_queen, 
     coordinates(shp_pr_municipios), 
     add = TRUE, 
     col = "#33638DFF")

# Informa?ções relevantes sobre a vizinhança queen estabelecida:

summary(vizinhos_queen)

# cria a matriz w de vizinhança

matrizW_queen <- nb2mat(neighbours = vizinhos_queen,
                        style = "B",
                        zero.policy = TRUE)

# renomeia as colunas da matriz w com o nome dos municípios

colnames(matrizW_queen) <- shp_pr_municipios@data$NM_MUN


# PADRONIZAÇÃO DA MATRIZ ESPACIAL ###########################################

# tipos: matriz w em linha, dupla padronização e padronização pela estabilização da variância

# padronição em linha da matriz w

matrizW_queen_linha <- nb2mat(vizinhos_queen,
                              style = "W",
                              zero.policy = TRUE)

# renomeia as colunas para melhor visualização

colnames(matrizW_queen_linha) <- rownames(matrizW_queen_linha)


# AUTOCORRELAÇÃO ESPACIAL ##################################################

# Autocorrelação Global

# Para o cálculo da Estatística I de Moran, nosso algoritmo espera como
# declaração um objeto de classe listw. Como exemplo, voltaremos a 
# utilizar o objeto matrizW_queen:

listw_queen <- mat2listw(matrizW_queen)
class(listw_queen)


# Vamos importar os dados de IDH dos municípios do Paraná:

dados_pr <- read_excel("idh_municipios_pr.xlsx")


# rename das colunas e tratamento dos dados para melhor organização e compatibilidade
# com os dados do shapefile

names(dados_pr)
dados_pr <- dados_pr %>% rename(CD_MUN = CÓDIGO,
                                NM_MUN = MUNICÍPIO,
                                idhm_1991 = "IDHM 1991",
                                idhm_educ_1991 = "IDHM - Dimensão Educação 1991",
                                idhm_long_1991 = "IDHM  - Dimensão Longevidade 1991",
                                idhm_renda_1991 = "IDHM - Dimensão Renda 1991",
                                ranking_1991 = "Ranking \r\nno estado 1991",
                                idhm_2000 = "IDHM 2000",
                                idhm_educ_2000 = "IDHM - Dimensão Educação 2000",
                                idhm_long_2000 = `IDHM - Dimensão Longevidade 2000`,
                                idhm_renda_2000 = "IDHM - Dimensão Renda 2000",
                                ranking_2000 = "Ranking \r\nno estado 2000",
                                idhm_2010 = "IDHM 2010",
                                idhm_educ_2010 = "IDHM - Dimensão Educação 2010",
                                idhm_long_2010 = "IDHM  - Dimensão Longevidade 2010",
                                idhm_renda_2010 = "IDHM - Dimensão Renda 2010",
                                ranking_2010 = "Ranking \r\nno estado 2010")

dados_pr$CD_MUN <- as.character(dados_pr$CD_MUN)
dados_pr <- dados_pr %>% select(1, 3:17)
summary(dados_pr)
glimpse(dados_pr)

# Vamos adicionar os dados carregados ao nosso shapefile:

shp_pr_municipios@data %>% 
  left_join(dados_pr, by = "CD_MUN") -> shp_pr_municipios@data

# Verificando o join:
shp_pr_municipios@data %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = TRUE, 
                font_size = 12)

shp_pr_municipios@data

# Após isso, poderemos utilizar a função

moran.test(x = shp_pr_municipios@data$idhm_2010, 
           listw = listw_queen, 
           zero.policy = TRUE)

# O Diagrama da Estatística I de Moran 
moran.plot(x = shp_pr_municipios@data$idhm_2010, 
             listw = listw_queen, 
             zero.policy = TRUE,
             xlab = "IDH", 
             ylab = "IDH Espacialmente Defasado",
             pch = 19)

# Autocorrelação Local - a Estatística Moran Local

# Seguindo o proposto por Anselin (1995), devemos padronizar em linha nossa 
# matriz de pesos espaciais W:

matrizW_queen_linha <- nb2mat(vizinhos_queen,
                              style = "W",
                              zero.policy = TRUE)
listw_queen_local <- mat2listw(matrizW_queen_linha)

# Considerando a variável idh do objeto PR_dados, podemos aferir sua Estatística 
# Moran Local, com o uso da função localmoran(), como se segue:
moran_local <- localmoran(x = shp_pr_municipios@data$idhm_2010, 
                          listw = listw_queen_local, 
                          zero.policy = TRUE)

# Juntando os resultados da Estatística Moran Local no dataset do objeto shp_pr_municipios:

moran_local_mapa <- cbind(shp_pr_municipios, moran_local)

# Plotando a Estatística Moran Local de forma espacial:

quantile(moran_local_mapa@data$Ii, type = 5, probs = c(0,0.2,0.4,0.6,0.8))

moran_local_mapa@data <- moran_local_mapa@data %>% 
  mutate(faixa_quantis = factor(quantcut(x = Ii, q = 5))) 

tm_shape(shp = moran_local_mapa) +
  tm_fill(col = "faixa_quantis", palette = "-magma") +
  tm_borders()

# Outra coisa interessante sobre o objeto moran_local

attr(x = moran_local, which = "quadr")

moran_local_mapa <- cbind(moran_local_mapa, 
                          attr(x = moran_local, which = "quadr")[1])

tm_shape(shp = moran_local_mapa) +
  tm_fill(col = "mean", palette = "-viridis") +
  tm_borders(col = "gray")


# CLUSTERIZAÇÃO LISA ########################################################

# O primeiro passo é o estabelecimento de um objeto que reservar os espaços para 
# conter, no futuro, os quadrantes AA, AB, BA e BB (Alto Alto, Alto Baixo, Baixo Alto e Baixo Baixo):

quadrantes <- vector(mode = "numeric", length = nrow(moran_local))

# Criando um vetor que contenha o centro das observações da variável idh ao 
# redor de sua média:

centro_idh <- shp_pr_municipios@data$idhm_2010 - mean(shp_pr_municipios@data$idhm_2010)

# Criando um vetor que contenha o centro dos valores da Estatística Moran Local 
# em torno de sua média:

centro_moran_local <- moran_local[,1] - mean(moran_local[,1])

# Criando um objeto que guarde a significância a ser adotada:

sig <- 0.05

# Enquadrando nossas observações em seus respectivos quadrantes:

quadrantes[centro_idh > 0 & centro_moran_local > 0] <- "HH"
quadrantes[centro_idh > 0 & centro_moran_local < 0] <- "HL"
quadrantes[centro_idh < 0 & centro_moran_local > 0] <- "LH"
quadrantes[centro_idh < 0 & centro_moran_local < 0] <- "LL"

# Ajustando a presença da observação em razão de sua significância estatística:

quadrantes[moran_local[,5] > sig] <- "Estatisticamente_nao_significante"

# Juntando o objeto quadrantes ao objeto moran_local_mapa

moran_local_mapa@data["quadrantes"] <- factor(quadrantes)

# Plotando os quadrantes de forma espacial (versão 'default'):

tm_shape(shp = moran_local_mapa) +
  tm_polygons(col = "quadrantes", 
              pal = c(HH = "darkred",
                      HL = "red", 
                      LH = "lightblue", 
                      LL = "darkblue",
                      Estatisticamente_nao_significante = "white")) +
  tm_borders()


# make some bbox magic (ajustando a legenda)
bbox_new <- st_bbox(moran_local_mapa) # current bounding box

xrange <- bbox_new$xmax - bbox_new$xmin # range of x values
yrange <- bbox_new$ymax - bbox_new$ymin # range of y values

# bbox_new[1] <- bbox_new[1] - (0.25 * xrange) # xmin - left
bbox_new[3] <- bbox_new[3] + (0.25 * xrange) # xmax - right
# bbox_new[2] <- bbox_new[2] - (0.25 * yrange) # ymin - bottom
bbox_new[4] <- bbox_new[4] + (0.2 * yrange) # ymax - top

bbox_new <- bbox_new %>%  # take the bounding box ...
  st_as_sfc() # ... and make it a sf polygon

# looks better, does it?

tm_shape(shp = moran_local_mapa, bbox = bbox_new) +
  tm_polygons(col = "quadrantes", 
              pal = c(HH = "darkred",
                      HL = "red", 
                      LH = "lightblue", 
                      LL = "darkblue",
                      Estatisticamente_nao_significante = "white")) +
  tm_layout(legend.position = c("right", "top"), 
            title= 'IDHM 2010 Paraná: Moran Local', 
            title.position = c('right', 'top'))+
  tm_borders()


#  A Estatística G de Getis e Ord #############################################

# A Estatística G de Getis e Ord, por definição, espera uma matriz espacial
# W de distâncias geográficas. Utilizaremos o objeto matrizW_distancias.

# Vizinhanças por Distância Geográfica, arbitrando distância de 60 Km

vizinhos_distancias <- dnearneigh(coordinates(shp_pr_municipios), 
                                  d1 = 0, 
                                  d2 = 60, 
                                  longlat = TRUE)

# Informações relevantes sobre a vizinhança estabelecida:

summary(vizinhos_distancias)

# Visualizando a vizinhança estabelecida:

plot(shp_pr_municipios, border = "lightgray")
plot(vizinhos_distancias, 
     coordinates(shp_pr_municipios), 
     add = TRUE, 
     col = "#CC6A70FF")

# A matriz W pode ser extraída da seguinte forma:

matrizW_distancias <- nb2mat(neighbours = vizinhos_distancias,
                             style = "B",
                             zero.policy = T)
colnames(matrizW_distancias) <- shp_pr_municipios@data$NM_MUN
rownames(matrizW_distancias) <- shp_pr_municipios@data$NM_MUN

# Um último ajuste é a transformação do objeto matrizW_distancias em um
# objeto de classe listw:

listw_dist <- mat2listw(x = matrizW_distancias)

# Calculando a Estatística G de Getis e Ord:

g_local <- localG(x = shp_pr_municipios@data$idhm_2010,
                  listw = listw_dist)

# Juntando as informações do objeto g_local ao nosso shapefile:

mapa_G <- cbind(shp_pr_municipios, as.matrix(g_local))

# Renomeando a nova variável para facilitar:

mapa_G@data %>% 
  rename(estistica_g = 20) -> mapa_G@data

# Plotando a Estatística G de forma espacial (versão 'default'):

mapa_G@data <- mapa_G@data %>% 
  mutate(faixa_quantis = factor(quantcut(x = estistica_g, q = 8)))

tm_shape(mapa_G) + 
  tm_fill("faixa_quantis", 
          palette = "-RdBu") + 
  tm_borders()

# Versão do gráfico anterior para daltônicos:

tm_shape(mapa_G) + 
  tm_fill("faixa_quantis", 
          palette = "plasma", title = "Estatística G") +
  tm_borders()




# -----------------------------------------------------------------------------#