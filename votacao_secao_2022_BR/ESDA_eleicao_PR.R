# Instalação e Carregamento dos Pacotes Necessários

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

# Carregando shapefile do estado do Paraná disponibilizado pelo IBGE

shp_pr_municipios <- readOGR(dsn = "PR_Municipios_2020", 
                             layer = "PR_Municipios_2020", 
                             encoding = "UTF8",
                             use_iconv = TRUE)

# Vamos importar os dados dos votos em bolsonaro no primeiro turno nos municípios do Paraná:

dados_pr <- read.csv("PR2022.csv")

# Nomes de municípios com problema, cujo ajuste é necessário:
# ROWS:  16 98 159 263 297 335 345 
# "Antônio Olinto", "Diamante D'Oeste", "Itapejara d'Oeste", "Pérola d'Oeste",
# "Rancho Alegre D'Oeste", "Santo Antônio do Caiuá", "São Jorge d'Oeste"

shp_pr_municipios@data$NM_MUN[16] <- "Antonio Olinto"
shp_pr_municipios@data$NM_MUN[98] <- "Diamante d Oeste"
shp_pr_municipios@data$NM_MUN[159] <- "Itapejara d Oeste"
shp_pr_municipios@data$NM_MUN[263] <- "Pérola d Oeste"
shp_pr_municipios@data$NM_MUN[297] <- "Rancho Alegre d Oeste"
shp_pr_municipios@data$NM_MUN[335] <- "Santo Antonio do Caiuá"
shp_pr_municipios@data$NM_MUN[345] <- "São Jorge d Oeste"

# rename das colunas e tratamento dos dados para melhor organização e compatibilidade
# com os dados do shapefile

names(dados_pr)
dados_pr <- dados_pr %>% rename(NM_MUN = "NM_MUNICIPIO")

# normaliza strings
# Converte para minusculo para conseguir compatibilizar observações de ambos os dataframes
dados_pr <- dados_pr %>% 
  mutate_if(is.character, tolower)

shp_pr_municipios@data <- shp_pr_municipios@data %>% 
  mutate_if(is.character, tolower)


# Vamos adicionar os dados carregados ao nosso shapefile:

shp_pr_municipios@data %>% 
  left_join(dados_pr, by = "NM_MUN") -> shp_pr_municipios@data

# Converte para maiusculo
shp_pr_municipios@data <- shp_pr_municipios@data %>% 
  mutate_if(is.character, toupper)

# Verificando o join:
shp_pr_municipios@data %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = TRUE, 
                font_size = 12)

# ESTABELENDO VIZINHANÇA ####################################################### 
# Critérios disponíveis: contiguidade, distância geográfica, k near, distância social
# tipos: queen, rook, bishop

# vizinhança por contiguidade de ordem 1

vizinhos_queen <- poly2nb(pl = shp_pr_municipios,
                          queen = TRUE,
                          row.names = shp_pr_municipios@data$NM_MUN)

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
# utilizar o objeto matrizW_queen_linha:

listw_queen <- mat2listw(matrizW_queen_linha)
class(listw_queen)

# Após isso, poderemos utilizar a função moran.test para as variáveis de interesse


############################### MORAN GLOBAL ###############################

names(dados_pr)
moran.test(x = shp_pr_municipios@data$percentual, 
           listw = listw_queen, 
           zero.policy = TRUE)



# O Diagrama da Estatística I de Moran 
moran.plot(x = shp_pr_municipios@data$percentual, 
           listw = listw_queen, 
           zero.policy = TRUE,
           xlab = "Rec.TribxRec.Tot", 
           ylab = "Rec.TribxRec.Tot Espacialmente Defasado",
           pch = 19)


############################### Moran Local ###############################

# Autocorrelação Local - a Estatística Moran Local

# Seguindo o proposto por Anselin (1995), devemos padronizar em linha nossa 
# matriz de pesos espaciais W:

# vizinhança por contiguidade de ordem 1

vizinhos_queen <- poly2nb(pl = shp_pr_municipios,
                          queen = TRUE,
                          row.names = shp_pr_municipios@data$NM_MUN)

matrizW_queen_linha <- nb2mat(vizinhos_queen,
                              style = "W",
                              zero.policy = TRUE)
listw_queen_local <- mat2listw(matrizW_queen_linha)

# Considerando a variável indicador do objeto PR_dados, podemos aferir sua Estatística 
# Moran Local, com o uso da função localmoran(), como se segue:

moran_local <- localmoran(x = shp_pr_municipios@data$percentual, 
                          listw = listw_queen_local, 
                          zero.policy = TRUE)

# Juntando os resultados da Estatística Moran Local no dataset do objeto shp_pr_municipios:

moran_local_mapa <- cbind(shp_pr_municipios, moran_local)

# Plotando a Estatística Moran Local de forma espacial:

quantile(moran_local_mapa@data$Ii, type = 5, probs = c(0,0.2,0.4,0.6,0.8))
moran_local_mapa@data <- moran_local_mapa@data %>% 
  mutate(faixa_quantis = factor(quantcut(x = Ii, q = 5))) 
attr(x = moran_local, which = "quadr")
moran_local_mapa <- cbind(moran_local_mapa, 
                          attr(x = moran_local, which = "quadr")[1])

tm_shape(shp = moran_local_mapa) +
  tm_fill(col = "mean", palette = "-viridis") +
  tm_borders(col = "gray")


# CLUSTERIZAÇÃO LISA ########################################################

# vizinhança por contiguidade de ordem 1

vizinhos_queen <- poly2nb(pl = shp_pr_municipios,
                          queen = TRUE,
                          row.names = shp_pr_municipios@data$NM_MUN)

matrizW_queen_linha <- nb2mat(vizinhos_queen,
                              style = "W",
                              zero.policy = TRUE)
listw_queen_local <- mat2listw(matrizW_queen_linha)

# Considerando a variável indicador do objeto PR_dados, podemos aferir sua Estatística 
# Moran Local, com o uso da função localmoran(), como se segue:


moran_local <- localmoran(x = shp_pr_municipios@data$percentual, 
                          listw = listw_queen_local, 
                          zero.policy = TRUE)

# O primeiro passo é o estabelecimento de um objeto que reservar os espaços para 
# conter, no futuro, os quadrantes AA, AB, BA e BB (Alto Alto, Alto Baixo, Baixo Alto e Baixo Baixo):

quadrantes <- vector(mode = "numeric", length = nrow(moran_local))

# Criando um vetor que contenha o centro das observações da variável ao 
# redor de sua média:

centro <- shp_pr_municipios@data$percentual - mean(shp_pr_municipios@data$percentual)

# Criando um vetor que contenha o centro dos valores da Estatística Moran Local 
# em torno de sua média:

centro_moran_local <- moran_local[,1] - mean(moran_local[,1])

# Criando um objeto que guarde a significância a ser adotada:

sig <- 0.05

# Enquadrando nossas observações em seus respectivos quadrantes:

quadrantes[centro > 0 & centro_moran_local > 0] <- "HH"
quadrantes[centro > 0 & centro_moran_local < 0] <- "HL"
quadrantes[centro < 0 & centro_moran_local > 0] <- "LH"
quadrantes[centro < 0 & centro_moran_local < 0] <- "LL"

# Ajustando a presença da observação em razão de sua significância estatística:

quadrantes[moran_local[,5] > sig] <- "Estatisticamente_nao_significante"

# Juntando os resultados da Estatística Moran Local no dataset do objeto shp_pr_municipios:

moran_local_mapa <- cbind(shp_pr_municipios, moran_local)

# Juntando o objeto quadrantes ao objeto moran_local_mapa

moran_local_mapa@data["quadrantes"] <- factor(quadrantes)

tm_shape(shp = moran_local_mapa) +
  tm_polygons(col = "quadrantes", 
              pal = c(HH = "darkred",
                      HL = "red", 
                      LH = "lightblue", 
                      LL = "darkblue",
                      Estatisticamente_nao_significante = "white")) +
  tm_layout(legend.outside = TRUE)


dados <- moran_local_mapa@data
LL <- dados %>% filter(quadrantes == "LL")
LH <- dados %>% filter(quadrantes == "LH")
HH <- dados %>% filter(quadrantes == "HH")
HL <- dados %>% filter(quadrantes == "HL")


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
shp_pr_municipios@data$NM_MUN[209]

# A matriz W pode ser extraída da seguinte forma:

matrizW_distancias <- nb2mat(neighbours = vizinhos_distancias,
                             style = "W",
                             zero.policy = T)
colnames(matrizW_distancias) <- shp_pr_municipios@data$NM_MUN
rownames(matrizW_distancias) <- shp_pr_municipios@data$NM_MUN

# Um último ajuste é a transformação do objeto matrizW_distancias em um
# objeto de classe listw:

listw_dist <- mat2listw(x = matrizW_distancias)


# Calculando a Estatística G de Getis e Ord:

g_local <- localG(x = shp_pr_municipios@data$percentual,
                  listw = listw_dist)

# Juntando as informações do objeto g_local ao nosso shapefile:

mapa_G <- cbind(shp_pr_municipios, as.matrix(g_local))

# Renomeando a nova variável para facilitar:

mapa_G@data %>% 
  rename(estistica_g = 10) -> mapa_G@data

# Plotando a Estatística G de forma espacial (versão 'default'):

mapa_G@data <- mapa_G@data %>% 
  mutate(faixa_quantis = factor(quantcut(x = estistica_g, q = 8)))

tm_shape(mapa_G) + 
  tm_fill("faixa_quantis", 
          palette = "-RdBu") + 
  tm_borders() +
  tm_layout(legend.text.size = 0.7,
            legend.title.size = 0.9,
            frame = F,
            main.title = "Estatística G em função do percentual de votos em Bolsonaro",
            legend.outside = TRUE)


