library(tidyverse)

# carregar dados

df <- read.csv("votacao_secao_2022_BR.csv", sep = ";", encoding = "Latin 1")

# filtra PR

df <- df %>% filter(SG_UF == "PR")

# filtra variáveis úteis

df <- df %>% select(CD_MUNICIPIO, NM_MUNICIPIO, NR_ZONA, NR_SECAO,
                    NM_VOTAVEL, QT_VOTOS)

# Agrupamento por candidato

df = df %>% group_by(NM_MUNICIPIO, NM_VOTAVEL) %>%
  summarise(votos = sum(QT_VOTOS, na.rm=TRUE)) %>% 
  ungroup () %>% droplevels(.) %>% arrange(NM_MUNICIPIO)

# calcula total de votos por município

votos_municipio = df %>% group_by(NM_MUNICIPIO) %>%
  summarise(votos = sum(votos, na.rm=TRUE)) %>% 
  ungroup () %>% droplevels(.) %>% arrange(NM_MUNICIPIO)

# Filtra Bolsonaro

df <- df %>% filter(NM_VOTAVEL == "JAIR MESSIAS BOLSONARO")
names(df)[3] <- "votos_bolsonaro"
names(votos_municipio)[1] <- "municipio"

# Cria df que totaliza os percentuais de votos em Bolsonaro

df_total <- cbind(df, votos_municipio)
df_total <- df_total %>% select(NM_MUNICIPIO, NM_VOTAVEL, votos_bolsonaro, votos)

df_total$percentual <- df_total$votos_bolsonaro / df_total$votos

# Salva resultado em csv

write.csv(df_total,file='PR2022.csv')
