rm(list=ls())

## Pacotes

library(readxl)
library(tidyverse)
library(magrittr)
library(PNADcIBGE)


#--------------------------------------------------------------------------------

## Download e leitura da PPH

url <- paste0("https://eletrobras.com/pt/AreasdeAtuacao/",
              "PPH%202019%20-%20Banco%20de%20Dados%20V2.xlsx")

download.file(url, destfile = paste0(tempdir(), "\\pph.xlsx"), mode = 'wb')

pph <- read_xlsx(paste0(tempdir(), "/pph.xlsx"), 
                 sheet = "Banco de Dados")

## Número de observações por Estado

n_amostra <- pph %>% select(ENTREVISTA, UF) %>% unique() %>% 
             mutate(n_amostra = 1) 

n_amostra %<>% group_by(UF) %>% summarise(n_amostra = sum(n_amostra))

#--------------------------------------------------------------------------------

## PNAD Contínua 2018

url2 <- paste0("ftp://ftp.ibge.gov.br/",
               "Trabalho_e_Rendimento/Pesquisa_Nacional_por_Amostra_de_Domicilios_continua/",
               "Anual/Caracteristicas_Gerais_dos_Domicilios_e_dos_Moradores_2018/",
               "PNAD_Continua_2016_2018_Caracteristicas_Gerais_dos_Domicilios.xlsx")

download.file(url2, destfile = paste0(tempdir(), "\\pnadc.xlsx"), mode = 'wb')

pnadc <- read_xlsx(paste0(tempdir(), "/pnadc.xlsx"), 
                 sheet = "Serviços Básicos",
                 skip = 5)

## Cleaning

pnadc %<>% filter(pnadc[1] == "Domicílios (mil domicílios)",
                  pnadc[2] == "UF", 
                  pnadc[4] == "Energia elétrica",
                  pnadc[5] == "Rede geral ou fonte alternativa")

## População por UF

n_pop <-  pnadc %>% select(UF = 3, n_pop = 10)

n_pop %<>% mutate(UF = as.factor(UF), n_pop = as.numeric(n_pop)*1000)

## Sigla da UF

levels(n_pop$UF) <- c("AC", "AL", "AP", "AM", "BA", "CE", "DF", "ES", "GO", "MA",
                      "MT", "MS", "MG", "PA", "PB", "PR", "PE", "PI", "RJ", "RN",
                      "RS", "RO", "RR", "SC", "SP", "SE", "TO")
  
#--------------------------------------------------------------------------------

## Fator de expansão por UF

# Calcula o fator de expansão

fator <- merge(n_pop, n_amostra, by = "UF") %>% 
         mutate(Fator = n_pop/n_amostra) %>%
         select(UF, Fator)

# Insere o fator de expansão na PPH

pph %<>% left_join(fator, by = "UF")
         
#--------------------------------------------------------------------------------






