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

## Número de observações

n_amostra <- pph %>% select(ENTREVISTA, UF, CLASSE) %>% unique() %>% 
             mutate(n_amostra = 1) 

n_amostra %<>% group_by(UF, CLASSE) %>% summarise(n_amostra = sum(n_amostra))

#--------------------------------------------------------------------------------

## Microdado PNAD Continua 2018 (talvez tenha que utilizar a POF)

# Download e leitura

pnadc <- get_pnadc(year = 2018, interview = 1, design = FALSE)

# Aplica o critério Brasil (2017)

?????


#--------------------------------------------------------------------------------

## Fator de expansão por UF

# Calcula o fator de expansão

fator <- merge(n_pop, n_amostra, by = "UF") %>% 
  mutate(Fator = n_pop/n_amostra) %>%
  select(UF, Fator)

# Insere o fator de expansão na PPH

pph %<>% left_join(fator, by = "UF")

#--------------------------------------------------------------------------------



