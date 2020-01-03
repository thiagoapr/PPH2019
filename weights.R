
## Análise exploratoria para o calculo dos pesos amostrais

#--------------------------------------------------------------------------------

## Pacotes

library(readxl)
library(tidyverse)
library(magrittr)
library(PNADcIBGE)

#--------------------------------------------------------------------------------

## PPH

# Download e leitura

url <- paste0("https://eletrobras.com/pt/AreasdeAtuacao/",
              "PPH%202019%20-%20Banco%20de%20Dados%20V2.xlsx")

download.file(url, destfile = paste0(tempdir(), "\\pph.xlsx"), mode = 'wb')

pph <- read_xlsx(paste0(tempdir(), "/pph.xlsx"), 
                 sheet = "Banco de Dados")

# Numero de observacoes por UF

n_amostra <- pph %>% select(ENTREVISTA, UF) %>% unique() %>% 
             mutate(N_amostra = 1) 

n_amostra %<>% group_by(UF) %>% summarise(N_amostra = sum(N_amostra))

#--------------------------------------------------------------------------------

## PNAD Continua 2018 (usar a POF quando estiver disponivel)

# Download e leitura

pnadc <- get_pnadc(year = 2018, interview = 1, design = FALSE)

# Numero de domicilios da populacao com energia eletrica por UF

n_pop <- pnadc %>% filter(V2005 == "01" & S01014 == "1") %>% group_by(UF) %>% 
                   summarise(N_pop = sum(V1032)) %>% mutate(UF = as.factor(UF))

# Insere sigla da UF

levels(n_pop$UF) <- c("RO","AC","AM","RR","PA","AP","TO","MA","PI","CE","RN","PB",
                      "PE","AL","SE","BA","MG","ES","RJ","SP","PR","SC","RS","MS",
                      "MT","GO","DF")

#--------------------------------------------------------------------------------

## Fator de expansao

# Calcula o fator de expansao por UF

fator <- merge(n_pop, n_amostra, by = "UF") %>% 
               mutate(Fator = N_pop/N_amostra) %>%
               select(UF, Fator)

# Insere o fator de expansao na PPH

pph %<>% left_join(fator, by = "UF")

# Mantem apenas a PPH

rm(list=setdiff(ls(), "pph"))

#--------------------------------------------------------------------------------
