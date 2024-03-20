####Pacotes####
library(tidyverse)
library(here)
library(genderBR) # Dicionário de nomes e gênero
library(stringi)
library(textclean)

# Unificação dos bancos 2004 - 2022 ####
# Banco 2004-2012
banco0412 <- read.csv2("dados/capes/docente_04-12.csv", 
                   fileEncoding = "ISO-8859-1",
                   row.names = NULL) |> 
  select(AN_BASE,
         SG_ENTIDADE_ENSINO,
         CS_STATUS_JURIDICO,
         NM_REGIAO_ENTIDADE,
         SG_UF_ENTIDADE_ENSINO,
         NM_PROGRAMA_IES,
         NM_MODALIDADE_PROGRAMA,
         NM_AREA_AVALIACAO,
         NM_DOCENTE,
         DS_CATEGORIA_DOCENTE,
         DS_TIPO_NATUR_VINCULO_DOCENTE,
         NM_AREA_CONHEC_FORM_DOCENTE) |> 
  filter(NM_AREA_AVALIACAO == "FILOSOFIA / TEOLOGIA:SUBCOMISSÃO FILOSOFIA") |> 
  rename(SG_UF_PROGRAMA = SG_UF_ENTIDADE_ENSINO,
         NM_REGIAO = NM_REGIAO_ENTIDADE,
         NM_AREA_BASICA_TITULACAO= NM_AREA_CONHEC_FORM_DOCENTE,
         DS_TIPO_VINCULO_DOCENTE_IES = DS_TIPO_NATUR_VINCULO_DOCENTE) |> 
  mutate(CD_CONCEITO_PROGRAMA = NA)

# Banco 2013 - 2016
banco1316 <- purrr::map_dfr(list.files(path = "dados/capes/", 
                                                pattern = "13|14|15|16", 
                                   full.names = TRUE),
                                     readr::read_csv2,
                            locale = readr::locale(encoding = "ISO-8859-1")) |> 
  select(AN_BASE,
         SG_ENTIDADE_ENSINO,
         CS_STATUS_JURIDICO,
         NM_REGIAO,
         SG_UF_PROGRAMA,
         NM_PROGRAMA_IES,
         NM_MODALIDADE_PROGRAMA,
         NM_AREA_AVALIACAO,
         NM_DOCENTE,
         DS_CATEGORIA_DOCENTE,
         DS_TIPO_VINCULO_DOCENTE_IES,
         NM_AREA_BASICA_TITULACAO,
         CD_CONCEITO_PROGRAMA) |> 
  filter(NM_AREA_AVALIACAO == "FILOSOFIA/TEOLOGIA:SUBCOMISSÃO FILOSOFIA"
         |NM_AREA_AVALIACAO == "FILOSOFIA")

# Banco 2017 
banco17 <- read.csv2("dados/capes/docente_17.csv", 
                       fileEncoding = "ISO-8859-1",
                       row.names = NULL) |> 
  select(AN_BASE,
         SG_ENTIDADE_ENSINO,
         CS_STATUS_JURIDICO,
         NM_REGIAO,
         SG_UF_PROGRAMA,
         NM_PROGRAMA_IES,
         NM_MODALIDADE_PROGRAMA,
         NM_AREA_AVALIACAO,
         NM_DOCENTE,
         DS_CATEGORIA_DOCENTE,
         DS_TIPO_VINCULO_DOCENTE_IES,
         NM_AREA_BASICA_TITULACAO,
         CD_CONCEITO_PROGRAMA) |> 
  filter(NM_AREA_AVALIACAO == "FILOSOFIA")

# Banco 1822
banco1822 <- purrr::map_dfr(list.files(path = "dados/capes/", 
                                       pattern = "18|19|20|21|22", 
                                       full.names = TRUE),
                            readr::read_csv2,
                            locale = readr::locale(encoding = "ISO-8859-1")) |> 
  select(AN_BASE,
         SG_ENTIDADE_ENSINO,
         CS_STATUS_JURIDICO,
         NM_REGIAO,
         SG_UF_PROGRAMA,
         NM_PROGRAMA_IES,
         NM_MODALIDADE_PROGRAMA,
         NM_AREA_AVALIACAO,
         NM_DOCENTE,
         DS_CATEGORIA_DOCENTE,
         DS_TIPO_VINCULO_DOCENTE_IES,
         NM_AREA_BASICA_TITULACAO,
         CD_CONCEITO_PROGRAMA) |> 
  filter(NM_AREA_AVALIACAO == "FILOSOFIA") |> 
  mutate(CD_CONCEITO_PROGRAMA = as.factor(CD_CONCEITO_PROGRAMA))

teste <- bind_rows(banco0412, banco1316, banco17)
