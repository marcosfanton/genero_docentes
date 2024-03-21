####Pacotes####
library(tidyverse)
library(here)
library(genderBR) # Dicionário de nomes e gênero

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
         CD_PROGRAMA_IES,
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
  mutate(CD_CONCEITO_PROGRAMA = NA,
         CD_CAT_BOLSA_PRODUTIVIDADE = NA)

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
         CD_PROGRAMA_IES,
         NM_MODALIDADE_PROGRAMA,
         NM_AREA_AVALIACAO,
         NM_DOCENTE,
         DS_CATEGORIA_DOCENTE,
         DS_TIPO_VINCULO_DOCENTE_IES,
         NM_AREA_BASICA_TITULACAO,
         CD_CONCEITO_PROGRAMA,
         CD_CAT_BOLSA_PRODUTIVIDADE) |> 
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
         CD_PROGRAMA_IES,
         NM_MODALIDADE_PROGRAMA,
         NM_AREA_AVALIACAO,
         NM_DOCENTE,
         DS_CATEGORIA_DOCENTE,
         DS_TIPO_VINCULO_DOCENTE_IES,
         NM_AREA_BASICA_TITULACAO,
         CD_CONCEITO_PROGRAMA,
         CD_CAT_BOLSA_PRODUTIVIDADE) |> 
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
         CD_PROGRAMA_IES,
         NM_MODALIDADE_PROGRAMA,
         NM_AREA_AVALIACAO,
         NM_DOCENTE,
         DS_CATEGORIA_DOCENTE,
         DS_TIPO_VINCULO_DOCENTE_IES,
         NM_AREA_BASICA_TITULACAO,
         CD_CONCEITO_PROGRAMA,
         CD_CAT_BOLSA_PRODUTIVIDADE) |> 
  filter(NM_AREA_AVALIACAO == "FILOSOFIA") |> 
  mutate(CD_CONCEITO_PROGRAMA = as.factor(CD_CONCEITO_PROGRAMA))

dados <- bind_rows(banco0412, banco1316, banco17) |> 
  mutate(CD_CONCEITO_PROGRAMA = as.factor(CD_CONCEITO_PROGRAMA)) |> 
  bind_rows(banco1822) |> 
  mutate(GENERO = genderBR::get_gender(NM_DOCENTE))

# Seleção dos nomes sem gênero atribuído - n: 1275
semgenero <- dados |>  
  filter(is.na(GENERO)) |> # Filtra NAs do nome do orientador
  distinct(NM_DOCENTE)   # Mantêm apenas os nomes únicos

# Vetor para mulheres
mulheres <- c("FRANCIONE CHARAPA ALVES",
              "ALCIONE ROBERTO ROANI",
              "NELSI KISTEMACHER WELTER",
              "FILLIPA CARNEIRO SILVEIRA",
              "TAYNAM SANTOS LUZ BUENO",
              "SELOUA LUSTE BOULBINA",
              "MOJCA KUPLEN",
              "FÁTIMA REGINA RODRIGUES EVORA",
              "OLGARIA CHAIN FERES MATOS",
              "NELCI DO NASCIMENTO GONÇALVES",
              "ZÉLIA MARIA DANTAS DE OLIVEIRA",
              "ÂNGELA MARIA PAIVA CRUZ",
              "CECÍLIA MARIA PINTO PIRES",
              "ELNÔRA MARIA GONDIM MACHADO LIMA",
              "ROSARIO PECORARO",
              "ACYLENE MARIA CABRAL FERREIRA",
              "MÁRCIA SÁ CAVALCANTE SCHUBACK",
              "KÁTIA RODRIGUES MURICY",
              "DÉBORAH DANOWSKI",
              "MÁRCIA CRISTINA FERREIRA GONÇALVES",
              "LÍVIA MARA GUIMARÃES",
              "MÍRIAM CAMPOLINA DINIZ PEIXOTO",
              "CÍNTIA VIEIRA DA SILVA",
              "ANDRÉA MARIA ALTINO DE CAMPOS LOPARIC",
              "DÉBORA CRISTINA MORATO PINTO",
              "PATRÍCIA CORADIM SITA",
              "ANDRÉA LUISA BUCCHILE FAGGION",
              "SÔNIA TERESINHA FELIPE",
              "CLÁUDIA PELLEGRINI DRUCKER")

# Vetor para homens
homens <- semgenero %>%
  filter(!NM_DOCENTE %in% mulheres) |> # Filtra os nomes de mulheres
  pull() # Extrai vetor de nomes 

# Tabela Docente - Gênero####
docentes <- dados |> 
  distinct(NM_DOCENTE, .keep_all = TRUE) |> 
  select(NM_DOCENTE, GENERO)

# Rotular atribuição errônea de gênero
homem <- c("GABRIELE CORNELLI", #51
           "NOELI DUTRA ROSSATTO", #133
           "ROSARIO PECORARO", #738
           "HENNY ANDRE LUCRECE BLOMME") #1251

mulher <- c("OLGÁRIA CHAIN FÉRES MATOS", #295
            "OTÍLIA BEATRIZ FIORI ARANTES", #384
            "IRLEY FERNANDES FRANCO", #537
            "CLÁUDIA PEREIRA DO CARMO MURTA", #571
            "SÍLVIA FAUSTINO DE ASSIS SAES") #610

# Atribuição de gênero para os casos correspondentes no restante do banco
dados <- dados |> 
  mutate(
    GENERO = case_when(
      NM_DOCENTE %in% mulheres ~ "Female", 
      NM_DOCENTE %in% homens ~ "Male", 
      TRUE ~ GENERO # Preserva os valores correspondentes nos demais casos
    )) |> 
  mutate(across((where(is.character) & !matches(c("SG_ENTIDADE_ENSINO", 
                                                  "NM_REGIAO",
                                                  "SG_UF_PROGRAMA",
                                                  "CD_PROGRAMA_IES",
                                                  "CD_CAT_BOLSA_PRODUTIVIDADE"))),
                ~str_to_title(.)),
         GENERO = str_replace_all(GENERO, 
                                  pattern = c("Male" = "Homem",
                                              "Female" = "Mulher"))) |> 
  select(!NM_AREA_AVALIACAO)  



# Salvar banco limpo
docentes |>
  readr::write_csv("dados/dados_docente-genero.csv")

# Rotular corretamente gênero 

homem_novo <- 




  
# Nome de PPGS --> ver arquivo docentes_nome-ppgs.R
dados <- dados |> 
  mutate(NM_PROGRAMA = paste(SG_ENTIDADE_ENSINO, 
                             "(", CD_PROGRAMA_IES, ")", 
                             sep = ""))

# Salvar banco limpo
dados |>
  readr::write_csv("dados/dados_docentes.csv")
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              