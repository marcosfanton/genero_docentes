####Pacotes####
library(tidyverse)
library(here)
library(ggtext)

# Banco 
dados <- read.csv("dados/dados_docentes.csv") |> 
  mutate(across((where(is.character) & !matches(c("SG_ENTIDADE_ENSINO", "NM_REGIAO", "SG_UF_PROGRAMA"))),
                ~str_to_title(.))) |> 
  mutate_if(is.character, as.factor) 

# Evolução Tempo - Total ####
dados_tempo <- dados |> 
  group_by(SG_UF_PROGRAMA,   GENERO) |> 
  summarize(total = n()) |> 
  mutate(frequencia = round(total/sum(total)*100,2))

?mutate_if

# Gráfico 01 - Evolução Docentes Permanentes####
dados |> 
  drop_na(CD_CONCEITO_PROGRAMA) |> 
  filter(DS_CATEGORIA_DOCENTE == "PERMANENTE") |> 
  ggplot(aes(x = AN_BASE, 
             fill = GENERO)) +
  geom_bar(position = "fill")  +
  scale_x_continuous(limits = c(2012, 2023), breaks = seq(1990, 2021, 5)) +
  scale_y_continuous(labels=scales::percent, position = "right") +
  scale_fill_manual(values = c("Homem" = "#59106e",
                               "Mulher" = "#d24644")) +
  theme_classic() +
  labs(x = "",
       y = "",
       fill = "",
       title = "Proporção de Docentes Permanentes em PPGs de Filosofia por Gênero (1994-2022)",
       subtitle = "**N**: 13.294 | <span style= 'color:#59106e;'>**Homem**</span>: 79.58% - <span style= 'color:#d24644;'>**Mulher**</span>: 20.42%",
       caption = "Elaboração: Dataphilo | Dados: CAPES") +
  theme(plot.title = element_markdown(face = "bold"),  
        plot.subtitle = element_markdown(hjust = 0.5),
        legend.position = "none",
        text = element_text(size = 15)) +
    facet_wrap(~NM_REGIAO)
  
# Salvar gráfico
ggsave(
  "figs/graf1_total.png",
  bg = "white",
  width = 11,
  height = 6,
  dpi = 1200,
  plot = last_plot())

# TABELA 01 #####
# Tabela REGIÃO 
regiao <- dados |> 
  group_by(NM_REGIAO) |> 
  summarize(total = n()) |> 
  mutate(NM_REGIAO = recode(NM_REGIAO,
                            "NORTE" = "Norte",
                            "NORDESTE" = "Nordeste",
                            "CENTRO-OESTE" = "Centro-Oeste",
                            "SUDESTE" = "Sudeste",
                            "SUL" = "Sul"),
         frequencia = round(total/sum(total)*100,2),
         variavel = "Região") |> 
    arrange(desc(total)) |> 
    rename(categorias = NM_REGIAO) 

# Tabela ESTATUTO JURÍDICO 
estatuto <- dados |> 
  group_by(CS_STATUS_JURIDICO, GENERO) |> 
  summarize(total = n()) |> 
  mutate(frequencia = round(total/sum(total)*100,2),
         variavel = "Estatuto Jurídico") |> 
  arrange(desc(total)) 

# Tabela MODALIDADE 
modalidade <- dados |> 
  group_by(NM_MODALIDADE_PROGRAMA, GENERO) |> 
  summarize(total = n()) |> 
  mutate(frequencia = round(total/sum(total)*100,2),
         variavel = "Modalidade") |> 
  arrange(desc(total)) 

# Tabela CATEGORIA 
categoria <- dados |> 
  group_by(DS_CATEGORIA_DOCENTE, GENERO) |> 
  summarize(total = n()) |> 
  mutate(frequencia = round(total/sum(total)*100,2),
         variavel = "Categoria") |> 
  arrange(desc(total)) 

# Tabela CONCEITO 
categoria <- dados |> 
  group_by(CD_CONCEITO_PROGRAMA, GENERO) |> 
  summarize(total = n()) |> 
  mutate(frequencia = round(total/sum(total)*100,2),
         variavel = "Conceito") |> 
  arrange(desc(total)) 

