####Pacotes####
library(tidyverse)
library(here)
library(ggtext)


# Banco 
dados <- read.csv("dados/dados_docentes.csv") |> 
  mutate_if(is.character, as.factor) 

# Evolução Tempo - Total ####
dados_tempo <- dados |> 
  group_by(AN_BASE, GENERO) |> 
  summarize(total = n()) |> 
  mutate(frequencia = round(total/sum(total)*100,2))


dados_total <- dados |> 
  filter(DS_CATEGORIA_DOCENTE == "PERMANENTE") |> 
  group_by(AN_BASE, GENERO) |> 
  summarize(total = n()) |> 
  mutate(frequencia = round(total/sum(total)*100,2))

# Gráfico 01 - Evolução todos ao longo do tempo####
dados |> 
  ggplot(aes(x = AN_BASE, 
             fill = GENERO)) +
  geom_bar(position = "fill")  +
  scale_x_continuous(limits = c(2003, 2023), breaks = seq(1990, 2021, 5)) +
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
        text = element_text(size = 15))
# Salvar gráfico
ggsave(
  "figs/graf1_total.png",
  bg = "white",
  width = 11,
  height = 6,
  dpi = 1200,
  plot = last_plot())
