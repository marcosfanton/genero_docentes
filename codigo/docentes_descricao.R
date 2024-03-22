####Pacotes####
library(tidyverse)
library(here)
library(ggtext)
library(gt)

# Banco 
dados <- read.csv("dados/dados_docentes.csv") |> 
  mutate_if(is.character, as.factor) |> 
  filter(AN_BASE == 2022)


# Gráfico 01 - Evolução Docentes Permanentes####
dados |> 
#  drop_na(CD_CONCEITO_PROGRAMA) |> 
#  filter(NM_MODALIDADE_PROGRAMA == "Acadêmico" & DS_CATEGORIA_DOCENTE == "Permanente") |> 
  ggplot(aes(x = AN_BASE, 
             fill = GENERO)) +
  geom_bar(position = "fill")  +
  scale_x_continuous(limits = c(2003, 2023)) +
  scale_y_continuous(labels=scales::percent, position = "right") +
  scale_fill_manual(values = c("Homem" = "#59106e",
                               "Mulher" = "#d24644")) +
  theme_classic() +
  labs(x = "",
       y = "",
       fill = "",
       title = "Proporção de Docentes Permanentes em PPGs de Filosofia por Gênero (1994-2022)",
       subtitle = "**N**: 13.294 | <span style= 'color:#59106e;'>**Homem**</span>: 79.66% - <span style= 'color:#d24644;'>**Mulher**</span>: 20.34%",
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

# TABELA 01 #####
# Tabela REGIÃO 
regiao <- dados |> 
  group_by(NM_REGIAO, GENERO) |> 
  summarize(total = n()) |> 
  mutate(NM_REGIAO = recode(NM_REGIAO,
                            "NORTE" = "Norte",
                            "NORDESTE" = "Nordeste",
                            "CENTRO-OESTE" = "Centro-Oeste",
                            "SUDESTE" = "Sudeste",
                            "SUL" = "Sul"),
         frequencia = round(total/sum(total)*100,2),
         variavel = "Região") |> 
    rename(categorias = NM_REGIAO) 

# Tabela ESTATUTO JURÍDICO 
estatuto <- dados |> 
  group_by(CS_STATUS_JURIDICO, GENERO) |> 
  summarize(total = n()) |> 
  mutate(frequencia = round(total/sum(total)*100,2),
         variavel = "Estatuto") |> 
  rename(categorias = CS_STATUS_JURIDICO) 

# Tabela MODALIDADE 
modalidade <- dados |> 
  group_by(NM_MODALIDADE_PROGRAMA, GENERO) |> 
  summarize(total = n()) |> 
  mutate(frequencia = round(total/sum(total)*100,2),
         variavel = "Modalidade") |> 
  rename(categorias = NM_MODALIDADE_PROGRAMA) 

# Tabela CATEGORIA 
categoria <- dados |> 
  group_by(DS_CATEGORIA_DOCENTE, GENERO) |> 
  summarize(total = n()) |> 
  mutate(frequencia = round(total/sum(total)*100,2),
         variavel = "Categoria") |> 
  rename(categorias = DS_CATEGORIA_DOCENTE) 

# Tabela CONCEITO 
conceito <- dados |> 
  group_by(CD_CONCEITO_PROGRAMA, GENERO) |> 
  summarize(total = n()) |> 
  mutate(frequencia = round(total/sum(total)*100,2),
         variavel = "Conceito") |> 
  rename(categorias = CD_CONCEITO_PROGRAMA) 

# Tabela BOLSA PQ 
bolsa <- dados |> 
  group_by(CD_CAT_BOLSA_PRODUTIVIDADE, GENERO) |> 
  summarize(total = n()) |> 
  rename(categorias = CD_CAT_BOLSA_PRODUTIVIDADE) |> 
  mutate(frequencia = round(total/sum(total)*100,2),
         variavel = "Bolsa PQ",
         categorias = fct_na_value_to_level(categorias, "Sem Bolsa"))

# Tabela FORMAÇÃO 
formacao <- dados |> 
  group_by(FORMACAO, GENERO) |> 
  summarize(total = n()) |> 
  mutate(frequencia = round(total/sum(total)*100,2),
         variavel = "Formação") |> 
  rename(categorias = FORMACAO)

# Junção das tabelas 
tabela1 <- bind_rows(regiao,
                     categoria,
                     modalidade,
                     estatuto,
                     conceito,
                     bolsa,
                     formacao) 

# Deixa tabela no formato
tabela1 <- tabela1 |> tidyr::pivot_wider(names_from = "GENERO", values_from = c("total", "frequencia"), 
                   names_glue = "{GENERO}_{.value}")

# Construção da TABELA 1#### 
tab1 <- 
tabela1 |> 
gt(groupname_col = "variavel",
   rowname_col = "categorias") |> 
  cols_merge(
    columns = c(Homem_total, Homem_frequencia),
    pattern = "{1} ({2}%)") |>  
  cols_merge(
    columns = c(Mulher_total, Mulher_frequencia),
    pattern = "{1} ({2}%)") |> 
  cols_label(
    Homem_total = "Homem",
    Mulher_total = "Mulher") |> 
  fmt_number(
    drop_trailing_zeros = TRUE,
    decimals = 2,
    sep_mark = ".",
    dec_mark = ",") |>   
  tab_header(
    title = md("**Tabela 1:** Descrição de Docentes em PPGs de Filosofia por Gênero (2022) | N: 1.350")
  ) |> 
  tab_source_note(
    source_note = "Elaboração: Dataphilo. Dados: CAPES."
  ) |> 
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_row_groups()) |>
  tab_stub_indent(
    rows = everything(),
    indent = 5
  ) |>
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels()) |> 
  sub_missing(
    columns = everything(),
    rows = everything(),
    missing_text = "0"
  )  |> 
  cols_align(
    align = "center",
    columns = c(Homem_total, Mulher_total)
  ) |> 
  cols_width(categorias ~ px(115),
             variavel ~ px(80),
             Homem_total ~ px(100),
             Mulher_total ~ px(100)) |>  
 # opt_table_font(font = google_font(name = "Gentium Book Basic")) |> 
  tab_options(heading.title.font.size = px(12),
              table.font.size = px(12),
              row_group.padding = px(1),
              data_row.padding =  px(1.5),
              source_notes.font.size = px(10)) |> 
  tab_style(
    cell_borders(sides = c("bottom", "top"), color = "black"),
    locations = cells_title()) |>
  tab_style(
    cell_borders(sides = c("bottom", "top"), color = "black"),
    locations = cells_source_notes()) |> 
  opt_table_lines("none")

#Salvar
gtsave(tab1, 
       "docentes_tab1.png", 
       path = "figs",
       expand = 20)

# Tabela 02 - PPGs
tabela2 <- dados |> 
  filter(NM_MODALIDADE_PROGRAMA == "Acadêmico" & DS_CATEGORIA_DOCENTE == "Permanente") |> 
  group_by(NM_PROGRAMA, GENERO, FORMACAO) |> 
  summarize(total = n()) |> 
  mutate(frequencia = round(total/sum(total)*100,2)) |> 
  tidyr::pivot_wider(names_from = "GENERO",
                     values_from = c("total", "frequencia"), 
                     names_glue = "{GENERO}_{.value}") |> 
 # arrange(desc(Mulher_frequencia)) |> 
  ungroup()

# Construção Tabela 2
tab3 <-
tabela2 |> 
  gt(rowname_col  = "NM_PROGRAMA") |> 
  cols_merge(
    columns = c(Homem_total, Homem_frequencia),
    pattern = "{1} ({2}%)") |>  
  cols_merge(
    columns = c(Mulher_total, Mulher_frequencia),
    pattern = "{1} ({2}%)") |> 
  cols_label(
    Homem_total = "Homem",
    Mulher_total = "Mulher",
    FORMACAO = "Formação") |> 
fmt_number(
  drop_trailing_zeros = TRUE,
  decimals = 2,
  sep_mark = ".",
  dec_mark = ",") |>   
  tab_header(
    title = md("**Tabela 2:** Docentes Permanentes de PPGs Acadêmicos de Filosofia por Gênero (2022)")
  ) |> 
  tab_source_note(
    source_note = "Elaboração: Dataphilo. Dados: CAPES."
  ) |> 
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_row_groups()) |>
  tab_stub_indent(
    rows = everything(),
    indent = 5
  ) |>
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels()) |> 
  sub_missing(
    columns = everything(),
    rows = everything(),
    missing_text = "0"
  )  |> 
  cols_align(
    align = "center",
    columns = c(Homem_total, Mulher_total)
  ) |> 
  cols_width(NM_PROGRAMA ~ px(230),
             Homem_total ~ px(100),
             Mulher_total ~ px(100)) |>  
  # opt_table_font(font = google_font(name = "Gentium Book Basic")) |> 
  tab_footnote(
    footnote = "Área de Conhecimento da última titulação. Filosofia: História da Filosofia, Metafísica, Lógica, Ética, Epistemologia, Filosofia Brasileira.",
    locations = cells_column_labels(columns = FORMACAO)
  ) |> 
  tab_options(heading.title.font.size = px(12),
              table.font.size = px(12),
              row_group.padding = px(1),
              data_row.padding =  px(1.5),
              source_notes.font.size = px(10)) |> 
  tab_style(
    cell_borders(sides = c("bottom", "top"), color = "black"),
    locations = cells_title()) |>
  tab_style(
    cell_borders(sides = c("bottom", "top"), color = "black"),
    locations = cells_source_notes()) |> 
  opt_table_lines("none")

#Salvar
gtsave(tab3, 
       "docentes-formacao_tab3.png", 
       path = "figs",
       expand = 20)
