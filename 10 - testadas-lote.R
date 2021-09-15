library(sf)
library(nngeo)
library(dplyr)
library(tibble)
library(ggplot2)
library(stringr)

#### carregando os arquivos
arquivo <- "01 - poc-teste-faces-lote.gpkg"
ruas <- st_read( arquivo , layer = "logradouros" )
lotes <- st_read( arquivo , layer = "lotes" )

#### transformando ruas em tabela pra juntar as informações posteriormente
ruas_tabela <- ruas %>%
  # tirando a geometria
  st_drop_geometry() %>%
  # transformando índice em coluna em formato de número
  rownames_to_column(var = "index") %>%
  mutate( index = as.numeric(index) )
  

######## extraindo faces externas da quadra
#### buffer da borda da quadra
quadras_borda <- lotes %>%
  # agregando por quadra
  group_by( lo_setor , lo_quadra )%>%
  summarise() %>%
  ungroup() %>%
  # transformando em linhas
  st_cast( to = "MULTILINESTRING" ) %>%
  st_buffer( 1 )

#### lista de ruas associadas a cada lote
# pegando só as linhas externas do lote
lotes_ruas <- st_segments(lotes) %>%
  # extraindo só aqueles dentro do buffer da borda
  st_join( quadras_borda , join = st_within, left = FALSE) %>%
  # verificando se caiu na borda da própria quadra e removendo/renomeando depois da checagem
  filter( (lo_setor.x == lo_setor.y) & (lo_quadra.x == lo_quadra.y) ) %>%
  # descobrindo qual a rua mais próxima de cada linha
  mutate( rua_próxima = ( st_nn( . , ruas ) %>% unlist() ) ) %>%
  # juntando as informações da rua pelo índice
  inner_join( ruas_tabela , by = c("rua_próxima" = "index") ) %>%
  # removendo as colunas que não precisa e renomeando
  select( -c("lo_setor.y","lo_quadra.y","result") ) %>%
  rename_with( ~str_replace_all( .x , ".x" , "" ) ) %>%
  # resumindo e contabilizando o número de testadas
  group_by( lo_setor, lo_quadra, lo_lote ) %>%
  mutate( lo_testadas = n_distinct( lg_nome ) ) %>%
  # removendo duplicados
  distinct( lg_nome , .keep_all = TRUE) %>%
  ungroup()

# resumindo os números de testadas por setor, quadra, lote
lotes_ruas_tabela <- lotes_ruas %>%
  st_drop_geometry() %>%
  group_by( lo_setor , lo_quadra , lo_lote ) %>%
  distinct( lo_testadas )

#### testadas por lote
lotes <- lotes %>% left_join( lotes_ruas_tabela )