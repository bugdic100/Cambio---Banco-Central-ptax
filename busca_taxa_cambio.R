library(lubridate)
library(magrittr)
library(dplyr)
library(stringr)
library(readr)
library(httr)
library(ggplot2)
library(tidyr)

# Tabelas de tradução dos códigos da base
tb_moeda <- read_delim(file = "tb_moedas.csv",delim = ";",
                       col_names = c("id_moeda","no_moeda","co_moeda","co_pais","no_pais","tp_moeda","dt_exc"),
                       col_types = 'ccccccc',
                       skip = 1,
                       locale = locale(encoding = "iso-8859-1")) %>% 
  mutate(dt_exc = dmy(str_replace_all(dt_exc,'\\/','-')),
         no_tp_moeda = ifelse(tp_moeda == "A",'moeda estrangeira por unidade de dólar',
                              'unidade de dólar por moeda estrangeira')) %>% 
  as.data.frame()

mes_abb <- c('jan','Fev','Mar','Abr','Mai','Jun','Jul','Ago','Set','Out','Nov','Dez')

max_ano <- year(Sys.Date())

# Cria calendário para busca na base do banco central

if(day(Sys.Date()) >= 10){

calendario <- data.frame(co_data = seq(as.Date(paste0(year(Sys.Date()),'-01-01')),
                                  as.Date(paste0(year(Sys.Date()),'-12-31')),"days")) %>%  
  filter(month(co_data) == month(Sys.Date())) %>%
  mutate(co_data = str_replace_all(string = as.character(co_data),pattern = '\\-','')) %>% 
  pull(co_data)

 max_mes_nome <- mes_abb[month(Sys.Date())]
}else{
  calendario <- data.frame(co_data = seq(as.Date(paste0(year(Sys.Date()),'-01-01')),
                                         as.Date(paste0(year(Sys.Date()),'-12-31')),"days")) %>%  
    filter(month(co_data) == month(Sys.Date())-1) %>%
    mutate(co_data = str_replace_all(string = as.character(co_data),pattern = '\\-','')) %>% 
    pull(co_data)
  
  max_mes_nome <- mes_abb[month(Sys.Date())-1]
}
# Importa base de câmbio do banco central 

base_cambio <- data.frame() 

for(dia in calendario){
  
  avalia <- GET(paste0("https://www4.bcb.gov.br/Download/fechamento/",dia,".csv"))
  
  if(status_code(avalia) == 200){
    
    df <- read_delim(file = paste0("https://www4.bcb.gov.br/Download/fechamento/",dia,".csv"),
                     col_names = c('co_data','id_moeda','tp_moeda','co_moeda',
                                   'tx_compra','tx_venda','paridade_compra','paridade_venda'),
                     delim = ";",col_types = 'cccccccc') %>%  
      mutate(co_data = dmy(str_replace_all(string = co_data,pattern = '\\/',replacement = '-'))) %>% 
      apply(MARGIN = 2,FUN = function(x){str_replace(string = x,pattern = '\\,',replacement = '.')}) %>% 
      as.data.frame() %>% 
      mutate(co_data = as.Date(co_data),
             id_moeda = as.character(id_moeda),
             tp_moeda = as.character(tp_moeda),
             co_moeda = as.character(co_moeda),
             tx_compra = as.double(tx_compra),
             tx_venda = as.double(tx_venda),
             paridade_compra = as.double(paridade_compra),
             paridade_venda = as.double(paridade_venda))
    
    base_cambio <- rbind(base_cambio,df)
    rm(df)
    rm(avalia)
  }
}

# moedas_nao_excluidas
moedas_nao_excluidas <- tb_moeda %>% 
  filter(is.na(dt_exc)) %>% 
  select(id_moeda,no_moeda,no_pais) 

base_cambio_final <- base_cambio %>% 
  left_join(moedas_nao_excluidas,"id_moeda") %>% 
  apply(X = .,MARGIN = 2,FUN = function(x){str_trim(x,"both")}) %>% 
  as.data.frame() %>% 
  mutate(co_data = ymd(co_data))

dados.finais <- list(max_mes_nome = max_mes_nome,
                     max_ano = max_ano,
                     base_cambio_final = base_cambio_final)

path <- getwd()

rmarkdown::render(input = './vizualiza_cambio.Rmd',
                  output_file = paste0(path,'/',"cambio.pdf"),
                  params = list(dados = dados.finais),
                  encoding="UTF-8")
