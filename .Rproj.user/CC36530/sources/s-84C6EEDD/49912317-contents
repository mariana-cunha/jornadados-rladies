#Preparação
library(tidyverse)

#importar a base
raw_base <- read.csv2("data/base_legislativo_2018.csv", encoding = "UTF-8")

glimpse(raw_base)

mais_votados <- raw_base %>%
  select(NOME_URNA_CANDIDATO, SIGLA_UF , SIGLA_PARTIDO, 
         DESCRICAO_CARGO, QTDE_VOTOS, DESC_SIT_TOT_TURNO,
         valor_receita, valor_despesa_contratada) %>%
  filter(DESCRICAO_CARGO == "DEPUTADO FEDERAL") %>%
  arrange(desc(QTDE_VOTOS)) %>%
  head(20)
  View()
  
  maior_receita <- raw_base %>%
    select(NOME_URNA_CANDIDATO, SIGLA_UF , SIGLA_PARTIDO, 
           DESCRICAO_CARGO, QTDE_VOTOS, DESC_SIT_TOT_TURNO,
           valor_receita, valor_despesa_contratada) %>%
    #filter(SIGLA_UF == "MA") %>%
    arrange(desc(valor_receita)) %>%
    head(10) %>%
    View()
  
melhor_investimento <- raw_base %>%
  select(NOME_URNA_CANDIDATO, SIGLA_UF , SIGLA_PARTIDO, 
         DESCRICAO_CARGO, QTDE_VOTOS, DESC_SIT_TOT_TURNO,
         valor_receita, valor_despesa_contratada) %>%
  filter (str_detect(DESC_SIT_TOT_TURNO, "ELEITO"),
          DESC_SIT_TOT_TURNO != "NÃO ELEITO",
          QTDE_VOTOS > 100000) %>%
  mutate(custo_voto = valor_receita/QTDE_VOTOS) %>%
  arrange(QTDE_VOTOS)%>%
  head(10)%>%
  View()


media_votos <- raw_base %>%
  mutate(QTDE_VOTOS = ifelse(is.na(QTDE_VOTOS), 0, QTDE_VOTOS)) %>%
  select(NOME_URNA_CANDIDATO, SIGLA_UF , SIGLA_PARTIDO, 
         DESCRICAO_CARGO, QTDE_VOTOS, DESC_SIT_TOT_TURNO,
         valor_receita, valor_despesa_contratada) %>%
  filter(DESCRICAO_CARGO %in% c("DEPUTADO FEDERAL", "DEPUTADO ESTADUAL"))%>%
  group_by(SIGLA_UF, DESCRICAO_CARGO, DESC_SIT_TOT_TURNO) %>%
  summarise(votos_media = mean(QTDE_VOTOS)) %>%
  View()
