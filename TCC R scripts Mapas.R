library(dplyr)
library(ggplot2)
library(odbc)
library(tibble)
library(tidyverse)
library(tidyquant)
library(tmap)
library(sp)
library(rgdal)
library(RColorBrewer)


con <- DBI::dbConnect(odbc(),
                      Driver   = 'ODBC Driver 11 for SQL Server',
                      Server   = "XXXXXXXX",
                      Database = "XXXXXXXX",
                      UID      = 'XXXXXXXX',
                      PWD      = 'XXXXXXXX',
                      Port     = XXXXXXXX,
                      encoding = "latin1")



qrt <- dbGetQuery(con,
                  "
SELECT  [IdAgente]
      ,[Sigla]
      ,[ClassifAgente]
      ,[Categoria]
      ,[Tipologia]
      ,SubCategoria
      ,[Decisao]
      ,[Situacao]
      ,[AnoCriacao]
      ,[MesCriacao]
      ,[QtdeMes]
      ,[QtdeMovel]
      ,[Numcons]
      ,[QRT]
      ,[QRTMovel]
  FROM XXXXXX

                         
")

cor <- qrt %>% filter (AnoCriacao == 2020 & Categoria == "Reclamações" ) %>% 
  group_by(IdAgente) %>% summarise(rec = sum(QRT))



concessionaria <- readOGR(dsn="C:\\Users\\maxwellcarvalho\\ANEEL\\Atividades NIAAD - Documentos\\Ciência de dados\\Scripts R\\Shape_ARAT\\ARAT.shp")


brasilrec <- merge(concessionaria, cor, by.x = "ID_AGENTE", by.y="IdAgente")



tm_shape(brasilrec) + tm_polygons(col='rec')


mapa <- tm_shape(brasilrec) + tm_polygons(col='rec') + tm_facets(by='Regiao')


tmap_save(mapa, "reclamacoes2020.png", width=1920, height=1080, asp=0)


subqual <- qrt %>% filter (AnoCriacao == 2020 & Categoria == "Reclamações" & SubCategoria == 'Qualidade do Fornecimento') %>% 
  group_by(IdAgente) %>% summarise(rec = sum(QRT))


brasilqual <- merge(concessionaria, subqual, by.x = "ID_AGENTE", by.y="IdAgente")

mapa <- tm_shape(brasilqual) + tm_polygons(col='rec') + tm_facets(by='Regiao')

tmap_save(mapa, "reclamacoesqual2020.png", width=1920, height=1080, asp=0)



subfat <- qrt %>% filter (AnoCriacao == 2020 & Categoria == "Reclamações" & SubCategoria == 'Faturamento') %>% 
  group_by(IdAgente) %>% summarise(Reclamacoes = sum(QRT))


brasilfat <- merge(concessionaria, subfat, by.x = "ID_AGENTE", by.y="IdAgente")

mapa <- tm_shape(brasilfat) + tm_polygons(col='Reclamacoes') +    tm_facets(by='Regiao')+
         tm_layout(title='Reclamações de Faturamento a cada 10 mil UCs', title.position = c("center","top"), title.size = 40)

tmap_save(mapa, "reclamacoesfat2020.png", width=1920, height=1080, asp=0)

subcob <- qrt %>% filter (AnoCriacao == 2020 & Categoria == "Reclamações" & SubCategoria == 'Cobranças') %>% 
  group_by(IdAgente) %>% summarise(Reclamacoes = sum(QRT))


brasilcob <- merge(concessionaria, subfat, by.x = "ID_AGENTE", by.y="IdAgente")

mapa <- tm_shape(brasilcob) + tm_polygons(col='Reclamacoes', textNA = "Sem reclamações") +    tm_facets(by='Regiao')

tmap_save(mapa, "reclamacoescob2020.png", width=1920, height=1080, asp=0)


str(concessionaria, max.level = 3)
