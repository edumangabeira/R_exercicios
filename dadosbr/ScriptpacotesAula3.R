# Dados Brasileiros no R
# Patrick Maia e Daniel Duque
# 02/12/2019

install.packages("electionsBR")
install.packages("devtools")
devtools::install_github('wilsonfreitas/rbcb')
devtools::install_github("lucasmation/microdadosBrasil")
devtools::install_github("rfsaldanha/microdatasus")
install.packages("sidrar")
install.packages("BETS")
install.packages("survey")
install.packages("stringi")
install.packages(sf)
install.packages(ggplot2)



library(knitr)
library(dplyr)

# Pacotes de Dados Brasileiros
# Dados Macro 

# SidraR: Dados do IBGE
# rbcb: Dados do Banco Central
# BETS: Dados de origens diversas

# Pacotes para Dados Micro no Brasil

# PNADcIBGE: Dados da PNADC
# MicrodadosBR: Dados de origens diversas
# Microdatasus: Dados do DataSUS
# ElectionsBR: Dados eleitorais

#### SidraR ####

# Basicamente, permite baixar qualquer tabela que esteja no SIDRA - IBGE
# É possivel escolher qualquer periodo ou subgrupo do site
# O maior problema é que é preciso saber o numero das tabelas
# Sabendo disso, no R você descobre por onde pode explorar


## Renda Média na PNAD por UF

library(sidrar)
info_sidra(1860)
estados_renda <- get_sidra(1860, 
                           variable = 772,
                           period = "all",
                           geo = "State",
                           classific = c("c12021"),
                           category = list(106827))

View(estados_renda)

# Plotando a Renda Media em 2014

plot(subset(estados_renda, Ano == 2014)$`Unidade da Federação (Código)`,
     subset(estados_renda, Ano == 2014)$Valor,
     ylab = "Renda Média em 2014", xlab = "Código da UF")

#### RBCB ####

# Pode baixar dados não apenas de series que o BC adquire de outras entidades,
# mas tambem as que produz.
# Por exemplo, as de **expectativas do mercado.**
# É fácil ajustar o resultado da chamada para o período que você está analisando.

## IPCA, Dólar Corrente e Expectativa do PIB

library(rbcb)
ipca <- get_series(c(IPCA = 433), last = 24)
dolar <- get_currency("USD",
                      "2017-11-30", 
                      "2019-11-26")

expPIBnov19 <- get_quarterly_market_expectations("PIB Total",
                                                 end_date = "2019-11-22", 
                                                 `$top` = 5)

## Plotando Dólar e Inflação

plot(x = ipca$date, y = ipca$IPCA, 
     ylab="IPCA",  xlab="", 
     type="l", col="red")

## Plotando Dolar e Inflação

plot(x = dolar$date, y = dolar$ask,
     ylab = "Dolar", xlab = "",
     type = "l", col = "green")

## Plotando Expecativa do PIB futuro

plot(x = expPIBnov19$reference_quarter, y = expPIBnov19$mean,
     ylab = "Var PIB",  xlab = "Trimestre")


#### BETS ####

## Base de dados de diversas fontes:
# BCB
# IBGE
# FGV (e Ibre)
# Sidra

#  No entanto, bastante pesado.

## Fontes de Dados

library(BETS)
BETSsources()

## Confiança do Consumidor

confs <- BETSsearch(description = "confidence")

View(confs)

confd <- BETSget(4393, 
                 data.frame = TRUE)

View(confd)

## Plotando a Confiança do Consumidor

plot(x = confd$date, y = confd$value,
     ylab = "Confiança do Consumidor", xlab = "Data",
     type = "l")

## Dados Micro: PNADcIBGE

# Baixa e/ou lê os microdados das PNAD Contínuas
# Pode baixar um subset pré definido de variáveis
# Pode baixar já com os pesos amostrais complexos
# Sua origem vem de um iniciativa de leitura de 
# praticamente todo microdado do IBGE: http://asdfree.com/
  
## Exemplo: PNADC 2019-03
  
library(PNADcIBGE)
library(survey)

dadosPNADc <- get_pnadc(year = 2019, 
                        quarter = 3, 
                        vars=c("V2009","VD4002","VD4019"),
                        design=TRUE)


## PNADC 2019-03: Desemprego por idade 

svyciprop(~VD4002, design = subset(dadosPNADc, V2009 >= 18 & V2009 <= 24))
svyciprop(~VD4002, design = subset(dadosPNADc, V2009 >= 25 & V2009 <= 34))
svyciprop(~VD4002, design = subset(dadosPNADc, V2009 >= 35 & V2009 <= 44))
svyciprop(~VD4002, design = subset(dadosPNADc, V2009 >= 45 & V2009 <= 64))
svyciprop(~VD4002, design = subset(dadosPNADc, V2009 >= 65 & V2009 <= 80))

## PNADC 2019-03: Renda do Trabalho por idade 

svymean(~VD4019,design=subset(dadosPNADc,V2009 >= 18 & V2009 <= 24),na.rm=T)
svymean(~VD4019,design=subset(dadosPNADc,V2009 >= 25 & V2009 <= 34),na.rm=T)
svymean(~VD4019,design=subset(dadosPNADc,V2009 >= 35 & V2009 <= 44),na.rm=T)
svymean(~VD4019,design=subset(dadosPNADc,V2009 >= 45 & V2009 <= 64),na.rm=T)
svymean(~VD4019,design=subset(dadosPNADc,V2009 >= 65 & V2009 <= 80),na.rm=T)

#### MicrodadosBR ####

## Agrega diversas fontes de microdados:
# PNAD
# Censo
# PME
# POF
# Censo Escolar
# Censo Ed. Superior
# Caged
# RAIS


## Baixando o Censo Ed. Superior

library('microdadosBrasil')

download_sourceData("CensoEducacaoSuperior", 
                    i = 2004, 
                    unzip = TRUE)

institutos <- read_CensoEducacaoSuperior(ft = 'ies', i = 2004)

## Valor investido em pesquisa por Tipo de Instituição

tab <- as.data.frame(table(institutos$DS_CATEGORIA_ADMINISTRATIVA))

for(x in 1:6){
  tab$Pesquisa[x] = sum(subset(institutos,
                               DS_CATEGORIA_ADMINISTRATIVA==tab$Var1[x])$VL_DES_PESQUISA)
}

View(tab)

#### Microdatasus ####

# Basicamente baixa e limpa dados disponíveis no DataSUS
# Por sua vez, os dados do DataSUS são bem completos.


## Infartos por município no Rio de Janeiro

library(microdatasus)

infartos <- c("I21", "I210", "I211", "I212", "I213", "I214", "I219")
RJsus <- fetch_datasus(year_start = 2015, 
                       year_end = 2017, 
                       uf = "RJ", 
                       information_system = "SIM-DO")
RJsus <- process_sim(RJsus)
RJsus <- RJsus %>%
  filter(CAUSABAS %in% infartos | CAUSABAS_O %in% infartos) %>% 
  rename(code_muni=CODMUNRES)

infartosRJ <- as.data.frame(table(RJsus$code_muni)) %>% 
  rename(code_muni=Var1)

## Infartos por município no Rio de Janeiro

# Agora vamos ter o nome dos municípios e seus códigos em um dataframe

RJsus <- RJsus %>% select(code_muni,munResNome) %>% unique %>%
  mutate(code_muni = as.character(code_muni))

#Agora juntando-os

infartosRJ  <- left_join(infartosRJ,RJsus,by="code_muni") %>% 
  filter(code_muni != "330000")

#### geobr e brazilmaps ####

# geobr baixa vários shapefiles do Brasil em formato sf.
# Já o brazilmaps baixa os shapefiles em vários formatos.
# O brazilmaps também tem informações geográficas.

## Instalação e uso

library(geobr)
library(sf)
library(brazilmaps)

RJ <- read_municipality(code_muni=33,year=2010) %>%
  mutate(code_muni=as.character(substr(code_muni,1,6)))

PopRJ <- brazilmaps::pop2017 %>%
  mutate(UF=substr(mun,1,2),
         code_muni=as.character(substr(mun,1,6))) %>%
  filter(UF=="33") 

## Merge com nossa base de dados 

RJ <- merge(RJ,infartosRJ)  
RJ <- merge(RJ,PopRJ)

## Agora o Gráfico: ggplot

library(ggplot2)

# Fazer o Gráfico

plot1 <- ggplot()+
  geom_sf(data=RJ,aes(fill=((Freq/pop2017)*1000)),
          color = "grey") +
  scale_fill_distiller(palette = "Blues",
                       direction=1)+
  guides(fill = guide_legend(reverse = TRUE))+
  labs(x="",y="",title="Infartos no RJ",
       fill="Infartos por 100 mil habitantes")+
  coord_sf(datum = NA)

## E voilà 

plot1

#### ElectionsBR ####

# Basicamente, baixa dados do TSE, a um nível super desagregado.
# No entanto, os dados são pesados. 

library(electionsBR)

# Que tal um mapa eleitoral do Flávio Bolsonaro?

Senadores <- vote_mun_zone_fed(2018, uf="RJ") %>%
  select("SIGLA_UF", "SIGLA_PARTIDO", 
         "DESCRICAO_CARGO", "CODIGO_MUNICIPIO", 
         "TOTAL_VOTOS","NOME_MUNICIPIO", 
         "NOME_URNA_CANDIDATO", "NUM_TURNO","NUMERO_ZONA") %>%
  filter(DESCRICAO_CARGO == "Senador") %>%
  arrange(NOME_URNA_CANDIDATO,NOME_MUNICIPIO) 

library(stringi)
Senadores <- Senadores %>%  
  mutate(NOME_MUNICIPIO = stri_trans_general(NOME_MUNICIPIO, "Latin-ASCII"),
         NOME_MUNICIPIO=as.character(NOME_MUNICIPIO)) 


SenadoresDef1 <- Senadores %>% 
  group_by(NOME_MUNICIPIO) %>% 
  summarise(SUMMun = sum(TOTAL_VOTOS))

Flavio <- Senadores %>% 
  group_by(SIGLA_UF,NOME_MUNICIPIO,NOME_URNA_CANDIDATO,SIGLA_PARTIDO) %>% 
  summarise(SUMCand = sum(TOTAL_VOTOS)) %>% 
  filter(NOME_URNA_CANDIDATO=="FLÁVIO BOLSONARO") 

Flavio <- merge(Flavio,SenadoresDef1,by=c("NOME_MUNICIPIO")) %>%
  mutate(Percentual=SUMCand/SUMMun)

# Usar pacote geobr para pegar mapa dos Municípios por Estado  
library(geobr)

RJ <- read_municipality(code_muni="RJ", year=2010)

RJ <- RJ %>% mutate(NOME_MUNICIPIO = toupper(name_muni),
                    NOME_MUNICIPIO = stri_trans_general(NOME_MUNICIPIO, "Latin-ASCII"),
                    NOME_MUNICIPIO=as.character(NOME_MUNICIPIO))

Flavio <- left_join(RJ,Flavio,by="NOME_MUNICIPIO")

plot2 <-   ggplot() + geom_sf(data=Flavio, aes(fill= Percentual),
                              color = "grey", size = 0.01) +
  scale_fill_distiller(palette = "Blues",
                       direction=1)+
  guides(fill = guide_legend(reverse = TRUE))+
  geom_sf_text(data=Flavio,
               aes(label = paste(paste(NOME_MUNICIPIO,
                                       round(Percentual,digits=0),
                                       sep=" "),"%",sep=""))
               , colour = "black",size=0.65)+
  labs(x="",y="",title="Flávio Bolsonaro - PSL/RJ")+
  coord_sf(datum = NA)

# Mapa eleitoral do Flávio Bolsonaro

plot2
