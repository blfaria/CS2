require(dplyr)

##http://www.portaldatransparencia.gov.br/downloads/servidores.asp
#Total remuneracao servidores CDT por estado

#for fix buggg
Sys.setlocale("LC_ALL", "C")

#carregando dataframes das bases de dados
dfr = read.csv("20180430_Remuneracao.csv",sep="\t",dec = ",")
dfc = read.csv("20180430_Cadastro.csv",sep="\t",dec = ",")

#create dataframe filter CDT
CDT = dfc%>%
  filter( REGIME_JURIDICO == "CONTRATO TEMPORARIO")

#Merge nas tabelas
merge = merge(dfr, CDT, by="CPF")

#filtro educacao
merge = merge%>%
  filter( ORGSUP_EXERCICIO== "MINISTERIO DA EDUCACAO")



##filtro norte

NORTE = merge%>%
  filter( UF_EXERCICIO == "AM" | UF_EXERCICIO == "PA" | UF_EXERCICIO == "AC" | UF_EXERCICIO == "RR" | UF_EXERCICIO == "RO" | UF_EXERCICIO == "TO" |  UF_EXERCICIO == "AP")%>% 
  group_by(ORG_EXERCICIO) %>% 
  summarise(REMUNERA..O.B.SICA.BRUTA..R.. = sum(REMUNERA..O.B.SICA.BRUTA..R..))

somanorte = sum(NORTE$REMUNERA..O.B.SICA.BRUTA..R..)/1000000

#filtro nordeste

NORDESTE = merge%>%
  filter( UF_EXERCICIO == "AL" | UF_EXERCICIO == "BA" | UF_EXERCICIO == "CE" | UF_EXERCICIO == "MA" | UF_EXERCICIO == "PE" | UF_EXERCICIO == "PB" |  UF_EXERCICIO == "PI" | UF_EXERCICIO == "SE" |  UF_EXERCICIO == "RN")%>% 
  group_by(ORG_EXERCICIO) %>% 
  summarise(REMUNERA..O.B.SICA.BRUTA..R.. = sum(REMUNERA..O.B.SICA.BRUTA..R..))

somaNE = sum(NORDESTE$REMUNERA..O.B.SICA.BRUTA..R..)/1000000

# RJ

RJ = merge%>%
  filter( UF_EXERCICIO == "RJ" )%>% 
  group_by(ORG_EXERCICIO) %>% 
  summarise(REMUNERA..O.B.SICA.BRUTA..R.. = sum(REMUNERA..O.B.SICA.BRUTA..R..))

#merge NORTE E RJ
NORTERJ = rbind(NORTE,RJ)

somaRJ = sum(RJ$REMUNERA..O.B.SICA.BRUTA..R..)/1000000


BRA = list(AL,BA,CE,MA,PB,PE,PI,RN,SE,AM,AC,PA,RR,RO,TO,AP)
filenames = c("AL","BA","CE","MA","PB","PE","PI","RN","SE","AM","AC","PA","RR","RO","TO","AP")
N = list(AM,AC,PA,RR,RO,TO,AP)
filenames2 =  c("AM","AC","PA","RR","RO","TO","AP")


#alocacao de cargos temporarios por estado
testado = merge %>% 
  group_by(UF_EXERCICIO) %>% 
  summarize(n())

#gasto por orgao
y =merge %>% 
  group_by(ORG_EXERCICIO) %>% 
  summarise(REMUNERA..O.B.SICA.BRUTA..R.. = sum(REMUNERA..O.B.SICA.BRUTA..R..))

##media do salario por funcionario
z =merge %>% 
  group_by(UF_EXERCICIO) %>% 
  summarise(REMUNERA..O.B.SICA.BRUTA..R.. = sum(REMUNERA..O.B.SICA.BRUTA..R..)/n())

#gasato remuneracao por estado
z =merge %>% 
  group_by(UF_EXERCICIO) %>% 
  summarise(REMUNERA..O.B.SICA.BRUTA..R.. = sum(REMUNERA..O.B.SICA.BRUTA..R..))

# supersalarios
super =merge %>%
  filter( REMUNERA..O.B.SICA.BRUTA..R.. > 15000)

# ###############################  RJ
rj =merge %>%
  filter(UF_EXERCICIO == "RJ")

#numero lotados no rio por orgao e numero lotados educacao nacional
nRj =  rj %>%
  group_by(ORG_EXERCICIO) %>%
  summarize(n())

x =  merge %>%
  group_by(ORG_EXERCICIO) %>%
  summarize(n())

# supersalarios
superRJ =rj %>%
  filter( REMUNERA..O.B.SICA.BRUTA..R.. > 15000)

#orgaos maiores supersalario
OrGsuperRJ=  superRJ %>%
  group_by(ORG_EXERCICIO) %>%
  summarize(n())

#gasto por orgao RJ
rjgasto =rj %>% 
  group_by(ORG_EXERCICIO) %>% 
  summarise(REMUNERA..O.B.SICA.BRUTA..R.. = sum(REMUNERA..O.B.SICA.BRUTA..R..))

#####
###OUTputs

##planilhas
write.csv(rjgasto ,"gastoOrgaoRio.csv")



###
###VISUALIZAÇÃO
#####

library(ggplot2)
# Get quantmod

install.packages("ggthemes")
library(ggthemes)


theme_set(theme_bw())
theme_set(theme_classic())
p = ggplot(NORTERJ, aes(x = reorder (ORG_EXERCICIO, - REMUNERA..O.B.SICA.BRUTA..R..) , y = REMUNERA..O.B.SICA.BRUTA..R../1000000)) +
  geom_bar(stat='identity',  width = 0.5, fill="tomato2") +
  coord_flip() +
  theme(axis.text.x = element_text(angle=0, vjust=0.6))
p = p + ggtitle(" ") +
  xlab(" ") + ylab(" ")

p + theme_economist() +  scale_color_economist()

##visualizacao por estado
theme_set(theme_classic())



p = ggplot(merge, aes(x=UF_EXERCICIO ))+ geom_bar(stat = "count", width = 0.5, fill="tomato2") 
p = p + theme(axis.text.x = element_text(angle=0, vjust=0.6))
p = p + ggtitle(" ") +
  xlab(" ") + ylab(" ")
p = p + theme_economist() +  scale_color_economist()

p
p

