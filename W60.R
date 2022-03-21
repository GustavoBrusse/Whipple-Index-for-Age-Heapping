#Programa para estimação do Índices Whipple 60

#Baseado em Randall & Coast (2016)

#Data de criação: 5 de maio de 2017
#Elaborado pelo Grupo de Estudos em Qualidade de Dados (GEQD).
#Formação: Pedro Gomes Andrade, Gustavo Pedroso de Lima Brusse, 
#          Ana Camila Ribeiro Pereira e Kelly Cristina de Moraes Camargo.

#Responsáveis pela programação:
#Pedro Gomes Andrade         email:pedrogandrade@yahoo.com.br

#o dado de entradam vetor de população em cada idade simples de, indo desde idade zero, 60, 61, ... 95 anos (não é 95 anos e mais)

IW60 = function(data){
  ind_num = c(rep(0,60), rep(c(1,0,0,0,0),7), 1, rep(0,5))
  ind_dem = c(rep(0,58), rep(1,40), rep(0,3))
  IW60_num =  sum(data*ind_num)
  IW60_dem = (1/5)*(sum(data*ind_dem))
  IW60 = (IW60_num/IW60_dem)*100
  return(round(IW60,2))
}


#teste

setwd("C:\\Users\\PedroGomes\\Google Drive\\Artigos\\GRUPOS DE PESQUISA\\Estudos Demográficos em Qualidade de Dados\\NOTAS DE POBLA\\Dados")
dados = read.csv2(file="DADOS_NOTAS_POBLA.csv")

#AP_M_80<-dados[61:96,3]
#AP_M_80<-dados[,3] #pegando apenas a coluna de Amapá sexo masculino, ano 1980
#IW60(AP_M_80)
#plot(AP_M_80,type="l")

  Resultados = matrix(NA,nrow=(ncol(dados)-1),ncol=2)
  for (i in 1:(ncol(dados)-1)) {                                 
    Resultados[i,1] = names(dados[i+1])
    Resultados[i,2] = IW60(dados[,i+1])
  }
  Resultados

write.csv2(as.data.frame(Resultados), file="Resultados.csv")


##########################################################
#elaboração do boxplot

setwd("C:\\Users\\PedroGomes\\Google Drive\\Artigos\\GRUPOS DE PESQUISA\\Estudos Demográficos em Qualidade de Dados\\NOTAS DE POBLA\\Dados")
data. = read.csv2(file="analise.data.csv")
nrow(data)#tem que dar 72
library(ggplot2)

#boxplot

#x11()
plot1 <- ggplot(aes(y = IW60, x = factor(Decada) , fill = declaracao_idade), data = data.) +
  ggtitle("Población de ambos sexos") +  labs(x="Decenio",y="IW60+") +
  scale_fill_grey(start = 1, end = .6,labels=c("Fecha de nacimiento","Edad declarada"))+
  geom_boxplot()+
  theme(plot.title = element_text(lineheight=1.1, face="bold"))+
  scale_y_continuous(limits=c(100, 290), breaks = c(105,110,125,175, 280))+
  guides(fill=guide_legend(title="")) +
  theme(legend.position="bottom", legend.direction="horizontal")

data.full = read.csv2(file="analise.data.full.csv")
nrow(data.full)#x/3 = 72
library(ggplot2)
#boxplot


data.full.masc=subset(data.full, Sexo=="Male")

#x11()
plot2 <- ggplot(aes(y = IW60, x = factor(Decada) , fill = declaracao_idade), data = data.full.masc) +
  ggtitle("Población masculina") +  labs(x="Decenio",y="IW60+") +
  scale_fill_grey(start = 1, end = .6,labels=c("Fecha de nacimiento","Edad declarada"))+  geom_boxplot()+
  theme(plot.title = element_text(lineheight=1.1, face="bold"))+
  scale_y_continuous(limits=c(100, 290), breaks = c(105,110,125,175, 280))+
  guides(fill=guide_legend(title="")) +
  theme(legend.position="bottom", legend.direction="horizontal")
  

data.full.fem=subset(data.full, Sexo=="Female")

#x11()
plot3 <- ggplot(aes(y = IW60, x = factor(Decada) , fill = declaracao_idade), data = data.full.fem) +
  ggtitle("Población femenina") +  labs(x="Decenio",y="IW60+") +
  scale_fill_grey(start = 1, end = .6,labels=c("Fecha de nacimiento","Edad declarada"))+
  geom_boxplot()+
  theme(plot.title = element_text(lineheight=1.1, face="bold"))+
  scale_y_continuous(limits=c(100, 290), breaks = c(105,110,125,175, 280))+
  guides(fill=guide_legend(title="")) +
  theme(legend.position="bottom", legend.direction="horizontal")

ggsave("plot_all.jpeg", arrangeGrob(plot1, plot2, plot3), device="jpeg", width = 40, height = 60, units="cm", dpi = 300, limitsize = FALSE, scale=0.45)

#
#outra forma de visualizar
data.full.data=subset(data.full, Sexo!="Ambossex", declaracao_idade=="data")

x11()
plot3 <- ggplot(aes(y = IW60, x = factor(Decada) , fill = declaracao_idade), data = data.full.masc)
plot1 +  labs(x="Decade",y="IW60+", title="Male") +
  scale_fill_grey(start = 1, end = .6)+
  geom_boxplot()+
  theme(plot.title = element_text(lineheight=1.1, face="bold"))+
  scale_y_continuous(limits=c(100, 290), breaks = c(105,110,125,175, 280))+
  guides(fill=guide_legend(title="")) +
  theme(legend.position="bottom", legend.direction="horizontal")


###############################
################################################################## PIRAMIDES "PIORES" ##############################################################################################################################
install.packages("plotrix")
library(plotrix)
setwd("C:\\Users\\PedroGomes\\Google Drive\\Artigos\\GRUPOS DE PESQUISA\\Estudos Demográficos em Qualidade de Dados\\NOTAS DE POBLA\\Dados")
dados = read.csv2(file="DADOS_NOTAS_POBLA.csv")
#dados<-dados[59:nrow(dados),]
attach(dados)
require("plotrix")

#Prog. pirâmide Equador 1962
xy.pop<-(dados$Equador_1962_Male/(sum(dados$Equador_1962_Male)+ sum(dados$Equador_1962_Female)))*100
xx.pop<-(dados$Equador_1962_Female/(sum(dados$Equador_1962_Male)+ sum(dados$Equador_1962_Female)))*100
jpeg(filename = "Equador1962.jpeg", units = 'cm', width = 10.67, height = 12.57, res = 300, quality = 200, bg = 'white')
par(lwd=.8)
par(mar=pyramid.plot(xy.pop,xx.pop,
                     main="",lxcol="dark grey",rxcol="light grey",
                     gap=0,show.values=FALSE,xlim=c(3,3.5),top.labels = c("Male", "", 
                                                                          "Female"),labelcex=0.8, space=0, ppmar=c(4,4,4,0),labels=rep(NA,101)))
axis(2,at=c(0,10,20,30,40,50,60,70,80,90,100),labels=c(0,10,20,30,40,50,60,70,80,90,100),pos=-3)
mtext("Age", side=2, line = 3)
mtext("Population", side=1, line = 3.5, adj=0.46)
mtext("Ecuador 1962", side=3, line = 2, adj=0.46, font=2)
mtext("IW60+ = 273.55", side=3, line = 1, adj=0.46,font=0.1, cex=0.6)
dev.off()
#FIM Prog. pirâmide Equador 1962


#Prog. pirâmide Cuba 2002
xy.pop<-(dados$Cuba_2002_Male/(sum(dados$Cuba_2002_Male)+ sum(dados$Cuba_2002_Female)))*100
xx.pop<-(dados$Cuba_2002_Female/(sum(dados$Cuba_2002_Male)+ sum(dados$Cuba_2002_Female)))*100
jpeg(filename = "Cuba2002.jpeg", units = 'cm', width = 10.67, height = 12.57, res = 300, quality = 200, bg = 'white')
par(lwd=.8)
par(mar=pyramid.plot(xy.pop,xx.pop,
                     main="",lxcol="dark grey",rxcol="light grey",
                     gap=0,show.values=FALSE,xlim=c(3,3.5),top.labels = c("Male", "", 
                                                                          "Female"),labelcex=0.8, space=0, ppmar=c(4,4,4,0),labels=rep(NA,101)))
axis(2,at=c(0,10,20,30,40,50,60,70,80,90,100),labels=c(0,10,20,30,40,50,60,70,80,90,100),pos=-3)
mtext("Age", side=2, line = 3)
mtext("Population", side=1, line = 3.5, adj=0.46)
mtext("Cuba 2002", side=3, line = 2, adj=0.46, font=2)
mtext("IW60+ = 100.89", side=3, line = 1, adj=0.46,font=0.1, cex=0.6)
dev.off()
#FIM Prog. pirâmide Cuba 2002

