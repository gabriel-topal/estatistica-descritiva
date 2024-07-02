#Limpando o environment
  rm(list=ls())

# ------------------- Carregando dados -----------------

#Definindo pasta de trabalho \
  setwd("C:/Users/ra195239/Desktop/volatididade")
#lendo os dados
  dados<-read.table("Volatilidade das Açoes.txt",sep = "\t",dec = ",",header = TRUE)

# ------------------- Média --------------------
# Programando a função para o cálculo
# Definindo quantos valores são
  numobservacoes<-length(dados$volatilidade)
  media1<- (sum(dados$volatilidade))/numobservacoes
#usando a função nativa para cálculo de média
  media2<-mean(dados$volatilidade)

# -------------------- Mediana ----------------------
# Programando a função para o cálculo
  ordenado<-sort(dados$volatilidade)
  if(numobservacoes%%2 == 0) { #número par de observações
  mediana1<-((ordenado[numobservacoes/2])+(ordenado[(numobservacoes/2)+1]))/2
  } else { #número ímpar de observações
  mediana<-ordenado[(numobservacoes+1)/2]
  }
# Usando a função nativa para cálculo de mediana
  mediana2<-median(dados$volatilidade)
  
# ------------------ INTERVALO -----------------------
  intervalo<-max(dados$volatilidade) - min(dados$volatilidade)
#Primeiro quartil
  Q1<-unname(quantile(dados$volatilidade)[2])
#Terceiro quartil
  Q3<-unname(quantile(dados$volatilidade)[4])
#Intervalo interquartílico
  IIQ<-Q3-Q1
  
# ---------------------------- DESVIO PADRÃO ----------------------------
  desvioPadrao<-sd(dados$volatilidade)
  
# -------------------------- RESUMINDO DADOS ----------------------------
summary(dados$volatilidade)
# Instalando uma biblioteca
# Verificando se a biblioteca está disponível
  if(!require(psych)) { #se não estiver disponível, entrar no if
  install.packages("psych") #instala a biblioteca
  }
  library(psych) #carrega a biblioteca
#Resumindo dados com a função describe - da biblioteca psych
  describe(dados$volatilidade)

# ------------------------ HISTOGRAMA ------------------------
  hist(dados$volatilidade,xlab = "Valor da ação [R$]",ylab = "Meses", main = "Volatilidade", breaks = 10)
  abline(v=media1, col = "red", lwd = 2)
  abline(v=mediana1, col = "blue", lty = 2)
  
# ------------------------- SÉRIE TEMPORAL ----------------------------
  plot(dados$mes,dados$volatilidade, xlab = "Mês",ylab = "Valor da ação [R$]",type = "p")
  
# ------------------------ DIAGRAMA BOX-PLOT ---------------------------
  ano1<-dados$volatilidade[1:12]
  ano2<-dados$volatilidade[13:24]
  ano3<-dados$volatilidade[25:36]
  ano4<-dados$volatilidade[37:48]
  ano5<-dados$volatilidade[49:60]
  distribuicaoAnual<-cbind(ano1,ano2,ano3,ano4,ano5)
  colnames(distribuicaoAnual) [1:5]<-cbind("2018","2019","2020","2021","2022")
  boxplot(distribuicaoAnual)
  
  
  
  
  