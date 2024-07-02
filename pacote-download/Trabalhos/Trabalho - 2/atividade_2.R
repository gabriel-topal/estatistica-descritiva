#Limpando o environment
rm(list=ls())

# ------------------- Carregando dados -----------------

#Definindo as variáveis \
dados <- data.frame (
  casas = c(2,2,3,10,13,14,15,15,16,16,
            18,18,20,21,22,22,23,24,25,25,
            26,27,29,29,30,32,36,42,44,45,
            45,46,48,52,58,59,61,61,61,65,
            66,66,68,75,78,80,89,90,92,97),
  quarteirao = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10,
                 11, 12, 13, 14, 15, 16, 17, 18, 19, 20,
                 21, 22, 23, 24, 25, 26, 27, 28, 29, 30,
                 31, 32, 33, 34, 35, 36, 37, 38, 39, 40,
                 41, 42, 43, 44, 45, 46, 47, 48, 49, 50)
)

# ------------------- Média --------------------
# Programando a função para o cálculo
# Definindo quantos valores são
numobservacoes<-length(dados$casas)
media1<- (sum(dados$casas))/numobservacoes
#usando a função nativa para cálculo de média
media2<-mean(dados$casas)

# -------------------- Mediana ----------------------
# Programando a função para o cálculo
ordenado<-sort(dados$casas)
if(numobservacoes%%2 == 0) { #número par de observações
  mediana1<-((ordenado[numobservacoes/2])+(ordenado[(numobservacoes/2)+1]))/2
} else { #número ímpar de observações
  mediana<-ordenado[(numobservacoes+1)/2]
}
# Usando a função nativa para cálculo de mediana
mediana2<-median(dados$casas)

# ------------------ INTERVALO -----------------------
intervalo<-max(dados$casas) - min(dados$casas)
#Primeiro quartil
Q1<-unname(quantile(dados$casas)[2])
#Terceiro quartil
Q3<-unname(quantile(dados$casas)[4])
#Intervalo interquartílico
IIQ<-Q3-Q1

# ---------------------------- DESVIO PADRÃO ----------------------------
desvioPadrao<-sd(dados$casas)

# -------------------------- RESUMINDO DADOS ----------------------------
summary(dados$casas)
# Instalando uma biblioteca
# Verificando se a biblioteca está disponível
if(!require(psych)) { #se não estiver disponível, entrar no if
  install.packages("psych") #instala a biblioteca
}
library(psych) #carrega a biblioteca
#Resumindo dados com a função describe - da biblioteca psych
describe(dados$casas)

# ------------------------ HISTOGRAMA ------------------------

hist(dados$casas,xlab = "Quarteirão",ylab = "N° de casas", main = "Histograma", breaks = 10)
abline(v=media1, col = "red", lwd = 2)
abline(v=mediana1, col = "blue", lty = 2)

# ------------------------- SÉRIE TEMPORAL ----------------------------
plot(dados$quarteirao,dados$casas, xlab = "Quarteirão",ylab = "N° de casas",type = "p")

# ------------------------ DIAGRAMA BOX-PLOT ---------------------------
if(!require(ggplot2)) { #se não estiver disponível, entrar no if
  install.packages("ggplot2") #instala a biblioteca
}
library(ggplot2) #carrega a biblioteca

boxplot(dados$casas)




