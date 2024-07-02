rm(list=ls())

dados <- data.frame (
  casas = c(2,2,3,10,13,14,15,15,16,16,
            18,18,20,21,22,22,23,24,25,25,
            26,27,29,29,30,32,36,42,44,45,
            45,46,48,52,58,59,61,61,61,65,
            66,66,68,75,78,80,89,90,92,97))

media<-mean(dados$casas)
mediana<-median(dados$casas)
desvioPadrao<-sd(dados$casas)

intervalo<-max(dados$casas) - min(dados$casas)
Q1<-unname(quantile(dados$casas)[2])
Q3<-unname(quantile(dados$casas)[4])
IIQ<-Q3-Q1

hist(dados$casas, breaks = 20, main = "Histograma",
     xlab = "N° de casas",ylab = "Frequência", 
     col = "darkblue")
abline(v=media, col = "red", lwd = 2)
abline(v=mediana, col = "green", lty = 2)
# acrescentando quadro com legenda
legend(x="topright", #posicao da legenda
       c("Mediana","Media"), #nomes da legenda
       col=c("green","red"), #cores
       lty=c(2,1), #estilo da linha
       lwd=c(2,2)) #grossura das linhas

max(dados$casas)
min(dados$casas)
