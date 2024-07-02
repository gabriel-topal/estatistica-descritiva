# Limpando environment
rm(list = ls())

# Instalando e carregando a biblioteca 'psych'
# install.packages("psych")
library(psych)

# Adicionando os dados
dados <- data.frame (
  m1 = c(5.4, 5.9, 6.3, 6.5, 6.5, 6.7, 6.7, 6.7, 6.8, 6.9, 7.0, 7.2, 7.2, 7.3, 7.4, 7.5, 7.7, 7.8, 7.8, 8.2, 8.3, 9.0), # direito civil 9
  m2 = c(2.0, 4.5, 6.5, 6.9, 7.0, 7.5, 7.7, 7.7, 7.8, 8.2, 8.3, 8.4, 8.5, 8.6, 7.9, 8.7, 8.8, 8.8, 8.9, 9.0, 9.3, 9.6), # deontologia e ética
  m3 = c(3.5, 6.0, 7.4, 7.75, 7.8, 8.2, 8.3, 8.3, 8.4, 8.5, 8.5, 8.5, 8.6, 8.7, 8.75, 8.9, 9.0, 9.1, 9.2, 9.3, 9.8, 10) # direito eletrônico
);

semestre <- c(sem = c(2.0, 3.5, 4.5, 5.4, 5.9, 6.0, 6.3, 6.5, 6.5, 6.7, 6.7, 6.7, 6.8, 6.9, 7.0, 7.0, 7.2, 7.2, 7.3, 7.4, 7.5, 7.5,
                      7.7, 7.7, 7.7, 7.8, 7.8, 7.8, 7.9, 7.9, 8.2, 8.2, 8.2, 8.3, 8.3, 8.3, 8.3, 8.4, 8.4, 8.5, 8.5, 8.5, 8.5, 8.6,
                      8.6, 8.7, 8.7, 8.8, 8.8, 8.8, 8.9, 8.9, 8.9, 9.0, 9.0, 9.0, 9.0, 9.1, 9.2, 9.3, 9.3, 9.3, 9.6, 9.8, 10) # semestre
);

# tratamento estatístico m1
# summary(m1);
media_m1<-mean(dados$m1);
mediana_m1<-median(dados$m1);
desvioPadrao_m1<-sd(dados$m1);
intervalo_m1<- max(dados$m1) - min(dados$m1);
Q1_m1<-unname(quantile(dados$m1)[2]);
Q3_m1<-unname(quantile(dados$m1)[4]);
IIQ_m1<-Q3_m1-Q1_m1;
# tratamento estatístico m2
# summary(m2);
media_m2<-mean(dados$m2);
mediana_m2<-median(dados$m2);
desvioPadrao_m2<-sd(dados$m2);
intervalo_m2<- max(dados$m2) - min(dados$m2);
Q1_m2<-unname(quantile(dados$m2)[2]);
Q3_m2<-unname(quantile(dados$m2)[4]);
IIQ_m2<-Q3_m2-Q1_m2;
# tratamento estatístico m3
# summary(m3);
media_m3<-mean(dados$m3);
mediana_m3<-median(dados$m3);
desvioPadrao_m3<-sd(dados$m3);
intervalo_m3<- max(dados$m3) - min(dados$m3);
Q1_m3<-unname(quantile(dados$m3)[2]);
Q3_m3<-unname(quantile(dados$m3)[4]);
IIQ_m3<-Q3_m3-Q1_m3;
# tratamento estatístico semestre
# summary(semestre);
media_semestre<-mean(semestre);
mediana_semestre<-median(semestre);
desvioPadrao_semestre<-sd(semestre);
intervalo_semestre<- max(semestre) - min(semestre);
Q1_semestre<-unname(quantile(semestre)[2]);
Q3_semestre<-unname(quantile(semestre)[4]);
IIQ_semestre<-Q3_semestre-Q1_semestre;


# HISTOGRAMAS

hist(dados$m1, xlab = "Notas", ylab = "Frequência", main = "Direito Civil 9", breaks = 10);
abline(v=media_m1, col = "red", lwd = 2);
abline(v=mediana_m1, col = "blue", lty = 2);
legend(x="topright",
       c("Média", "Mediana"),
       col = c("red", "blue"),
       lty = c(1,2),
       lwd = c(2,2));

hist(dados$m2, xlab = "Notas", ylab = "Frequência", main = "Deontologia e Ética", breaks = 10);
abline(v=media_m2, col = "red", lwd = 2);
abline(v=mediana_m2, col = "blue", lty = 2);
legend(x="top",
       c("Média", "Mediana"),
       col = c("red", "blue"),
       lty = c(1,2),
       lwd = c(2,2));

hist(dados$m3, xlab = "Notas", ylab = "Frequência", main = "Direito Eletrônico", breaks = 10);
abline(v=media_m3, col = "red", lwd = 2);
abline(v=mediana_m3, col = "blue", lty = 2);
legend(x="top",
       c("Média", "Mediana"),
       col = c("red", "blue"),
       lty = c(1,2),
       lwd = c(2,2));

hist(semestre, xlab = "Notas", ylab = "Frequência", main = "Semestre", breaks = 10);
abline(v=media_m3, col = "red", lwd = 2);
abline(v=mediana_m3, col = "blue", lty = 2);
legend(x="top",
       c("Média", "Mediana"),
       col = c("red", "blue"),
       lty = c(1,2),
       lwd = c(2,2));

# BOX-PLOT

distribuicaoNotas<-cbind(dados$m1, dados$m2, dados$m3);
colnames(distribuicaoNotas) [1:3]<-cbind("Direito Civil 9","Deontologia e Ética","Direito Eletrônico");
boxplot(distribuicaoNotas, ylab = "Notas");

boxplot(semestre, xlab = "Semestre", ylab = "Notas");


# DIAGRAMA DE DISPERSÃO

plot(dados$m1, dados$m2,
     xlab = "Direito Civil 9",ylab = "Deontologia e Ética",
     main = "Direito Civil 9 - Deontologia e Ética",
     pch = 19,
     ylim = c(2, 10),
     xlim = c(5, 9));
tend_m1m2<-lm(formula = dados$m2~dados$m1) #regressão linear
abline(tend_m1m2)

plot(dados$m1, dados$m3,
     xlab = "Direito Civil 9",ylab = "Direito Eletrônico",
     main = "Direito Civil - Direito Eletrônico",
     pch = 19,
     ylim = c(2, 10),
     xlim = c(5, 9));
tend_m1m3<-lm(formula = dados$m3~dados$m1); #regressão linear
abline(tend_m1m3);

plot(dados$m2, dados$m3,
     xlab = "Deontologia e Ética",ylab = "Direito Eletrônico",
     main = "Deontologia e Ética - Direito Eletrônico",
     pch = 19,
     ylim = c(2, 10),
     xlim = c(2, 10));
tend_m2m3<-lm(formula = dados$m3~dados$m2); #regressão linear
abline(tend_m2m3);

# CORRELAÇÃO ENTRE AS VARIÁVEIS

correlacao<-cor(dados);
