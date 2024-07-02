rm(list = ls())

#Dados
dados <- data.frame(
  q1 = c(10, 6, 6, 8, 8, 10, 4, 8, 4),
  q2 = c(10, 8, 8, 10, 10, 10, 10, 6, 6),
  q3 = c(10, 6, 2.5, 2.5, 0, 0, 2.5, 0, 0),
  q4 = c(10, 7.5, 5, 7.5, 7.5, 7.5, 10, 7.5, 5),
  q5 = c(10, 2.5, 2.5, 0, 5, 2.5, 7.5, 7.5, 0),
  q6 = c(10, 10, 6.67, 5.83, 7.5, 7.5, 3.33, 4.17, 3.33),
  q7 = c(5, 0, 5, 7.5, 10, 7.5, 10, 0, 7),
  nf = c(9, 6, 5.4, 6.05, 7.3, 6.75, 6.4, 4.15, 3.9)
);


# Analisando Q1
  summary(dados$q1);
# Analisando Q2
  summary(dados$q2);
# Analisando Q3
  summary(dados$q3);
# Analisando Q4
  summary(dados$q4);
# Analisando Q5
  summary(dados$q5);
# Analisando Q6
  summary(dados$q6);
# Analisando Q7
  summary(dados$q7);
# Analisando NF
  summary(dados$nf);

plot(dados$q1, xlab = "Frequência",ylab = "Nota",type = "p")
