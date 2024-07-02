rm(list = ls())

x <-c(-10, -7, -4, -1, 3, 7, 9);
y <-c(-56, -45, -2, 10, 7, 25, 75);

# plot(x,y); #gráfico de dispersão

regLinear<- lm(formula = y~x); # REGRESSÃO LINEAR: saída~entrada (entra x e sai y)
# no caso acima x = "a"e y = "b"
#abline(regLinear);
a<-unname(regLinear$coefficients[2]); # o $ permite escolher um dos vários modelos da regLinear
b<-unname(regLinear$coefficients[1]);

x_teorico<- -10:10;
y_teorico<- (a*x_teorico)+b;

plot(x_teorico, y_teorico);
regLinear1<- lm(formula = y_teorico~x_teorico);
abline(regLinear1);





