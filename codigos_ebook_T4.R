#Instalando os pacotes

install.packages("lubridate")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("ggpubr")
install.packages("data.table")
install.packages("knitr")

#Carregando as bibliotecas

library("lubridate")
library("ggplot2")
library("dplyr")
library("ggpubr")
library("readr")
library("data.table")
library("knitr")

#Códigos Ebook

#Leitura dos dados

##Exemplos de Análise de Regressão Linear

reg.data <- read.table("C:/Users/MarcosAcácio/OneDrive - GT Group/Área de Trabalho/Mackenzie/Análise Estatistica/Trilha 4/regression.txt", header= T)

str(reg.data)

p <- ggplot(data = reg.data, aes(x = tannin, y = growth)) +
  geom_point(color = "red", fill = "blue") + xlab("Tanning") +
  ylab("Growth") + theme_pubr(legend = "right") + labs_pubr() +
  theme(plot.caption = element_text(hjust = 0))

p

mod <- lm(growth ~ tannin, data=reg.data)

summary(mod)

anova(mod)

qf(0.95, 1, 7)

1-pf(30.974, 1, 7)

##Diagnóstico do Modelo de Regressão Linear

###Gráficos Disgnósticos

woman <- data.frame(women)

fit <- lm(weight ~ height, data=women)

summary(fit)

par(mfrow=c(2,2))

plot(fit)

text(-.38, -6, "Fonte: Elaborado pelo Autor", xpd = NA, cex = 0.8)

####Histograma de Residuos

hist(residuals(fit), breaks = 5)

fit.df <- as.data.frame(residuals(fit))

names(fit.df) <- c("residuos")

g <- ggplot(data = fit.df, aes(residuos)) + 
  geom_histogram(fill="#aabbee", color = "blue", stat = "bin", bins = 5) + 
  theme_pubr() + labs_pubr() + xlab("Resíduos") + ylab("Frequência") +
  theme(plot.caption = element_text(hjust=0)) +
  labs(caption = "Fonte: Elaborado pelo autor")

g


##ANOVA

labPrec <- data.frame(S1AP = c(10.21, 10.25, 10.24, 9.8, 9.77, 9.73),
                      S2BP = c(11.32, 11.20, 11.40, 10.50, 10.68, 10.90),
                      S3CP = c(11.60, 11.90, 11.80, 12.30, 12.20, 12.20))

labPrecstck <- stack(labPrec)

names(labPrecstck) <- c("Valores", "Amostras")

head(labPrec)

sapply(labPrec, mean)

labPrecstck

labDesc <- data.frame(S1AD = c(9.03, 10.26, 11.60, 11.40, 8.01, 9.70),
                      S2BD = c(9.56, 13.40, 10.68, 11.32, 10.68, 10.36),
                      S3CD = c(10.45, 9.64, 9.59, 13.40, 14.50, 14.42))

labDescstck <- stack(labDesc)

names(labDescstck) <- c("Valores", "Amostras")

head(labDesc)

sapply(labDesc, mean)

labBothstck <- rbind(labPrecstck, labDescstck)

p <- ggplot(labBothstck, aes(y=Valores, x=Amostras)) + geom_boxplot()

p + coord_flip() +
  stat_summary(fun.y=mean, geom="point", shape=18, size=3, color="red") +
  theme_pubr() + labs_pubr() + theme(plot.caption=element_text(hjust=0)) +
  labs(caption="Fonte: Elaborado pelo Autor")


aovPrec <- aov(Valores ~ Amostras, data=labPrecstck)

summary(aovPrec)

aovDesc <- aov(Valores ~ Amostras, data=labDescstck)

summary(aovDesc)

with(labDescstck, pairwise.t.test(Valores, Amostras, p.adjust="bonferroni"))

TukeyHSD(aovDesc)


#Códigos material apoio

reg.data <- read.table("C:/Users/MarcosAcácio/OneDrive - GT Group/Área de Trabalho/Mackenzie/Análise Estatistica/Trilha 4/regression.txt", header= T)

str(reg.data)

names(reg.data)

X <- reg.data$tannin

Y <- reg.data$growth

n <- length(X)

p <- ggplot(data = reg.data, aes(x = tannin, y = growth)) +
  geom_point(color = "red", fill = "blue") + xlab("Tanning") +
  ylab("Growth") + theme_pubr(legend = "right") + labs_pubr() +
  theme(plot.caption = element_text(hjust = 0))

p

##Calculo manual dos coeficientes

(SX <- sum(X))

(SSX <- sum((X - mean(X))^ 2))

(SY <- sum(Y))

(SSY <- sum((Y - mean(Y))^ 2))

(SSXY <- sum((X - mean(X)) * (Y - mean(Y))))

##Calculo dos Parametros

(b_1 <- SSXY / SSX)

(b_0 <- mean(Y) - b_1 *mean(X))

##Visualizando graficamente

p <- ggplot(data= reg.data, aes(x=tannin, y= growth)) + 
  geom_point(color="red", fill="blue") + xlab("Tanning") + ylab("Growth") +
  theme_pubr(legend="right") + labs_pubr() + labs(caption = "Fonte: Autor") +
  labs_pubr() + theme(plot.caption = element_text(hjust=0))

p <- p + geom_abline(intercept = b_0, slope = b_1, color= "blue")

p

##Analise da Varianca (ANOVA) da regressao

y_i = b_0 + b_1 * X

(SSM = sum((y_i - mean(Y))^ 2))

(MSM = SSM/1)

(RSS = sum((Y - y_i)^ 2))

(MSE = RSS / (n - 2))

(F = MSM/MSE)

qf(0.95, 1, n-2)

###erro nos paramentros de inclinacao b_1

se_b_1 = sqrt(MSE/SSX)

###deslocamento de b_0

se_b_0 <- sqrt((MSE * sum(X ^ 2))/(n * SSX))

###R-Quadrado

R2 <- 1 - RSS/SSY

###R-Quadrado Ajustado

R2adj <- (SSY / (n - 1) - MSE) / (SSY / (n-1))

###O erro padrão residual

rse <- sqrt(MSE)

##Analise dos Resultados dos Coeficientes (p-value)

(tvalue_b0 <- b_0/se_b_0)

(Prvalue_b0 <- 2 * (1 - pt(abs(tvalue_b0), n - 2)))  

(tvalue_b1 <- b_1 / se_b_1)  

Prvalue_b1 <- 2 * (1 - pt(abs(tvalue_b1), n - 2))  
  
##Exemplo 2

stat500uns <- read.csv("C:/Users/MarcosAcácio/OneDrive - GT Group/Área de Trabalho/Mackenzie/Análise Estatistica/Trilha 4/stat500.csv", header= TRUE)

stat500 <- data.frame(scale(stat500uns))  

###verificacao da estrutura dos dados

str(stat500)

sapply(stat500, mean)

sapply(stat500, sd)

###Grafico de dispersao

p <- ggplot(data = stat500)+ geom_point(aes(x = midterm, y= final )) + 
  geom_abline(intercept = 0, slope = 1, color = "gray", linetype = 1) +
  theme_pubr(legend = "none") + labs_pubr() + 
  labs(caption = "Fonte: Autor") + theme(plot.caption = element_text(hjust = 0))

p

###Ajuste do modelo com a funcao lm

g <- lm(final ~ midterm, stat500)

cor(stat500)

###Visualizando o ajuste graficamente

cofs <- coefficients(g)

p <- ggplot(data = stat500, aes(x = midterm, y = final )) + 
  geom_point() + 
  geom_abline(aes(intercept = 0, slope = 1, color = "y=x"), show.legend = TRUE) +
  geom_abline(aes(intercept = cofs[1], slope = cofs[2], color = "Regressao")) +
  theme_pubr(legend = "right") + labs_pubr() + 
  labs(color="") + labs(caption = "Fonte: Autor") +
  theme(plot.caption = element_text(hjust=0))

p

###Analisando o resultado do ajuste

summary(g)

anova(g)

##Graficos Diagnosticos

fit <- lm(weight ~ height, data = women)

summary(fit)

par(mfrow = c(2,2))

plot(fit)

text(-0.38, -6, "Fonte: Autor", xpd = NA, cex = 0.8)

####Histograma de Residuos

hist(residuals(fit), breaks = 5)

fit.df <- as.data.frame(residuals(fit))

names(fit.df) <- c("residuos")

g <- ggplot(data = fit.df, aes(residuos)) + 
  geom_histogram(fill="#aabbee", color = "blue", stat = "bin", bins = 5) + 
  theme_pubr() + labs_pubr() + xlab("Resíduos") + ylab("Frequência") +
  theme(plot.caption = element_text(hjust=0)) +
  labs(caption = "Fonte: Elaborado pelo autor")

g

