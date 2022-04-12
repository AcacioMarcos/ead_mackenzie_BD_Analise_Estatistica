#Instalando os pacotes

install.packages("lubridate")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("ggpubr")
install.packages("data.table")
install.packages("knitr")
install.packages("ggfortify")
install.packages("GGally")
install.packages("effects")
install.packages("faraway")
install.packages("leaps")
install.packages("AER")
install.packages("Hmisc")
install.packages("robust")
install.packages("qcc")

#Carregando as bibliotecas

library("lubridate")
library("ggplot2")
library("dplyr")
library("ggpubr")
library("readr")
library("data.table")
library("knitr")
library("ggfortify")
library("GGally")
library("effects")
library("faraway")
library("leaps")
library("AER")
library("Hmisc")
library("robust")
library("qcc")

#Códigos Ebook

data(Affairs, package = "AER")

summary(Affairs)

str(Affairs, strict.width = "wrap")

sapply(Affairs[,c(1:5)], table)

sapply(Affairs[, c(6:9)], table)

descricao <- describe(Affairs)

descricao

Affairs$ynaffair[Affairs$affairs > 0] <- 1

Affairs$ynaffair[Affairs$affairs == 0] <- 0

Affairs$ynaffair <- factor(Affairs$ynaffair, levels = c(0,1), 
                           labels = c("Nao", "Sim") )

table(Affairs$ynaffair)

##Regressao Logistica sem nenhuma variavel preditora

fit.1 <- glm(ynaffair ~ 1, data = Affairs, family = binomial())

summary(fit.1)

addmargins(table(Affairs$ynaffair))

p <- sum(Affairs$ynaffair == "Sim")/length(Affairs$ynaffair)

p

##Regressao Logistica com uma preditora Binária

fit.2 <- glm(ynaffair ~ gender, data = Affairs, family = binomial())

summary(fit.2)

addmargins(table(Affairs$ynaffair, Affairs$gender))

coef(fit.2)

exp(coef(fit.2))

as.numeric(exp(coef(fit.2))[1] * exp(coef(fit.2))[2])

##Regressao logistica com uma preditora continua

fit.3 <- glm(ynaffair ~ yearsmarried, data = Affairs, family = binomial())

coef(fit.3)

exp(coef(fit.3))

summary(fit.3)

beta0 <- as.numeric(coef(fit.3)[1])

beta0

beta1 <- as.numeric(coef(fit.3)[2])

beta1

ym <- mean(Affairs$yearsmarried)

ym

logit_p1 <- beta0 + beta1 * ym

logit_p1

logit_p2 <- beta0 + beta1 * (ym + 1)

logit_p2

logit_p2 - logit_p1

p1 <- exp(logit_p1) / (1 + exp(logit_p1))

p1

p1_odds <- p1/(1 - p1)

p2 <- exp(logit_p2) / (1 + exp(logit_p2))

p2

p2_odds <- p2/(1-p2)

p2_odds / p1_odds

##Regressao logistica com mais de uma preditora

fit.4 <- glm(ynaffair ~ yearsmarried + rating + age,
             data = Affairs, family = binomial())

summary(fit.4)

ym <- mean(Affairs$yearsmarried)

age <- mean(Affairs$age)

rating <- mean(Affairs$rating)

beta0 <- as.numeric(coef(fit.4)[1])

beta1 <- as.numeric(coef(fit.4)[2])

beta2 <- as.numeric(coef(fit.4)[3])

beta3 <- as.numeric(coef(fit.4)[4])

c(beta0, beta1, beta2, beta3)

exp(c(beta0, beta1, beta2, beta3))

logit_p1 <- beta0 + beta1 * ym + beta2 * rating + beta3 * age

logit_p2 <- beta0 + beta1 * (ym + 1) + beta2 * rating + beta3 * age

logit_p2 - logit_p1

p1 <- exp(logit_p1) / (1 + exp(logit_p1))

p1_odds <- p1 / (1 - p1)

p2 <- exp(logit_p2) / (1 + exp(logit_p2))

p2_odds <- p2 / (1 - p2)

c(p1, p1_odds, p2, p2_odds)

p2_odds / p1_odds

##Comparacao entre modelos

###Anova

####Modelo Completo

fit.full <- glm(ynaffair ~ gender + age + yearsmarried + children +
                  religiousness + education + occupation + rating, 
                data = Affairs, family = binomial())


summary(fit.full)

####Modelo Reduzido

fit.reduced <- glm(ynaffair ~ age + yearsmarried + religiousness + rating, 
                   data = Affairs, family = binomial())

summary(fit.reduced)

anova(fit.reduced, fit.full, test = "Chisq")

###Akaike Information Criteria (AIC)

coef(fit.reduced)

exp(coef(fit.reduced))

ym_mean = mean(Affairs$yearsmarried)

relig_mean = mean(Affairs$religiousness)

testdata <- data.frame(rating = c(1,2,3,4,5), age = mean(Affairs$age), 
                       yearsmarried = ym_mean, religiousness = relig_mean)

testdata

testdata$prob <- predict(fit.reduced, newdata = testdata, type = "response")

testdata

relig <- mean(Affairs$religiousness)

beta0 <- coef(fit.reduced)[1]

beta1 <- coef(fit.reduced)[2]

beta2 <- coef(fit.reduced)[3]

beta3 <- coef(fit.reduced)[4]

beta4 <- coef(fit.reduced)[5]

rseq <- seq(1, 5)

logit_p2 <- beta0 + beta1 * age + beta2 * ym + beta3 * relig + beta4 * rseq

logit_p2

exp(logit_p2) # coefic exponenciados

exp(logit_p2)/(1 + exp(logit_p2)) # probabilidades

#####Teste com Idade

ym_mean = mean(Affairs$yearsmarried)

relig_mean = mean(Affairs$religiousness)

testdata <- data.frame(rating = mean(Affairs$rating), age = seq(17, 57, 10), 
                       yearsmarried = ym_mean, religiousness = relig_mean)
testdata$prob <- predict(fit.reduced, newdata = testdata, type = "response")

testdata

###Analise de Super Dispersao

deviance(fit.reduced)/df.residual(fit.reduced)

fit <- glm(ynaffair ~ age + yearsmarried + religiousness + rating, 
           family = binomial(), data = Affairs)

fit.od <- glm(ynaffair ~ age + yearsmarried + religiousness +
                rating, family = quasibinomial(), data = Affairs)

pchisq(summary(fit.od)$dispersion * fit$df.residual, fit$df.residual,lower = F)


##Regrassao de Poisson

data(breslow.dat)

names(breslow.dat)[c(1:5)]

names(breslow.dat)[c(6:12)]

summary(breslow.dat[c(6, 7, 8, 10)])

attach(breslow.dat)

hst <- ggplot(data = breslow.dat, aes(sumY)) + geom_histogram() +
  theme_pubr() + labs_pubr() + xlab("Num Convulsões") +
  ggtitle("Distribuição de Convulsões") + ylab("Frequência")

bxp <- ggplot(data = breslow.dat) + 
  geom_boxplot(aes(y = sumY, x = Trt)) + theme_pubr() + labs_pubr() + 
  xlab("Tratamento") + ggtitle("Comparação de Grupos") + ylab("Num Convulsões")

ggarrange(hst, bxp, ncol = 2) + labs(caption = "Fonte: Elaborado pelo autor") +
  theme(plot.caption = element_text(hjust = 0, size = 8))

fit.pois <- glm(sumY ~ Base + Age + Trt,data = breslow.dat, family = poisson())

summary(fit.pois)

coef(fit.pois)

exp(coef(fit.pois))


####Analise de super dispersao

deviance(fit.pois)/df.residual(fit.pois)

qcc.overdispersion.test(breslow.dat$sumY, type = "poisson")

fit.od <- glm(sumY ~ Base + Age + Trt, data = breslow.dat, 
              family = quasipoisson())

summary(fit.od)

coef(fit.od)




