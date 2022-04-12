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

#Códigos Ebook

states <- as.data.frame(state.x77[, 
                                  c("Murder","Population","Illiteracy",
                                    "Income","Frost")])

##Exploracao dos dados

ggcorr(states, palette = "RdYlGn", name = bquote(rho),
       label = TRUE, label_color = "black") + 
  labs( caption = "Fonte: Elaborado pelo autor") + 
  theme(plot.caption = element_text(hjust = 0,size = 8))


ggpairs(states, columns = 1:ncol(states), title = "", axisLabels = "show") + 
  theme(axis.text.x = element_text(angle=90, vjust=0.5, size=10)) + 
  theme_pubr() + labs_pubr() + 
  labs( caption = "Fonte: Elaborado pelo autor") + labs_pubr() + 
  theme(plot.caption = element_text(hjust = 0, size = 8))

##Ajustando um modelo de regressao linear


fit <- lm(Murder ~ Population + Illiteracy + Income + Frost, data=states)

summary(fit)

##Analisando o modelo multivariado ajustado

###Comecando pela variavel frost

fit <- update(fit, . ~ . - Frost)

summary(fit)

###Comecando pela variavel frost

fit <- update(fit, . ~ . - Income)

summary(fit)

##Representando Interações ou Efeito Moderador

fit.mpg <- lm(mpg ~ hp + wt + hp:wt, data=mtcars)

summary(fit.mpg)

efeitos <- effect("hp:wt", fit.mpg, , list(wt=c(2.2, 3.2, 4.2)))

df.ef <- as.data.frame(efeitos)

df.ef$wt <- as.factor(df.ef$wt)  

ggplot(data = df.ef) + 
  geom_line(aes(y = fit, x = hp, shape = wt, color = wt)) +
  geom_point(aes(x = hp, y = fit, shape = wt, color = wt)) +
  ylab("mpg") + theme_pubr(legend = "right") + labs_pubr() +
  labs(caption = "Fonte: Elaborado pelo autor") + labs_pubr() +
  theme(plot.caption = element_text(hjust=0, size=8))
  
fit.coef <- lm(Murder ~ Population + Illiteracy + Income + Frost, data = states)  

confint(fit.coef)  

autoplot(fit, which = 1:2, ncol = 2, label.size = 2) + theme_pubr() +
  labs(caption = "Fonte: Elaborado pelo autor") + labs_pubr() +
  theme(plot.caption = element_text(hjust = 0, size = 8))
  
##Regrassão Múltipla com termo quadratico

summary(women)

str(women)

g1 <- ggplot(data = women) + 
  geom_point(aes(x = height, y = weight), color = "red") +
  theme_pubr() + labs_pubr() + 
  labs(caption = "Fonte: Elaborado pelo autor") + labs_pubr() + 
  theme(plot.caption = element_text(hjust = 0, size = 8))

g1


fit.w1 = lm(weight ~ height, data = women)

summary(fit.w1)

slope = as.numeric(fit.w1$coefficients[2])

interc = as.numeric(fit.w1$coefficients[1])

g1 <- ggplot(data = women) + 
  geom_point(aes(x = height, y = weight), color = "red") + 
  geom_abline(slope = slope, intercept = interc, color = "blue", 
              data = women) + theme_pubr() + labs_pubr() +
  labs(caption = "Fonte: Elaborado pelo autor")  + labs_pubr() +
  theme(plot.caption = element_text(hjust = 0, size = 8))

g1  

autoplot(fit.w1, which = 1:2, ncol = 2, label.size = 3) +
  theme_pubr() + labs_pubr() +
  labs(caption = "Fonte: Elaborado pelo autor") + labs_pubr() +
  theme(plot.caption = element_text(hjust = 0, size = 8))

fit.w2 <- lm(weight ~ height + I(height^2), data=women)

summary(fit.w2)

autoplot(fit.w2, which = 1:2, ncol = 2, label.size = 3) +
  theme_pubr() + labs_pubr() +
  labs(caption = "Fonte: Elaborado pelo autor") + labs_pubr() +
  theme(plot.caption = element_text(hjust = 0, size = 8))

g2 <- ggplot(data = women, aes(x = height, y = weight)) +
  geom_point(color = "red") +
  geom_smooth(method = "lm", formula = y ~ x + I(x^2),
              se = FALSE, color = "blue", data = women) +
  theme_pubr() + labs_pubr() +
  labs(caption = "Elaborado pelo autor") + labs_pubr() +
  theme(plot.caption = element_text(hjust = 0, size = 8))

g2

ozdata <- faraway::ozone[,c("03","temp")]

names(ozdata) <- c("ozonio", "temperatura")

head(ozdata)

g <- ggplot(data = ozdata, aes(x = temperatura, y = ozonio)) +
  geom_point() + xlab("Temperatura") + ylab("Ozonio") + theme_pubr() +
  labs_pubr() + labs(caption = "Elaborado pelo autor") + 
  theme(plot.caption = element_text(hjust = 0, size = 8))

g

ozl <- lm(ozonio ~ temperatura, ozdata)

summary(ozl)

##Teste da normalidade dos residuos

shapiro.test(residuals(ozl))

autoplot(ozl, which = 1:2, ncol = 2, label.size = 2, smooth.linetype = 0) +
  theme_pubr() + labs_pubr() + labs(caption = "Elaborado pelo autor") +
  labs_pubr() + theme(plot.caption = element_text(hjust = 0, size = 8))

df.hist <- data.frame(Residuos = residuals(ozl), Ajustados = fitted(ozl))

gh <- ggplot(data = df.hist, aes(x = Residuos)) + geom_histogram() +
  theme_pubr() + labs_pubr() + ylab("Frequência") + 
  labs(caption = "Elaborado pelo autor") + labs_pubr() + 
  theme(plot.caption = element_text(hjust = 0, size = 8))

gh

require(MASS)

set.seed(123456)

options(digits = 7)

par(mfrow = c(1,2))

boxcox(ozl, esp = 0.001)

mtext("Elaborado pelo autor", xpd = NA, cex = 0.7,
      side = 1, line = 3.8, adj = -1)

boxcox(ozl, lambda = seq(0.2, 0.4, by = 0.01), eps = 0.001)

par(mfrow = c(1,1))

bx <- boxcox(ozl, lambda = seq(0.2, 0.4, by=0.01), plotit = FALSE, eps = 0.001)

bx.df <- data.frame(x = bx$x, y = bx$y)

bx2.df <- bx.df[with(bx.df, order(-bx.df$y)),]

bx2.df[1,]

round(bx2.df[1,"x"], 4)

lmbd <- round(bx2.df[1,"x"],3)

ozdatatrans <- mutate(ozdata, ozoniotrans = (ozonio^lmbd - 1) / lmbd)

head(ozdatatrans)

g <- ggplot(ozdatatrans, aes(x = temperatura, y = ozoniotrans)) + 
  geom_point() + ylab(expression(Ozônio~transformado~y==
                                   (y^lambda - 1)/lambda)) +
  theme_pubr() + labs_pubr() + labs(caption="Elaborado pelo autor") +
  labs_pubr() + theme(plot.caption = element_text(hjust = 0, size = 8))

g

oz2 <- lm(ozoniotrans ~ temperatura, ozdatatrans)

summary(oz2)

autoplot(oz2, which = 1:2, ncol = 2, label.size = 3, smooth.linetype = 0) +
  theme_pubr() + labs_pubr() + labs(caption = "Elaborado pelo autor") +
  labs_pubr() + theme(plot.caption = element_text(hjust = 0, size = 8))

df.hist <- data.frame(Residuos = residuals(oz2), Ajustados = fitted(oz2))

gh <- ggplot(data = df.hist, aes(x = Residuos)) + geom_histogram() +
  theme_pubr() + labs_pubr() + ylab("Frequencia") +
  labs(caption = "Elaborado pelo autor") + labs_pubr() + 
  theme(plot.caption = element_text(hjust = 0, size = 8))

gh

shapiro.test(residuals(oz2))

##Transformacao na variavel explicativa

treino_venda <- data.frame(Tempo=c(0.5,0.5,1,1,1.5,1.5,2,2,2.5,2.5),
                           Desempenho=c(42.5,50.6,68.5,80.7,89,99.6,105.3
                                        ,111.8,112.3,125.7))

head(treino_venda)

g <- ggplot(treino_venda, aes(x = Tempo, y = Desempenho)) + 
  geom_point() + xlab("Tempo de treinamento") +
  theme_pubr() + ylab("Desempenho de venda") + labs_pubr() +
  labs(caption = "Elaborado pelo autor") + labs_pubr() +
  theme(plot.caption = element_text(hjust = 0, size = 8))

g

ajuste1 <- lm(Desempenho ~ Tempo, data = treino_venda)

summary(ajuste1)

shapiro.test(residuals(ajuste1))

autoplot(ajuste1, wich = 1:2, ncol = 2, label.size = 3, smooth.linetype = 0) +
  theme_pubr() + labs_pubr() + labs(caption = "Elaborado pelo autor") +
  labs_pubr() + theme(plot.caption = element_text(hjust = 0, size = 8))

df.aj1 <- data.frame(residuos = residuals(ajuste1),
                     ajustados = fitted(ajuste1))

g <- ggplot(data = df.aj1, aes(x = residuos)) +
  ylab("Frequencia") + geom_histogram(binwidth = 4) + theme_pubr() +
  labs_pubr() + labs(caption = "Elaborado pelo autor") + labs_pubr() +
  theme(plot.caption = element_text(hjust = 0, size = 8))

g

it <- coef(ajuste1)[1]

sl <- coef(ajuste1)[2]

ggplot(data = treino_venda, aes(x = Tempo, y = Desempenho)) +
  geom_point() + geom_abline(slope = sl, intercept = it) +
  theme_pubr() + labs_pubr() + labs(caption = "Elaborado pelo autor") +
  labs_pubr() + theme(plot.caption = element_text(hjust = 0, size = 8))

treino_venda_trans <- mutate(treino_venda, Tempotrans = sqrt(Tempo))

ggplot(data = treino_venda_trans, aes(x = Tempotrans, y = Desempenho)) +
  geom_point() + theme_pubr() + labs_pubr() +
  xlab("Tempo de Treinamento Transformado") + ylab("Desempenho de Venda") +
  labs(caption = "Elaborado pelo autor") + labs_pubr() +
  theme(plot.caption = element_text(hjust = 0, size = 8))

ajuste2 <- lm(Desempenho ~ Tempotrans, data = treino_venda_trans)

summary(ajuste2)

shapiro.test(residuals(ajuste2))

it <- coef(ajuste2)[1]

sl <- coef(ajuste2)[2]

ggplot(data = treino_venda_trans, aes(x = Tempotrans, y = Desempenho)) +
  geom_point() + geom_abline(slope = sl, intercept = it) + 
  xlab("Tempo de treinamento Transformado") + theme_pubr() + 
  labs_pubr() + ylab("Desempenho de venda") + 
  labs(caption = "Elaborado pelo autor") + labs_pubr() +
  theme(plot.caption = element_text(hjust = 0, size = 8))

autoplot(ajuste2, which = 1:2, ncol = 2, label.size = 3, smooth.linetype = 0) +
  theme_pubr() + labs_pubr() +
  labs(caption = "Elaborado pelo autor") + labs_pubr() + 
  theme(plot.caption = element_text(hjust = 0, size = 8))

#Códigos Material complementar

set.seed(123456)

observ <- sample(1:99, size = 20)

contin <- sample(rep(c("Africa","America","Europa","Asia","Oceania"),100),20)

idademax <- rbinom(n=20, size= 100, prob = 0.8)

numvmed <- rbinom(n=20, size = 20, prob = 0.4)

df <- data.frame(observ, contin, idademax, numvmed)

df

str(df, strict.width = "wrap")

mod <- lm(idademax ~ contin + numvmed, data = df)

summary(mod)

data("prostate")

prostata <- prostate[, c('lpsa', 'lcavol', 'lweight',
                         'age', 'lbph', 'svi', 'lcp', 'gleason', 'pgg45')]

names(prostata) <- c("Y","X1","X2","X3","X4","X5","X6","X7","X8")

head(prostata)

nulo <- lm(Y ~ 1, data = prostata)

completo <- lm(Y ~ ., data = prostata)

step(nulo, scope = list(lower=nulo, upper=completo),
     data = prostata, direction = "forward", trace = FALSE)

summary(lm(Y ~ X1 + X2 + X3 + X4 + X5, data = prostata))

summary(lm(Y ~ X1 + X2 + X4 + X5, data = prostata))

summary(lm(Y ~ X1 + X2 + X5, data = prostata))

require(leaps)

rsbs <- regsubsets(Y ~ ., nbest = 2, data = prostata)

plot(rsbs)

mtext("Elaborado pelo autor", xpd = NA, cex = 0.7,
      side = 1, line = 4.5, adj = -.2)

(rs <- summary(rsbs))

summary(rsbs)$cp

summary(rsbs)$bic

n_parametros = as.numeric(rownames(rs$which)) + 1

Cp = rs$cp

R2_ajustado = rs$adjr2

s2 = rs$rss

df.n <- data.frame(np = n_parametros, Cp = Cp)

g1 <- ggplot(data = df.n, aes(x = np, y = Cp)) +
  geom_point() + geom_abline(slope = 1, intercept = 0) +
  theme_pubr() + labs_pubr() + ylab("Estatistica Cp") +
  xlab("Num Parametros")

df.s2 <- data.frame(np = n_parametros, s2 = s2)

g2 <- ggplot(df.n, aes(x = n_parametros, y = s2)) +
  geom_point() + theme_pubr() + labs_pubr() +
  ylab("Estatistica S2") + xlab("Num Parametros")

df.r2 <- data.frame(np=n_parametros, r2 = R2_ajustado)

g3 <- ggplot(df.r2, aes(x = n_parametros, y = r2)) + 
  geom_point() + theme_pubr() + labs_pubr() +
  ylab("Estatistica R2") + xlab("Num Parametros") +
  theme(plot.caption = element_text(hjust = 0, size = 9)) +
  labs(caption = "Elaborado pelo autor")

ggarrange(g1, g2, g3, ncol = 2, nrow = 2)

n_variaveis = n_parametros-1

cbind(n_variaveis, n_parametros, Cp, R2_ajustado, s2)








