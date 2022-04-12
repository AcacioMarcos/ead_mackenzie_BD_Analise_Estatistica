
#Instalando os pacotes

install.packages("ggplot2")
install.packages("dplyr")
install.packages("ggpubr")

#Carregando as bibliotecas

library("dplyr")
library("ggplot2")
library("ggpubr")

sample(1:40,5)

sample(c("H","T"),10, replace=TRUE)  #Jogar a moeda 10 vezes

sample(1:6,10,replace = TRUE) #Jogar o dado 10 vezes

#Indicar a probabilidade do evento falhar ou não
sample(c("sucess","fail"),10, replace = T, prob=c(0.85,0.15))

#Exemplo de amostra de pesquisa de opinião
sample(rep(c("sim","não"), c(6800, 3200)), size=10, replace=T)

#Exemplo Probabilidade Clássica

p0 <- 1/2 # Moeda Balanceada

n = 100:5000

fr = mapply(function(x) sum(rbinom(x,1,p0))/x,n)

df.nfr <- tibble(n = n, fr = fr)

g <- ggplot(data = df.nfr) + geom_line(aes(x = n, y = fr))

g <- g + geom_hline(yintercept = 0.5, linetype = 2, color = "green")

g <- g + theme_pubr() + labs_pubr()

g <- g + theme(plot.caption = element_text(hjust = -0.1))

g <- g + labs(x = "Numero de lancamentos", y = "Frequencia Relativa", caption = "Fonte do autor")

g

########Calculos de probabilidade combinatória##########

1/prod(40:36)

prod(5:1) / prod(40:36)

1/choose(40,5)

###Distribuição Binomial###

dbinom(0:4, size=4, prob=0.5) #fonte de probabilidade em massa

pbinom(0:4, size=4, prob=0.5) #fonte de probabilidade acumulada

####Probabilidade com no minimo 2 heads

dbinom(2:4, size = 4, prob = 0.5)

sum(dbinom(2:4, size = 4, prob = 0.5))

1-pbinom(0, size = 4, prob = 0.5)

punif(0.7, min = 0, max = 1)

punif(0.3, min = 0, max = 1)

punif(0.7, min = 0, max = 1) - punif(0.3, min = 0, max = 1)

1-punif(0.5, min = 0, max = 1)

1 - punif(0.8, min = 0, max = 1)

1 - punif(0.5, min = 0, max = 1) + 1 - punif(0.8, min = 0, max = 1)

randnums = runif(1000, min = 0, max = 1)

prop.table(table(randnums > 0.3 & randnums < 0.7))

####Distribuições built-in

##Densidade

x <- seq(-4, 4, 0.1)

ggplot(data = tibble(x = x, y = dnorm(x))) +
  geom_line(aes(x=x, y=y)) + labs_pubr() + theme_pubr() + 
  theme(plot.caption = element_text(hjust = 0)) + 
  labs(x="x", y="densidade de probabilidade", caption = "Pelo autor")


curve(dnorm(x), from =-4, to=4) #Alternativa


###Discreta

x <- 0:50

ggplot(data = tibble(x = x, y = dbinom(x, size = 50, prob = 0.33))) +
  geom_bar(aes(x = x, y = y), fill = "white", color = "black", stat = "identity", position = "dodge") + labs_pubr() +
  labs(x = "x", y = "Probabilidade Pontual", caption = "Pelo autor") +
  theme_pubr() + theme(plot.caption = element_text(hjust = 0))
  
###Distribuições Cumulativas

1 - pnorm(160, mean=132, sd=13)

###Quantis

xbar <- 83

sigma <- 12

n<-5

(sem <- sigma/sqrt(n))

xbar + sem * qnorm(0.025)

xbar + sem * qnorm(0.975)

#####Teste hipotese

p <- pbinom(32, 100, 0.5) + 1 - pbinom(67, 100, 0.5)

p


xbar <- 62

mu0 <- 60

Var <- 25

sigma <- sqrt(Var)

n <- 16

z <- (xbar - mu0) / (sigma / sqrt(n))

p <- 2 * pnorm(-abs(z))

p

####Estimacao intervalar


#lim_inf <- mu0 - qnorm(1 - alpha/2) * sigma/sqrt(n)
#lim_sup <- mu0 + qnorm(1 - alpha/2) * sigma/sqrt(n)


alpha = 1-pnorm(2) + 1- pnorm(2)

alpha/2

mu0 = 60

xbar = 62.5

sigma = 5

n = 16

sem = sigma / sqrt(n)

z <- (xbar - mu0) / (sigma / sqrt(n))

##Calcunaod o B

n = 16

sigma = sqrt(25)

sem = sigma/sqrt(n)

#Alpha anterior

mu0 = 60

I = c(alpha/2, 1-alpha/2)

q = qnorm(I, mean = mu0, sd = sem)

q

mu = 63.5

p = pnorm(q, mean = mu, sd= sem)

p

diff(p)

poder = 1 - diff(p)

poder

###Conclusao

k <- qnorm(0.05, mean = 200)

k

##Unilateral a esquerda

xbar = 195

mu0 = 200

sigma = 10

n = 100

z = (xbar -mu0) / (sigma / sqrt(n))

z

alpha = 0.05

#Esse é o valor que divide a curva em 0.05 e 0.95

rc = qnorm(alpha)

rc

##Estatistica do teste

xbar = 14.6 #media da amostra

mu0 = 15.4 #valor da hipotese

sigma = 2.5 #Desvio padrao da populacao

n = 35 #tamanho da amostra

z = (xbar - mu0) / (sigma / sqrt(n)) #Estatistica do teste

z

##Teste de Hipotese com significancia de 0.05

alpha = 0.05

z.half.alpha = qnorm(1 - alpha/2)

c(-z.half.alpha, z.half.alpha)

##A estatística de teste z = -1.8931 está entre os valores críticos 
#-1.96, 1.96, ou seja, está dentro da região de aceitação. Portanto, 
#a um nível de significância de 0.05, nós não rejeitamos a hipotese nula

##Alternativa

p <- 2 * pnorm(z)

p

#Como o valor p = 0.0583 é maior do que o nível de significância ??, 
#nós não rejeitamos a hipótese nula de que a média seja 15.4



###Distribuicao T Student

##Se n = 12, são 11 graus de liberdade. Se tivermos H1: µ 6= µ0 
#escolhendo ?? = 0.05, temos p/2 = ??/2, ou seja, p = 0.05 (teste bilateral)

alpha = 0.05

df = 11

tc = qt(alpha / 2, df = df)

tc

##Se n = 28, são 27 graus de liberdade. Se tivermos H1: µ < µ0 
#escolhendo ?? = 0.01, temos p/2 = ??, ou seja, p = 2?? = 0.02 (teste unilateral)

alpha = 0.02

df = 27

tc = qt(alpha / 2, df = df)

tc


##Exemplo 16

alpha = 0.05

df = 19

n = 20 #tamanho da amostra

S = 20 #desvio padrao da amostra

mu0 = 115

xbar = 118

tc <- qt(alpha / 2, df = df)

tc

Tt = (sqrt(n)* (xbar - mu0)) / S

Tt
