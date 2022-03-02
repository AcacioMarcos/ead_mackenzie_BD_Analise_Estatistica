##### ATIVIDADE DE APROFUNDAMENTO DA TRILHA 2
#### DISCIPLINA DE ANALISE DE DADOS
### MARCOS VINICIUS ACACIO DOS SANTOS
## RA 9217847

#Instalando os pacotes

install.packages("ggplot2")
install.packages("dplyr")
install.packages("ggpubr")

#Carregando as bibliotecas

library("dplyr")
library("ggplot2")
library("ggpubr")

###### EXERCICIO 1
#### HIPOTESES
### HO: Média = 15.4kg
### H1: Média <> 15.4kg

## Cálculos do teste

xbar = 14.1 #media da amostra

mu0 = 15.4 #valor da hipotese

sigma = 2 #Desvio padrao da populacao

n = 35 #tamanho da amostra

z = (xbar - mu0) / (sigma / sqrt(n)) #Estatistica do teste

z


##Teste de Hipotese com significancia de 0.05

alpha = 0.05

z.half.alpha = qnorm(1 - alpha/2)

c(-z.half.alpha, z.half.alpha)


##Alternativa

p <- 2 * pnorm(z)

p


## A estatística de teste z = -3.845452 está fora dos valores críticos 
## -1.96, 1.96, ou seja, está fora da região de aceitação. Portanto, a um 
## nível de significância de 0.05, nós rejeitamos a hipotese nula o que indica 
## que sim, o peso dos pinguins mudou quando comparado com o último ano
## 
## Pela solução alternativa, podemos ver que o valor de p está em 0.0001203305,
## abaixo do valor alpha, rejeitamos a hipótese nula e mantemos a hipótese 
## alternativa.

###### EXERCICIO 2
#### HIPOTESES
### HO: Média = 53kg
### H1: Média <> 53kg

## Cálculos do teste

xbar = 50 #media da amostra

mu0 = 53 #valor da hipotese

variancia = 16 #Valor da variancia observada

sigma = sqrt(variancia) #Desvio padrao da populacao

n = 15 #tamanho da amostra

z = (xbar - mu0) / (sigma / sqrt(n)) #Estatistica do teste

z

##Teste de Hipotese com significancia de 0.05

alpha = 0.05

z.half.alpha = qnorm(1 - alpha/2)

c(-z.half.alpha, z.half.alpha)

##Alternativa

p <- 2 * pnorm(z)

p


## A estatística de teste z = -2.904738 está fora dos valores críticos 
## -1.96, 1.96, ou seja, está fora da região de aceitação. Portanto, a um 
## nível de significância de 0.05, nós rejeitamos a hipotese nula o que indica 
## que sim, a troca dos fornecedores realmente alterou a qualidade dos produtos
## 
## Pela solução alternativa, podemos ver que o valor de p está em 0.003675612,
## abaixo do valor alpha, rejeitamos a hipótese nula e mantemos a hipótese 
## alternativa.


###### EXERCICIO 3
#### HIPOTESES
### HO: Média = 7.4s
### H1: Média < 7.4s

alpha = 0.01

amostra <- c(6.8, 7.1, 5.9, 7.5, 6.3, 6.9, 7.2, 7.6, 6.6, 6.3)

n = length(amostra)# tamanho da amostra

df = n-1 # graus de liberdade

S = sd(amostra)# desvio padrão da amostra

mu0 = 7.4 # valor da hipótese

xbar = mean(amostra) # média da amostra

tc <- qt(alpha/2,df = df) # Equação para calculo de TC

tc

Tt <- (sqrt(n)*(xbar - mu0))/S # Equação da Estatística de Teste

Tt

## O resultado do teste T retornou em -3.326528, sendo esse valor superior
## a -3.249836, ou seja, está dentro da região de crítica. Portanto, a um 
## nível de significância de 0.01, nós rejeitamos a hipotese nula o que indica 
## que sim, as mudanças reduziram o tempo de transmissão dos dados.
