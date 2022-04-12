##### ATIVIDADE DE APROFUNDAMENTO DA TRILHA 3
#### DISCIPLINA DE ANALISE DE DADOS
### MARCOS VINICIUS ACACIO DOS SANTOS
## RA 9217847

#Instalando os pacotes

install.packages("lubridate")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("ggpubr")
install.packages("data.table")
install.packages("knitr")
install.packages("skimr")

#Carregando as bibliotecas

library("lubridate")
library("ggplot2")
library("dplyr")
library("ggpubr")
library("readr")
library("data.table")
library("knitr")
library("skimr")

#Entendendo a fonte de dados

?diamonds

#Carregando dados

diamantes <- data.frame(diamonds)

#Verificação do conteúdo

nrow(diamantes)

ncol(diamantes)

#Estrutura do Conjunto

str(diamantes)

#Exploração inicial e final do conjunto

head(select(diamantes, c(1:10, 10)))

tail(select(diamantes, c(1:10, 10)))

#Sumarização dos dados

summary(diamantes)

quantile(diamantes$price, seq(0, 1, 0.1))

#Exploração da variável price

##Boxplot univariado

ggplot(data = diamantes) + geom_boxplot(aes(x = "price", y = price),
                                        witdh = 0.3, varwidth = F) +
  labs(y = "Preço", x = "", caption = "Fonte: Elaborado por Marcos Acacio") +
  geom_abline(slope = 0, intercept = 12, color = "green") +
  scale_x_discrete(labels = NULL, breaks = NULL) +
  labs_pubr() + theme_pubr() + theme(plot.caption = element_text(hjust = 0))

##Histograma

ggplot(diamantes) + geom_histogram(aes(price), bins = 16, fill = "white",
                                   color = "black")  + geom_rug(aes(price)) +
  labs(title = "Azul: mediana",
       x = "Preço", y = "Frequencia", caption = "Fonte: Elabotado por Marcos Acacio") +
  geom_vline(xintercept = median(diamantes$price), color = "blue", lwd = 2) + 
  labs_pubr() + theme_pubr() + theme(plot.caption = element_text(hjust = 0))

##Boxplot multivariado
###Price vs Cut

ggplot(diamantes) + geom_boxplot(aes(cut, price), width = 0.5) + 
  labs(x = "Tipos de corte", y = "Preço", 
       caption = "Fonte: Elaborado por Marcos Acacio") + labs_pubr() + 
  theme_pubr() + theme(plot.caption = element_text(hjust = 0))

###Price vs clarity

ggplot(diamantes) + geom_boxplot(aes(clarity, price), width = 0.5) + 
  labs(x = "Clareza do Diamante", y = "Preço", 
       caption = "Fonte: Elaborado por Marcos Acacio") + labs_pubr() + 
  theme_pubr() + theme(plot.caption = element_text(hjust = 0))

###Price vs Color

ggplot(diamantes) + geom_boxplot(aes(color, price), width = 0.5) + 
  labs(x = "Cor do Diamante", y = "Preço", 
       caption = "Fonte: Elaborado por Marcos Acacio") + labs_pubr() + 
  theme_pubr() + theme(plot.caption = element_text(hjust = 0))

##Histograma com mais partes
###Price vs Cut

ggplot(diamantes) + geom_histogram(aes(price), bins = 10, fill = "green", 
                                   color = "black") + 
  labs(title = "Magenta = Mediana", x = "Preço", y = "Frequência", 
       caption = "Fonte: Elaborado por Marcos Acacio") + facet_grid(. ~ cut) + 
  geom_vline(xintercept = median(diamantes$price), color = "magenta", lwd = 2) + 
  labs_pubr() + theme_pubr() + theme(plot.caption = element_text(hjust = 0))

###Price vs Clarity

ggplot(diamantes) + geom_histogram(aes(price), bins = 4, fill = "green", 
                                   color = "black") + 
  labs(title = "Magenta = Mediana", x = "Preço", y = "Frequência", 
       caption = "Fonte: Elaborado ppor Marcos Acacio") + facet_grid(. ~ clarity) + 
  geom_vline(xintercept = median(diamantes$price), color = "magenta", lwd = 2) + 
  labs_pubr() + theme_pubr() + theme(plot.caption = element_text(hjust = 0))

###Price vs Color

ggplot(diamantes) + geom_histogram(aes(price), bins = 4, fill = "green", 
                                   color = "black") + 
  labs(title = "Magenta = Mediana", x = "Preço", y = "Frequência", 
       caption = "Fonte: Elaborado por Marcos Acacio") + facet_grid(. ~ color) + 
  geom_vline(xintercept = median(diamantes$price), color = "magenta", lwd = 2) + 
  labs_pubr() + theme_pubr() + theme(plot.caption = element_text(hjust = 0))

#Exploração da variável carat

##Boxplot univariado

ggplot(data = diamantes) + geom_boxplot(aes(x = "price", y = carat),
                                        witdh = 0.3, varwidth = F) +
  labs(y = "Preço", x = "", caption = "Fonte: Elaborado por Marcos Acacio") +
  geom_abline(slope = 0, intercept = 12, color = "green") +
  scale_x_discrete(labels = NULL, breaks = NULL) +
  labs_pubr() + theme_pubr() + theme(plot.caption = element_text(hjust = 0))

##Histograma

ggplot(diamantes) + geom_histogram(aes(carat), bins = 16, fill = "white",
                                   color = "black")  + geom_rug(aes(carat)) +
  labs(title = "Azul: mediana",
       x = "Preço", y = "Frequencia", caption = "Fonte: Elabotado por Marcos Acacio") +
  geom_vline(xintercept = median(diamantes$carat), color = "blue", lwd = 2) + 
  labs_pubr() + theme_pubr() + theme(plot.caption = element_text(hjust = 0))

##Boxplot multivariado
###carat vs Cut

ggplot(diamantes) + geom_boxplot(aes(cut, carat), width = 0.5) + 
  labs(x = "Cut", y = "Carat", 
       caption = "Fonte: Elaborado por Marcos Acacio") + labs_pubr() + 
  theme_pubr() + theme(plot.caption = element_text(hjust = 0))

###Carat vs clarity

ggplot(diamantes) + geom_boxplot(aes(clarity, carat), width = 0.5) + 
  labs(x = "Clarity", y = "Carat", 
       caption = "Fonte: Elaborado por Marcos Acacio") + labs_pubr() + 
  theme_pubr() + theme(plot.caption = element_text(hjust = 0))

###Carat vs Color

ggplot(diamantes) + geom_boxplot(aes(color, carat), width = 0.5) + 
  labs(x = "Color", y = "Carat", 
       caption = "Fonte: Elaborado por Marcos Acacio") + labs_pubr() + 
  theme_pubr() + theme(plot.caption = element_text(hjust = 0))

##Histograma com mais partes
###Carat vs Cut

ggplot(diamantes) + geom_histogram(aes(carat), bins = 10, fill = "green", 
                                   color = "black") + 
  labs(title = "Magenta = Mediana", x = "Preço", y = "Frequência", 
       caption = "Fonte: Elaborado por Marcos Acacio") + facet_grid(. ~ cut) + 
  geom_vline(xintercept = median(diamantes$carat), color = "magenta", lwd = 2) + 
  labs_pubr() + theme_pubr() + theme(plot.caption = element_text(hjust = 0))

###Carat vs Clarity

ggplot(diamantes) + geom_histogram(aes(carat), bins = 10, fill = "green", 
                                   color = "black") + 
  labs(title = "Magenta = Mediana", x = "Preço", y = "Frequência", 
       caption = "Fonte: Elaborado por Marcos Acacio") + facet_grid(. ~ clarity) + 
  geom_vline(xintercept = median(diamantes$carat), color = "magenta", lwd = 2) + 
  labs_pubr() + theme_pubr() + theme(plot.caption = element_text(hjust = 0))

###Carat vs Color

ggplot(diamantes) + geom_histogram(aes(carat), bins = 10, fill = "green", 
                                   color = "black") + 
  labs(title = "Magenta = Mediana", x = "Preço", y = "Frequência", 
       caption = "Fonte: Elaborado por Marcos Acacio") + facet_grid(. ~ color) + 
  geom_vline(xintercept = median(diamantes$carat), color = "magenta", lwd = 2) + 
  labs_pubr() + theme_pubr() + theme(plot.caption = element_text(hjust = 0))

#Exploração das demais variáveis

##Variável cut

###Gráfico de Barras

ggplot(diamantes) + geom_bar(aes(x = cut), stat = "count", 
                             fill = "wheat", color = "black") + 
  labs(y = "Qtde Registros", caption = "Fonte: Elaborado por Marcos Acacio", x = "") +
  labs_pubr() + theme_pubr() + theme(plot.caption = element_text(hjust = 0))

##Variáveis table x depth

###Scatterplot

ggplot(diamantes) + geom_point(aes(x = depth, y = table)) + 
  geom_abline(slope = 0, intercept = 12, linetype = 2) + 
  labs(caption = "Fonte: Elaborado por Marcos Acacio") + 
  labs_pubr() + theme_pubr() + theme(plot.caption = element_text(hjust = 0))


