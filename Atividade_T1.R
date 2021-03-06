##### ATIVIDADE DE APROFUNDAMENTO DA TRILHA 1
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


#Carregando dados CSV

vendas <- read.csv(
  file = "C:/Users/MarcosAc�cio/OneDrive - GT Group/�rea de Trabalho/Mackenzie/An�lise Estatistica/Trilha 1/Atividade_Aprofundamento/PelicanStores.csv",
  sep = ',',
  dec = '.',
)

#Visualizando os dados

View(vendas)

#Mudando vendas liquidas de chr para dbl

vendas$Vendas.Liquidas <- as.numeric(gsub(",", ".", as.character(vendas$Vendas.l�.quidas)))


#Apresenta��o dos resumos

glimpse(vendas)

#Estatistica Descritiva
##Vari�veis Itens, Vendas L�quidas e Idade

##Calculo de M�dia

mean(vendas$Itens)
mean(vendas$Idade)
mean(vendas$Vendas.Liquidas)

##Calculo de Mediana

median(vendas$Itens)
median(vendas$Idade)
median(vendas$Vendas.Liquidas)

##Calculo de DesvPad

sd(vendas$Itens)
sd(vendas$Idade)
sd(vendas$Vendas.Liquidas)

##Calculo Variancia

var(vendas$Idade)
var(vendas$Itens)
var(vendas$Vendas.Liquidas)

##Calculo Moda

### Criando uma funcao.
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

moda_idade <- getmode(vendas$Idade)
moda_itens <- getmode(vendas$Itens)
moda_vendas_liquidas <- getmode(vendas$Vendas.Liquidas)


#Tabula��o variaveis categ�ricas

table(vendas$Tipo.de.Cliente)
table(vendas$M�.todo.de.Pagamento)
table(vendas$Gênero)
table(vendas$Estado.Civil)
