
#Instalando os pacotes

install.packages("ggplot2")
install.packages("dplyr")
install.packages("ggpubr")
install.packages("data.table")
install.packages("knitr")
install.packages("lubridate")

#Carregando as bibliotecas

library("dplyr")
library("ggplot2")
library("ggpubr")
library("readr")
library("data.table")
library("knitr")
library("lubridate")

#Códigos Ebook

###Parte 1 - Análise Exploratória de Dados

##Iniciando os dados

ozone <- read.csv(file="C:/Users/MarcosAcácio/OneDrive - GT Group/Área de Trabalho/Mackenzie/Análise Estatistica/Trilha 3/ozonesmall.csv", 
                  sep=",",
                  header = TRUE)

ozone <- as.data.table(ozone, col_types = "ccccinnccccccncnnccccccc")

##Etapa 04

ozonesmall <- as.data.table(ozone,
                    col_types = cols( 'State Code' = col_character(),
                                      'County Code' = col_character(),
                                      'Site Num' = col_character(), 
                                      'Parameter Code' = col_character(),
                                      Longitude = col_double(), 
                                      Datum = col_character(),
                                      'Parameter Name' = col_character(),
                                      'Date Local' = col_character(), 
                                      'Time Loca' = col_character(),
                                      'Date GMT' = col_character(), 
                                      'Time GMT' = col_character(),
                                      'Sample Measurement' = col_double(),
                                      'Units of Measure' = col_character(),
                                      MDL = col_double(), 
                                      Uncertainty = col_double(),
                                      Qualifier = col_character(), 
                                      'Method Type' = col_character(),
                                      'Method Code' = col_character(),
                                      'State Name' = col_character(), 
                                      'County Name' = col_character(),
                                      'Date of Last Change' = col_character()))

setnames(ozone, names(ozone), gsub(" ",".", names(ozone), ignore.case = TRUE))

##Etapa 03

nrow(ozone)

ncol(ozone)

##Etapa 04

str(ozone, strict.width = "wrap", width = 80, give.attr = FALSE)

##Etapa 05

head(select(ozone, c(6:7, 10)))

tail(select(ozone, c(6:7, 10)))

##Etapa 06

hrs <- ozone%>% group_by(Time.Local) %>% summarise(n=n())

bind_cols(slice(hrs, 1:12), slice(hrs, 13:24))

select(ozone, State.Name) %>% unique %>% nrow

estados <- select(ozone, State.Name) %>% unique

metade <- round(nrow(estados)/ 2, 0)

l1 <- data.frame(slice(estados, 1:metade))

l1[27, ] <- NA

l2 <- data.frame(slice(estados, (metade + 1) : nrow(estados)))

bind_cols(l1, l2)

ozone %>% filter (!State.Name %in% c("District Of Columbia", "Puerto Rico", 
  "Country Of Mexico")) %>% select(State.Name) %>% unique %>% 
  unique() %>% nrow()

ozone <- ozone %>% filter(!State.Name %in%
                            c("District Of Columbia", "Puerto Rico", 
                                             "Country Of Mexico"))

##Etapa 07

summary(ozone$Sample.Measurement)

quantile(ozone$Sample.Measurement, seq(0, 1, 0.1))

##Etapa 08

ranking <- group_by(ozone, State.Name, County.Name) %>% 
  summarise(ozone = mean(Sample.Measurement)) %>%
  as.data.frame %>% arrange(desc(ozone))

head(ranking, 10)

tail(ranking, 10)

filter(ozone, State.Name =="California" & County.Name =="Mariposa") %>% nrow

ozone <- mutate(ozone, Date.Local = as.Date(Date.Local))

CcrekCO <- filter(ozone, State.Name == "Colorado" & 
                    County.Name == "Clear Creek") %>% 
  mutate(month = lubridate::month(Date.Local, label = TRUE, abbr = FALSE)) %>%
  group_by(month) %>% summarize(ozone = mean(Sample.Measurement, na.rm = T),
                                n_meas = n())

mariposaCA <- filter(ozone, State.Name == "California" & 
                       County.Name == "Mariposa") %>%
  mutate(month = lubridate::month(Date.Local, label = TRUE, abbr = FALSE)) %>%
  group_by(month) %>% summarize(ozone = mean(Sample.Measurement), n_meas = n())

full_join(CcrekCO, mariposaCA, by ="month", suffix = c(".CCreek", 
                                                       ".Mariposa")) %>%
  arrange(month)

filter(ozone, State.Name == "Oklahoma" & County.Name == "Caddo") %>% nrow

caddopornomes <- filter(ozone, State.Name == "Oklahoma" &
                          County.Name == "Caddo") %>%
  mutate(month = lubridate::month(Date.Local, label = TRUE, abbr = FALSE)) %>%
  group_by(month) %>% summarize(ozone = mean(Sample.Measurement), n_meas = n())

caddopornomes

###Parte 2 - Graficos Exploratorios

##Iniciando os dados

classes <- c("numeric", "character", "factor", "numeric", "numeric")

pollution <- read.csv(file="C:/Users/MarcosAcácio/OneDrive - GT Group/Área de Trabalho/Mackenzie/Análise Estatistica/Trilha 3/avgpm25.csv", 
                  sep=",",
                  header = TRUE)

head(pollution)

##Inspeção de Dados

str(pollution, strict.width = "wrap", give.attr = FALSE)

##Boxplot univariado

ggplot(data = pollution) + geom_boxplot(aes(x = "identity", y = pm25),
                                        witdh = 0.3, varwidth = F) +
  labs(y = "Nível", x = "", caption = "Fonte: Elaborado pelo autor") +
  geom_abline(slope = 0, intercept = 12, color = "red") +
  scale_x_discrete(labels = NULL, breaks = NULL) +
  labs_pubr() + theme_pubr() + theme(plot.caption = element_text(hjust = 0))

##Boxplot multivariado

ggplot(pollution) + geom_boxplot(aes(region, pm25), width = 0.5) + 
  labs(x = "Regiões", y = "Nível de PM2.5", 
       caption = "Fonte: Elaborado pelo autor") + 
  geom_abline(slope = 0,intercept = 12, color = "red") + labs_pubr() + 
  theme_pubr() + theme(plot.caption = element_text(hjust = 0))

##Histograma

ggplot(pollution) + geom_histogram(aes(pm25), bins = 16, fill = "green",
                                   color = "black") + 
  geom_vline(xintercept = 12, color = "yellow", lwd = 2) + geom_rug(aes(pm25)) +
  labs(title = "Magenta: mediana; Amarelo: limite nacional",
       x = "Níveis", y = "Frequencia", caption = "Fonte: Elabotado pelo autor") +
  geom_vline(xintercept = median(pollution$pm25), color = "magenta", lwd = 2) + 
  labs_pubr() + theme_pubr() + theme(plot.caption = element_text(hjust = 0))

##Histograma com mais partes

ggplot(pollution) + geom_histogram(aes(pm25), bins = 100, fill = "green", 
                                   color = "black") + geom_rug(aes(pm25)) + 
  labs(x = "Níveis", y = "Frequência", caption = "Fonte: Elaborado pelo autor") +
  labs_pubr() + theme_pubr() + theme(plot.caption = element_text(hjust = 0))

ggplot(pollution) + geom_histogram(aes(pm25), bins = 16, fill = "green", 
                                   color = "black") + 
  labs(x = "Níveis", y = "Frequência", caption = "Fonte: Elaborado pelo autor") + 
  facet_grid(. ~ region) + geom_vline(xintercept = 12, color = "yellow", lwd = 2) + 
  geom_vline(xintercept = median(pollution$pm25), color = "magenta", lwd = 2) + 
  labs_pubr() + theme_pubr() + theme(plot.caption = element_text(hjust = 0))

##Gráfico de Barras

ggplot(pollution) + geom_bar(aes(x = region), stat = "count", 
                             fill = "wheat", color = "black") + 
  labs(y = "Num. Municípios", caption = "Fonte: Elaborado pelo autor", x = "") +
  labs_pubr() + theme_pubr() + theme(plot.caption = element_text(hjust = 0))

##Scatterplot opção sem cor

ggplot(pollution) + geom_point(aes(x = latitude, y = pm25)) + 
  geom_abline(slope = 0, intercept = 12, linetype = 2) + 
  labs(caption = "Fonte: Elaborado pelo autor") + 
  labs_pubr() + theme_pubr() + theme(plot.caption = element_text(hjust = 0))

##Scatterplot opção com cor

ggplot(pollution) + geom_point(aes(x = latitude, y = pm25, color = region)) + 
  geom_abline(slope = 0, intercept = 12, linetype = 2) + 
  labs(caption = "Fonte: Elaborado pelo autor", color = "Região") + 
  labs_pubr() + theme_pubr() + theme(plot.caption = element_text(hjust = 0))

