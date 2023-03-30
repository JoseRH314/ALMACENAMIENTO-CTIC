#### EXAMEN FINAL ####

#PREGUNTA 1
rm(list = ls())
setwd("C:/Users/EQUIPO/Desktop/CTIC/PROGRAMACION EN R/EXAMEN_FINAL")

matriz <- matrix(data = 1:24, ncol= 6,
                 byrow = FALSE,
                 dimnames = list(c("f1","f2","f3","f4"),
                                 c("c1","c2","c3","c4",
                                   "c5","c6")))
# PREGUNTA 2
data1 <- read.csv("https://raw.githubusercontent.com/robintux/Datasets4StackOverFlowQuestions/master/Ginzberg.csv")
summary(data1$adjdep)
library(gridExtra)

# histogram, Q-Q plot, and boxplot
par(mfrow = c(2, 4))
hist(data1$adjdep, main = "Histogram")
boxplot(data1$adjdep, main = "Boxplot")
qqnorm(data1$adjdep, main = "Normal Q-Q plot")

# dar IQR
IQR(data1$adjdep)

#obtener valores de umbral para valores atípicos

Tmin = 0.5966 - (1.5*0.665185)
Tmax = 1.2617 + (1.5*0.665185)

# Encontrar valor atipico
data1$adjdep[which(data1$adjdep < Tmin | data1$adjdep > Tmax)]

# vavlores atipicos
boxplot.stats(data1[,"adjdep"])$out

# PREGUNTA 3
data2 <- read.csv("https://raw.githubusercontent.com/robintux/Datasets4StackOverFlowQuestions/master/BikeSharing.csv")

valor_max_coltemp <- max(data2$temp)

# PREGUNTA 4
# respuesta : ggplot

# PREGUNTA 5
data3 <- read.csv("https://raw.githubusercontent.com/robintux/Datasets4StackOverFlowQuestions/master/%20HumanResourcesAnalytics.csv")
NumMediumSalary <- nrow(data3[data3$salary == "medium", ])

# PREGUNTA 6

# Carguemos el paquete gapminder
library(gapminder)

# Carguemos el data gapminder
data("gapminder")
View(gapminder)

# PREGUNTA 7
data4 <- read.csv("https://raw.githubusercontent.com/robintux/Datasets4StackOverFlowQuestions/master/ClimaAUSTRALIA.csv")
NumValFaltantes <- colSums(is.na(data4))

# PREGUNTA 8
library(tidyverse)
# modelo 

modelo <- lm(formula = MinTemp ~ MaxTemp, data = data4, na.action=na.omit)
modelo

# Matematicamente : MinTemp = 0.6623*MaxTemp - 3.1890
# Veamos los residuos
modelo$residuals
sum(is.na(modelo$residuals))
sum(modelo$residuals)
summary(modelo)
# Mostremos un diagrama de dispersion
plot(modelo$residuals)
# 
# Mostremo un boxplot de los residuos 
boxplot(modelo[["residuals"]],
        main ="Boxplot de los residuos",
        ylab = "Residuos") 

sum(modelo[["residuals"]])

# PREGUNTA 9
kaka <- gapminder %>%
  filter(year == 1952) %>%
  select(continent) %>%
  unique()

# PREGUNTA 10
Notas2 <- c(12,9,3,18,14,15,1)
Paridad <- ifelse(Notas2%%2 == 0, "A", "B")

# PREGUNTA 11
data5 <- read.csv("https://raw.githubusercontent.com/robintux/Datasets4StackOverFlowQuestions/master/haberman.csv")
x <- data5$Edad
armonic<-1/mean(1/x)
armonic

# PREGUNTA 12
unique(data5$Estatus)

# PREGUNTA 13
# Respuesta : getwd
getwd()

# PREGUNTA 14

summary(data1$adjsimp)

hist(data1$adjsimp, main = "Histogram")
boxplot(data1$adjsimp, main = "Boxplot")
qqnorm(data1$adjsimp, main = "Normal Q-Q plot")

# dar IQR
IQR(data1$adjsimp)

#obtener valores de umbral para valores atípicos

Tmin = 0.6980  - (1.5*0.5038325)
Tmax = 1.2018 + (1.5*0.5038325)

# Encontrar valor atipico
atipicos <- data1$adjsimp[which(data1$adjsimp < Tmin | data1$adjsimp > Tmax)]
sum(atipicos)

# estos son los elementos atipicos
boxplot.stats(data1[,"adjsimp"])$out
# la suma de los elemntos atipicos
sum(boxplot.stats(data1[,"adjsimp"])$out)