####PRIMEROS PASOS####
#LIMPIAR LA MEMORIA
rm(list=ls())
#
# Definimos nuestro directorio de trabajo
setwd("C:/Users/EQUIPO/Desktop/CTIC/PROGRAMACION EN R/SESION1")
####ESTRUCTURAS MATRICIALEAS####
# Vectores
# Matrices

# Definaos una par de vectores
x <- c(rnorm(12,2.3,0.1),runif(24,-1,2))
y <- rweibull(36,shape=2,scale=5)

#Definimos un par de matrices
MatX <- matrix(data=x, ncol=4)
MatY <- matrix(data=y, nrow=18)

# Acceso a lso elementos de los objetos vectoriales/matriciales
# primera componenete de X
x[1]
x[3]

#Cuantas componentes tiene x
NumElementos <- length(x)

# Para acceder a las componentes de un objeto bidimensional
MatX[1,2]

# tercera fila de MatX
MatX[3, ]
# cuarta columna de MatX
MatX[ ,4]

#Funciones asociadas a las estructuras vectoriales
sum(x)
sd(x)
sd(MatY[,2])

#### DATA FRAMES####

library(help = datasets)

#funcion data : Sirve para cargar un dato del paquete datasets
data("airquality")
str(airquality)

# acceso a una columna en un dataframe
# notacion matricial
#notacion propia de los dataframes

#segunda columna de airquality
airquality[,2]
airquality$Solar.R

#carguemos otro dataset
data("WWWusage")

#instalemos un paquete  nuevvo : ISLR2
#Carguemos el paquete ISLR2
library(ISLR2)

#cargamos ggplot2
library(ggplot2)

# lista de componentes del paquete ISLR2
library(help = ISLR2)

data("Boston")
View(Boston)


#carguemos datos de archivo de texto
vuelos2013 <- read.csv("https://raw.githubusercontent.com/robintux/Datasets4StackOverFlowQuestions/master/nycflights13.csv")


#datos de lso matriculados de la uni (2016-2022)
DataUNI <- read.csv("Matriculados_2016_al_2022.csv")
View(DataUNI)



