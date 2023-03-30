#Bienvenidos al curso de programacion en R
#Fecha: 31/01/2023

#Configuracion de mi directorio de trabajo
setwd("C:/Users/EQUIPO/Desktop/CTIC/PROGRAMACION EN R/SESION1")

#Verificacion del directorio de trabajo
getwd()

### Definicion de variables ###


#### Definicion de variables ####
NombreVariable <- 12.8
z <- "Clase 1"
y <- TRUE
z <- 4

#Una vez definidas las variables ya se puede operar con ellas 
#Operaciones aritmeticas : + - * / ^
# %% : residuo de la division entera
# %/% : cociente de la division entera
w <- NombreVariable^z

#Limpiar la memoria ram
rm(list = ls())

#Creo una variable numerica
t1 <- 666

#Solo deseo borrar una variable en particular 
rm(t1)

# Operadores logicos
# && : and logico
## || : or logico

t2 <- TRUE && FALSE

# ! : Negacion
t3 <- !FALSE
t3

#Funcion c
help(c)
#
vec1 <-  c(12,15,2,4,1,9)
vec2 <- c("UNO",1,"I")
vec3 <- c(vec1,vec2)

# Otra forma de definir un vecto
vec4 <- 14:20

# Las funciones rep, seq : veamos su documentacion
help(rep)
#
vec5 <- rep(vec1, times = 10)
vec6 <- rep(666, times=666)

# Funcion seq :
help(seq)

vec7 <- seq(0,66,3)
vec8 <- seq(12,120,5)
vec9 <- seq(10,110,length.out = 1100)

#otra forma: usando algoritmos para generar numeros
#pseudoaleatorios
help(rnorm)
#
# Generemos una muestra de tamaño 1600 de una distribucion
# normal con paramaetros mean=12.9 y sd=9.81
vec10 = rnorm(n=1600,mean=12.9,sd=9.81)

#otras funciones
help(runif)
help("rweibull")
help(rt)
help("rpois")

#### Paquetes que se cargan por efecto####

sessionInfo()

#leamos la documentacion del paquete stats
library(help = stats)
help(median)

#### definicion de matrices####
datos1 <- 1:10
#reforteamos datos1 para obtener una matriz

mat1 <- matrix(data=datos1,ncol=2)

mat2 <- matrix(data=datos1,ncol=3)

#modifiquemos el argumento byrow del afuncion matriz
mat3 <- matrix(data = datos1,ncol =2, byrow=TRUE)

#asignemos una etiqueta de las dimensiones de mi matriz
mat4 <- matrix(data=rnorm(24,12,1.2),ncol=6,byrow = TRUE,
               dimnames = list(c("f1","f2","f3","f4"),c("c1","c2","c3","c4","c5","c6")))


