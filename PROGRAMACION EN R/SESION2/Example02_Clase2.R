#### Configuracion inicial ####
# 
# Configuracion del directorio de trabajo
setwd("C:/Users/EQUIPO/Desktop/CTIC/PROGRAMACION EN R/SESION2")

# Limpiemos la memoria ram 
rm(list = ls())

# Cargamos el dataset
# https://www.datosabiertos.gob.pe/dataset/alumnos-matriculados
UNI <- read.csv("Matriculados_2016_al_2022.csv")

#### Preprocesamiento ####

# Nombre de cada columnas
NombreVars <- colnames(UNI)

# Dimensiones del dataset
dim(UNI)
nrow(UNI)
ncol(UNI)

# Valores Faltantes / MIssing values por columna
colSums(is.na(UNI))

#### Columna Año.Matricula ####
# 
# Los posibles valores de la columna Año.Matricula 
unique(UNI$Año.Matrícula)

year2k16 <- UNI[UNI$Año.Matrícula == 2016, ]
nrow(UNI[UNI$Año.Matrícula == 2017, ])
nrow(UNI[UNI$Año.Matrícula == 2018, ])


NumMatriculados <- c()
for(year in unique(UNI$Año.Matrícula)){
  print(paste("Año procesado : ", year))
  NumMatriculados <- c(NumMatriculados, nrow(UNI[UNI$Año.Matrícula == year,]))
}

# 
# Empaquetemos esta informacion en un dataframe
# help(data.frame)
NumMatriculados_df <- data.frame(Año =unique(UNI$Año.Matrícula),
                                 NumMat = NumMatriculados)

#### Columna Periodo ####

# LOs posibles valores de la columna 
unique(UNI$Período)

#### Columna Nacionalidad ####
unique(UNI$Nacionalidad)
# 
# Funcion tabla
table(UNI$Nacionalidad)
# 
# COnsideremo el año 2016
table(UNI[UNI$Año.Matrícula == 2016,]$Nacionalidad)





