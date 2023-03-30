#### Configuraciones iniciales ####
rm(list = ls())
setwd("C:/Users/azamudio/Desktop/Programacion_R/Sesion5")
library(tidyverse)

# Cargamos los datos 
creditdata <- read.table("german.data", stringsAsFactors = TRUE)

# Coloquemos nombres a las variables/columnas
colnames(creditdata) <- c("account.status",
                          "months",
                          "credit.history",
                          "purpose",
                          "credit.amount",
                          "savings",
                          "employment",
                          "installment.date",
                          "personal.status",
                          "guarantors",
                          "residence",
                          "property",
                          "age",
                          "other.installments",
                          "housing",
                          "credit.cards",
                          "jobs",
                          "dependents",
                          "phone",
                          "foreign.worker",
                          "credit.rating")


# Recodifiquemos la variable dependiente
# originalmente :
# Good = 1
# Bad = 2
# 
# Recodificacion
# Good = 1
# Bad = 0
creditdata$credit.rating <- ifelse(creditdata$credit.rating == 1, 1, 0)
table(creditdata$credit.rating)

#### Primer modelo de regresion logistica ####
# Variable dependiente : credit.rating
# modelo1 : utilicemos solo variables cuantitativas

modelo1 <- glm(formula = credit.rating ~  months + credit.amount + installment.date + 
                 residence + age + credit.cards + dependents,
               data = creditdata,
               family = binomial)

# Veamos un resumen de los campos del objeto modelo1
summary(modelo1)

# Filtremos las variables que tienen un p-value por debajo del umbral 0.05
Var_pvalue <- summary(modelo1)$coeff[,4] < 0.05
Var1 <- names(Var_pvalue)[Var_pvalue == TRUE]


# Construir la formula para un nuevoo modelo considerando solo las variables 
# estadisticamente significativas (pvalue < 0.05)
# Sea fx la formula a construir 
# 
listaVarSig <- c()
# 
fx <- "credit.rating ~"
# 
for(index in 1:length(Var1)){
  if(Var1[index] != "(Intercept)"){
    # Construimos el vector de variables significativas
    listaVarSig <- c(listaVarSig, Var1[index])
    if (fx == "credit.rating ~"){
      fx <- paste(fx , Var1[index], sep =" ")
    }
    else {
      fx <- paste(fx , Var1[index], sep = "+")
    }
  }
}

# Filtremos el dataframe de datos, considerando solo las variables que se 
# encuentren en listaVarsig
creditdataVarSig <- creditdata %>% 
  select(all_of(listaVarSig)) %>% 
  mutate(credit.rating= creditdata$credit.rating)


# Construyamos un segundo modelo de regresion logistica 
modelo2 <- glm(formula = fx,
               data = creditdataVarSig,
               family = binomial)

# Resumen del modelo 
summary(modelo2)


# Construyamos algunos pronosticos para este nuevo modelo 
Indextest <- sample(1:nrow(creditdataVarSig), 0.15*nrow(creditdataVarSig))
# 
# Construyamos un dataset para testear mi modelo 
test <- creditdataVarSig[Indextest, ]

# Calculamos los pronosticos para las observaciones de test
pronostico <- predict.glm(modelo2, newdata = test[, -5], type = "response")

# Binaricemos la variable pronostico 
predicciones <- floor(pronostico + 0.6)
unique(predicciones)
# 
table(predicciones)

# Matriz de confusion
table(test$credit.rating , predicciones)













