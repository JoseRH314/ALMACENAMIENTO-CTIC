#### COnfiguraciones iniciales ####
rm(list = ls())
setwd("C:/Users/azamudio/Desktop/Programacion_R/Sesion5")

# Cargamos paquetes y datos 
library(tidyverse)

# Cargamos datos 
df <- read.csv("Pesos_Bebe_Meses.csv")

#### Regresion Lineal ####

# Construyamos un diagrama de dispersion
# Analisis visual de la relacion que existe entre mis variables
ggplot(data = df,mapping = aes(x = Edad, y = Peso))+
  geom_point()+
  geom_smooth(method = "lm")

# Es muy natural asumir de que existe linealidad entre mis variables
# y[i] = m * x[i] + b + e[i]


# Observacion : MOdelo de regresion no lineal 
# y[i] = f(x[i], parametros) + e[i]
# Sera adecuado un modelo no lineal ?
# Si se cumple por lo menos una de tres opciones :
  # Hay linealidad entre la variables mas no en parametros (esta informacion
  # la provee f)
# 
  # Que los parametros presenten linealidad , pero las variables no 
# 
  # No cabe linealidad entre variables ni entre parametros 


# PAra el calculo de los parametros del modelo (y algunas cosas mas)
# usaremos la funcion lm (paquete stats)
# 
# Consideremos el modelo que toma al Peso como variable independiente
# y en consecuencia la variable edad sera la variable dependiente
# 

modelo <- lm(formula = Edad ~ Peso, data = df)
modelo
# Matematicamente : Edad = 2.014*Peso - 6.913

# Analisis del modelo 
summary(modelo)
# Call:
#   lm(formula = Edad ~ Peso, data = df)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -0.9296 -0.2473 -0.1310  0.3784  0.8060 
# 
# Coefficients:
#             Estimate      Std. Error   t value   Pr(>|t|)    
# (Intercept) -6.91312      0.48270     -14.32     1.85e-08 ***
#   Peso         2.01429    0.07236      27.84     1.50e-11 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.4812 on 11 degrees of freedom
# Multiple R-squared:  0.986,	Adjusted R-squared:  0.9847 
# F-statistic: 774.9 on 1 and 11 DF,  p-value: 1.503e-11

# Observemos al campo Residuals del objeto modelo
modelo$residuals
modelo[["residuals"]]
# 
# Mostremos un diagrama de dispersion
plot(modelo$residuals)
# 
# Mostremo un boxplot de los residuos 
boxplot(modelo[["residuals"]],
        main ="Boxplot de los residuos",
        ylab = "Residuos") 

# Los valores pronosticos por el modelo 
modelo$fitted.values
# 
# Otra forma de calcular los errores (residuals):
df$Edad - modelo$fitted.values


# Evaluacion del modelo 
par(mfrow = c(2,2))
plot(modelo)
