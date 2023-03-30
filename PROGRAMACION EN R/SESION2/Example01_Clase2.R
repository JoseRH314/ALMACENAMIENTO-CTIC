#### Configuracion inicial ####
# 
# Configuracion del directorio de trabajo
setwd("C:/Users/EQUIPO/Desktop/CTIC/PROGRAMACION EN R/SESION2")

# Limpiemos la memoria ram 
rm(list = ls())

# Carguemos en memoria el paquete/libreria/modulo 
library(ggplot2)

# Usemos el dataset diamonds
data("diamonds")

# Como diamonds pertenece a ggplot, este trae consigo documentacion
help("diamonds")


# Observacion
# Warning message:
#   package ‘ggplot2’ was built under R version 4.2.2
# Recomendacion : Desintalar la version antigua de R y actualizar a la 
# version 4.2.2

#### Preprocesamiento ####
# 
# Nombres de las columnas/variables 
NombreVariables <- colnames(diamonds)

# Dimensiones del dataset/dataframe
FilasColumnas <- dim(diamonds)
NumFilas <- FilasColumnas[1]
NumCols <- FilasColumnas[2]
# 
ncol(diamonds)
nrow(diamonds)
# 

# Funcion summary
summary(diamonds)

# NUmero de valores faltantes (missing values) por columna
colSums(is.na(diamonds))

#### Analisis grafico : Histogramas ####
# 
# funcion hist
help("hist")

# Analicemos la variable price 
# 
hist_Price <- hist(diamonds$price)
# 
# Define los intervalos de clase 
hist_Price$breaks

# Define el numero de elementos en cada intervalo de clase 
hist_Price$counts

# Kernel density estimation
hist_Price$density

# Punto medio de cada intervalo de clase 
hist_Price$mids

# Nombre dela variables
hist_Price$xname

# Valor booleano
hist_Price$equidist

# Personalicemos nuestro primer histograma 
# 
# Si deseo que el grafico se abra en una venta externa : dev.new()
dev.new()
# 
hist(x = diamonds$price,
     main = "Histograma del Precio de diamantes",
     col = "blue",
     ylim = c(0, max(hist_Price$counts)+2000),
     # Deseo modificar los valores que aparecen en el eje-X
     # dos pasos : Primer Paso 
     xaxt = "n"
     
     )
# El segundo paso para modificar los valores que aparecen en el eje-Y
axis(side = 1, at = seq(0, max(diamonds$price) ,by = 3000))
# 
# Deseo agregar el numero de elementos en cada intervalo de clase 
text(x = hist_Price$mids, y = hist_Price$counts,
     labels = hist_Price$counts,
     pos = 3)
# 
# Podemnos agregar una caja 
box()


# Personalicemos algo mas nuestro primer histograma 
# Modifiquemos los intervalos de clase 
hist_Price_2 <- hist(x = diamonds$price,
                        breaks = seq(0,20000, 2000))

dev.new()
hist_Price_Ver2 <- hist(x = diamonds$price,
                        breaks = seq(0,20000, 2000),
                        # Agregamos un titulo 
                        main = "Histograma de Price [diamonds]",
                        ylim =c(0, max(hist_Price_2$counts)+2000),
                        # MOdifiquemos el color de cada barrita
                        # col = c("green", "green", "green",
                        #                "yellow", "yellow", "yellow", "yellow",
                        #                "red", "red", "red"),
                        col = c("#5DE90E", "#5DE90E", "#5DE90E",
                                         "#08D7B1", "#08D7B1", "#08D7B1", "#08D7B1",
                                         "#EC05FA", "#EC05FA", "#EC05FA"),
                        # Modifiquemos los puntos que aparecen sobre el ejeX
                        xaxt ="n",
                        # 
                        # Modifiquemos las etiquetas del ejeX y del ejeY
                        xlab = "Precio [USD]",
                        ylab = "Frecuencia",
                        # Agreguemos un subtitulo 
                        sub = paste("[", min(diamonds$price), "-" ,max(diamonds$price), "]")
                        )
axis(side = 1, at = seq(0, max(diamonds$price), 3500))
# 
# Numero de elementos de cada intervalo de clase 
text(x = hist_Price_2$mids, y = hist_Price_2$counts,
     labels = hist_Price_2$counts,
     pos = 3,
     col = c("#5DE90E", "#5DE90E", "#5DE90E",
                      "#08D7B1", "#08D7B1", "#08D7B1", "#08D7B1",
                      "#EC05FA", "#EC05FA", "#EC05FA"))
# 
# Agreguemos el polinomio de frecuencias 
lines(hist_Price_2$mids, hist_Price_2$counts,
      type = "b",
      col = "red",
      lwd = 3)
box()

#### Analicemos price en funcion de cut #####
# Posibles valores de la columna cut 
levels(diamonds$cut)

# Guardemos nuestra imagen en el disco duro 
# jpeg(filename = "Hist_Variable_cut.jpeg",width = 500,
#      height = 2500,
#      units = "px")
pdf(file = "Hist_Variable_cut.pdf",
    width = 5,
    height = 25)
# Juntemos todos estos histogramas en una matriz de graficos 
par(mfrow = c(5,1))
# 
hist(diamonds[diamonds$cut == "Fair",]$price, 
     col = "royalblue1",
     main = "cut == Fair",
     xlab = "Precio [USD]",
     ylab = "Frecuencias")
hist(diamonds[diamonds$cut == "Good",]$price,
     col = "royalblue2",
       main = "cut == Good",
       xlab = "Precio [USD]",
       ylab = "Frecuencias")
hist(diamonds[diamonds$cut == "Very Good",]$price,
     col = "royalblue3",
       main = "cut == Very Good",
       xlab = "Precio [USD]",
       ylab = "Frecuencias")
hist(diamonds[diamonds$cut == "Premium",]$price,
     col = "royalblue4",
       main = "cut == Premium",
       xlab = "Precio [USD]",
       ylab = "Frecuencias")
hist(diamonds[diamonds$cut == "Ideal",]$price,
     col = "blue",
       main = "cut == Ideal",
       xlab = "Precio [USD]",
       ylab = "Frecuencias")
# 
dev.off()
# 

#### COnsideremos boxplots ####

boxplot(diamonds$price ~ diamonds$cut,
        main = "Boxplot : Precio vs Calidad de corte [cut]",
        col = c("yellow", "orange", "red", "cyan", "blue"),
        xlab = "Calidad de corte",
        ylab = "Precio")

# Numero de niveles de la variable cualitativa clarity
levels(diamonds$clarity)
# 

pdf("Boxplot_PriceClarity.pdf",
    width = 9,
    height = 6)
boxplot(diamonds$price ~ diamonds$clarity,
        main = "Boxplot : Precios vs Claridad",
        col = c("yellow", "yellow", "orange", "orange",
                        "blue", "blue", "cyan", "cyan"),
        xlab = "Claridad del diamante",
        ylab = "Precio",
        horizontal = TRUE)
dev.off()

























