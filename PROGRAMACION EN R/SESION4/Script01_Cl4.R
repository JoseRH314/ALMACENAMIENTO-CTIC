#### Primeros pasos usando ggplot2 ####
rm(list = ls())
setwd("C:/Users/azamudio/Desktop/Programacion_R/Sesion4")

# Cargamos librerias 
library(tidyverse)
library(car)

# Datos 
data("Salaries")
help("Salaries")

# Primer Paso : Setear/configurar la data y la estetica de la 
# capa base
p <- ggplot(data = Salaries,mapping = aes(x = yrs.service,y =salary))
class(p)

# Visualizacion de la capa base
p

# Diagrama de dispersion : al objeto p le agregamos una capa
# geometrica
p + geom_point()


# Consideremos otro par de variables 
q <- ggplot(data = Salaries, mapping = aes(x = yrs.since.phd,
                                           y = salary))
class(q)

# A la capa base definida en q le agregamos una capa geometrica
q + geom_point()

# MOdifiquemos el caracter de cada par ordenado y tambien 
# el tamaño de cada par ordenado 
q + geom_point(shape = "diamond", size = 3.5)

# Posibles valores para el argumento shape 
help(geom_point)
# 
# de lo aprendimos en :
# https://ggplot2.tidyverse.org/articles/ggplot2-specs.html
q + geom_point(shape = 5, size = 3.5)
q + geom_point(shape = 24, size = 3.5, fill = "red")
q + geom_point(shape = 24, size = 3.5, fill = "#17235F")


# Agreguemos una dimension mas : Consideremos el siguiente grafico
# tridimensional : 2 dimensiones geometricas  (variables 
# cuantitativas) y 1 dimension de forma (variable cualitativa)
p + geom_point(aes(shape = sex), size = 3)
q + geom_point(aes(shape = rank), size = 3.3)

# Personalicemos algunos de los componentes de nuestros 
# graficos anteriores
p +
  # Deseo que la forma del caracter de cada par ordenado y el 
  # color de cada ordenado dependan de la variable sex
  geom_point(aes(shape = sex, col = sex), size = 2.5) +
  # Deseo modificar/personalizar la forma/shape y el color de cada 
  # par ordenado de mi capa geometrica
  # scale_shape_manual(values = c(3,4))+
  scale_shape_manual(values = c(0,1))+
  scale_color_manual(values = c("blue", "orange"))+
  # La capa theme
  theme(legend.position = c(0.9,0.95))+
  theme(legend.direction = "horizontal")

ggsave(filename = "GRafico1.png", width = 15, height = 20,
       units = "cm")


# 

plt1 <- p +
  # Deseo que la forma del caracter de cada par ordenado y el 
  # color de cada ordenado dependan de la variable sex
  geom_point(aes(shape = sex, col = sex), size = 2.5) +
  # Deseo modificar/personalizar la forma/shape y el color de cada 
  # par ordenado de mi capa geometrica
  # scale_shape_manual(values = c(3,4))+
  scale_shape_manual(values = c(0,1))+
  scale_color_manual(values = c("blue", "orange"))+
  # La capa theme
  theme(legend.position = c(0.9,0.95))+
  theme(legend.direction = "horizontal")

ggsave(filename = "GRafico1.png", width = 15, height = 20,
       units = "cm", plot = plt1)

# De manera similar a como hemos personalizado algunas de las propiedades
# de nuestros graficos, ya existen varios paquetes que manejan temas
library(ggthemes)
library(help = ggthemes)

plt2 <- p +
  geom_point(aes(shape = sex, col = sex), size = 2.5)+
  theme_gdocs()
plt2

plt3 <- p +
  geom_point(aes(shape = sex, col = sex), size = 2.5)+
  theme_excel_new()
plt3

plt4 <- p +
  geom_point(aes(shape = sex, col = sex), size = 2.5)+
  theme_stata()
plt4

# Guardemos en el disco duro estos ultimos 3 graficos 
ggsave(filename = "GRafico1_tema_gdocs.png", width = 15, height = 20,
       units = "cm", plot = plt2)
ggsave(filename = "GRafico1_tema_excel.png", width = 15, height = 20,
       units = "cm", plot = plt3)
ggsave(filename = "GRafico1_tema_stata.png", width = 15, height = 20,
       units = "cm", plot = plt4)

# Sigamos con la personalizacion
p + 
  geom_point(aes(shape = sex, color = rank ))+
  # Personalicemos los caracteres que usamos en cada 
  # par ordenado 
  scale_shape_manual(values = c(3,5))+
  # Modifiquemos el color de cada par ordenado 
  scale_color_manual(values = c("yellow", "green", "blue"))+
  # Modifiquemos la forma de presentar los valores del eje-Y
  scale_y_continuous(labels = scales::scientific)+
  # Agreguemos etiquetas
  labs(title = "Salarios : Ambito Academico",
       subtitle = "Dataset : Salaries",
       x = "Años de Servicio",
       y = "Salarios [USD]",
       caption = "Clase 4 (PIT2023)")+
  # Deseo agregar un texto al grafico 
  annotate(geom = "text",
    x = 30, y = 225000,label = "Library(car)")
ggsave(filename = "Grafico_con_etiquetas.png",
       width = 20,
       height = 20,
       units = "cm")


# Empaquetemos en un objeto todo el grafico anterior
PlotLabels_p <- p + 
  geom_point(aes(shape = sex, color = rank ))+
  # Personalicemos los caracteres que usamos en cada 
  # par ordenado 
  scale_shape_manual(values = c(3,5))+
  # Modifiquemos el color de cada par ordenado 
  scale_color_manual(values = c("yellow", "green", "blue"))+
  # Modifiquemos la forma de presentar los valores del eje-Y
  scale_y_continuous(labels = scales::scientific)+
  # Agreguemos etiquetas
  labs(title = "Salarios : Ambito Academico",
       subtitle = "Dataset : Salaries",
       x = "Años de Servicio",
       y = "Salarios [USD]",
       caption = "Clase 4 (PIT2023)")+
  # Deseo agregar un texto al grafico 
  annotate(geom = "text",
           x = 30, y = 225000,label = "Library(car)")

# Modifiquemos simple y llanamente la capa base para 
# definir otro objeto 
PlotLabels_q <- q + 
  geom_point(aes(shape = sex, color = rank ))+
  # Personalicemos los caracteres que usamos en cada 
  # par ordenado 
  scale_shape_manual(values = c(3,5))+
  # Modifiquemos el color de cada par ordenado 
  scale_color_manual(values = c("yellow", "green", "blue"))+
  # Modifiquemos la forma de presentar los valores del eje-Y
  scale_y_continuous(labels = scales::scientific)+
  # Agreguemos etiquetas
  labs(title = "Salarios : Ambito Academico",
       subtitle = "Dataset : Salaries",
       x = "Años desde el PHD",
       y = "Salarios [USD]",
       caption = "Clase 4 (PIT2023)")+
  # Deseo agregar un texto al grafico 
  annotate(geom = "text",
           x = 30, y = 225000,label = "Library(car)")


# Agreguemos una capa estadistica con informacion regresional 
PlotLabels_p + 
  # Agregamos una linea de tendencia no parametrica
  geom_smooth()

# Modifiquemos la regresion no parametrica (geom_smooth) y busquemos
# agregar una regresion parametrica
PlotLabels_p + 
  geom_smooth(method = "lm",
              formula = y ~ poly(x,2))


# Si deseo independizar este grafico por la variable sex
PlotLabels_p + 
  geom_smooth(method = "lm",
              formula = y ~ poly(x,1))+
  facet_grid(~sex)

# Independicemos el grafico anterior considerando la
# variable rank
PlotLabels_p +
  geom_smooth(method = "lm",
              formula = y ~ poly(x,1))+
  facet_grid(~rank)
# 
PlotLabels_p +
  geom_smooth(method = "lm",
              formula = y ~ poly(x,1),
              # Modifiquemos el color de la recta de
              # regresion
              color = "black",
              # Eliminemos el intervalo de confianza
              # asociado a la regresion
              se = FALSE
              )+
  facet_grid(~discipline)


# COnsideremos una sola variable :sex
CapaBaseSex <- ggplot(data = Salaries,
                      mapping = aes(x = sex))
# 
# Agreguemos una capa geometrica a la capa base de sexo
CapaBaseSex + geom_bar()

# Consideremos graficar un diagrama de barras para la variable
# rank
Bar_rank <- ggplot(data = Salaries, mapping = aes(x = rank))+
  geom_bar()

# Consideremos un grafico univariado para una variable cuantitativa
# como por ejemplo : yrs.service
Hist_service <- ggplot(data = Salaries, mapping = aes(yrs.service))+
  geom_histogram()

# Guardemos en disco duro estos objetos graficos 
ggsave(filename = "Barras_rank.png",
       width = 10, 
       height = 10,
       units = "cm",
       plot = Bar_rank)

ggsave(filename = "Histograma_Service.png",
       width = 10, 
       height = 10,
       units = "cm",
       plot = Hist_service)

# Instalemos el paquete gridExtra
library(gridExtra)
g1 <- PlotLabels_q
g2 <- Bar_rank + 
  labs(title = "Diagrama de barras : rank")
g3 <- Hist_service + 
  labs(title = "Histograma : yrs.service")
grid.arrange(g1, g2, g3, ncol = 3)


# Grafiquemos el boxplot de la variable yrs.since.phd
ggplot(data = Salaries, mapping = aes(y = yrs.since.phd))+
  geom_boxplot()
# 
ggplot(data = Salaries, mapping = aes(y = yrs.since.phd,
                                      x = sex))+
  geom_boxplot()
# 
ggplot(data = Salaries, mapping = aes(y = salary,
                                      x = sex))+
  geom_boxplot()
# 
ggplot(data = Salaries, mapping = aes(y = salary,
                                      x = discipline))+
  geom_boxplot()


# Practicar con este dataset 
library(nycflights13)
data("flights")














