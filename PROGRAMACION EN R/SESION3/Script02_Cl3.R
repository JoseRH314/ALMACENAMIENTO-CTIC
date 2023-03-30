# Instalacion de tidyverse
library(tidyverse)
#
# dplyr : Gramatica para manipulacion de datos (basada en verbos)
# ggplot2 : Crea graficos utilizando una gramatica de capas
# tidyr : Permite transformar la organizacion de un objeto de clase
          # dataframe a un tibble
# purr : Permite eliminar los bucles haciendo uso de funciones que 
          # permiten concatenar las operaciones repetitivas
# tibble : Sirve para almacenar base de datos que complementa a la clase dataframe
# stringr : Brinda funciones para manipulat cadenas de caracteres
# forcats : Rutinas para manipular datos de tipo factor

# Lista de componentes de un paquete
library(help = "ggplot2")
data(package = "ggplot2")

#### Operador Pipe ####
#
# Concatenamiento de Operaciones
x1 <- mean(rnorm(120,mean = 12.5, sd = 9.81))
x2 <- tanh(x1)
round(x2, digits = 4)

#Otra forma 
round(tanh(mean(rnorm(120,mean = 12.5, sd = 9.81))), digits = 4)

# Usamos el operador pipe (%>%)

# DEfinimos un vector
rnorm(120,mean=12.5,sd=9.81)%>%
  # Calculamos la media de este vecotr
  mean() %>%
  # Calculamos la tangente hiperbolica
  tanh() %>%
  # redondeamos
  round(digits = 4)

#### Objeto de tipo tibble ####
# Instalemos el paquete gapminder
#
# Carguemos el paquete gapminder
library(gapminder)

# Carguemos el data gapminder
data("gapminder")
View(gapminder)

# Ccual es la clase del objeto gapminder
class(gapminder)

# Transformemos el objeto de tipo tbl_df a un dataframe
gapminderDF <- as.data.frame(gapminder)

#### Gapminder ####

# nombres de las columnas
colnames(gapminder)
help("gapminder")

# filter : filtramos los datos concernientes al continente
# americano en el 1997
Americas1997 <- gaminder %>%
  # filtrar la columnda continente : "Americas"
  filter(continent == "Americas") %>%
  # Filtremos los datos que corresponden al año 1997
  filter(year == 1997)
View(Americas1997)

# Filtremos la informacion que corresponde a los continentes de 
# Europa y Asia
EuropeAsia <- gapminder %>%
  filter(continent %in% c("Europe","Asia")) %>%
  filter(year == 1997)
View(EuropeAsia)

# El verbo arrange : ordenar
# Deseo obtener los datos del continente oceania para los
# años posteriores al 2000. Ademas deseo que se muestre la 
# informacion en orden alfabetico por la variable country
gapminder %>%
  filter(continent == "Oceania") %>%
  filter(year > 2000) %>%
  arrange(country)

# El ordenamiento puede realizarse mediante una variable cuantitativa
# Organicemos los años en forma descendente
gapminder %>%
  filter(continent == "Oceania") %>%
  filter(year > 2000) %>%
  arrange(desc(year))

# El verbo rename
gapminder <- rename(gapminder, pais = country)
colnames(gapminder)
#
# De manera nalogo podemos cambiar los nombres de las otras
# columnas
gapminder <- rename(gapminder, continente = continent,
                    año = year,
                    esp_vida = lifeExp,
                    poblacion = pop,
                    pbi_per_cap = gdpPercap)

# Observemos los nombres de las columnas
colnames(gapminder)

# El verbo relocate : modifica la ubicacion de las columnas
#
# Pbi_per_cap debe ser la primera columna
gapminder <- relocate(gapminder, pbi_per_cap)
# Deseo que año esta despues de pbi_per_cap
gapminder <- relocate(gapminder, pbi_per_cap, .before = año)


# el verbo select : seleccionar columnas
# defines df1 seleccionando solo las columnas pais, año y poblacion
df1 <- select(.data = gapminder, pais, año, poblacion)
df2 <- gapminder %>%
  select(pais,año,poblacion)

# Si el objeto df2 desseo seleccionar la informacion que esta despues del
# año 2000

df3 <- df2  %>%
  filter(año > 2000)

#### Instalamos el paquete nycflights13 ####
library(nycflights13)
#
VuelosNY <- nycflights13::flights
View(VuelosNY)

# Nombres de la columnas
colnames(VuelosNY)

# Veamos la informaacion de la columna year
 unique(VuelosNY$year)
# Documentacion
 help("flights")
 
 # Filtremos los datos de marzo (month == 3)
 VuelosNY %>%
   filter(month == 3)
 # Para el mes de junio veamos cual es el promedio de la columnas
 # dep_delay
 VuelosNY %>%
   filter(month == 6) %>%
   select(dep_delay) %>%
   mean(na.rm = TRUE)
 # Error corregir xd




