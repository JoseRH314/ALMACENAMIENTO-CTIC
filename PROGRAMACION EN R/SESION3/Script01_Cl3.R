#### Configuraciones iniciales ####
rm(list = ls())

#Definicion del diectorio dee trabajo
setwd("C:/Users/EQUIPO/Desktop/CTIC/PROGRAMACION EN R/SESION3")

#### Estructuras de decision ####
# Estructura if #
# Evalua una expresion (retornara un valor booleano)
# Esta expresion puede tener dos posibles valores:
  # TRUE
  # FALSE
#
# if (condicion){
# sentencia1
# sentencia2
# sentencia3
#}

# Por ejemplo, generemos una variable con distribucion uniforme
# y transformemosla a un numero entero
# Si este entero es mayor o igual a 17 entonces la variable categoria
# tendra el valor de "A"

a <- as.integer(runif(1,14,20))
# Sobre el valor generado aleatoriamente vamos a evaluar una condicion
if(a >= 17){
  categoria <- "A"
}
print(a)
print(categoria)
help(runif)
# Estructura algo mas general del if : if-else
if(4>5){
  "False"
}else{
  "verdadero"
}

# Ejemplito :
# Generemos un total de 50 notas aleatorias con distribucion uniforme en el
# intervalo de [0,20]. Si el promedio de estas notas es mayor que 14}
# entonces que las 10 menores notas se le sume 0.5. Si el promedio des tas notas
# no es mayor a 14 que a todos se les sume 0.7

# Creamos el vector de notas
notas <- as.integer(runif(50,0,20))
Promedio <- mean(notas)
if (Promedio > 14){
  MenoresNotas = sort(notas)[1:10] + 0.5
  MenoresNotas
}else{
  notas <- notas + 0.7
  notas
}

# Estructura ifelse
#
# Ejemplito : Generemos 80 notas, binaricemos estas notas con el siguiente
# Si nota >14 : Asignamos el valor de "A"
# Si la nota <= 14 : asignaremos al valor de
#
Notas2 <- as.integer(runif(100,0,40))
Notas2
DataParidad <- ifelse( Notas2 %% 2 == 0, "A", "B")

#### Estructura de repeticion for ####
# La estructura for : nos permite ejecutar un bucle, de manera que se realiza
# una operacion por cada elemento
# for(elemento in vector){
#   operacion_sobre_elemento
# }

# Simulemos 10 lanzamientos de un dado
for (lanzamiento in 1:10){
  print(as.integer(runif(1,1,6)))
}

# Deseo guardar los resultados de la simulacion de mi codigo anterior
# es decir, cada vez que simulo un lanzamiento, lo guardo en un vector
Apuestas <- c()
for (lanzamiento in 1:10){
  l <- as.integer(runif(1,1,6))
  print(l)
  Apuestas <- c(Apuestas, l)
}
Apuestas

# Genere una matriz de 200 apuestas (cada apuesta en un vector de 10 
# componenetes)
# 
# generemos la matriz donde vamos a almacenar los resultados
Matriz_Apuestas <- matrix(ncol = 10)

# Luego usaremos una estructura  repetitiva para cada fila es
# una apuestas )
for (fila in 1:200){
  # pra cada una de esas filas : iterador fila
  # generar un vector de apuestas
  for (lanzamiento in 1:200){
    apuestas <- c()
    l <- as.integer(runif(1,1,6))
    apuestas <- c(apuestas, l)
    #
    # Generemos un vector fila con 10 columnas
    apuesta <- matrix(apuestas, ncol = 10)
  }
  # Una vez generado el veector de apuestas
  # Vamos llenando la fila a fila la matriz de apuestas
  Matriz_Apuestas <- rbind(Matriz_Apuestas, apuesta)
    
}

#### Estructuras de repeticion while ####

inicio <- 0
final <- 5
while(inicio<final){
  print("aun no sellega a final")
  inicio <- inicio + 1
}

# generemos numeros aleatorios en [0,20] hasta que salga un 20
nota <- 0
numveces <- 0
while(nota!=20){
  numveces=numveces +1
  nota <- as.integer(runif(1,0,21))
  print(nota)
}

# Generemos numeros aleatorios en [0,21] hasata ue salga un 20
# Almacenando los vaalores generados

nota <- 0
vectornotas <- c()
while(nota != 20){
  nota <- as.integer(runif(1,0,21))
  vectornotas <- c(vectornotas, nota)
  print(nota)
}
vectornotas

#### Break y Next ####
# break : interrumpe un bucle
for (i in 1:10) {
  if(i == 7){
    break
  }
  print(i)
}

#### Next ####
for(t in 111:666){
  if(t == 222 | t == 333 | t == 444 | t == 555){
    next
  }
  print(t)
}

#### Definimos Funciones ####
# NombreFuncion <- function(arg1,arg2,arg3){
#   sentencia1
#   sentencia2
#   sentencia3
#   return() #opcional
# }

# Implementemos una funcion que genere jugadas de la tinka
CreadorJugadasT <- function(NumJugadas){
  NumJugadas <- NumJugadas + 1
  # Generemos NumJugadas con numeros de 1 al 40
  MatrizT <- matrix(ncol = 6)
  for (jugada in 1:NumJugadas) {
    # Construir una apuesta
    apuestaT <- as.integer(runif(6,1,40))
    MatrizT <- rbind(MatrizT, apuestaT)
  
  }
  # Limpiamos la primera fila de NA
  MatrizT <- MatrizT[2:NumJugadas, 1:6]
  
  # Renombrar las dimensiones de la matriz
  #
  # Construyaños un vector para el nombre de las filas
  VectorNombrefilas <- c()
  for (fila in 1:nrow(MatrizT)) {
    VectorNombrefilas <- c(VectorNombrefilas, paste("Apuesta", fila, sep = "-"))
    
  }
  # Asignamos VectorNombrefilas al rownames de MatrizT
  rownames(MatrizT) <- VectorNombrefilas
  
  
  # De manera naloga construyo un vector con los nombres de las columnas
  VectorNombreColumnas <- c()
  for (b in 1:ncol(MatrizT)) {
    VectorNombreColumnas <- c(VectorNombreColumnas, paste("Bola", b, sep="-"))
  }
  
  # Asignamos el VectorNombreColumnas al colnames de MatrizT
  colnames(MatrizT) <- VectorNombreColumnas
  return (MatrizT)
}

#ejemplos de uso
CreadorJugadasT(10)





