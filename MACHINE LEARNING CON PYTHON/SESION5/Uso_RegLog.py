# %% Modulos y datos 

import os 
os.chdir("/home/abraham/Desktop/PythonML_Pit2023")

import pickle

# Carguemos el modelo en memoria 
Modelo_AbrahamZamudio = "ModRegLogCancer_Clase5_PIT2023.pkl"
with open(Modelo_AbrahamZamudio, "rb") as ModFileCLase5:
    ModProduccion = pickle.load(ModFileCLase5)

# Carguemos datos 
import numpy as np
import pandas as pd 
cancer = pd.read_csv("https://raw.githubusercontent.com/robintux/Datasets4StackOverFlowQuestions/master/breast-cancer-wisconsin.csv")

# Separar variables independientes y dependiente 
y = cancer.diagnosis
X = cancer.drop(["Unnamed: 32", "id", "diagnosis"], axis = 1)


# %% 
# COnstruyamos nuevos datos (una muestra)
# Construyamos una muestra de indices 
Muestra1Idx = np.random.randint(0, cancer.shape[0], round(0.5*cancer.shape[0]))

# Construimos una muestra de las variables independientes (X) y una muestra
# de la variable dependiente 
Muestra1VarDep = y.iloc[Muestra1Idx]
Muestra1VarIndep = X.iloc[Muestra1Idx, : ]

# Calculamos pronosticos haciendo uso de Muestra1VarIndep 
YForecast1 = ModProduccion.predict(Muestra1VarIndep)


# Indicador de calidad 
from sklearn.metrics import confusion_matrix
confusion_matrix(Muestra1VarDep, YForecast1)


# PAra un 5% de las observaciones 
# array([[21,  0],
#        [ 0,  7]])

# Para un 10% de las observaciones
# array([[38,  2],
#        [ 1, 16]])

# Para un 20% de las observaciones 
# array([[75,  0],
#        [ 4, 35]])

# Para un 50% de las observaciones 
# array([[175,   3],
#        [  8,  98]])

