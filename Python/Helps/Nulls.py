# -*- coding: utf-8 -*-
"""
Created on Tue Oct 27 17:29:30 2020

@author: Carlos Heredia Pimienta

Explanation: How to know if my dataset has null values and how to replace them
by 0
    
"""

import pandas as pd

# Seleccionamos el set de datos
df_train = pd.read_csv("train.csv")

# Mi set de datos tiene nulls?
TF = df_train.isnull().values.any()

if TF == True:
    
# Cuantos hay?
    print (df_train.isnull().sum())

# Los quitamos
    for column in df_train.columns:
        
        TF = df_train[column].isnull().values.any() 
        
        if TF == True:
            
            df_train[column] = df_train[column].fillna(0)
        
    
    