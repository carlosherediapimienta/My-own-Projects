# -*- coding: utf-8 -*-
"""
Created on Wed Oct 28 13:02:22 2020

@author: Carlos Heredia Pimienta

Explanation: Comparison IF vs .loc

"""
import pandas as pd

# Seleccionamos el set de datos
df_train_if = pd.read_csv("train.csv")
df_train_loc = pd.read_csv("train.csv")

df_train_if['Title'] = df_train_if.Name.str.extract(' ([A-Za-z]+)\.', expand = False)
df_train_loc['Title'] = df_train_loc.Name.str.extract(' ([A-Za-z]+)\.', expand = False)

# Con el IF
for i in range(len(df_train_if.Title)):
    if df_train_if.Title[i] == 'Mlle':
            df_train_if.Title[i] = 'Miss'
    elif df_train_if.Title[i] =='Ms':
            df_train_if.Title[i] = 'Miss'
    elif df_train_if.Title[i] == 'Mme':
            df_train_if.Title[i] = 'Mrs'
    elif (df_train_if.Title[i] != 'Mrs') and (df_train_if.Title[i] != 'Miss') and \
            (df_train_if.Title[i] != 'Mr') and (df_train_if.Title[i] != 'Master'):
            df_train_if.Title[i] = 'Rare'
        
# Con el .loc
    df_train_loc.loc[df_train_loc['Title'] == 'Mlle', 'Title'] = 'Miss'
    df_train_loc.loc[df_train_loc['Title'] == 'Ms', 'Title'] = 'Miss'
    df_train_loc.loc[df_train_loc['Title'] == 'Mme', 'Title'] = 'Mrs'
    df_train_loc.loc[(df_train_loc['Title'] != 'Mrs') & (df_train_loc['Title'] != 'Miss') &\
          (df_train_loc['Title'] != 'Mr') & (df_train_loc['Title'] != 'Masters'), 'Title'] = 'Rare'
    
# Marc's proposal - Como mejorar el loc
# df['Title'].replace(wrong_values, right_values, inplace = True)
# df.loc[~df['Title'].isin(right_values), 'Title'] = 'Rare'
