# -*- coding: utf-8 -*-
"""
Created on Tue Oct 27 12:59:05 2020

@author: Carlos Heredia Pimienta

Explanation: To change from categorical to ordinal variable

"""
import pandas as pd

df_train = pd.read_csv("train.csv")

df_train['Title'] = df_train.Name.str.extract(' ([A-Za-z]+)\.', expand = False)

# Para mejorar esta parte, se debería cambiar el bucle FOR por el .loc, tal y como está
# en el documento IF vs LOC.
for i in range(len(df_train.Title)):
    if df_train.Title[i] == 'Mlle':
            df_train.Title[i] = 'Miss'
    elif df_train.Title[i] =='Ms':
            df_train.Title[i] = 'Miss'
    elif df_train.Title[i] == 'Mme':
            df_train.Title[i] = 'Mrs'
    elif (df_train.Title[i] != 'Mrs') and (df_train.Title[i] != 'Miss') and \
             (df_train.Title[i] != 'Mr') and (df_train.Title[i] != 'Master'):
            df_train.Title[i] = 'Rare'

from sklearn.preprocessing import LabelEncoder

le = LabelEncoder()

# Ordinal variable
df_train['Title'] = le.fit_transform(df_train['Title'])

# Invertir Ordinal variable
df_train['Title'] = le.inverse_transform(df_train['Title'])
