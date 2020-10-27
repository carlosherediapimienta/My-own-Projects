# -*- coding: utf-8 -*-
"""
Created on Tue Oct 27 12:59:05 2020

@author: Carlos Heredia Pimienta

Explanation: To change from categorical variable to 0/1 columns.

"""
import pandas as pd

df_train = pd.read_csv("train.csv")

df_train['Title'] = df_train.Name.str.extract(' ([A-Za-z]+)\.', expand = False)
    
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

# Para el ejemplo, sólo nos quedamos con la columna Title
Title = pd.DataFrame(df_train['Title'])            

from sklearn.preprocessing import LabelBinarizer

lb = LabelBinarizer()

# Columns
result = lb.fit_transform(Title)
result = pd.DataFrame(result, columns = lb.classes_)
