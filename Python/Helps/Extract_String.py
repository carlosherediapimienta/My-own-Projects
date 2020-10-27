# -*- coding: utf-8 -*-
"""
Created on Mon Oct 26 17:36:30 2020

@author: Carlos Heredia Pimienta

Explanation: How to move rows from one dataframe to another with pandas and
and substract the title's name.
    
"""
import pandas as pd

df = pd.read_csv("train.csv")

# Primera opción:
df_copy = pd.DataFrame(df.Name[0:100])

# Segunda opción:
# df_copy = df.copy()
# df_copy = pd.DataFrame(df_copy.Name[0:100])

# Extraemos grupos empezando por mayúsculas
# y continuando por minúsculas '[A-Za-z]+'

df_copy['Title'] = df_copy.Name.str.extract(' ([A-Za-z]+)\.', expand = False)
