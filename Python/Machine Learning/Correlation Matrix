# -*- coding: utf-8 -*-
"""
Created on Tue Oct 27 09:17:38 2020

@author: Carlos Heredia Pimienta

Explanation: Correlation Matrix

"""

import pandas as pd
import seaborn as sn
import matplotlib.pyplot as plt

df_train = pd.read_csv("train.csv")

# Encontramos las correlaciones; recuerda que ya se escogen
# las variables numéricas
corr = df_train.corr()

# Mostramos la correlación
sn.heatmap(corr, annot = True)
plt.show()



