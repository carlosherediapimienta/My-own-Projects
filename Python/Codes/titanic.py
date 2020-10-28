# -*- coding: utf-8 -*-
"""
Created on Wed Oct 28 16:13:13 2020

@author: Carlos Heredia Pimienta

Explanation: Machine Learning prediction with Titanic's dataset. This code
is based on https://www.kaggle.com/startupsci/titanic-data-science-solutions

"""

import pandas as pd

# Seleccionamos el set de datos
df_train = pd.read_csv("train.csv")
df_test = pd.read_csv("test.csv")

# Juntamos los datasets
df = [df_train, df_test]

# Información de los nulls:
for dset in df:
    dset.info()
    print('_'*40)
    print(dset.describe())

# Quitamos columnas inecesarias
for dset in df:
    dset = dset.drop(['Ticket','Cabin', 'PassengerId'],axis =1, inplace= True)
        
# Visualización del dato (histograma)
import seaborn as sns
import matplotlib.pyplot as plt

# Probabilida de sobrevivir según el sexo (sólo para el training)
df_train_g = df_train[['Sex','Survived']].groupby(['Sex'], as_index = False)
df_train_s = df_train_g.mean().sort_values(by = 'Survived', ascending = False)

print(df_train_s)

# Probabilidad de sobrevivir según el sexo y clase
grid = sns.FacetGrid(df_train, col= 'Pclass', row='Survived', size=2.2, aspect=1.4)
grid.map(plt.hist, 'Age', alpha=.5, bins=20, color= '#A2892B')
grid.add_legend()
plt.show()


from sklearn.preprocessing import LabelEncoder
from sklearn.impute import SimpleImputer
import numpy as np

le = LabelEncoder()
si = SimpleImputer(missing_values=np.nan, strategy= 'mean')

wrong_values = ['Mlle', 'Ms', 'Mme']
right_values = ['Miss','Miss','Mrs']

# Extraemos información adicional
for dset in df:
    
    dset['Title'] = dset.Name.str.extract(' ([A-Za-z]+)\.', expand = False)

    dset['Title'].replace(wrong_values,right_values, inplace=True)
    dset.loc[~dset['Title'].isin(right_values),'Title'] = 'Rare'

# Ordinal variable
    dset['Title'] = le.fit_transform(dset['Title'])
    dset['Sex'] = le.fit_transform(dset['Sex'])
            
# Rellenamos los valores Nulls - Embarked.
    freq_embar = dset.Embarked.dropna().mode()[0]
    dset['Embarked'] = dset['Embarked'].fillna(freq_embar)
    dset['Embarked'] = le.fit_transform(dset['Embarked'])

# Rellenamos los valores Nulls - Numeric Columns

    df_num = dset.select_dtypes(include=np.number)
    df_num = pd.DataFrame(si.fit_transform(df_num), columns=df_num.columns)
    dset['Age'] = df_num['Age'].astype(int)
    dset['Fare'] = df_num['Fare'].astype(int)
    
# Agrupamos por rango de edades
    dset.loc[dset['Age'] <= 16, 'Age'] = 0 
    dset.loc[(dset['Age'] > 16) & (dset['Age'] <= 32), 'Age'] = 1
    dset.loc[(dset['Age'] > 32) & (dset['Age'] <= 48), 'Age'] = 2
    dset.loc[(dset['Age'] > 48) & (dset['Age'] <= 64), 'Age'] = 3
    dset.loc[dset['Age'] > 64, 'Age'] = 4
    
# Agrupamos por rango de Fare
    dset.loc[ dset['Fare'] <= 8, 'Fare'] = 0
    dset.loc[(dset['Fare'] > 8) & (dset['Fare'] <= 14), 'Fare'] = 1
    dset.loc[(dset['Fare'] > 14) & (dset['Fare'] <= 31), 'Fare']   = 2
    dset.loc[ dset['Fare'] > 31, 'Fare'] = 3
    dset['Fare'] = dset['Fare'].astype(int)

# Creamos columnas nuevas

# 1. Viajaba solo?
    dset['FamilySize'] = dset['SibSp'] + dset['Parch'] + 1
    dset['IsAlone'] = 0
    dset.loc[dset['FamilySize'] == 1, 'IsAlone'] = 1
    
# 2. Clase por edad
    dset['Age*Class'] = dset['Age']*dset['Pclass']   
    
#   Eliminamos las variables inecesarias
    dset = dset.drop(['Name','FamilySize','SibSp','Parch'], axis=1, inplace = True)


# Correlación
corr = df_train.corr()
sns.heatmap(corr, annot = True)
plt.show()

# machine learning
from sklearn.linear_model import LogisticRegression
from sklearn.svm import SVC, LinearSVC
from sklearn.ensemble import RandomForestClassifier
from sklearn.neighbors import KNeighborsClassifier
from sklearn.naive_bayes import GaussianNB
from sklearn.linear_model import Perceptron
from sklearn.linear_model import SGDClassifier
from sklearn.tree import DecisionTreeClassifier


x_train = df_train.drop('Survived', axis=1)
y_train = df_train['Survived']
x_test = df_test.copy()

# Información: Los modelos implementados están configurados por defecto

# Logistic Regression

lr = LogisticRegression()
lr.fit(x_train, y_train)
y_pred = lr.predict(x_test)
lr_acc = round(lr.score(x_train,y_train)*100,2)

# Support Vector Machine

svm = SVC()
svm.fit(x_train, y_train)
y_pred = svm.predict(x_test)
svm_acc= round(svm.score(x_train,y_train)*100,2)

# KNN

knn = KNeighborsClassifier(n_neighbors = 3)
knn.fit(x_train, y_train)
y_pred = knn.predict(x_test)
knn_acc= round(knn.score(x_train,y_train)*100,2)

# Gaussian Naive Bayes

g = GaussianNB()
g.fit(x_train, y_train)
y_pred = g.predict(x_test)
g_acc = round(g.score(x_train, y_train) * 100, 2)

# Perceptron

NN = Perceptron()
NN.fit(x_train, y_train)
y_pred = NN.predict(x_test)
NN_acc = round(NN.score(x_train, y_train) * 100, 2)

# Linear SVC

l_svc = LinearSVC()
l_svc.fit(x_train, y_train)
y_pred = l_svc.predict(x_test)
l_svc_acc = round(l_svc.score(x_train, y_train) * 100, 2)

# Stochastic Gradient Descent

sgd = SGDClassifier()
sgd.fit(x_train, y_train)
y_pred = sgd.predict(x_test)
sgd_acc = round(sgd.score(x_train, y_train) * 100, 2)

# Decision Tree

dtree = DecisionTreeClassifier()
dtree.fit(x_train, y_train)
y_pred = dtree.predict(x_test)
dtree_acc = round(dtree.score(x_train, y_train) * 100, 2)

# Random Forest

rf = RandomForestClassifier(n_estimators=100)
rf.fit(x_train, y_train)
y_pred = rf.predict(x_test)
rf.score(x_train, y_train)
rf_acc = round(rf.score(x_train, y_train) * 100, 2)

# Lo guardamos en un dataframe

models = pd.DataFrame({ 'Model': ['Support Vector Machines', 'KNN', 'Logistic Regression',
              'Random Forest', 'Naive Bayes', 'Perceptron', 'Stochastic Gradient Decent', 'Linear SVC', 'Decision Tree'],
            'Score': [svm_acc, knn_acc, lr_acc, rf_acc, g_acc, NN_acc, sgd_acc, l_svc_acc, dtree_acc]})

print(models.sort_values(by='Score', ascending=False))
