# -*- coding: utf-8 -*-
"""
Created on Fri Jan 29 11:29:00 2021

@author: Carlos Heredia Pimienta

Explanation: How to solve and ODE with Python
    
"""

# ODE to solve: y'(t) + a y(t) = b where  y(t==0) == c

import numpy as np
import matplotlib.pyplot as plt
from scipy.integrate import odeint

b = 10
a = 5/100

# y'(t) definition
def dydt(y,x): return b - a*y

# Solution definition
def sol(t,y0): return odeint(dydt, y0, t)

# Numerical separation
t = np.linspace(0,100)

c = 20
y = sol(t,c)

plt.plot(t,y)
plt.show()
