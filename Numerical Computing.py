#!/usr/bin/env python
# coding: utf-8

# In[58]:


# Project 2
# Yi Chiew

import numpy as np
import matplotlib.pyplot as plt
import h5py
from scipy import optimize

# Part 1
f= h5py.File('expdata.h5','r')
x=f['X'][:]
y=f['Y'][:]
f.close()


# In[59]:


plt.plot(x,y, 'o')


# In[60]:


def test_func(x, a, b):
    return a * np.exp(b * x)
#def test_func(x, a, b):
#    return a * np.sin(b * x)


# In[61]:


params, parms_cov = optimize.curve_fit(test_func, x, y, p0=[1, 1])


# In[66]:


plt.figure()
t = np.linspace(-5,40)
plt.plot(t, test_func(t, a, b), 'r-', label='curve fit')
plt.plot(x,y, 'o', label='data')
plt.legend()


# In[63]:


a, b = params
a, b

