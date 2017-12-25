#!/usr/bin/env python

import numpy as np
import matplotlib.pyplot as plt

projectionData = np.genfromtxt("sine_ic.dat")
ncells = int(len(projectionData[:, 0]) / 2)

x = np.linspace(np.amin(projectionData[:, 0]), np.amax(projectionData[:, 0]), 200)
y = np.sin(x)

fig1 = plt.figure()
plt.plot(x, y)

for n in range(0, ncells):
    idx0 = 2 * n
    idx1 = 2 * n + 1
    plt.plot([projectionData[idx0,0], projectionData[idx1,0]], [projectionData[idx0,1], projectionData[idx1,1]], "-ok")

plt.show()
