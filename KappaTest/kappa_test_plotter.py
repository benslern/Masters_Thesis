#KAPPA TEST PLOTTER

import sys
import numpy as np
import matplotlib.pyplot as plt

filename = sys.argv[1]

epsilon = []
val1 = []
val2 = []
val3 = []
val = []
val32 = []
logval32 = []

with open(filename, 'r') as file:
    for line in file:
        data = line.strip().split()
        epsilon.append(float(data[0]))
        val1.append(float(data[1]))
        val.append(float(data[2]))
        val3.append(float(data[3]))
        val2.append(float(data[4]))
        val32.append(float(data[5])-1.0)
        logval32.append(np.log(abs(float(data[5])-1.0)))


plt.xscale('log')
plt.xlabel('epsilon')
plt.ylabel('kappa')
plt.plot(epsilon,val32)
plt.savefig(filename[:-4]+'_kappa.jpg')
#plt.show()
