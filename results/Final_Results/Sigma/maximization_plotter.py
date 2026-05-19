#Maximization of Phi plotter

import matplotlib.pyplot as plt
import os
import numpy as np
from mpl_toolkits.axes_grid1.inset_locator import mark_inset

colors = ['darkorange','darkturquoise','sienna','royalblue']
shapes = ['P','*','v','s']

fig, ax = plt.subplots(figsize=[8, 4])

#ax.set_xlim(0,5)

ax.set_xlabel(r'$n$',fontsize="14")
ax.set_ylabel(r'$\Phi$',fontsize="14")

ax.tick_params(
    axis='both',          # applies to both x and y axes
    which='both',         # applies to both major and minor ticks
    top=True,             # show ticks on top
    right=True,           # show ticks on right
    bottom=True,          # show ticks on bottom
    left=True,            # show ticks on left
    labelbottom=True,     # show labels on bottom
    labelleft=True,       # show labels on left
    labeltop=False,       # hide labels on top
    labelright=False,     # hide labels on right
    labelsize=12,
    direction='in'       # direction of the ticks ('in', 'out', or 'inout')
)

alpha = 16/1024
plt.ion()
data = []
for resol in [256]:
    count = 1
    for sigma in [1,2,3,4,5]:
        dirname = './R'+str(resol)+'/alpha_16_1024/sigma_1E-'+str(sigma)+"/"
        print(dirname)
        #energy_files = [f for f in os.listdir(dirname) if (f[-4:]==".dat" and f[:11]=="energy_fwd_")]
        m = dirname + "maximization_cost.dat"

        
        NS = []
        PHIS = []
        
        PHI_0 = 1
        counter = 0
        with open(m, 'r') as file:
            for line in file:
                line = " ".join(line.split())
                vals = line.split()
                if(counter == -1):
                    PHI_0 = -1*float(vals[1])
                else:
                    NS.append(float(vals[0]))
                    PHIS.append(-1*float(vals[1]))
                counter += 1
        plt.figure(count)
        data.append(PHIS)
        fig, ax = plt.subplots(figsize=[8, 4])
        
        ax.tick_params(
            axis='both',          # applies to both x and y axes
            which='both',         # applies to both major and minor ticks
            top=True,             # show ticks on top
            right=True,           # show ticks on right
            bottom=True,          # show ticks on bottom
            left=True,            # show ticks on left
            labelbottom=True,     # show labels on bottom
            labelleft=True,       # show labels on left
            labeltop=False,       # hide labels on top
            labelright=False,     # hide labels on right
            labelsize=12,
            direction='in'       # direction of the ticks ('in', 'out', or 'inout')
        )
        #ax.set_xlim(0,5)
        #ax.set_ylim(0,0.007)
        ax.set_title(r'Maximization of $\Phi_{\alpha,T}: \sigma = $1E-'+str(sigma))
        ax.set_xlabel(r'$n$',fontsize="14")
        ax.set_ylabel(r'$\Phi_{\alpha,T}$',fontsize="14")
        ax.plot(NS,PHIS,'o-',ms=4)
        count = count + 1
        
        plt.savefig(m[:-4]+'.png')
plt.figure(count+1)
plt.rc('text', usetex=True)
fig, ax = plt.subplots(figsize=[8, 4])
ax.tick_params(
    axis='both',          # applies to both x and y axes
    which='both',         # applies to both major and minor ticks
    top=True,             # show ticks on top
    right=True,           # show ticks on right
    bottom=True,          # show ticks on bottom
    left=True,            # show ticks on left
    labelbottom=True,     # show labels on bottom
    labelleft=True,       # show labels on left
    labeltop=False,       # hide labels on top
    labelright=False,     # hide labels on right
    labelsize=12,
    direction='in'       # direction of the ticks ('in', 'out', or 'inout')
)
ax.set_title(r'Maximization of $\Phi_{\alpha,T}(\boldmath{\eta}_{\alpha,T}^{(n)})$')
ax.set_xlabel(r'$n$',fontsize="14")
ax.set_ylabel(r'$\Phi_{\alpha,T}(\mathbf{\eta}_{\alpha,T}^{(n)})$',fontsize="14")
count = 1
for x in data:
    ax.plot(np.arange(len(x)),x,'-',ms=2,label=r'$\sigma$: 1E-'+str(count))
    count += 1
plt.legend(loc='lower right')
plt.show()
