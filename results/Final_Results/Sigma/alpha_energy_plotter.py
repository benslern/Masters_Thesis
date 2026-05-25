#Conservation of alpha-energy plotter

import matplotlib.pyplot as plt
import os
from mpl_toolkits.axes_grid1.inset_locator import mark_inset

colors = ['darkorange','darkturquoise','sienna','royalblue']
shapes = ['P','*','v','s']

fig, ax = plt.subplots(figsize=[8, 4])

ax.set_xlim(0,5)

ax.set_xlabel(r'$t$',fontsize="14")
ax.set_ylabel(r'$\alpha$-energy',fontsize="14")

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
for resol in [256]:
    for sigma in [5]:
        dirname = './R'+str(resol)+'/alpha_16_1024/sigma_1E-'+str(sigma)+"/"
        print(dirname)
        energy_files = [f for f in os.listdir(dirname) if (f[-4:]==".dat" and f[:11]=="energy_fwd_")]

        count = 1
        for e in energy_files:
            it = e[e.rfind("_")+1:e.index('.')]
            
            ax.set_title(r'Conservation of $\alpha$-energy: $\sigma = $1E-'+str(sigma)+",iter: "+it)
            TS = []
            KE = []
            AAE = []
            TOT = []
            with open(dirname+e, 'r') as file:
                for line in file:
                    line = " ".join(line.split())
                    vals = line.split()
                    TS.append(float(vals[0]))
                    KE.append(2*float(vals[1]))
                    AAE.append((alpha**2)*float(vals[5]))
                    TOT.append(2*float(vals[1]) + (alpha**2)*float(vals[5]))
            plt.figure(count)
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
            ax.set_xlim(0,5)
            ax.set_ylim(0,0.007)
            ax.set_xlabel(r'$t$',fontsize="14")
            ax.set_ylabel(r'$\alpha$-energy',fontsize="14")
            ax.plot(TS,KE,'o-',ms=4)
            ax.plot(TS,AAE,'o-',ms=4)
            ax.plot(TS,TOT,'o-',ms=4)
            count = count + 1
            
            plt.savefig(dirname+e[:-4]+'.png')
            fig, ax = plt.subplots(figsize=[8, 4])
        plt.ioff()
        plt.ion()
                    
