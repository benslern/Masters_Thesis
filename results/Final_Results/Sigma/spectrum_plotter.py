#Spectrum of Euler-Voigt plotter

import matplotlib.pyplot as plt
import os
from mpl_toolkits.axes_grid1.inset_locator import mark_inset

colors = ['darkorange','darkturquoise','sienna','royalblue','forestgreen','darkviolet']
shapes = ['P','*','v','s']

fig, ax = plt.subplots(figsize=[8, 4])


ax.set_xlabel(r'$k$',fontsize="14")
ax.set_ylabel(r'$|e(k)|$',fontsize="14")

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
        spectrum_files = [f for f in os.listdir(dirname) if (f[-4:]==".dat" and f[:13]=="spectrum_fwd_")]
        print(spectrum_files)
        count = 1
        for e in spectrum_files:
            it = e[e.rfind("_")+1:e.index('.')]
            
            ax.set_title(r'Spectrum of $u_\alpha$: $\sigma = $1E-'+str(sigma)+",iter: "+it)
            KS = []
            EK = []
            k_count = 0
            k_count_2 = 0
            c = 0
            add = True
            with open(dirname+e, 'r') as file:
                for line in file:
                    line = " ".join(line.split())
                    vals = line.split()

                    if k_count < 221:
                        
                        KS.append(float(vals[0]))
                        EK.append(float(vals[1]))
                        k_count = k_count + 1
                    else:
                        if add:
                            ax.plot(KS,EK,'o-',ms=4,color=colors[c])
                            add = not add
                        
                        KS = []
                        EK = []
                        if k_count_2 < 7*222:
                            k_count_2 = k_count_2 + 1
                        else:
                            add = True
                            c += 1
                            k_count = 0
                            k_count_2 = 0

                        
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
            #ax.set_xlim(0,5)
            #print(KS)
            #print(EK)
            ax.set_ylim(1E-40,1)
            ax.set_yscale('log')
            ax.set_xlabel(r'$k$',fontsize="14")
            ax.set_ylabel(r'$|e(k)|$',fontsize="14")
            
            count = count + 1
            
            plt.savefig(dirname+e[:-4]+'.png')
            fig, ax = plt.subplots(figsize=[8, 4])
            print("plot")
        #plt.show()
