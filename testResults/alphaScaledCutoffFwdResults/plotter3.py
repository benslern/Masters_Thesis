import matplotlib.pyplot as plt
import numpy as np

fig, ax = plt.subplots(figsize=[8, 4])

#ax.set_ylim(3.5,27)
ax.set_yscale('log')
ax.set_ylabel(r'$\|\omega\|_{L^\infty}$',fontsize="14")
#ax.set_ylabel(r'$\|u_\alpha\|_{\dot{H}^1}$',fontsize="14")

ax.set_xlim(0,5)
#ax.set_xscale('log')
ax.set_xlabel(r'$t$',fontsize="14")

ax.set_title(r"$\|\omega\|_{L^\infty}$ vs $t$")
#ax.set_title(r"$\|u_\alpha\|_{\dot{H}^1}$ vs $t$ - init $\|u_\alpha\|_{\dot{H}^1} = \sqrt{3}/2$")

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

Data = []
alphas = [0,2,4,8,12,16,20,24,28,32,36]
for i in alphas:
    filename = "./alpha_"+str(i)+"_1024/energy_fwd_0.dat"
    Ts = []
    sqrt_enstrophy = []
    vorticity = []
    with open(filename, 'r') as file:
        for line in file:
            line = " ".join(line.split())
            vals = line.split()
            sqrt_enstrophy.append(float(vals[5])**0.5)
            vorticity.append(float(vals[4]))
            Ts.append(round(float(vals[0]),2))
            
    ax.plot(Ts,vorticity,label=r"$1024\alpha: $"+str(i))
    #ax.plot(Ts,sqrt_enstrophy,label=r"$1024\alpha: $"+str(i))
    #print(len(Ts))
    #Data.append(sqrt_enstrophy)    

#ax.set_yticks([1,10])
#ax.set_yticklabels(["1","10"])
ax.set_xticks([0,1,2,3,4,5])
ax.set_xticklabels(["0","1","2","3","4","5"])
ax.legend(loc="upper left")
plt.show()
