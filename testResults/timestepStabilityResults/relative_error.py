import matplotlib.pyplot as plt
import numpy as np

fig, ax = plt.subplots(figsize=[8, 4])

ax.set_ylim(0.0,0.008)
#ax.set_yscale('log')
#ax.set_ylabel(r'$\|\omega\|_{L^\infty}$',fontsize="14")

ax.set_xlim(0,5)
#ax.set_xscale('log')
ax.set_xlabel(r'$t$',fontsize="14")

#ax.set_title(r"$\alpha$-Energy vs $t$ - $\alpha=12/1024$ - init $\|u_\alpha\|_{\dot{H}^1} = \sqrt{3}/2$")
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
alphas = [1,2,4,8,16]
dt = "2"
for dt in ["2","3","4","5","6","7","8"]:
    for i in alphas:
        ax.set_title(r"$\alpha$-Energy vs $t$ - $\alpha=$"+str(i)+r"$/1024$")

        filename = "./resol_1024/alpha_"+str(i)+"_1024/energy_fwd_"+dt+".dat"
        Ts = []
        ase = []
        energy = []
        tot = []
        rel = []
        with open(filename, 'r') as file:
            for line in file:
                line = " ".join(line.split())
                vals = line.split()
                try:
                    ase.append(((i/1024)**2)*float(vals[5]))
                    energy.append(2*float(vals[1]))
                    tot.append(((i/1024)**2)*float(vals[5]) + 2*float(vals[1]))
                    Ts.append(round(float(vals[0]),2))
                except:
                    #do nothing
                    dt = dt
        for x in range(len(tot)):
            rel.append(abs((tot[0]-tot[x])/tot[0]))
               
        #ax.plot(Ts,ase,label=r"$\alpha^2\|\nabla u_\alpha\|_{L^2}^2$")
        #ax.plot(Ts,energy, label=r"$\|u_\alpha\|_{L^2}^2$")
        #ax.plot(Ts,tot, label=r"$\|u_\alpha\|_{L^2}^2 + \alpha^2\|\nabla u_\alpha\|_{L^2}^2$")
        print(i,dt,max(rel))
        #print(len(Ts))
        #Data.append(sqrt_enstrophy)
        input()

#ax.set_xticks([0,1,2,3,4,5])
#ax.set_xticklabels(["0","1","2","3","4","5"])
#ax.legend(loc="center left")
#plt.show()
