import matplotlib.pyplot as plt
import numpy as np

fig, ax = plt.subplots(figsize=[8, 4])

ax.set_ylim(1,100)
ax.set_yscale('log')
ax.set_ylabel(r'$\|\omega\|_{L^\infty}(t)$',fontsize="14")
#ax.set_ylabel(r'$\|u_\alpha\|_{\dot{H}^1}$',fontsize="14")

ax.set_xlim(0,4)
#ax.set_xscale('log')
ax.set_xlabel('time',fontsize="14")

#ax.set_title(r"$\|\omega\|_{L^\infty}$ vs $t$ - init $\|u_\alpha\|_{\dot{H}^1} = \sqrt{3}/2$")
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

# Add the inset axes
axins = ax.inset_axes([0.21, 0.4, 0.56, 0.52])
axins.tick_params(
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

# Plot data in the inset axes (zoomed region)
x1, x2, y1, y2 = 3.6, 4.0, 3, 70
axins.set_xlim(x1, x2)
axins.set_yscale('log')
axins.set_ylim(y1, y2)

Data = []
alphas = [0,1,2,4,8,16]
for i in alphas:
    if i==1:
        filename = "./alpha_"+str(i)+"_1024/energy_fwd_5.dat"
    else:
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
            Ts.append(float(vals[0]))
            if float(vals[0])>4:
                break
            
    ax.plot(Ts,vorticity,'-o',ms=4,mfc='none',label=r"$1024\alpha: $"+str(i))
    axins.plot(Ts,vorticity,'-s',ms=4,mfc='none',label=r"$1024\alpha: $"+str(i))
    #ax.plot(Ts,sqrt_enstrophy,label=r"$1024\alpha: $"+str(i))
    #print(len(Ts))
    #Data.append(sqrt_enstrophy)    

#ax.set_yticks([1,10])
#ax.set_yticklabels(["1","10"])
axins.set_xticks([3.6,3.7,3.8,3.9,4],[3.6,3.7,3.8,3.9,4])
axins.set_yticks([10,20,30,40,50,60,70])
axins.set_yticklabels(['$10^1$','','','','','',''])
ax.set_xticks([0,0.5,1,1.5,2,2.5,3,3.5,4])
ax.set_xticklabels(["0","0.5","1","1.5","2","2.5","3","3.5","4"])
#ax.legend(loc="upper left")
plt.show()
