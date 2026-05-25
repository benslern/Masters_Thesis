import matplotlib.pyplot as plt
import numpy as np

fig, ax = plt.subplots(figsize=[8, 4])

#ax.set_ylim(3.5,27)
#ax.set_yscale('log')
ax.set_ylabel('slope',fontsize="14")

ax.set_xlim(0,5)
#ax.set_xscale('log')
ax.set_xlabel('time',fontsize="14")

#ax.set_title(r"Fig 3 inset - init $\|u_\alpha\|_{\dot{H}^1} = \sqrt{3}/2$")

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
for i in alphas:
    filename = "R256/T3/alpha_"+str(i)+"_1024/energy_fwd_final.dat"
    Ts = []
    sqrt_enstrophy = []
    j = 0
    add = True
    with open(filename, 'r') as file:
        for line in file:
            line = " ".join(line.split())
            vals = line.split()
            if (i==1) and add:
                sqrt_enstrophy.append(float(vals[5])**0.5)
                sqrt_enstrophy[j//2] = max(sqrt_enstrophy)
                Ts.append(round(float(vals[0]),2))
            if i>1:
                sqrt_enstrophy.append(float(vals[5])**0.5)
                sqrt_enstrophy[j] = max(sqrt_enstrophy)
                Ts.append(round(float(vals[0]),2))
            j += 1
            add = not add
            
            #print(round(float(vals[0]),2),float(vals[5])**0.5)

    Data.append(sqrt_enstrophy)
    print(len(Ts))
ND = np.asarray(Data).T
Data = list(ND)

for i in range(0,4):
    slopes = []
    print(alphas[i+1],alphas[i])
    
    for n in range(0,193,1):
        m = (np.log(Data[n][i+1])-np.log(Data[n][i]))/(np.log(alphas[i+1]/1024) - np.log(alphas[i]/1024))
        slopes.append(m)
    if True:
        ax.plot(Ts,slopes,'o-',ms=4,label=r"$1024\alpha$: "+str(alphas[i])+' - '+str(alphas[i+1]))
    print(min(slopes))
ax.set_xticks([0,1,2,3])
ax.set_xticklabels(["0","1","2","3"])
ax.legend(loc="lower left")
plt.show()
