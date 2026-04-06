import matplotlib.pyplot as plt
import numpy as np

fig, ax = plt.subplots(figsize=[8, 4])

#ax.set_ylim(3.5,27)
#ax.set_yscale('log')
ax.set_ylabel('slope',fontsize="14")

ax.set_xlim(0,5)
#ax.set_xscale('log')
ax.set_xlabel(r'$t$',fontsize="14")

ax.set_title(r"Fig 3 inset - init $\|u_\alpha\|_{\dot{H}^1} = \sqrt{3}/2$")

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
alphas = [2,4,8,12,16,20,24,28,32,36]
for i in alphas:
    filename = "./alpha_"+str(i)+"_1024/energy_fwd_0.dat"
    Ts = []
    sqrt_enstrophy = []
    j = 0
    with open(filename, 'r') as file:
        for line in file:
            line = " ".join(line.split())
            vals = line.split()
            sqrt_enstrophy.append(float(vals[5])**0.5)
            sqrt_enstrophy[j] = max(sqrt_enstrophy)
            Ts.append(round(float(vals[0]),2))

            j += 1
            #print(round(float(vals[0]),2),float(vals[5])**0.5)

    Data.append(sqrt_enstrophy)
    
ND = np.asarray(Data).T
Data = list(ND)

for i in range(0,9):
    if i==3:
        slopes = []
        print(alphas[i+1],alphas[i])
        for n in range(0,101,1):
            m = (np.log(Data[n][i+1])-np.log(Data[n][i]))/(np.log(alphas[i+1]/1024) - np.log(alphas[i]/1024))
            slopes.append(m)
        ax.plot(Ts,slopes,label=r"$1024\alpha$: "+str(alphas[i])+' - '+str(alphas[i+1]))

ax.set_xticks([0,1,2,3,4,5])
ax.set_xticklabels(["0","1","2","3","4","5"])
ax.legend(loc="lower left")
plt.show()
