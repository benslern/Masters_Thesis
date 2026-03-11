import matplotlib.pyplot as plt
import numpy as np

fig, ax = plt.subplots(figsize=[8, 4])

ax.set_ylim(0.8,2)
ax.set_yscale('log')
ax.set_ylabel(r'$\|u_\alpha(x,T)\|_{L^2}$',fontsize="14")

ax.set_xlim(12,36)
ax.set_xscale('log')
ax.set_xlabel(r'$1024\alpha$',fontsize="14")

ax.set_title(r"Fig 3 - T=0,0.2,...,5.0 - init $\|u_\alpha\|_{\dot{H}^1} = \sqrt{3}/2$")

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

for i in range(12,40,4):
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

    #print(len(Ts))
    Data.append(sqrt_enstrophy)
    
ND = np.asarray(Data).T
Data = list(ND)

#print(Data[0])

for n in range(0,101,4):
    interpolation_factor = n / (101 - 1)

    # Red component decreases from 255 to 0
    red = ((1 - interpolation_factor))
    # Green component stays 0
    green = 0
    # Blue component increases from 0 to 255
    blue = (interpolation_factor)

    ax.plot(list(range(12,40,4)),Data[n],'o-',color=(blue,green,red))


ax.set_yticks([0.8,0.9,1,1.5,2])
ax.set_yticklabels(["","","1","1.5","2"])
ax.set_xticks([12,14,16,18,20,22,24,26,28,30,32,34,36])
ax.set_xticklabels(["12","","16","","20","","24","","28","","32","","36"])
plt.show()
