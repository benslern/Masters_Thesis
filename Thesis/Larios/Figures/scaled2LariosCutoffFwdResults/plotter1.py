import matplotlib.pyplot as plt
import numpy as np

fig, ax = plt.subplots(figsize=[8, 4])

ax.set_ylim(3,45)
ax.set_yscale('log')
ax.set_ylabel(r'$\text{max}_{t\in[0,T]}\|\nabla u_\alpha(x,T)\|_{L^2}$',fontsize="14")

#ax.set_xlim(12,36)
ax.set_xscale('log')
ax.set_xlabel(r'$1024\alpha$',fontsize="14")

ax.set_title(r"Fig 3 - init $\|u_\alpha\|_{\dot{H}^1} = \pi$")

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
r = 8
for i in range(r,40,4):
    filename = "./alpha_"+str(i)+"_1024/energy_fwd_0.dat"
    Ts = []
    sqrt_enstrophy = []
    j = 0
    with open(filename, 'r') as file:
        for line in file:
            line = " ".join(line.split())
            vals = line.split()
            sqrt_enstrophy.append((float(vals[5])**0.5))
            sqrt_enstrophy[j] = max(sqrt_enstrophy)
            Ts.append(round(float(vals[0]),2))
            j += 1
            #print(round(float(vals[0]),2),float(vals[5])**0.5)

    #print(len(Ts))
    for j in range(len(sqrt_enstrophy)):
        sqrt_enstrophy[j] = max(sqrt_enstrophy[0:j+1])
    Data.append(sqrt_enstrophy)
    
ND = np.asarray(Data).T
Data = list(ND)

#print(Data[0])

for n in range(0,101,2):
    interpolation_factor = n / (101 - 1)

    # Red component decreases from 255 to 0
    red = ((1 - interpolation_factor))
    # Green component stays 0
    green = 0
    # Blue component increases from 0 to 255
    blue = (interpolation_factor)

    print(Ts[n],n)
    ax.plot(list(range(r,40,4)),Data[n],'o-',color=(blue,green,red),lw=0.5,ms=2)
ax.scatter([12]*4,[Data[100][1],Data[98][1],Data[96][1],Data[94][1]],zorder=2,s=8,color='g')
temp = []
for t in range(19):
    temp.append(Data[100-t*2][0])
ax.scatter([8]*19,temp,zorder=2,s=8,color='g') 
C = 275
plt.plot([8,12,16,20,24,28,32,36],[C/8,C/12,C/16,C/20,C/24,C/28,C/32,C/36],'o-',color='black',mfc='none')

ax.set_yticks([5,10,20,30,40])
ax.set_yticklabels([5,10,20,30,40])
ax.set_xticks([8,10,12,14,16,18,20,22,24,26,28,30,32,34,36])
ax.set_xticklabels(["8","","12","","16","","20","","24","","28","","32","","36"])
plt.show()
