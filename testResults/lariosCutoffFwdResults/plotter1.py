import matplotlib.pyplot as plt
import numpy as np

fig, ax = plt.subplots(figsize=[8, 4])

ax.set_ylim(0.8,4)
ax.set_yscale('log')
ax.set_ylabel(r'$\text{max}_{t\in[0,T]}\|\nabla u_\alpha(x,T)\|_{L^2}$',fontsize="14")

#ax.set_xlim(1.9,38)
ax.set_xscale('log')
ax.set_xlabel(r'$1024\alpha$',fontsize="14")

ax.set_title(r"Fig 3 - init $\|u_\alpha\|_{\dot{H}^1} = \sqrt{3}/2$")

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
    ax.plot(alphas,Data[n],'o-',color=(blue,green,red),lw=0.5,ms=4,zorder=0)
ax.scatter([2,2],[Data[100][0],Data[98][0]],color='g',zorder=2,s=16)
#ax.plot(alphas,Data[84],'o-',color=(0,1,0),lw=0.5,ms=4)
C = 5.75
D = -0.417
#ax.plot([4,8,12,16,20,24,28,32,36],[C/4,C/8,C/12,C/16,C/20,C/24,C/28,C/32,C/36],'o-',color='black')
ax.plot([2,4,8,12,16,20,24,28,32,36],[C*(2**(D)),C*(4**(D)),C*(8**(D)),C*(12**(D)),C*(16**(D)),C*(20**(D)),C*(24**(D)),C*(28**(D)),C*(32**(D)),C*(36**(D))],'o-',mfc='none',color='black')


ax.set_yticks([0.8,1,1.5,2,2.5,3,3.5,4])
ax.set_yticklabels(["","1","","2","","3","","4"])
ax.set_xticks([2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,32,34,36])
ax.set_xticklabels(["2","4","","8","","12","","16","","20","","24","","28","","32","","36"])
plt.show()
