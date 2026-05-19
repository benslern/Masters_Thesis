import matplotlib.pyplot as plt
import numpy as np

fig, ax = plt.subplots(figsize=[8, 4])

ax.set_ylim(0.8,4)
ax.set_yscale('log')
ax.set_ylabel(r'$\text{max}_{t\in[0,T]}\|\nabla u_\alpha(x,T)\|_{L^2}$',fontsize="14")

ax.set_xlim(0.9,18)
ax.set_xscale('log')
ax.set_xlabel(r'$1024\alpha$',fontsize="14")

#ax.set_title(r"Fig 3 - init $\|u_\alpha\|_{\dot{H}^1} = \sqrt{3}/2$")

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
    labelsize=10,
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
            #print(vals)
            
            #print(vals[0])
            if i!=1:
                sqrt_enstrophy.append((abs(float(vals[5]))**0.5))
                Ts.append(round(float(vals[0]),2))
            if add and i==1:
                sqrt_enstrophy.append((abs(float(vals[5]))**0.5))
                Ts.append(round(float(vals[0]),2))
            add = not add
            j += 1
        
            #if(float(vals[0])>=3):
                #print(len(Ts))
            #    break
            #print(round(float(vals[0]),2),float(vals[5])**0.5)

    print(len(sqrt_enstrophy))
    print(len(Ts))
    print(Ts[192])
    
    for j in range(len(sqrt_enstrophy)):
        sqrt_enstrophy[j] = max(sqrt_enstrophy[0:j+1])
    Data.append(sqrt_enstrophy)
    
ND = np.asarray(Data).T
Data = list(ND)

#print(Data[0])

for n in range(0,193,64):
    interpolation_factor = n / (193 - 1)

    # Red component decreases from 255 to 0
    red = ((1 - interpolation_factor))
    # Green component stays 0
    green = 0
    # Blue component increases from 0 to 255
    blue = (interpolation_factor)

    #print(Ts[n],n)
    #print(Data[n])
    ax.plot(alphas,Data[n],'o-',color=(blue,green,red),lw=0.5,ms=4,zorder=0)
#ax.scatter([2,2],[Data[100][1],Data[98][1]],color='g',zorder=2,s=16)
#ax.plot(alphas,Data[84],'o-',color=(0,1,0),lw=0.5,ms=4)
C = 5.5
D = -0.160
#ax.plot([4,8,12,16,20,24,28,32,36],[C/4,C/8,C/12,C/16,C/20,C/24,C/28,C/32,C/36],'o-',color='black')
ax.plot([1,2,4,8,16],[C*(1**(D)),C*(2**(D)),C*(4**(D)),C*(8**(D)),C*(16**(D))],'o-',ms=4,color='black')
C = 60
D = -1
ax.plot([1,2,4,8,16],[C*(1**(D)),C*(2**(D)),C*(4**(D)),C*(8**(D)),C*(16**(D))],'o--',ms=4,color='black')

#ax.plot([1,2,4,8,12,16],[C*(1**(D)),C*(2**(D)),C*(4**(D)),C*(8**(D)),C*(12**(D)),C*(16**(D))],'o-',ms=4,color='black')


ax.set_yticks([0.8,0.9,1,1.5,2,2.5,3,3.5,4,4.5,5,5.5,6,6.5,7])
ax.set_yticklabels(["","","1","","2","","3","","4","","5","","6","","7"])
#ax.set_xticks([1,2,4,6,8,10,12,14,16])
#ax.set_xticklabels(["1","2","4","","8","","12","","16"])

ax.set_xticks([1,2,4,8,16])
ax.set_xticklabels(["1","2","4","8","16"])
plt.show()
