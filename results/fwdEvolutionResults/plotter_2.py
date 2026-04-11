#Plot alpha^2 phi eta^0 at T=3 and T=5
import matplotlib.pyplot as plt
from mpl_toolkits.axes_grid1.inset_locator import mark_inset

colors = ['darkorange','darkturquoise','sienna','royalblue','black']
shapes = ['P','*','v','s','1']
lines = ['solid','solid','dotted','dashed','dashdot']

fig, ax = plt.subplots(figsize=[8, 4])
ax.set_ylim(0,30)
ax.set_xlim(0,5)
ax.set_xlabel(r'$t$',fontsize="14")
ax.set_ylabel(r'$\|\boldsymbol{u}_\alpha(\boldsymbol{x},t)\|_{\dot{H}^1}^2$',fontsize="14")
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
'''
axins = ax.inset_axes([0.12, 0.4, 0.52, 0.52])
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
x1, x2, y1, y2 = 4, 5, 0, 12
axins.set_xlim(x1, x2)
axins.set_ylim(y1, y2)
'''
count = 0
for resol in [128,256,512,1024]:
    ts = []
    phis = []
    count2 = 0
    for a in [1]:
        alpha = a/1024
        alpha_path_str = str(a)+"_1024"

        filename = "alpha_"+alpha_path_str+"/resol_"+str(resol)+"/energy_fwd_1.dat"
        

        phi = 0.0
        ts = []
        phis = []
        with open(filename, 'r') as file:
            for line in file:
                line = " ".join(line.split())
                vals = line.split()
                ts.append(float(vals[0]))
                phis.append(float(vals[5]))
        print(len(ts))
        ax.plot(ts,phis,linestyle=lines[count2],ms=4,mfc='none',color=colors[count],marker=shapes[count])
        #axins.plot(ts,phis,linestyle=lines[count2],ms=4,mfc='none',color=colors[count],marker=shapes[count])
        count2 += 1

    count += 1
            
ax.set_xticks([0,1,2,3,4,5],["0","1","2","3","4","5"])
ax.set_yticks([0,5,10,15,20,25,30],["0","5","10","15","20","25","30"])
#axins.set_xticks([4,5],["4","5"])
#axins.set_yticks([0,4,8,12],["0","4","8","12"])


plt.show()
