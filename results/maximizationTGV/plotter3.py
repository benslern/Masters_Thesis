#Plot alpha^2 phi eta^0 at T=3 and T=5
import matplotlib.pyplot as plt
from mpl_toolkits.axes_grid1.inset_locator import mark_inset

colors = ['darkorange','darkturquoise','sienna','royalblue']
shapes = ['P','*','v','s']

fig, ax = plt.subplots(figsize=[8, 4])
ax.set_ylim(0,30)

ax.set_xlim(-20,350)
ax.set_xlabel('n',fontsize="14")
ax.set_ylabel(r'$\phi_{\alpha,T}\left(\eta_{\alpha}^{(n)}\right)$',fontsize="14")
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
'''
axins = ax.inset_axes([0.42, 0.52, 0.52, 0.42])
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
x1, x2, y1, y2 = -1,10, 0, 12
axins.set_xlim(x1, x2)
#axins.set_xscale('log',base=2)
axins.set_ylim(y1, y2)
'''

count = 0
for resol in [256]:#,256,512,1024]:
    for T in [5.0]:#[3.0,5.0]:
        for a in [1,2,4,8,16]:
            alpha = a/1024
            alpha_path_str = str(a)+"_1024"

            filename = "R256/T3/alpha_"+alpha_path_str+"/maximization_cost.dat"


            phi = 0.0
            iters = []
            phis = []
            with open(filename, 'r') as file:
                for line in file:
                    line = " ".join(line.split())
                    vals = line.split()

                    it = float(vals[0])
                    phi = -1*float(vals[1])

                    iters.append(it)
                    phis.append(phi)
            print(iters)
            ax.plot(iters,phis,'-o',ms=2)
            #axins.plot(iters,phis,'-o',ms=4)
plt.show()











            
