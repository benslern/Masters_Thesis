#Plot alpha^2 phi eta^0 at T=3 and T=5
import matplotlib.pyplot as plt
from mpl_toolkits.axes_grid1.inset_locator import mark_inset

colors = ['darkorange','darkturquoise','sienna','royalblue']
shapes = ['P','*','v','s']

fig, ax = plt.subplots(figsize=[8, 4])
ax.set_ylim(0,32E-4)

ax.set_xlim(1*0.9/1024,16*1.1/1024)
ax.set_xscale('log',base=2)
ax.set_xlabel(r'$\alpha$',fontsize="14")
ax.set_ylabel(r'$\alpha^2\phi_{\alpha,T}\left(\tilde{\eta}_{\alpha,T}\right)$',fontsize="14")
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
axins = ax.inset_axes([0.15, 0.4, 0.52, 0.52])
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
x1, x2, y1, y2 = 0.9/1024, 2.2/1024, 0, 1.5E-4
axins.set_xlim(x1, x2)
axins.set_xscale('log',base=2)
axins.set_ylim(y1, y2)

count = 0
for resol in [128,256]:#,256,512,1024]:
    for T in [3,5]:#[3.0,5.0]:
        if T==5 and resol==256:
            continue
        alphas = []
        phis = []
        for a in [1,2,4,8,16]:
            alpha = a/1024
            alphas.append(alpha)
            alpha_path_str = str(a)+"_1024"

            filename = "R"+str(resol)+"/T"+str(T)+"/alpha_"+alpha_path_str+"/maximization_cost.dat"
            

            phi = 0.0
            with open(filename, 'r') as file:
                for line in file:
                    line = " ".join(line.split())
                    vals = line.split()
                    phi = -1*float(vals[1])
            phis.append(alpha*alpha*phi)
        print(phis)
        if T==3:
            ax.plot(alphas,phis,'--',ms=5,mfc='none',color=colors[count],marker=shapes[count])
            axins.plot(alphas,phis,'--',ms=5,mfc='none',color=colors[count],marker=shapes[count])
        else:
            ax.plot(alphas,phis,'-',ms=5,mfc='none',color=colors[count],marker=shapes[count])
            axins.plot(alphas,phis,'-',ms=5,mfc='none',color=colors[count],marker=shapes[count])

    count += 1
            
ax.set_xticks([2**(-10),2**(-9),2**(-8),2**(-7),2**(-6)],["1/1024","2/1024","4/1024","8/1024","16/1024"])
ax.set_yticks([0,8E-4,16E-4,24E-4,32E-4],["0","8E-4","16E-4","24E-4","32E-4"])
#ax.set_yticks([0,2E-4,4E-4,6E-4,8E-4],["0","2E-4","4E-4","6E-4","8E-4"])
axins.set_xticks([2**(-10),2**(-9)],["1/1024","2/1024"])
axins.set_yticks([0,5E-5,1E-4,1.5E-4],["0","5.0E-5","1E-4","1.5E-4"])


plt.show()
