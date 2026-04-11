#Plot alpha^2 phi eta^0 at T=3 and T=5
import matplotlib.pyplot as plt
from mpl_toolkits.axes_grid1.inset_locator import mark_inset

colors = ['darkorange','darkturquoise','sienna','royalblue']
shapes = ['P','*','v','s']

fig, ax = plt.subplots(figsize=[8, 4])
ax.set_ylim(0,8E-4)

ax.set_xlim(1*0.9/1024,16*1.1/1024)
ax.set_xscale('log',base=2)
ax.set_xlabel(r'$\alpha$',fontsize="14")
ax.set_ylabel(r'$\alpha^2\phi_{\alpha,T}\left(\eta_{\alpha}^{(0)}\right)$',fontsize="14")
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
x1, x2, y1, y2 = 0.9/1024, 2.2/1024, 0, 5E-5
axins.set_xlim(x1, x2)
axins.set_xscale('log',base=2)
axins.set_ylim(y1, y2)

count = 0
for resol in [128,256,512,1024]:
    for T in [3.0,5.0]:
        alphas = []
        phis = []
        for a in [1,2,4,8,16]:
            alpha = a/1024
            alphas.append(alpha)
            alpha_path_str = str(a)+"_1024"

            filename = "alpha_"+alpha_path_str+"/resol_"+str(resol)+"/energy_fwd_1.dat"
            

            phi = 0.0
            with open(filename, 'r') as file:
                for line in file:
                    line = " ".join(line.split())
                    vals = line.split()
                    if float(vals[0])==T:
                        phi = float(vals[5])
                        break
            phis.append(alpha*alpha*phi)
        print(phis)
        if T==3.0:
            ax.plot(alphas,phis,'--',ms=5,mfc='none',color=colors[count],marker=shapes[count])
            axins.plot(alphas,phis,'--',ms=5,mfc='none',color=colors[count],marker=shapes[count])
        else:
            ax.plot(alphas,phis,'-',ms=5,mfc='none',color=colors[count],marker=shapes[count])
            axins.plot(alphas,phis,'-',ms=5,mfc='none',color=colors[count],marker=shapes[count])

    count += 1
            
ax.set_xticks([2**(-10),2**(-9),2**(-8),2**(-7),2**(-6)],["1/1024","2/1024","4/1024","8/1024","16/1024"])
ax.set_yticks([0,2E-4,4E-4,6E-4,8E-4],["0","2E-4","4E-4","6E-4","8E-4"])
axins.set_xticks([2**(-10),2**(-9)],["1/1024","2/1024"])
axins.set_yticks([0,2.5E-5,5E-5],["0","2.5E-5","5.0E-5"])


plt.show()
