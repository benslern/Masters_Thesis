#Plot alpha^2 phi eta^0 at T=3 and T=5
import matplotlib.pyplot as plt
from mpl_toolkits.axes_grid1.inset_locator import mark_inset
import  numpy as np

colors = ['darkorange','darkturquoise','sienna','royalblue']
shapes = ['P','*','v','s']

fig, ax = plt.subplots(figsize=[8, 4])
#ax.set_ylim(-1.1,0.1)
#ax.set_yscale('log',base=2)
#ax.set_xlim(1*0.9/1024,16*1.1/1024)
#ax.set_xscale('log',base=2)

ax.set_xlabel(r'$t$',fontsize="14")
ax.set_ylabel('left slope',fontsize="14")
#ax.set_xlabel(r'$1024\alpha$',fontsize="14")
#ax.set_ylabel(r'$\|u_\alpha(x,T)\|_{L^2}$',fontsize="14")
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

for resol in [1024]:
    alpha_1 = []
    alpha_2 = []
    Ts = []
    for T in range(0,11):
        print(T/2)
        Ts.append(T/2)
        alphas = []
        max_sqrt_enstrophy = []
    
        for a in [1,2,4,8,16]:
            alpha = a/1024
            alphas.append(alpha*1024)
            alpha_path_str = str(a)+"_1024"

            filename = "alpha_"+alpha_path_str+"/resol_"+str(resol)+"/energy_fwd_1.dat"
            

            mse = 0.0
            with open(filename, 'r') as file:
                for line in file:
                    line = " ".join(line.split())
                    vals = line.split()
                    if float(vals[0])==T/2:
                        mse = float(vals[5])
                        break
            max_sqrt_enstrophy.append(mse**0.5)
            if a==8:
                alpha_1.append(mse**0.5)
            if a==16:
                alpha_2.append(mse**0.5)

        
        #ax.plot(alphas,max_sqrt_enstrophy,'o-',ms=5,mfc='none',color='r')
    deriv = []
    for i in range(len(alpha_2)):
        deriv.append(float((np.log10(alpha_2[i])-np.log10(alpha_1[i]))/(np.log10(16/1024) - np.log10(12/1024))))
    print(deriv)
    print(Ts)
    ax.plot(Ts,deriv)
#ax.set_xticks([1,2,4,8,16],["1","2","4","8","16"])

#ax.set_xticks([2**(-10),2**(-9),2**(-8),2**(-7),2**(-6)],["1/1024","2/1024","4/1024","8/1024","16/1024"])
#ax.set_yticks([0,2E-4,4E-4,6E-4,8E-4],["0","2E-4","4E-4","6E-4","8E-4"])



plt.show()
