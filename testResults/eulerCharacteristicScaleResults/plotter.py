
import matplotlib.pyplot as plt
import numpy as np

from mpl_toolkits.axes_grid1.inset_locator import mark_inset


fig, ax = plt.subplots(figsize=[8, 4])
#ax.set_title("Main Plot with Inset")
#ax.legend(loc='upper left')
colors = ['darkorange','darkturquoise','sienna','royalblue']
shapes = ['P','*','v','s']
ax.set_yscale('log')
ax.set_ylim(1,100)
ax.set_xlim(0,4)
ax.set_xlabel('time',fontsize="14")
ax.set_ylabel(r'$\|\omega\|_{\infty}(t)$',fontsize="14")
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
axins = ax.inset_axes([0.21, 0.4, 0.56, 0.52])
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
x1, x2, y1, y2 = 3.6, 4.0, 8, 70
axins.set_xlim(x1, x2)
axins.set_yscale('log')
axins.set_ylim(y1, y2)

#axins.set_xticks(visible=False)
#axins.set_yticks(visible=False)
resols = ['128','256','512','1024']
for resol in resols:
    t = []
    linf = []
    with open('tc_1/resol_'+resol+'/energy_fwd_1.dat', 'r') as file:
        for line in file:
            # Process each line here
            # Lines include the newline character ('\n') at the end
            #print(line.strip()) # Use strip() to remove leading/trailing whitespace and the newline character
            line = " ".join(line.split())
            vals = line.split(" ")

            t.append(float(vals[0]))
            linf.append(float(vals[4]))
    
    ax.plot(t,linf,color=colors[resols.index(resol)],ms=4,mfc='none',marker=shapes[resols.index(resol)])
    axins.plot(t, linf, color=colors[resols.index(resol)],ms=4,mfc='none',marker=shapes[resols.index(resol)])


# Draw indicator lines
#mark_inset(ax, axins, loc1=2, loc2=4, fc="none", ec="0.5")

# Display the plot
axins.set_xticks([3.6,3.7,3.8,3.9,4],[3.6,3.7,3.8,3.9,4])
axins.set_yticks([10,20,30,40,50,60,70])
axins.set_yticklabels(['$10^1$','','','','','',''])
ax.set_xticklabels(['0','0.5','1','1.5','2','2.5','3','3.5','4'])
plt.show()

#plt.ylim([1,100])
#plt.xlim([0,4])
#plt.xticks([0, 0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4],[0, 0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4])
#plt.yticks([1,10,100],['1E0','1E1','1E2'])
#plt.yscale('log')
#plt.grid('on',which="both")
#plt.xlabel("time")
#plt.ylabel("$\|\omega\|_{\infty}(t)$ ")
#plt.show()        
