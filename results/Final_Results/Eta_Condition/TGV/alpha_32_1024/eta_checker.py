#Eta condition checker
import numpy as np
from scipy.io import netcdf
import matplotlib.pyplot as plt
import netCDF4 as nc4


resol = 512
alpha = 16/1024

e_eta_path = '../../../Euler_Results/TGV/R'+str(resol)+'/Uvec_fwdTE_0.nc'
ev_eta_path = '../../../Optimization_Results/TGV/alpha_16_1024/Uvec_fwdTE_20.nc'

modeType   = 'r'
fileFormat = 'NETCDF4'
e_eta_data = nc4.Dataset(e_eta_path, mode=modeType, format=fileFormat)
ev_eta_data = nc4.Dataset(ev_eta_path, mode=modeType, format=fileFormat)

dif = []

dif.append(e_eta_data.variables['Ux'][:] - ev_eta_data.variables['Ux'][:])
dif.append(e_eta_data.variables['Uy'][:] - ev_eta_data.variables['Uy'][:])
dif.append(e_eta_data.variables['Uz'][:] - ev_eta_data.variables['Uz'][:])

# Calculate gradients
dux_dx, dux_dy, dux_dz = np.gradient(dif.variables['Ux'],1/resol,edge_order = 2)
duy_dx, duy_dy, duy_dz = np.gradient(dif.variables['Uy'],1/resol,edge_order = 2)
duz_dx, duz_dy, duz_dz = np.gradient(dif.variables['Uz'],1/resol,edge_order = 2)

l2norm_grad = np.sqrt(dux_dx**2 + dux_dy**2 + dux_dz**2 + duy_dx**2 + duy_dy**2 + duy_dz**2 + duz_dx**2 + duz_dy**2 + duz_dz**2)
t1 = (alpha**2)*(1/(resol**3))*np.sum(l2norm_grad**2)


U = dif.createVariable('U','f8', 
                            ('z','y','x'),
                            fill_value=1.0)
dif.variables['U'][:] = np.sqrt(dif.variables['Ux'][:]**2 + dif.variables['Uy'][:]**2 + dif.variables['Uz'][:]**2)

t2 = (1/(resol**3))*np.sum(dif['U'][:]**2)

print(t1+t2)

e_eta_data.close()
ev_eta_data.close()
