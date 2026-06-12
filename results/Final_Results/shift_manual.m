clear all;
%filename = 'q4Q9000T0005_L_B4.nc';
%filenamenew = 'q4Q9000T0005_L_B4_new.nc';
filename = './Nonoptimization_Results_T5/TGV/alpha_8_1024/Uvec_fwdTE_visual.nc';
filenamenew = './Nonoptimization_Results_T5/TGV/alpha_8_1024/Uvec_fwdTE_visual_shift.nc';

new_ori = [1, 0.3333, 1];   % the coordinates of new origin, in [0,1]^3
compute_vort = 0;      % set if need to compute vorticity

ncdisp(filename, '/' , 'full');
ux=ncread(filename, 'Ux');
uy=ncread(filename, 'Uy');
uz=ncread(filename, 'Uz');
resol=size(ux,1);

dx = floor(new_ori(1)*resol);
dy = floor(new_ori(2)*resol);
dz = floor(new_ori(3)*resol);

uxnew=ux([dx:end 1:dx-1],[dy:end 1:dy-1],[dz:end 1:dz-1]);
uynew=uy([dx:end 1:dx-1],[dy:end 1:dy-1],[dz:end 1:dz-1]);
uznew=uz([dx:end 1:dx-1],[dy:end 1:dy-1],[dz:end 1:dz-1]);

%if (compute_vort==1)
%[xmesh ymesh zmesh]=meshgrid(1/resol:1/resol:1,1/resol:1/resol:1,1/resol:1/resol:1);
%[vxnew,vynew,vznew,vav]=curl(xmesh,ymesh,zmesh,uxnew,uynew,uznew);
%vmagnew = sqrt(vxnew.^2+vynew.^2+vznew.^2);
%end

%if (compute_vort == 0)
%    vx=ncread(filename, 'Vx');
%    vy=ncread(filename, 'Vy');
%    vz=ncread(filename, 'Vz');
%    vmag=ncread(filename, 'Vmag');
%    vxnew=vx([dx:end 1:dx-1],[dy:end 1:dy-1],[dz:end 1:dz-1]);
%    vynew=vy([dx:end 1:dx-1],[dy:end 1:dy-1],[dz:end 1:dz-1]);
%    vznew=vz([dx:end 1:dx-1],[dy:end 1:dy-1],[dz:end 1:dz-1]);
%    vmagnew=vmag([dx:end 1:dx-1],[dy:end 1:dy-1],[dz:end 1:dz-1]);
%end


nccreate(filenamenew,'Ux','Dimensions',{'x',resol,'y',resol,'z',resol},'Datatype','double');
nccreate(filenamenew,'Uy','Dimensions',{'x',resol,'y',resol,'z',resol},'Datatype','double');
nccreate(filenamenew,'Uz','Dimensions',{'x',resol,'y',resol,'z',resol},'Datatype','double');
%nccreate(filenamenew,'Vx','Dimensions',{'x',resol,'y',resol,'z',resol},'Datatype','double');
%nccreate(filenamenew,'Vy','Dimensions',{'x',resol,'y',resol,'z',resol},'Datatype','double');
%nccreate(filenamenew,'Vz','Dimensions',{'x',resol,'y',resol,'z',resol},'Datatype','double');
%nccreate(filenamenew,'Vmag','Dimensions',{'x',resol,'y',resol,'z',resol},'Datatype','double');

ncdisp(filenamenew, '/' , 'full');
ncwrite(filenamenew,'Ux',uxnew);
ncwrite(filenamenew,'Uy',uynew);
ncwrite(filenamenew,'Uz',uznew);
%ncwrite(filenamenew,'Vx',vxnew);
%ncwrite(filenamenew,'Vy',vynew);
%ncwrite(filenamenew,'Vz',vznew);
%ncwrite(filenamenew,'Vmag',vmagnew);
