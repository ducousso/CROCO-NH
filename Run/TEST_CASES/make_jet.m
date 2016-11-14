%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Build a CROCO JET configuration.
%  Create a grid file and an intial file for the jet experiment
%  (for the parent and the child grid).
%  The jet is defined such as there is no motion below a definied
%  depth H0.
% 
%  Further Information:  
%  http://www.croco-ocean.org
%  
%  This file is part of CROCOTOOLS
%
%  CROCOTOOLS is free software; you can redistribute it and/or modify
%  it under the terms of the GNU General Public License as published
%  by the Free Software Foundation; either version 2 of the License,
%  or (at your option) any later version.
%
%  CROCOTOOLS is distributed in the hope that it will be useful, but
%  WITHOUT ANY WARRANTY; without even the implied warranty of
%  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%  GNU General Public License for more details.
%
%  You should have received a copy of the GNU General Public License
%  along with this program; if not, write to the Free Software
%  Foundation, Inc., 59 Temple Place, Suite 330, Boston,
%  MA  02111-1307  USA
%
%  Copyright (c) 2005-2006 by Pierrick Penven 
%  e-mail:Pierrick.Penven@ird.fr  
%
%  Ref: Penven, P., L. Debreu, P. Marchesiello et J.C. McWilliams,
%       Application of the ROMS embedding procedure for the Central 
%      California Upwelling System,  Ocean Modelling, 2006.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
clear all
close all
%%%%%%%%%%%%%%%%%%%%% USERS DEFINED VARIABLES %%%%%%%%%%%%%%%%%%%%%%%%
%
%  Title
%
title_case='JET';
%
%  Names
%
parent_grd='jet_grd.nc';
parent_ini ='jet_ini.nc';
parent_clm ='jet_clm.nc';
%
% Domain size
%
xmax=300e3;             % Domain half length LAT
ymax=500e3;             % Domain half length LON
H0=1000;                % Total Depth
%
% Jet parameters
%
umax=0.6;               % jet velocity (at its center)
Ljet=50.e3;             % jet width
Y0=0;                   % jet center
H=300;                  % jet vertical scale (level of no-motion)
N2=(0.75e-2)^2;         % Brunt-Vaissala frequency
lat=30;                 % Latitude 
bplane=1;               % bplane=0 or 1 (f-plane or beta-plane)
perturb=1;              % add jet perturbation in x direction (2*pi*R)
Pa=1013e2;              % Atmospheric pressure
rho0=1024.4;            % Mean ocean density
g=9.81;                 % Gravity acceleration
R=6367442.76;           % Earth radius
%
% Horizontal grid resolution
%
dx=20.e3; 
%
% Vertical grid parameters
%
N=30;
theta_s=5;
theta_b=0;
hc=100;
%
% Nesting parameters
%
nesting=0;
refinecoeff=3;
jmin=51;
jmax=70;
imin=51;
imax=70;
%
%%%%%%%%%%%%%%%%%%% END USERS DEFINED VARIABLES %%%%%%%%%%%%%%%%%%%%%%%
%
disp(' ')
disp(' ... BUILD JET CONFIGURATION FILES ...')
disp(' ')
%
%
% Horzontal Grid
%
dy=dx;
x=[-xmax-dx/2:dx:xmax+dx/2];
y=[-ymax-dy/2:dx:ymax+dy/2];
[X,Y]=meshgrid(x,y);
%
% Topo
%
h0=H0+0*X;
%
% Coriolis term (beta plane)
%
deg2rad=pi/180;
omega=2*pi/(24*3600);
f0=2*omega*sin(deg2rad*lat);
if bplane,
 beta=2*omega/R*cos(deg2rad*lat);
else
 beta=0;
end
f=f0+beta*Y;
%
% Compute zeta,ubar,vbar,u,v,t for the vortex
%
barocjet
%
%  Create the grid file
%
[Mp,Lp]=size(zeta);
M=Mp-1;
L=Lp-1;
disp(' ')
disp(['GRID DIMENSIONS Lm Mm : ',num2str(L-1),'  ',num2str(M-1)])
nc=netcdf(parent_grd, 'clobber');
%%redef(nc);		% for Octave compatiblity
nc('xi_rho') = Lp;
nc('eta_rho') = Mp;
nc('xi_psi') = L;
nc('eta_psi') = M;
nc('xi_u') = L;
nc('eta_u') = Mp;
nc('xi_v') = Lp;
nc('eta_v') = M;
nc('one') = 1;
nc{'el'} = ncdouble('one');
nc{'xl'} = ncdouble('one');
nc{'spherical'} = ncchar('one');
nc{'h'} = ncdouble('eta_rho', 'xi_rho');
nc{'f'} = ncdouble('eta_rho', 'xi_rho');
nc{'pm'} = ncdouble('eta_rho', 'xi_rho');
nc{'pn'} = ncdouble('eta_rho', 'xi_rho');
nc{'x_rho'} = ncdouble('eta_rho', 'xi_rho');
nc{'y_rho'} = ncdouble('eta_rho', 'xi_rho');
nc{'mask_rho'} = ncdouble('eta_rho', 'xi_rho');
%%endef(nc);		% for Octave compatiblity
nc.title = ncchar(title_case);
nc.title = title_case;
nc.date = ncchar(date);
nc.date = date;
nc.type = ncchar('CROCO grid file');
nc.type = 'CROCO grid file';
%
%  fill the grid file
%
nc{'xl'}(:)=x(end)-x(1);
nc{'el'}(:)=y(end)-y(1);
nc{'spherical'}(:)='F';
nc{'h'}(:)=h0;
nc{'f'}(:)=f;
nc{'pm'}(:)=1/dx;
nc{'pn'}(:)=1/dx;
nc{'x_rho'}(:)=X;
nc{'y_rho'}(:)=Y;
nc{'mask_rho'}(:)=1+0.*Y;
close(nc);
%
%  Create and fill the initial file
%
create_inifile(parent_ini,parent_grd,title_case,...
               theta_s,theta_b,hc,N,0,'clobber')
nc=netcdf(parent_ini,'write');
nc{'u'}(:) =  u; 
nc{'v'}(:) =  v; 
nc{'zeta'}(:) =  zeta; 
nc{'ubar'}(:) =  ubar; 
nc{'vbar'}(:) =  vbar; 
nc{'temp'}(:) =  t; 
close(nc)
%
%  Create and fill the climatology file
%
create_climfile(parent_clm,parent_grd,title_case,...
                theta_s,theta_b,hc,N,[25 75],100,'clobber')
nc=netcdf(parent_clm,'write');
nc{'u'}(1,:,:,:) =  u;
nc{'v'}(1,:,:,:) =  v;
nc{'zeta'}(1,:,:,:) =  zeta;
nc{'ubar'}(1,:,:,:) =  ubar;
nc{'vbar'}(1,:,:,:) =  vbar;
nc{'temp'}(1,:,:,:) =  t;
nc{'u'}(2,:,:,:) =  u;
nc{'v'}(2,:,:,:) =  v;
nc{'zeta'}(2,:,:,:) =  zeta;
nc{'ubar'}(2,:,:,:) =  ubar;
nc{'vbar'}(2,:,:,:) =  vbar;
nc{'temp'}(2,:,:,:) =  t;
close(nc)

if nesting, %-----------------------------------------------
 
child_grd=[parent_grd,'.1'];
child_ini=[parent_ini,'.1'];
child_clm=[parent_clm,'.1'];

%
% file_title
%
file_title=['Grid embedded in ',parent_grd,...
            ' - positions in the parent grid: ',num2str(imin),' - ',...
num2str(imax),' - ',...
num2str(jmin),' - ',...
num2str(jmax),'; refinement coefficient : ',num2str(refinecoeff)];
disp(file_title)
%
% Read in the parent grid
%
disp(' ')
disp(' Read in the parent grid...')
nc=netcdf(parent_grd,'r');
xr_parent=nc{'x_rho'}(:);
yr_parent=nc{'y_rho'}(:);
maskr_parent=nc{'mask_rho'}(:);
h_parent=nc{'h'}(:);
f_parent=nc{'f'}(:);
pm_parent=nc{'pm'}(:);
pn_parent=nc{'pn'}(:);
close(nc);
%
% Parent indices
%
[Mp,Lp]=size(h_parent);
[igrd_r,jgrd_r]=meshgrid((1:1:Lp),(1:1:Mp));
[igrd_p,jgrd_p]=meshgrid((1:1:Lp-1),(1:1:Mp-1));
[igrd_u,jgrd_u]=meshgrid((1:1:Lp-1),(1:1:Mp));
[igrd_v,jgrd_v]=meshgrid((1:1:Lp),(1:1:Mp-1));
%
% the children indices
%
ipchild=(imin:1/refinecoeff:imax);
jpchild=(jmin:1/refinecoeff:jmax);
irchild=(imin+0.5-0.5/refinecoeff:1/refinecoeff:imax+0.5+0.5/refinecoeff);
jrchild=(jmin+0.5-0.5/refinecoeff:1/refinecoeff:jmax+0.5+0.5/refinecoeff);
[ichildgrd_p,jchildgrd_p]=meshgrid(ipchild,jpchild);
[ichildgrd_r,jchildgrd_r]=meshgrid(irchild,jrchild);
[ichildgrd_u,jchildgrd_u]=meshgrid(ipchild,jrchild);
[ichildgrd_v,jchildgrd_v]=meshgrid(irchild,jpchild);
%
% interpolations
%
disp(' ')
disp(' Do the interpolations...')
xrchild=interp2(igrd_r,jgrd_r,xr_parent,ichildgrd_r,jchildgrd_r,'cubic');
yrchild=interp2(igrd_r,jgrd_r,yr_parent,ichildgrd_r,jchildgrd_r,'cubic');
fchild=interp2(igrd_r,jgrd_r,f_parent,ichildgrd_r,jchildgrd_r,'cubic');
maskrchild=interp2(igrd_r,jgrd_r,maskr_parent,ichildgrd_r,jchildgrd_r,'cubic');
pmchild=interp2(igrd_r,jgrd_r,pm_parent,ichildgrd_r,jchildgrd_r,'cubic');
pnchild=interp2(igrd_r,jgrd_r,pn_parent,ichildgrd_r,jchildgrd_r,'cubic');
%
% Create the grid file
%
disp(' ')
disp(' Create the grid file...')
[Mp,Lp]=size(xrchild);
M=Mp-1;
L=Lp-1;
nc=netcdf(child_grd, 'clobber');
%%redef(nc);
nc('xi_rho') = Lp;
nc('eta_rho') = Mp;
nc('xi_psi') = L;
nc('eta_psi') = M;
nc('one') = 1;
nc('four') = 4;
nc{'el'} = ncdouble('one');
nc{'xl'} = ncdouble('one');
nc{'spherical'} = ncchar('one');
nc{'h'} = ncdouble('eta_rho', 'xi_rho');
nc{'f'} = ncdouble('eta_rho', 'xi_rho');
nc{'pm'} = ncdouble('eta_rho', 'xi_rho');
nc{'pn'} = ncdouble('eta_rho', 'xi_rho');
nc{'x_rho'} = ncdouble('eta_rho', 'xi_rho');
nc{'y_rho'} = ncdouble('eta_rho', 'xi_rho');
nc{'mask_rho'} = ncdouble('eta_rho', 'xi_rho');
nc{'grd_pos'} = ncint('four');
nc{'refine_coef'} = ncint('one');
%%endef(nc);
nc.title = ncchar(file_title);
nc.title = file_title;
nc.date = ncchar(date);
nc.date = date;
nc.type = ncchar('CROCO grid file');
nc.type = 'CROCO grid file';
%
%  fill the grid file
%
nc{'xl'}(:)=max(max(xrchild))-min(min(xrchild));
nc{'el'}(:)=max(max(yrchild))-min(min(yrchild));
nc{'spherical'}(:)='F';
nc{'h'}(:)=H0;
nc{'f'}(:)=fchild;
nc{'pm'}(:)=refinecoeff*pmchild;
nc{'pn'}(:)=refinecoeff*pnchild;
nc{'refine_coef'}(:)=refinecoeff;
nc{'grd_pos'}(:) = [imin,imax,jmin,jmax];
nc{'x_rho'}(:)=xrchild;
nc{'y_rho'}(:)=yrchild;
nc{'mask_rho'}(:)=1+0*maskrchild;
close(nc);
disp(' ')
disp(['  Size of the child grid:  L = ',...
      num2str(L),' - M = ',num2str(M)])
%
%  read the grid file
%
nc=netcdf(child_grd,'r');
h0=nc{'h'}(:);
f=nc{'f'}(:);
X=nc{'x_rho'}(:);
Y=nc{'y_rho'}(:);
pm=nc{'pm'}(:);
pn=nc{'pn'}(:);
close(nc);
%
% Compute zeta,ubar,vbar,u,v,t for the vortex
%
barocjet
%
%  Create the initial file
%
create_inifile(child_ini,child_grd,title_case,...
               theta_s,theta_b,hc,N,0,'clobber')
nc=netcdf(child_ini,'write');
nc{'u'}(:) =  u; 
nc{'v'}(:) =  v; 
nc{'zeta'}(:) =  zeta; 
nc{'ubar'}(:) =  ubar; 
nc{'vbar'}(:) =  vbar; 
nc{'temp'}(:) =  t; 
close(nc);
%
%  Create and fill the climatology file
%
create_climfile(child_clm,child_grd,title_case,...
                theta_s,theta_b,hc,N,[25 75],100,'clobber')
nc=netcdf(child_clm,'write');
nc{'u'}(1,:,:,:) =  u;
nc{'v'}(1,:,:,:) =  v;
nc{'zeta'}(1,:,:,:) =  zeta;
nc{'ubar'}(1,:,:,:) =  ubar;
nc{'vbar'}(1,:,:,:) =  vbar;
nc{'temp'}(1,:,:,:) =  t;
nc{'u'}(2,:,:,:) =  u;
nc{'v'}(2,:,:,:) =  v;
nc{'zeta'}(2,:,:,:) =  zeta;
nc{'ubar'}(2,:,:,:) =  ubar;
nc{'vbar'}(2,:,:,:) =  vbar;
nc{'temp'}(2,:,:,:) =  t;
close(nc)
%
% agrif_fixedgrids.in file
%
create_agrif_fixedgrids_in(imin,imax,jmin,jmax,refinecoeff)
%

end %nesting %-----------------------------------------------

%
% Plot
%
xr=xr./1000; yr=yr./1000;
X=X./1000; Y=Y./1000;
zu=0.5*(zr(:,:,1:end-1)+zr(:,:,2:end));
xu=0.5*(xr(:,:,1:end-1)+xr(:,:,2:end));
yu=0.5*(yr(:,:,1:end-1)+yr(:,:,2:end));
%
ur=u2rho_2d(squeeze(u(N,:,:)));
vr=v2rho_2d(squeeze(v(N,:,:)));
spd=sqrt(ur.^2+vr.^2);
pcolor(X,Y,spd)
shading flat
axis image
colorbar
hold on
quiver(X,Y,ur,vr,'k')
hold off
title('Velocity')
%
figure
[M,L]=size(Y);
imid=round(M/2);
contourf(squeeze(yr(:,:,imid)),squeeze(zr(:,:,imid)),squeeze(t(:,:,imid)),20);
colorbar
title('Temperature')
%
figure
[M,L]=size(Y);
imid=round(L/2);
contourf(squeeze(yu(:,:,imid)),squeeze(zu(:,:,imid)),squeeze(u(:,:,imid)),10);
colorbar
title('Zonal velocity')
%
figure
pcolor(X,Y,zeta)
shading flat
colorbar
axis image
title('Surface height')
%
u=squeeze(u(N,:,:));
v=squeeze(v(N,:,:));
if nesting,
 nc=netcdf(child_grd);
else
 nc=netcdf(parent_grd);
end
pm=nc{'pm'}(:);
pn=nc{'pn'}(:);
xr=1e-3*nc{'x_rho'}(:);
yr=1e-3*nc{'y_rho'}(:);
[xu,xv,xp]=rho2uvp(xr);
[yu,yv,yp]=rho2uvp(yr);
[fu,fv,fp]=rho2uvp(f);
close(nc);
[vort]=vorticity(u,v,pm,pn);
vort=vort./fp;
figure
map=colormap(jet(20));
map(10:11,:)=1*[1 1 1 ; 1 1 1];
colormap(map)
contourf(xp,yp,vort,20); shading flat
colorbar
caxis([-0.8 0.8])
axis image
title('Vorticity/f')

