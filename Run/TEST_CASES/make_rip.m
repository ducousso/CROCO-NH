%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Build a CROCO RIP configuration.
%  Create a grid file for the Rip Current experiment
%  (for the parent and the child grid).
%  This file is based on make_vortex.m 
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
%  Ref: Weir, B., Uchiyama, Y.. (2010), 
%       A vortes force analysis of the interaction of rip currents
%       and surface gravity wave
%       JGR Vol. 116
%
%  Rachid Benshila, CNRS 2013
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
clear all
close all
%%%%%%%%%%%%%%%%%%%%% USERS DEFINED VARIABLES %%%%%%%%%%%%%%%%%%%%%%%%
%
%  Title
%
title='RIP';
%
%  Names
%
parent_grd='rip_grd.nc';
child_grd='rip_grd.nc.1';
%
% Parameters for the Parent Grid
%
dx=20.;                 % Horizontal resolution 
add_cor=0;              % add coriolis term
theta=0.;               % Latitude (beta-plane)
R=6367442.76;           % Earth radius
%
% Topo parameters
%
xs=150.;                % position of the rip     (m from the coast)
eps=0.1;                % amplitude of the rip    (m)
lambd=256.;             % perturbation lenghscale (m)
nbr_rip=3.;             % number of rips (m)
%
% Nesting parameters
%
nest=0;                 % 0 => no nest 
refinecoeff=3.;
jmin=6;
jmax=32;
imin=22;
imax=37;
%
%
%%%%%%%%%%%%%%%%%%% END USERS DEFINED VARIABLES %%%%%%%%%%%%%%%%%%%%%%%
%
%
% Horzontal Grid
%
lambd=ceil((lambd-1)/dx)*dx;
xmax=lambd*nbr_rip+dx;
x=[0:dx:xmax];
y=x;
dy=dx;
[X,Y]=meshgrid(x,y);
%
% Topo
%
alpha=0.02;
db=80.;
db2=db^2;
h0=-1.5*exp(-5./db2*(X-xs-db).^2)...
+  1.35*(1.+tanh(0.0253*(X-xs)))...
+ 0.0032*(X+1./alpha*log(cosh(alpha*(X-xs))/cosh(alpha*xs)));
pert=eps*cos(2*pi*Y/(lambd)).*exp(-5/db2*(X-xs-db).^2);
h=(1+pert).*h0;
h=h(:,end:-1:1);
%
% Coriolis term (beta plane)
%
if add_cor 
deg2rad=pi/180;
omega=2*pi/(24*3600);
f0=2*omega*sin(deg2rad*theta);
beta=2*omega/R*cos(deg2rad*theta);
f=f0+beta*Y;
else
f=0.;
end
%
%  Create the grid file
%
Mp=length(y);
Lp=length(x);
M=Mp-1;
L=Lp-1;
nc=netcdf(parent_grd, 'clobber');
%%redef(nc);		% for Octave compatiblity
nc('xi_rho') = Lp;
nc('eta_rho') = Mp;
nc('xi_psi') = L;
nc('eta_psi') = M;
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
%% endef(nc);		% for Octave compatiblity
nc.title = ncchar(title);
nc.title = title;
nc.date = ncchar(date);
nc.date = date;
nc.type = ncchar('CROCO grid file');
nc.type = 'CROCO grid file';
%
%  fill the grid file
%
nc{'xl'}(:)=dx*(L-1);
nc{'el'}(:)=dy*(M-1);
nc{'spherical'}(:)='F';
nc{'h'}(:)=h;
nc{'f'}(:)=f;
nc{'pm'}(:)=1/dx;
nc{'pn'}(:)=1/dx;
nc{'x_rho'}(:)=X;
nc{'y_rho'}(:)=Y;
nc{'mask_rho'}(:)=1+0.*Y;
close(nc);
%
if nest
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
hchild=interp2(igrd_r,jgrd_r,h_parent,ichildgrd_r,jchildgrd_r,'cubic');
%
h_coarse=interp2(igrd_r,jgrd_r,h_parent,ichildgrd_r,jchildgrd_r,'nearest');
maskr_coarse=interp2(igrd_r,jgrd_r,maskr_parent,ichildgrd_r,jchildgrd_r,'nearest');
pm_coarse=interp2(igrd_r,jgrd_r,pm_parent,ichildgrd_r,jchildgrd_r,'nearest');
pn_coarse=interp2(igrd_r,jgrd_r,pn_parent,ichildgrd_r,jchildgrd_r,'nearest');
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
nc{'xl'}(:)=dx*(L-1)/refinecoeff;
nc{'el'}(:)=dy*(L-1)/refinecoeff;
nc{'spherical'}(:)='F';
nc{'h'}(:)=hchild;
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
% agrif_fixedgrids.in file
%
create_agrif_fixedgrids_in(imin,imax,jmin,jmax,refinecoeff)
%
%
end
%
% Plot
%
fig1=figure;
mesh(X,Y,-h);
az = -24;
el = 28;
view(az, el);
axis([-1 770 -1 770 -7 1 ]);
xlabel('crosshore direction');
zlabel('depth');
colormap jet;
cmap = colormap;
cmap = flipud(cmap);
colormap(cmap);
colorbar;
if nest
hold on
mesh(X(jmin:jmax,imin:imax),Y(jmin:jmax,imin:imax),-h(jmin:jmax,imin:imax),'EdgeColor','black','Edgealpha',0.2)
hold off
end
%
fig2=figure;
plot(x(1,int64(350/dx):end),-h(1,int64(350/dx):end),'LineWidth',2,'Color','k')
axis([350 800 -6 1])
xlabel('crosshore direction')
ylabel('depth')
hold on
plot(x(1,int64(350/dx):end),-h(int64(lambd/2/dx),int64(350/dx):end),'LineWidth',2,'Color','r')
hold off
%
if nest 
fig3=figure;
mesh(xrchild,yrchild,-hchild);
az = -24;
el = 28;
view(az, el);
axis([ xrchild(1,1) xrchild(1,Lp) yrchild(1,1) yrchild(Mp,1) -7 1 ]);
xlabel('crosshore direction');
zlabel('depth');
colormap jet;
cmap = colormap;
cmap = flipud(cmap);
colormap(cmap);
colorbar;
end
