%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Make plot from the results of the SHOREFACE test case
% 
%  Further Information:  
%  http://www.crocoagrif.org/
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
%  Ref: Penven, P., L. Debreu, P. Marchesiello and J.C. McWilliams,
%       Application of the ROMS embedding procedure for the Central 
%      California Upwelling System,  Ocean Modelling, 2006.
%
%  Patrick Marchesiello, IRD 2013
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
clear all
close all
%================== User defined parameters ===========================
%
% --- model params ---
%
fname     = 'shoreface_his.nc';    % croco file name
yindex    = 3;                     % y index
makepdf   = 0;                     % make pdf file
%
%======================================================================

% ---------------------------------------------------------------------
% --- get grid from numerical model ---
% ---------------------------------------------------------------------

nc=netcdf(fname,'r');
tindex=length(nc{'scrum_time'}(:)); % reads last record

%
% horizontal grid
hr=squeeze(nc{'h'}(yindex,:));
xindex=1;
hr=hr(xindex:end);
L=length(hr);
xr=squeeze(nc{'x_rho'}(yindex,xindex:end));
yr=squeeze(nc{'y_rho'}(yindex,xindex:end));
dx=xr(2)-xr(1);
%
% vertical grid
N=length(nc('s_rho'));
theta_s=nc.theta_s(:); 
theta_b=nc.theta_b(:); 
hc=nc.hc(:); 
zeta=squeeze(nc{'zeta'}(tindex,yindex,xindex:end));
zr=squeeze(zlevs(hr,zeta,theta_s,theta_b,hc,N,'r',2));
dzr=zr(2:end,:)-zr(1:end-1,:);               % ---> zw(2:N,:)
zru=0.5*(zr(:,1:end-1)+zr(:,2:end));
dzru=zru(2:end,:)-zru(1:end-1,:);            % ---> zwu(2:N,:)
zw=squeeze(zlevs(hr,zeta,theta_s,theta_b,hc,N,'w',2));
dzw=zw(2:end,:)-zw(1:end-1,:);               % ---> zr
zwu=0.5*(zw(:,1:end-1)+zw(:,2:end));
dzwu=zwu(2:end,:)-zwu(1:end-1,:);            % ---> zru
%
xr2d=repmat(xr,[N 1]);
xw2d=repmat(xr,[N+1 1]);
D=hr+zeta;
D2d=repmat(D,[N 1]);

% ---------------------------------------------------------------------
% --- read/compute numerical model fields (index 1) ---
% --------------------------------------------------------------------
time=nc{'scrum_time'}(tindex)/86400;

zeta1=zeta;

% ... zonal velocity ...                         ---> xu,zru
u1=squeeze(nc{'u'}(tindex,:,yindex,xindex:end));

% ... meridional velocity ...                    ---> xr,zr
v1=squeeze(nc{'v'}(tindex,:,yindex,xindex:end));

% ... vertical velocity ...                      ---> xr,zw
w1=squeeze(nc{'w'}(tindex,:,yindex,xindex:end));

% ... temperature ...                            ---> xr,zr
t1=squeeze(nc{'temp'}(tindex,:,yindex,xindex:end));

% ... vertical viscosity/diffusivity
Akv=squeeze(nc{'AKv'}(tindex,:,yindex,xindex:end));
Akt=squeeze(nc{'AKt'}(tindex,:,yindex,xindex:end));


% ... wave setup ...  
sup=squeeze(nc{'sup'}(tindex,yindex,xindex:end));

% ... zonal Stokes dritf                         ---> xu,zru
ust=squeeze(nc{'ust'}(tindex,:,yindex,xindex:end));

% ... meridional Stokes drift ...                ---> xr,zr
vst=squeeze(nc{'vst'}(tindex,:,yindex,xindex:end));

% ... vertical Stokes drift ...                  ---> xr,zw
wst=squeeze(nc{'wst'}(tindex,:,yindex,xindex:end));

% eddy viscosity due to depth-induced wave breaking
Akb=squeeze(nc{'Akb'}(tindex,:,yindex,xindex:end));

% eddy diffusivity due to primary waves
Akw=squeeze(nc{'Akw'}(tindex,:,yindex,xindex:end));

close(nc)

%============================================================
% --- plot ---
%=============================================================
Dcrit=0.2;

xr=1.e-3*xr-1;
xr2d=1.e-3*xr2d-1;
u1(:,L)=u1(:,L-1);
zeta1(D<Dcrit)=NaN;
u1(D2d<Dcrit)=NaN;
v1(D2d<Dcrit)=NaN;
ust(:,L)=ust(:,L-1);
Akv(:,1)=NaN;
Akt(:,1)=NaN;
Akb(:,1)=NaN;
Akw(:,1)=NaN;

%
% Surface elevation + wave setup
% 
figure('Position',[100 100 700 400])
xmin=-1; xmax=-0; zmin=-0.1; zmax=0.3;
plot(xr,zeta1+sup,'color','g','LineWidth',3);
grid on
axis([xmin xmax zmin zmax])
thour=floor(time*24);
title(['SHOREFACE: Wave Setup at Time ',num2str(thour),' hour'])
hold off

if makepdf
 print -dpdf shoreface_z.pdf
 eval('!pdfcrop shoreface_z.pdf shoreface_z.pdf')
end

%-----------------------------------
%
% Eulerian velocities u,v,w
%
figure('Position',[100 150 500 750])

subplot(3,1,1)
cmin=-0.5; cmax=0.5; nbcol=40;
cint=(cmax-cmin)/nbcol;
map=colormap(jet(nbcol));
map(nbcol/2  ,:)=[1 1 1];
map(nbcol/2+1,:)=[1 1 1];
colormap(map);
contourf(xr2d,zr,u1,[cmin:cint:cmax]); hold on
shading flat; colorbar;
plot(xr,-hr,'color','k','LineWidth',3);
plot(xr,zeta1,'color','g','LineWidth',3);
grid on
axis([-Inf Inf -Inf 1])
caxis([cmin cmax])
thour=floor(time*24);
title(['SHOREFACE: U at Time ',num2str(thour),' hour'])
hold off

subplot(3,1,2)
cmin=-1.2; cmax=1.2; nbcol=40;
cint=(cmax-cmin)/nbcol;
map=colormap(jet(nbcol));
map(nbcol/2  ,:)=[1 1 1];
map(nbcol/2+1,:)=[1 1 1];
colormap(map);
contourf(xr2d,zr,v1,[cmin:cint:cmax]); hold on
shading flat; colorbar;
plot(xr,-hr,'color','k','LineWidth',3);
plot(xr,zeta1,'color','g','LineWidth',3);
grid on
axis([-Inf Inf -Inf 1])
caxis([cmin cmax])
thour=floor(time*24);
title(['SHOREFACE: V at Time ',num2str(thour),' hour'])
hold off

subplot(3,1,3)
cmin=-5.e-3; cmax=5.e-3; nbcol=40;
cint=(cmax-cmin)/nbcol;
map=colormap(jet(nbcol));
map(nbcol/2  ,:)=[1 1 1];
map(nbcol/2+1,:)=[1 1 1];
colormap(map);
contourf(xr2d,zr,w1,[cmin:cint:cmax]); hold on
shading flat; colorbar;
plot(xr,-hr,'color','k','LineWidth',3);
plot(xr,zeta1,'color','g','LineWidth',3);
grid on
axis([-Inf Inf -Inf 1])
caxis([cmin cmax])
thour=floor(time*24);
title(['SHOREFACE: W at Time ',num2str(thour),' hour'])
hold off

if makepdf
 print -dpdf shoreface_u.pdf
 eval('!pdfcrop shoreface_u.pdf shoreface_u.pdf')
end

%-----------------------------------
%
% Stokes drift ust,vst,wst
%
figure('Position',[100 150 500 750])

subplot(3,1,1)
cmin=-0.2; cmax=0.2; nbcol=40;
cint=(cmax-cmin)/nbcol;
map=colormap(jet(nbcol));
map(nbcol/2  ,:)=[1 1 1];
map(nbcol/2+1,:)=[1 1 1];
colormap(map);
contourf(xr2d,zr,ust,[cmin:cint:cmax]); hold on
shading flat; colorbar;
plot(xr,-hr,'color','k','LineWidth',3);
plot(xr,zeta1,'color','g','LineWidth',3);
grid on
axis([-Inf Inf -Inf 1])
caxis([cmin cmax])
thour=floor(time*24);
title(['SHOREFACE: UST at Time ',num2str(thour),' hour'])
hold off

subplot(3,1,2)
cmin=-2.e-2; cmax=2.e-2; nbcol=40;
cint=(cmax-cmin)/nbcol;
map=colormap(jet(nbcol));
map(nbcol/2  ,:)=[1 1 1];
map(nbcol/2+1,:)=[1 1 1];
colormap(map);
contourf(xr2d,zr,vst,[cmin:cint:cmax]); hold on
shading flat; colorbar;
plot(xr,-hr,'color','k','LineWidth',3);
plot(xr,zeta1,'color','g','LineWidth',3);
grid on
axis([-Inf Inf -Inf 1])
caxis([cmin cmax])
thour=floor(time*24);
title(['SHOREFACE: VST at Time ',num2str(thour),' hour'])
hold off

subplot(3,1,3)
cmin=-2.e-3; cmax=2.e-3; nbcol=40;
cint=(cmax-cmin)/nbcol;
map=colormap(jet(nbcol));
map(nbcol/2  ,:)=[1 1 1];
map(nbcol/2+1,:)=[1 1 1];
colormap(map);
contourf(xr2d,zr,wst,[cmin:cint:cmax]); hold on
shading flat; colorbar;
plot(xr,-hr,'color','k','LineWidth',3);
plot(xr,zeta1,'color','g','LineWidth',3);
grid on
axis([-Inf Inf -Inf 1])
caxis([cmin cmax])
thour=floor(time*24);
title(['SHOREFACE: WST at Time ',num2str(thour),' hour'])
hold off

if makepdf
 print -dpdf shoreface_ust.pdf
 eval('!pdfcrop shoreface_ust.pdf shoreface_ust.pdf')
end
%-----------------------------------
%
% Turbulent and wave-induced Viscosity/diffusivity
%
figure('Position',[100 150 500 750])

subplot(3,1,1)
cmin=0; cmax=0.05; nbcol=40;
cint=(cmax-cmin)/nbcol;
map=colormap(jet(nbcol));
colormap(map);
contourf(xw2d,zw,Akv,[cmin:cint:cmax]); hold on
shading flat; colorbar;
plot(xr,-hr,'color','k','LineWidth',3);
grid on
axis([-Inf Inf -Inf 1])
caxis([cmin cmax])
thour=floor(time*24);
title(['SHOREFACE: Akv at Time ',num2str(thour),' hour'])
hold off

subplot(3,1,2)
cmin=0; cmax=0.05; nbcol=40;
cint=(cmax-cmin)/nbcol;
map=colormap(jet(nbcol));
colormap(map);
contourf(xw2d,zw,Akb,[cmin:cint:cmax]); hold on
shading flat; colorbar;
plot(xr,-hr,'color','k','LineWidth',3);
grid on
axis([-Inf Inf -Inf 1])
caxis([cmin cmax])
thour=floor(time*24);
title(['SHOREFACE: Akb at Time ',num2str(thour),' hour'])
hold off

subplot(3,1,3)
cmin=0; cmax=1.e-5; nbcol=40;
cint=(cmax-cmin)/nbcol;
map=colormap(jet(nbcol));
colormap(map);
contourf(xw2d,zw,Akw,[cmin:cint:cmax]); hold on
shading flat; colorbar;
plot(xr,-hr,'color','k','LineWidth',3);
grid on
axis([-Inf Inf -Inf 1])
caxis([cmin cmax])
thour=floor(time*24);
title(['SHOREFACE: Akw at Time ',num2str(thour),' hour'])
hold off

if makepdf
 print -dpdf shoreface_Ak.pdf
 eval('!pdfcrop shoreface_Ak.pdf shoreface_Ak.pdf')
end



