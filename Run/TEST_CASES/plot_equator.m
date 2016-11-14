%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Make 1 plot from the results of the EQUATOR test case
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
%  Copyright (c) 2007 by Pierrick Penven 
%  e-mail:Pierrick.Penven@ird.fr  
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
clear all
close all
%
tndx=99; 
j=17;   % middle of the basin
i=21;
%
% Read data
%
nc=netcdf('equator_his.nc','r');
h=nc{'h'}(:);
x1=nc{'x_rho'}(:);
y1=nc{'y_rho'}(:);
x=squeeze(x1(j,:));
zeta=squeeze(nc{'zeta'}(tndx,:,:));
t=squeeze(mean(nc{'temp'}(tndx,:,j:j+1,:),2));
t2=squeeze(mean(mean(nc{'temp'}(:,:,j:j+1,i:i+1),4),3));
time=nc{'scrum_time'}(:);
[N,L]=size(t);
sst=squeeze(nc{'temp'}(tndx,N,:,:));
u=u2rho_2d(squeeze(nc{'u'}(tndx,N,:,:)));
v=v2rho_2d(squeeze(nc{'v'}(tndx,N,:,:)));
theta_s=nc.theta_s(:);
theta_b=nc.theta_b(:);
hc=nc.hc(:);
close(nc);
%
zr=zlevs(h,zeta,theta_s,theta_b,hc,N,'r');
z2=squeeze(mean(mean(zr(:,j:j+1,i:i+1),3),2));
zr=squeeze(zr(:,j,:));
xr=reshape(x,1,L);
xr=repmat(xr,[N 1])/1e5;
%
% First plot
%
figure(1)
[C4,h4]=contour(1980+time/(360*24*3600),z2,t2',[4:2:30],'k');
axis([-inf inf -300 0])
clabel(C4,h4)
xlabel('Time (years)')
ylabel('Depth (m)')
title('Time evolution from 10^{o}C isothermal conditions')
%
% Second plot
%
figure(2)
[C1,h1]=contour(xr(:,2:end-2),zr(:,2:end-2),t(:,2:end-2),[5:1:19],'k');
if isfinite(C1)
  clabel(C1,h1)
end
hold on
[C2,h2]=contour(xr(:,2:end-2),zr(:,2:end-2),t(:,2:end-2),[20 20],'r');
clabel(C2,h2)
set(h2,'LineWidth',1.5)
[C3,h3]=contour(xr(:,2:end-2),zr(:,2:end-2),t(:,2:end-2),[21:1:30],'k');
clabel(C3,h3)
%shading flat
axis([-inf inf -300 0])
hold off
xlabel('Longitude')
ylabel('Depth (m)')
title('Temperature [^oC] Section Equator')
%
% Third plot
%
figure(3)
subplot(2,1,1)
pcolor(x1(2:end-1,2:end-1)/1e5,y1(2:end-1,2:end-1)/1e5,...
         sst(2:end-1,2:end-1))
axis image
shading flat
caxis([10 25])
colorbar
xlabel('Longitude')
ylabel('Latitude')
title('SST [^oC]')
subplot(2,1,2)
sst=100*sqrt(u.^2+v.^2);
pcolor(x1(2:end-1,2:end-1)/1e5,y1(2:end-1,2:end-1)/1e5,...
         sst(2:end-1,2:end-1))
axis image
shading flat
caxis([0 150])
colorbar
hold on
quiver(x1(2:2:end-1,2:2:end-1)/1e5,y1(2:2:end-1,2:2:end-1)/1e5,...
         u(2:2:end-1,2:2:end-1),v(2:2:end-1,2:2:end-1),'k')
xlabel('Longitude')
ylabel('Latitude')
title('Speed [cm.s^{-1}]')
