%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Make 2 graphics from the results of the BASIN test case
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
%  Copyright (c) 2002-2006 by Pierrick Penven 
%  e-mail:Pierrick.Penven@ird.fr  
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
clear all
close all

tndx=11;
j=25;
%
% Read data
%
nc=netcdf('basin_his.nc','r');
h=nc{'h'}(:);
x1=nc{'x_rho'}(:);
y1=nc{'y_rho'}(:);
x=squeeze(x1(j,:));
zeta=squeeze(nc{'zeta'}(tndx,:,:));
t=squeeze(nc{'temp'}(tndx,:,j,:));
[N,M]=size(t);
theta_s=nc.theta_s(:);
theta_b=nc.theta_b(:);
hc=nc.hc(:);
close(nc);

zr=zlevs(h,zeta,theta_s,theta_b,hc,N,'r');
zr=squeeze(zr(:,j,:));
xr=reshape(x,1,M);
xr=repmat(xr,[N 1])/1000;

%
% First plot
%
figure(1)
contourf(xr,zr,t,[0:0.2:5])
shading flat
caxis([0 3])
colorbar
title('Basin temperature [^oC] vertical section')

%
% Second plot
%
figure(2)
contourf(x1(2:end-1,2:end-1)/1000,y1(2:end-1,2:end-1)/1000,...
         100*zeta(2:end-1,2:end-1),[-20:2:20])
axis image
shading flat
caxis([-8 8])
colorbar
hold on
contour(x1/1000,y1/1000,h,'k')
hold off
title('Basin sea surface elevation [cm]')

