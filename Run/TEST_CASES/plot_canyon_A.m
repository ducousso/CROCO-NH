%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Make 2 graphics from the results of the CANYON test case
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

tndx=3;
i=32;
%
% Read data
%
nc=netcdf('canyon_A_his.nc','r');
h=nc{'h'}(:);
x1=nc{'x_rho'}(:);
y1=nc{'y_rho'}(:);
y=squeeze(y1(:,i));
zeta=squeeze(nc{'zeta'}(tndx,:,:));
t=squeeze(nc{'rho'}(tndx,:,:,i));
[N,M]=size(t);
sst=squeeze(nc{'temp'}(tndx,N,:,:));
u=squeeze(nc{'u'}(tndx,N,:,:));
v=squeeze(nc{'v'}(tndx,N,:,:));
theta_s=nc.theta_s(:);
theta_b=nc.theta_b(:);
hc=nc.hc(:);
close(nc);

zr = zlevs(h,zeta,theta_s,theta_b,hc,N,'r');
zr=squeeze(zr(:,:,i));
yr=reshape(y,1,M);
yr=repmat(yr,[N 1])/1000;

%
% First plot
%
figure(1)
contourf(yr,zr,t,[28:0.1:32])
shading flat
colorbar
title('Canyon sigma vertical section')
caxis([28 31])
%
% Second plot
%
figure(2)
contourf(x1(2:end-1,2:end-1)/1000,y1(2:end-1,2:end-1)/1000,...
         100*zeta(2:end-1,2:end-1),[-0.5:0.1:2])
axis image
shading flat
caxis([-0.1 1])
colorbar
hold on
contour(x1(2:end-1,2:end-1)/1000,y1(2:end-1,2:end-1)/1000,...
        h(2:end-1,2:end-1),'k')
hold off
title('Canyon sea surface elevation [cm]')

