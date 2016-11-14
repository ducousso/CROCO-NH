%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Make 1 plot from the results of the SEAMOUNT test case
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
tndx=6;
N=1;
nc=netcdf('croco_his.nc','r');
time=(nc{'scrum_time'}(tndx))/(24*3600);
h=nc{'h'}(:);
x=nc{'x_rho'}(:)/1000;
y=nc{'y_rho'}(:)/1000;
u=squeeze(nc{'u'}(tndx,N,:,:));
v=squeeze(nc{'v'}(tndx,N,:,:));
close(nc);
spd=1000*sqrt( u2rho_2d(u).^2+v2rho_2d(v).^2);
contourf(x,y,spd,[0:0.5:5])
axis image
axis([0 500 0 500])
caxis([0 3])
shading flat
hold on
contour(x,y,h,'k')
colorbar
hold off
title(['SEAMOUNT - bottom speed [mm/s] - day = ',num2str(time)])


