%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Make a plot from the results of the VORTEX test case
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
%  Ref: Penven, P., L. Debreu, P. Marchesiello and J.C. McWilliams,
%       Application of the ROMS embedding procedure for the Central 
%      California Upwelling System,  Ocean Modelling, 2006.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
clear all
close all
%
% User defined parameters
%
vname='zeta';
tindex=11;
%
% Caxis depending of the variable name
%
if length(vname)>1
  if vname(1:2)=='te'
    type='r';
    ddd=1;
    cmin=17;
    dc=0.1;
    cmax=22;
    cff=1;
  elseif vname(1:2)=='ze'
    type='r';
    ddd=0;
    cmin=-100;
    dc=5;
    cmax=100;
    cff=100;
  elseif vname(1:2)=='ub'
    type='u';
    ddd=0;
    cmin=-50;
    dc=5;
    cmax=50;
    cff=100;
  elseif vname(1:2)=='vb'
    type='v';
    ddd=0;
    cmin=-50;
    dc=5;
    cmax=50;
    cff=100;
  end
else
  if vname(1)=='u'
    type='u';
    ddd=1;
    cmin=-100;
    dc=20;
    cmax=100;
    cff=100;
  elseif vname(1)=='v'
    type='v';
    ddd=1;
    cmin=-100;
    dc=20;
    cmax=100;
    cff=100;
  end
end
%
% Parent
%
nc=netcdf('vortex_his.nc','r');
N=length(nc('s_rho'));
time=round(nc{'scrum_time'}(tindex)/(24*3600));
disp(['Day : ',num2str(time)])
X=1e-3*nc{'x_rho'}(:);
Y=1e-3*nc{'y_rho'}(:);
if ddd==1
  t1=cff*squeeze(nc{vname}(tindex,N,:,:));
else
  t1=cff*squeeze(nc{vname}(tindex,:,:));
end
close(nc)
[Xu,Xv,Xrp]=rho2uvp(X);
[Yu,Yv,Yrp]=rho2uvp(Y);
if type=='r'
  X1=X;
  Y1=Y;
elseif type=='u'
  X1=Xu;
  Y1=Yu;
elseif type=='v'
  X1=Xv;
  Y1=Yv;
end
%
% Child
%
childhis='vortex_his.nc.1';
nc=netcdf(childhis,'r');
nestvortex=0;
if exist(childhis)
  nestvortex=1;
  X=1e-3*nc{'x_rho'}(:);
  Y=1e-3*nc{'y_rho'}(:);
  if ddd==1
    t=cff*squeeze(nc{vname}(tindex,N,:,:));
  else
    t=cff*squeeze(nc{vname}(tindex,:,:));
  end
  close(nc)
  [Xu,Xv,Xp]=rho2uvp(X);
  [Yu,Yv,Yp]=rho2uvp(Y);
  if type=='r'
    X2=X;
    Y2=Y;
  elseif type=='u'
    X2=Xu;
    Y2=Yu;
  elseif type=='v'
    X2=Xv;
    Y2=Yv;
  end
  Xbox=cat(1,Xp(1:end,1),  ...
                Xp(end,1:end)' ,...
                Xp(end:-1:1,end),...
                Xp(1,end:-1:1)');
  Ybox=cat(1,Yp(1:end,1),  ...
                Yp(end,1:end)' ,...
                Yp(end:-1:1,end),...
                Yp(1,end:-1:1)');
end
%
% Plots
%
%contour(X1,Y1,t1,[cmin:dc:cmax],'r')
contourf(X1,Y1,t1,[cmin:dc:cmax])
xlabel('X [km]')
ylabel('Y [km]')
title([vname,' - Day ',num2str(time)])
if nestvortex==1
  hold on
  contour(X2,Y2,t,[cmin:dc:cmax],'k--')
  plot(Xbox,Ybox,'k');
  hold off
  figure
  tint=interp2(X1,Y1,t1,X2,Y2,'cubic');
  tdiff=tint-t;
  disp(['max difference = ',num2str(max(max(abs(tdiff))),2)])
  disp(['relative difference = ',...
        num2str(100*max(max(abs(tdiff)))/max(max(abs(tint))),2),' %'])
  imagesc(flipud(tdiff))
  colorbar
  title(['Parent - Child : ',vname,' - Day ',num2str(time)])
end
