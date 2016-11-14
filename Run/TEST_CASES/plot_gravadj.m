%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Make 1 plot from the results of the GRAV_ADJ test case
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
%================== User defined parameters ===========================
%
fname     = 'gravadj_his.nc';

nbq       = 1;             % 0/1: hydro / non-hydro case
soliton   = 0;             % 0/1: lock-exchange / soliton

plot_psi  = 0;             % plot streamfunction
makepdf   = 1;             % make pdf file
%
%======================================================================

hFig = figure;

if soliton
 tindex    = [1;40;51];      % size gives subplot number
 AdvancedColormap('swr',20)
else
 tindex    = [1;4;11];
 colormap('lines')
end

nplot=length(tindex);
set(hFig, 'Position', [200 500 700 200*nplot])


for it=1:length(tindex); % --------------- time loop

tndx=tindex(it);

% -------------------------------------
%  --- Read model data ---
% -------------------------------------
nc=netcdf(fname);
tndx=min(tndx,length(nc{'scrum_time'}(:)));
disp(['tndx = ',num2str(tndx)'']);
h=nc{'h'}(:);
x=squeeze(nc{'x_rho'}(2,:));
zeta=squeeze(nc{'zeta'}(tndx,:,:));
t=squeeze(nc{'temp'}(tndx,:,2,:));
w=1000*squeeze(nc{'w'}(tndx,:,2,:));
[N,M]=size(t);
theta_s=nc.theta_s(:);
theta_b=nc.theta_b(:);
hc=nc.hc(:);
close(nc);

zr = zlevs(h,zeta,theta_s,theta_b,hc,N,'r',2);
zr=squeeze(zr(:,2,:));
xr=reshape(x,1,M);
if nbq,
 xr=repmat(xr,[N 1]);
else
 xr=repmat(xr,[N 1])/1000 - 32;
end

psi=zeros(size(w));
for i=2:M;
  psi(:,i)=psi(:,i-1)-w(:,i).*(xr(:,i)-xr(:,i-1));
end

t(t==0)=NaN;

% -------------------------------------
%  --- Plot ---
% -------------------------------------

subplot(length(tindex),1,it)
contourf(xr,zr,t,[15:1:40]);
if plot_psi
 hold on
 contour(xr,zr,psi,[-5:0.5:5],'k');
 hold off
end
caxis([15 38])
if nbq,
 xlabel('X [m]')
else
 xlabel('X [km]')
end
ylabel('Depth [m]')
shading flat
if ~nbq,
 colorbar
 caxis([12 32])
end
if it==1,
 title('Gravitational adjustment')
end

end % ----------------------- time loop

if makepdf
 export_fig -transparent -pdf gravadj.pdf
% print -dpdf gravadj.pdf
% eval('!pdfcrop gravadj.pdf gravadj.pdf')
end




