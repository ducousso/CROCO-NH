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
fname     = 'swash_his.nc';            % croco file name
g         = 9.81;                      % gravity acceleration (m^2/s)
yindex    = 2;                         % y index
makemovie = 0;                         % make movie using QTWriter
makepdf   = 0;                         % make pdf file
%
%======================================================================

% ---------------------------------------------------------------------
% --- get grid from numerical model ---
% ---------------------------------------------------------------------

nc=netcdf(fname);
tindex=length(nc{'scrum_time'}(:)); % reads last record

if makemovie,
 movObj = QTWriter('swash.mov');
 tstr=1;
 tend=tindex;
else,
 tstr=tindex;
 tend=tstr;
end

hf = figure('position',[500 500 700 300]);
axis tight; set(hf,'DoubleBuffer','on');
%set(gca,'nextplot','replacechildren');

for tindex=tstr:tend % ---------------------------------------------

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
 D=hr+zeta;
 D2d=repmat(D,[N 1]);

 % ---------------------------------------------------------------------
 % --- read/compute numerical model fields (index 1) ---
 % ---------------------------------------------------------------------
 time=nc{'scrum_time'}(tindex);

 % ... num zonal velocity ...                         ---> xu,zru
 u=squeeze(nc{'u'}(tindex,:,yindex,xindex:end));

 % ... num meridional velocity ...                    ---> xr,zr
 %v=squeeze(nc{'v'}(tindex,:,yindex,xindex:end));

 % ... num vertical velocity ...                      ---> xr,zw
 %w=squeeze(nc{'w'}(tindex,:,yindex,xindex:end));

 % ... num temperature ...                            ---> xr,zr
 %t=squeeze(nc{'temp'}(tindex,:,yindex,xindex:end));

 visc=squeeze(nc{'visc3d'}(tindex,:,yindex,xindex:end));


 %============================================================
 % --- plot ---
 %=============================================================
 Dcrit=nc{'Dcrit'}(:)+1.e-6;

 u(:,L)=u(:,L-1);
 visc(:,L)=visc(:,L-1);
 zeta(D<Dcrit)=NaN;
 u(D2d<Dcrit)=NaN;
 visc(D2d<Dcrit)=NaN;

 %cmin=-0.3; cmax=0.3; nbcol=20;
 cmin=0.; cmax=1.; nbcol=20;
 cint=(cmax-cmin)/nbcol;
 map=colormap(jet(nbcol));
 map(nbcol/2  ,:)=[1 1 1];
 map(nbcol/2+1,:)=[1 1 1];
 colormap(map);

 %contourf(xr2d,zr,u,[cmin:cint:cmax]); hold on
 contourf(xr2d,zr,visc,[cmin:cint:cmax]); hold on
 shading flat; colorbar;
 plot(xr,-hr,'color','k','LineWidth',3);
 hn=plot(xr,zeta,'color','r','LineWidth',2);
 grid on
 axis([0 110 -1 0.5])
 caxis([cmin cmax])
 tmin=floor(time/60);
 %title(['SWASH: U at ',num2str(tmin),' min'])
 title(['SWASH: U at ',num2str(time),' sec'])
 hold off

 if makemovie,  
  % Write each frame to the file
  movObj.FrameRate =  5;
  writeMovie(movObj,getframe(hf));
  clf('reset')
 end
%----------------------------------

end % time loop

if makemovie,  
    movObj.Loop = 'loop'; % Set looping flag
    close(movObj);        % Finish writing movie and close file
end

if makepdf
 export_fig -transparent swash.pdf
end

return

time=nc{'scrum_time'}(:);
NT=length(time);
ssh=squeeze(nc{'zeta'}(:,2,:));
h=squeeze(nc{'h'}(2,:));
h2d=repmat(h,[NT 1]);
ssh(h2d+ssh<Dcrit)=NaN;
xr2d=repmat(xr,[NT 1]);
time2d=repmat(time,[1 L]);

figure
pcolor(xr2d,time2d,ssh);
shading flat
colorbar
axis([0 90 75 110])
caxis([-0.1 0.1])

close(nc);




