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
fname     = 'thacker_his.nc';  % croco file name
%fname     = 'thacker_his_1D_ndtfast40.nc';
g         = 9.81;                  % gravity acceleration (m^2/s)
x0        = 101;
y0        = 1;                     % x and y origins
makemovie = 0;                     % make movie using QTWriter
makepdf   = 0;                     % make pdf file
%
%======================================================================

% ---------------------------------------------------------------------
% --- get grid from numerical model ---
% ---------------------------------------------------------------------

nc=netcdf(fname);
tindex=length(nc{'scrum_time'}(:)); % reads last record

if makemovie,
 movObj = QTWriter('thacker.mov');
 tstr=1;
 tend=tindex;
else,
 tstr=tindex;
 tend=tstr;
end

hf = figure;
axis tight; set(hf,'DoubleBuffer','on');
set(gca,'nextplot','replacechildren');

for tindex=tstr:tend % ---------------------------------------------

%
% horizontal grid
 hr=squeeze(nc{'h'}(y0,:));
 xindex=1;
 hr=hr(xindex:end);
 L=length(hr);
 xr=squeeze(nc{'x_rho'}(y0,xindex:end));
 yr=squeeze(nc{'y_rho'}(y0,xindex:end));
 dx=xr(2)-xr(1);
%
% vertical grid
 N=length(nc('s_rho'));
 theta_s=nc.theta_s(:); 
 theta_b=nc.theta_b(:); 
 hc=nc.hc(:); 
 zeta=squeeze(nc{'zeta'}(tindex,y0,xindex:end));
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

 zeta1=zeta;

 % ... num zonal velocity ...                         ---> xu,zru
 u1=squeeze(nc{'u'}(tindex,:,y0,xindex:end));

 % ... num meridional velocity ...                    ---> xr,zr
 v1=squeeze(nc{'v'}(tindex,:,y0,xindex:end));

 % ... num vertical velocity ...                      ---> xr,zw
 w1=squeeze(nc{'w'}(tindex,:,y0,xindex:end));

 % ... num temperature ...                            ---> xr,zr
 t1=squeeze(nc{'temp'}(tindex,:,y0,xindex:end));

 % ---------------------------------------------------------------------
 % --- compute analytical solutions (index 2) ---
 % ---------------------------------------------------------------------

 eta = 0.1;                % --> nondimensional periodic amplitude
 D0  = 10;                 % --> max depth at rest
 Lt  = 80.e3;              % --> distance at psi points
 f   = nc{'f'}(y0,1);      % --> 1.e-4;

 omega=sqrt(f^2 +2*g*D0/Lt^2);

 u2    =   -eta*omega*Lt*sin(omega*time)*ones(size(u1));
 v2    = -f*eta*omega*Lt*cos(omega*time)*ones(size(v1));
 zeta2 =        2*eta*D0*cos(omega*time)* ...
                   (xr./Lt-0.5*eta*cos(omega*time)) ...
                                    .*ones(size(zeta1));

 %============================================================
 % --- plot ---
 %=============================================================
 Dcrit=0.1;

 xr=1.e-3*xr;
 xr2d=1.e-3*xr2d;
 u1(:,L)=u1(:,L-1);
 u2(:,L)=u2(:,L-1);
 zeta1(D<Dcrit)=NaN;
 u1(D2d<Dcrit)=NaN;
 zeta2(zeta2<-hr)=NaN;

 cmin=-2; cmax=2; nbcol=20;
 cint=(cmax-cmin)/nbcol;
 map=colormap(jet(nbcol));
 map(nbcol/2  ,:)=[1 1 1];
 map(nbcol/2+1,:)=[1 1 1];
 colormap(map);

 contourf(xr2d,zr,u1-u2,[cmin:cint:cmax]);  hold on
 shading flat; colorbar;
 ha=plot(xr,zeta2,'color','g','LineWidth',2);
 hn=plot(xr,zeta1,'color','r','LineWidth',2);
 legend([ha,hn],'Analytical','Numerical')
 plot(xr,-hr,'color','k','LineWidth',3); hold off
 axis([-100 100 -10 5])
 caxis([cmin cmax])
 grid on
 thour=floor(time/3600);
 title(['THACKER: U Err at Time ',num2str(thour),' hour'])

% D1=~isnan(zeta1); D2=~isnan(zeta2);
% D=D1; if sum(D1)>sum(D2), D=D2; end
% std(zeta2(D)-zeta1(D))

 if makemovie,  
  % Write each frame to the file
  movObj.FrameRate =  5;
  writeMovie(movObj,getframe(hf));
  clf('reset')
 end
%----------------------------------

end % time loop

if makepdf
 print -dpdf thacker.pdf
 eval('!pdfcrop thacker.pdf thacker_72h.pdf')
end

if makemovie,  
    movObj.Loop = 'loop'; % Set looping flag
    close(movObj);        % Finish writing movie and close file
end

%============================================================
% --- plot time series at center point ---
%=============================================================

t0=nc{'scrum_time'}(1:tindex);
u10=squeeze(nc{'u'}(1:tindex,3,y0,x0));
u20=-eta*omega*Lt*sin(omega*t0);
%u20=-f*eta*omega*Lt*cos(omega*t0*86400);

figure
t0=t0/86400;
plot(t0,u20,'k',t0,u10,'r')
legend('Analytical','Numerical')
grid on
if makepdf
 print -dpdf thacker.pdf
 eval('!pdfcrop thacker.pdf thacker_Useries.pdf')
end

%============================================================
% --- plot sea level extremes at 6, 9, 12 h ---
%============================================================

%return

if tindex<64*2; 
 close(nc);
 return
end

xr=xr*1.e3;
tstr=1;

tindex=tstr+60*2; %8*2;
time=nc{'scrum_time'}(tindex);
zm_6h=squeeze(nc{'zeta'}(tindex,y0,xindex:end));
za_6h=2*eta*D0*cos(omega*time)* ...
            (xr./Lt-0.5*eta*cos(omega*time)) ...
                              .*ones(size(zeta1));

D=hr+zm_6h;
zm_6h(D<Dcrit)=NaN;
za_6h(za_6h<-hr)=NaN;

tindex=tstr+62*2; %10*2-1;
time=nc{'scrum_time'}(tindex);
zm_9h=squeeze(nc{'zeta'}(tindex,y0,xindex:end));
za_9h=2*eta*D0*cos(omega*time)* ...
            (xr./Lt-0.5*eta*cos(omega*time)) ...
                              .*ones(size(zeta1));

D=hr+zm_9h;
zm_9h(D<Dcrit)=NaN;
za_9h(za_9h<-hr)=NaN;

tindex=tstr+64*2; %11*2;
time=nc{'scrum_time'}(tindex);
zm_12h=squeeze(nc{'zeta'}(tindex,y0,xindex:end));
za_12h=2*eta*D0*cos(omega*time)* ...
            (xr./Lt-0.5*eta*cos(omega*time)) ...
                              .*ones(size(zeta1));

D=hr+zm_12h;
zm_12h(D<Dcrit)=NaN;
za_12h(za_12h<-hr)=NaN;

xr=xr*1.e-3;
figure
plot(xr,-hr,'color','k','LineWidth',4); hold on;
plot(xr,za_6h,'k--',xr,zm_6h,'k-','LineWidth',1);
text(60,zm_6h(160)+0.2,'60:00');
plot(xr,za_9h,'k--',xr,zm_9h,'k-','LineWidth',1);
text(60,zm_9h(160)+0.2,'62:00');
plot(xr,za_12h,'k--',xr,zm_12h,'k-','LineWidth',1);
text(60,zm_12h(160)+0.2,'64:00');
axis([20 100 -3 3])
grid on
hold off
title(['THACKER: numerical and analytical model sea level'])

if makepdf
 print -dpdf thacker.pdf
 eval('!pdfcrop thacker.pdf thacker_zcomp.pdf')
 eval('!rm thacker.pdf')
end


close(nc);




