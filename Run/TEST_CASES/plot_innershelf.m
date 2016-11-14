%======================================================================
%
%     ---             INNERSHELF Test Case                ---     
%     --- Compare Ekman 2D theorical model solution with  ---
%     --- CROCO numerical solution : vg, psi/|Uek| & v     ---
%
% References:
%
% Estrade P., P. Marchesiello, A. Colin de Verdiere, C. Roy, 2008: 
%  Cross-shelf structure of coastal upwelling : a two-dimensional 
%  expansion of Ekman's theory and a mechanism for innershelf upwelling
%  shut down. Journal of Marine Research, 66, 589-616.
%
% Marchesiello P., and P. Estrade, 2010: Upwelling limitation by 
%  geostrophic onshore flow. Journal of Marine Research, 68, 37-62.
%
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
% Philippe Estrade & Patrick Marchesiello - 2005, 2012
%======================================================================
clear all
close all
%================== User defined parameters ===========================
%
% --- model params ---
%
fname  = 'inner_avg.nc';  % croco file name
tauy   = 0.07;            % alongshore wind-stress (Pa)
taux   = 0;               % onshore wind-stress (Pa)
ug0    = 2;               % onshore geostrophic flow (cm/s)
g      = 9.81;            % gravity acceleration (m^2/s)
rho0   = 1000;            % reference density (kg/m^3)
%
% --- fig params ---
%
hoDmax = 2.5;             % index of offshore limit h/D
                          % h is bottom depth; D is Ekman depth
tick=[-2.4:0.2:-0.2];
lx=18;
ly=24;
dxleft=2;dxright=0.25;
dyup=2.5;dybot=1.5;dyint=0.5;
subfigratio=2/3;
%======================================================================

% ---------------------------------------------------------------------
% --- get grid, Av & f from numerical model ---
% ---------------------------------------------------------------------

nc=netcdf(fname,'r');
tindex=length(nc{'scrum_time'}(:)); % reads last record
yindex=3;                           % y index (with periodic conditions
                                    %          all fields are constant in y)
Av=nc.Akv_bak(:);                   % viscosity
f=nc{'f'}(yindex,1);                % Coriolis frequency
D=pi*sqrt(2*Av/abs(f));             % Ekman depth
%
% horizontal grid
hr=squeeze(nc{'h'}(yindex,:));
tmp=find(hr/D>hoDmax);         % reduce offshore grid
xindex=tmp(end);
hr=hr(xindex:end);
L=length(hr);
hu=0.5*(hr(1:end-1)+hr(2:end));
xr=squeeze(nc{'x_rho'}(yindex,xindex:end));
dx=xr(2)-xr(1);
xu=0.5*(xr(1:end-1)+xr(2:end));
%
% vertical grid
N=length(nc('s_rho'));
theta_s=nc.theta_s(:); 
theta_b=nc.theta_b(:); 
hc=nc.hc(:); 
zeta=squeeze(nc{'zeta'}(tindex,yindex,xindex:end));
zr=squeeze(zlevs(hr,zeta,theta_s,theta_b,hc,N,'r'));
dzr=zr(2:end,:)-zr(1:end-1,:);               % ---> zw(2:N,:)
zru=0.5*(zr(:,1:end-1)+zr(:,2:end));
dzru=zru(2:end,:)-zru(1:end-1,:);            % ---> zwu(2:N,:)
zw=squeeze(zlevs(hr,zeta,theta_s,theta_b,hc,N,'w'));
dzw=zw(2:end,:)-zw(1:end-1,:);               % ---> zr
zwu=0.5*(zw(:,1:end-1)+zw(:,2:end));
dzwu=zwu(2:end,:)-zwu(1:end-1,:);            % ---> zru
%
% topographic slope
hx=zeros(size(hr));
hx(2:end-1)=(hu(2:end)-hu(1:end-1))./ ...
            (xu(2:end)-xu(1:end-1));
hx=repmat(hx,[N+1 1]);

% ---------------------------------------------------------------------
% --- read/compute numerical model fields (index 1) ---
% ---------------------------------------------------------------------

% ... num geostrophic velocity vg1 = g/f dzeta ...   ---> xu
vg1=g/f/dx*(zeta(2:end)-zeta(1:end-1));

% ... num zonal velocity ...                         ---> xu,zru
u1=squeeze(nc{'u'}(tindex,:,yindex,xindex:end));

% ... num meridional velocity ...                    ---> xr,zr
v1=squeeze(nc{'v'}(tindex,:,yindex,xindex:end));

% ... num vertical velocity ...                      ---> xr,zw
w1=squeeze(nc{'w'}(tindex,:,yindex,xindex:end));

% ... num stream function ...                        ---> xu,zwu
psi1=zeros(N+1,L-1);
for k=1:N
  psi1(k+1,:)=psi1(k,:)-u1(k,:).*dzwu(k,:);
end

close(nc);

% ---------------------------------------------------------------------
% --- compute theorical fields (index 2) on rho and u grids ---
% ---------------------------------------------------------------------

v0=pi*tauy/(rho0*abs(f)*D);
ug=ug0*0.01;
gamma=f/abs(f);
i=complex(0,1);
c=(1+i*gamma)*pi/D;
cu0=(1-gamma*i)*pi*complex(taux,tauy)/(rho0*abs(f)*D);
%
% ... compute geostrophic velocities ...  ---> xr
qsir=pi*hr/D;
alphar=(cosh(qsir).^2).*(cos(qsir).^2)+(sinh(qsir).^2).*(sin(qsir).^2);
s1r=cosh(qsir).*cos(qsir)./alphar;
s2r=sinh(qsir).*sin(gamma*qsir)./alphar;
t1r=cosh(qsir).*sinh(qsir)./alphar;
t2r=cos(qsir).*sin(gamma*qsir)./alphar;
f1r=sinh(qsir).*cos(qsir);
f2r=cosh(qsir).*sin(gamma*qsir);
vgr=2*pi/(rho0*f*D)*((1-s1r)*tauy+s2r*taux)-ug.*(t1r+gamma*t2r-2*qsir);
vgr=vgr./(gamma*t1r-t2r);
ugr=hr*0+ug;
cugr=complex(ugr,vgr);
func=2 + (cosh(qsir).*sin(qsir)-sinh(qsir).*cos(qsir))./(t1r-t2r)./alphar ... 
       - 2*(1-s1r).*(cosh(qsir).^2-cos(qsir).^2)./alphar./((t1r-t2r).^2);
%                                    ...  ---> xu
qsiu=pi*hu/D;
alphau=(cosh(qsiu).^2).*(cos(qsiu).^2)+(sinh(qsiu).^2).*(sin(qsiu).^2);
s1u=cosh(qsiu).*cos(qsiu)./alphau;
s2u=sinh(qsiu).*sin(gamma*qsiu)./alphau;
t1u=cosh(qsiu).*sinh(qsiu)./alphau;
t2u=cos(qsiu).*sin(gamma*qsiu)./alphau;
f1u=sinh(qsiu).*cos(qsiu);
f2u=cosh(qsiu).*sin(gamma*qsiu);
vgu=2*pi/(rho0*f*D)*((1-s1u)*tauy+s2u*taux)-ug.*(t1u+gamma*t2u-2*qsiu);
vgu=vgu./(gamma*t1u-t2u);
ugu=hu*0+ug;
cugu=complex(ugu,vgu);

% ... compute total horizontal velocity ...   % ---> zr
wer=zeros(N,L);wer=complex(wer,wer);
for j=1:L
  wer(:,j)=cu0*sinh(c*(zr(:,j)+hr(j)))/cosh(c*hr(j))- ...
           cugr(j)*cosh(c*zr(:,j))/cosh(c*hr(j));
end
                                              % ---> zu
weu=zeros(N,L-1);weu=complex(weu,weu);
for j=1:L-1
  weu(:,j)=cu0*sinh(c*(zru(:,j)+hu(j)))/cosh(c*hu(j))- ...
          cugu(j)*cosh(c*zru(:,j))/cosh(c*hu(j));
end

% ... compute stream function ...             % ---> zwu
psi2=zeros(N+1,L-1);psi2=complex(psi2,psi2); 
for j=1:L-1
  psi2(:,j)=cu0*(1-cosh(c*(zwu(:,j)+hu(j)))/cosh(c*hu(j)))+ ...
                    cugu(j)*sinh(c*zwu(:,j))/cosh(c*hu(j));
  psi2(:,j)=psi2(:,j)/c-cugu(j)*zwu(:,j);
end
psi2=real(psi2);

% ... compute vertical velocity ...           % ---> zw
w2=zeros(N+1,L);w2=complex(w2,w2);
for j=1:L
  w2(:,j)=-sinh(c*zw(:,j))*(s1r(j)-i*s2r(j))* ...
   ((s1r(j)-i*s2r(j))*(1+i+2*i*vgr(j)/2/v0*(f1r(j)+i*f2r(j)))-(1+i)*func(j));
end
w2=v0*hx.*real(w2);

%============================================================
% --- plot ---
%=============================================================

figure('units','centimeters','position',[1 1 lx ly],'paperpositionmode','auto')
dy2=(ly-2*dyint-dyup-dybot)/(2+subfigratio);dy1=dy2*subfigratio;
dx=lx-dxleft-dxright;

%
% geostrophic velocity ---> xu
%
subplot(3,1,1)
hold on
tmp=plot(-hu/D,vgu);
set(tmp,'linewidth',3)
set(tmp,'color',[0.7 0.7 0.7])
tmp=plot(-hu/D,vg1,'k--');
set(tmp,'linewidth',2)
hold off
axis([-hoDmax -hr(end)/D -0.2 0.2])
set(gca,'box','on')
set(gca,'fontsize',16)
set(gca,'ytick',[-0.2:0.1:0.2])
set(gca,'yticklabel','|-0.1|0|0.1|')
set(gca,'xtick',tick)
set(gca,'xticklabel','')
grid on
h=text(-2.3,-0.,'$$v_G [m/s]$$','fontsize',18);
set(h,'Interpreter','latex','Fontsize',20);
legend('analytical','numerical       ','Location','SouthEast')
ga1=gca;

%
% normalized stream function ---> xu,zru
%
subplot(3,1,2)
hold on
% artifice for legend
tmp=plot([-hoDmax -hu(end)/D],[100 100]);
set(tmp,'linewidth',3)
set(tmp,'color',[0.7 0.7 0.7])
tmp=plot([-hoDmax -hu(end)/D],[101 101],'k:');
set(tmp,'linewidth',2)
set(gca,'fontsize',16)
legend('analytical','numerical','Location','SouthEast')
%
tmpx=repmat(-hu/D,[N+1 1]);
Uek=abs(tauy/rho0/f);
psirange1=[10:10:150];
psirange2=[-150:10:-10];
%
[c,tmp]=contour(tmpx,zwu/D,-100*psi2/Uek,psirange1,'k');
set(tmp,'linewidth',3)
set(tmp,'color',[1 1 1]*0.7)
[c,tmp]=contour(tmpx,zwu/D,-100*psi2/Uek,psirange2,'k:');
set(tmp,'linewidth',3)
set(tmp,'color',[1 1 1]*0.7)
%
[c,tmp]=contour(tmpx,zwu/D,-100*psi1/Uek,psirange1,'k');
set(tmp,'linewidth',2)
clabel(c,tmp)
[c,tmp]=contour(tmpx,zwu/D,-100*psi1/Uek,psirange2,'k:');
set(tmp,'linewidth',2)
clabel(c,tmp)
%
plot(-hr/D,-hr/D,'k')
hold off
h=text(-1,-1.5,'$$\psi / U_{ek} [\%]$$','fontsize',18);
set(h,'Interpreter','latex','Fontsize',20);
axis([-hoDmax -hu(end)/D -hoDmax zr(N,1)/D])
set(gca,'box','on')
set(gca,'xtick',tick)
set(gca,'xticklabel','')
set(gca,'ytick',get(gca,'xtick'))
set(gca,'yticklabel','|-2.2||-1.8||-1.4||-1||-0.6||-0.2|')
ylabel 'z/D'
grid on
ga2=gca;

%
% normalized meridional ekman velocity ---> xr,zr
%
subplot(3,1,3)
tmp=repmat(vgr,[N 1]);
v2=imag(wer)+tmp;
var=1000*(v1-v2);
vrange1=[-100:5:-5];
vrange2=[5:5:100];
tmpx=repmat(-hr/D,[N 1]);
hold on
%
tmp=plot([-hoDmax -hu(end)/D],[100 100]);
set(tmp,'linewidth',3)
set(tmp,'color',[0.7 0.7 0.7])
tmp=plot([-hoDmax -hu(end)/D],[101 101],'k:');
set(tmp,'linewidth',2)
set(gca,'fontsize',16)
legend('analytical','numerical','Location','SouthEast')
%
[c,tmp]=contour(tmpx,zr/D,100*v2,vrange1,'k:');
set(tmp,'linewidth',2)
[c,tmp]=contour(tmpx,zr/D,100*v2,vrange2,'k');
set(tmp,'linewidth',2)
[c,tmp]=contour(tmpx,zr/D,100*v2,[-1e10 0 1e10],'k');
set(tmp,'linewidth',2)
set(tmp,'color',[0.7 0.7 0.7])
%
[c,tmp]=contour(tmpx,zr/D,100*v1,vrange1,':');
set(tmp,'color',[1 1 1]*0.7,'linewidth',2)
clabel(c,tmp)
[c,tmp]=contour(tmpx,zr/D,100*v2,vrange2);
set(tmp,'color',[1 1 1]*0.7,'linewidth',2)
clabel(c,tmp)
[c,tmp]=contour(tmpx,zr/D,100*v2,[-1e10 0 1e10]);
set(tmp,'color',[1 1 1]*0.7,'linewidth',2)
%
plot(-hr/D,-hr/D,'k')
hold off
h=text(-0.8,-1.5,'$$v [cm/s]$$','fontsize',18);
set(h,'Interpreter','latex','Fontsize',20);
axis([-hoDmax -hu(end)/D -hoDmax zr(N,1)/D])
set(gca,'box','on')
set(gca,'fontsize',16)
set(gca,'xtick',tick)
set(gca,'xticklabel','|2.2||1.8||1.4||1||0.6||0.2|')
set(gca,'ytick',get(gca,'xtick'))
set(gca,'yticklabel',get(gca,'xticklabel'))
xlabel 'h/D'
ylabel 'z/D'
grid on
ga3=gca;

set(ga1,'units','centimeters','position',[dxleft dybot+2*(dy2+dyint) dx dy1])
set(ga2,'units','centimeters','position',[dxleft dybot+dy2+dyint dx dy2])
set(ga3,'units','centimeters','position',[dxleft dybot dx dy2])

print -dpdf valid_innershelf.pdf;


