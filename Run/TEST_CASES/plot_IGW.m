%======================================================================
%
%     ---                    IGW Test Case                ---     
%     ---     Internal Gravity Wave solution over a       ---
%     ---     continental slope and shelf (COMODO test)   ---
%
% Reference:
% ----------
% Pichon, A., 2007: Tests academiques de maree, Rapport interne 
% n 21 du 19 octobre 2007, Service Hydrographique et Oceanographique 
% de la Marine.
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
%  Patrick Marchesiello - 2015
%======================================================================
close all
clear all
%================== User defined parameters ===========================
%
gname = 'igw_grd.nc';
fname = 'igw_frc.nc';
hname = 'igw_his.nc';

jj    = 600;      % location of validation (over the shelf)
valid = 0;        % 1: valid against forcing data
%======================================================================
%
%  Process CROCO solutions
%
nc=netcdf(gname);
h=squeeze(nc{'h'}(:));
hsec=squeeze(nc{'h'}(2,:));
lonu=squeeze(nc{'lon_u'}(2,:));
lonr=squeeze(nc{'lon_rho'}(2,:));
close(nc)

nc=netcdf(hname);
N=length(nc('s_rho'));
theta_s=nc.theta_s(:); 
theta_b=nc.theta_b(:); 
hc=nc.hc(:); 
ssh=squeeze(nc{'zeta'}(:,2,:));
zeta=squeeze(nc{'zeta'}(end,:,:));
u=squeeze(nc{'ubar'}(:,2,:));
v=squeeze(nc{'vbar'}(:,2,:));
usec=squeeze(nc{'u'}(end,:,2,:));
vsec=squeeze(nc{'v'}(end,:,2,:));
wsec=squeeze(nc{'w'}(end,:,2,:));
tsec=squeeze(nc{'temp'}(end,:,2,:));
rsec=squeeze(nc{'rho'}(end,:,2,:));
drsec=rsec-squeeze(nc{'rho'}(1,:,2,:));
t=nc{'scrum_time'}(:);
close(nc)

zeta_u=rho2u_2d(zeta);
h_u=rho2u_2d(h);
z=zlevs(h_u,zeta_u,theta_s,theta_b,hc,N,'r',1);
zr=zlevs(h,zeta,theta_s,theta_b,hc,N,'r',1);
zsec=squeeze(z(:,2,:));
xsec=repmat(lonu,N,1);
zrsec=squeeze(zr(:,2,:));
xrsec=repmat(lonr,N,1);

%---------------------------------------------------------
%  Plot internal tides section for u,w,rhop
%---------------------------------------------------------
figure('position',[500 500 700 700])
subplot(3,1,1)
contourf(xsec,zsec,usec,20); 
shading flat
colorbar
%colormap(flipud(jet))
%caxis([-0.1 0.1])
title('Internal case: U section')
%
subplot(3,1,2)
%contourf(xrsec,zrsec,wsec,20);
pcolor(xrsec,zrsec,wsec);
shading flat
colorbar
%caxis([-0.01 0.01])
title('Internal case: W section')
%
subplot(3,1,3)
contourf(xrsec,zrsec,drsec,20); 
%hold on; contour(xrsec,zrsec,rsec,'k'); hold off
shading flat; colorbar
title('Internal case: rho anomaly')
%
export_fig -transparent IGW.pdf


%================================================
%  External tides validation
%================================================

if valid==1,
%
% Process forcing tidal data 
%
 omega = 2.*pi/(12.*3600);   % S2 tide
 rad=pi/180;
 disp('  ssh...')
 fname0='IGW_FILES/amp_ssh_S2.cdf';
 nc=netcdf(fname0);
 ssh_amp=squeeze(nc{'amp_ssh_S2'}(1,4,:));
 close(nc)
 fname0='IGW_FILES/pha_ssh_S2.cdf';
 nc=netcdf(fname0);
 ssh_pha=squeeze(nc{'pha_ssh_S2'}(1,4,:));
 close(nc)
 for i=1:length(t)
   sshd(i,:)=ssh_amp.*cos(omega*t(i)-rad*ssh_pha);
 end
  % or ...
%  nc=netcdf(fname);
%  ssh_amp=squeeze(nc{'tide_Eamp'}(1,2,:));
%  ssh_pha=squeeze(nc{'tide_Ephase'}(1,2,:));
%  close(nc)
%  for i=1:length(t)
%    sshd(i,:)=ssh_amp.*cos(omega*t(i)-rad*ssh_pha);
%  end
% Process U
 disp('  u...')
 fname0='IGW_FILES/amp_u_S2.cdf';
 nc=netcdf(fname0);
 uamp=squeeze(nc{'amp_u_S2'}(1,4,2:end));
 close(nc)
 fname0='IGW_FILES/pha_u_S2.cdf';
 nc=netcdf(fname0);
 upha=squeeze(nc{'pha_u_S2'}(1,4,2:end));;
 close(nc)
 for i=1:length(t)
   ud(i,:)=uamp.*cos(omega*t(i)-rad*upha);
 end
% Process V
 disp('  v...')
 fname0='IGW_FILES/amp_v_S2.cdf';
 nc=netcdf(fname0);
 vamp=squeeze(nc{'amp_v_S2'}(1,5,:));
 close(nc)
 fname0='IGW_FILES/pha_v_S2.cdf';
 nc=netcdf(fname0);
 vpha=squeeze(nc{'pha_v_S2'}(1,5,:));;
 close(nc)
 for i=1:length(t)
   vd(i,:)=vamp.*cos(omega*t(i)-rad*vpha);
 end

%
% Plot comparisons
%
 hfig=figure('position',[300,300,700,400]);
 h1=plot(t,ssh(:,jj),'b'); hold on;
 h2=plot(t,sshd(:,jj),'b--'); hold on;
 h3=plot(t,u(:,jj),'k');
 h4=plot(t,ud(:,jj),'k--');
 h5=plot(t,v(:,jj),'r');
 h6=plot(t,vd(:,jj),'r--'); hold off
 hleg1=legend([h1 h3 h5],{'ssh','u','v'});
 new_handle = copyobj(hleg1,hfig);
 legend([h1 h2],{'model','data'},'location','southeast')
 title('Barotropic tides validation')
 set(gcf,'PaperPositionMode','auto');
 export_fig -transparent IGW_tides.pdf
end


