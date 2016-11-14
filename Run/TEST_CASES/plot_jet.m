%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Make a plot from the results of the JET test case
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
fname='jet_his.nc';
gname='jet_grd.nc';
%
nesting=0;
fname_child=[fname,'.1'];
gname_child=[gname,'.1'];
%
tindex=input('tindex : ');
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Parent
%
nc=netcdf(fname,'r');
N=length(nc('s_rho'));
tlen=length(nc{'scrum_time'}(:));
tindex=min(tlen,tindex);
time=round(nc{'scrum_time'}(tindex)/(24*3600));
disp(['Day : ',num2str(time),'  index:',num2str(tindex)])
zeta=squeeze(nc{'zeta'}(tindex,:,:));
u=squeeze(nc{'u'}(tindex,:,:,:));
v=squeeze(nc{'v'}(tindex,:,:,:));
rho=squeeze(nc{'temp'}(tindex,:,:,:));
R0=30; TCOEF=0.28;
t=(-rho+R0)./TCOEF;
sst=squeeze(t(N,:,:));
close(nc)
%
nc=netcdf(gname,'r');
xr=1e-3*nc{'x_rho'}(:);
yr=1e-3*nc{'y_rho'}(:);
h=nc{'h'}(:,:);
pm=nc{'pm'}(:,:);
pn=nc{'pn'}(:,:);
f=nc{'f'}(:,:);
close(nc)
[xu,xv,xp]=rho2uvp(xr);
[yu,yv,yp]=rho2uvp(yr);
[fu,fv,fp]=rho2uvp(f);
zr=zlevs(h,zeta,5,0,100,N,'r');
u0=squeeze(u(N,:,:));
v0=squeeze(v(N,:,:));
[vort]=vorticity(u0,v0,pm,pn);
vort=vort./fp;
X=xr; Y=yr;
xr=tridim(X,N);
yr=tridim(Y,N);
zu=0.5*(zr(:,:,1:end-1)+zr(:,:,2:end));
xu=0.5*(xr(:,:,1:end-1)+xr(:,:,2:end));
yu=0.5*(yr(:,:,1:end-1)+yr(:,:,2:end));
ur=u2rho_2d(squeeze(u(N,:,:)));
vr=v2rho_2d(squeeze(v(N,:,:)));

%
% Child
%
if nesting,
 nc=netcdf(fname_child,'r');
 zeta2=squeeze(nc{'zeta'}(tindex,:,:));
 u2=squeeze(nc{'u'}(tindex,:,:,:));
 v2=squeeze(nc{'v'}(tindex,:,:,:));
 rho2=squeeze(nc{'temp'}(tindex,:,:,:));
 t2=(-rho2+R0)./TCOEF;
 sst2=squeeze(t2(N,:,:));
 close(nc)
 zeta2(:,1)=NaN; zeta2(:,end)=NaN;
 u2(:,:,1)=NaN; u2(:,:,end)=NaN;
 v2(:,:,1)=NaN; v2(:,:,end)=NaN;
 t2(:,:,1)=NaN; t2(:,:,end)=NaN;
 sst2(:,1)=NaN; sst2(:,end)=NaN;
%
 nc=netcdf(gname_child,'r');
 xr2=1e-3*nc{'x_rho'}(:);
 yr2=1e-3*nc{'y_rho'}(:);
 h2=nc{'h'}(:,:);
 pm2=nc{'pm'}(:,:);
 pn2=nc{'pn'}(:,:);
 f2=nc{'f'}(:,:);
 close(nc)
 [xu2,xv2,xp2]=rho2uvp(xr2);
 [yu2,yv2,yp2]=rho2uvp(yr2);
 [fu2,fv2,fp2]=rho2uvp(f2);
 zr2=zlevs(h2,zeta2,5,0,100,N,'r');
 u02=squeeze(u2(N,:,:));
 v02=squeeze(v2(N,:,:));
 [vort2]=vorticity(u02,v02,pm2,pn2);
 vort2=vort2./fp2;
 u02(:,1)=NaN; u02(:,end)=NaN;
 v02(:,1)=NaN; v02(:,end)=NaN;
 vort2(:,1:2)=NaN; vort2(:,end-1:end)=NaN;

%
 Xbox=cat(1,xp2(1:end,1),  ...
            xp2(end,1:end)' ,...
            xp2(end:-1:1,end),...
            xp2(1,end:-1:1)');
 Ybox=cat(1,yp2(1:end,1),  ...
            yp2(end,1:end)' ,...
            yp2(end:-1:1,end),...
            yp2(1,end:-1:1)');

 X2=xr2; Y2=yr2;
 xr2=tridim(X2,N);
 yr2=tridim(Y2,N);
 zu2=0.5*(zr2(:,:,1:end-1)+zr2(:,:,2:end));
 xu2=0.5*(xr2(:,:,1:end-1)+xr2(:,:,2:end));
 yu2=0.5*(yr2(:,:,1:end-1)+yr2(:,:,2:end));
 ur2=u2rho_2d(squeeze(u2(N,:,:)));
 vr2=v2rho_2d(squeeze(v2(N,:,:)));
end
%======================================================
%
% Plot
%
figure
map=colormap(jet(20));
map(10:11,:)=1*[1 1 1 ; 1 1 1];
colormap(map)
%pcolor(xp,yp,vort); shading flat
contourf(xp,yp,vort,20); shading flat
if nesting,
 hold on
 contourf(xp2,yp2,vort2,20); shading flat
 plot(Xbox,Ybox,'k');
 hold off
end
colorbar
caxis([-1 1])
axis image
title('Surface vorticity (/f)')
%
figure
colormap(jet)
contourf(X,Y,sst,20); shading flat
if nesting,
 hold on
 contourf(X2,Y2,sst2,20); shading flat
 plot(Xbox,Ybox,'k');
 hold off
end
colorbar
caxis([18 20])
axis image
title('Surface temperature')
%
figure
colormap(jet)
spd=sqrt(ur.^2+vr.^2);
contourf(X,Y,spd,20); hold on
quiver(X,Y,ur,vr,'k')
if nesting,
 spd2=sqrt(ur2.^2+vr2.^2);
 contourf(X2,Y2,spd2,20); 
 quiver(X2,Y2,ur2,vr2,'k')
 plot(Xbox,Ybox,'k');
end
axis image
caxis([0 0.5])
colorbar
hold off
title('Surface velocity')
%
figure
colormap(map)
contourf(X,Y,zeta,20); shading flat
if nesting,
 hold on
 contourf(X2,Y2,zeta2,20); shading flat
 plot(Xbox,Ybox,'k');
 hold off
end
shading flat
colorbar
caxis([-0.1 0.1])
axis image
title('Surface height')
%
figure
colormap(jet)
[M,L]=size(Y);
imid=round(M/2);
contourf(squeeze(yr(:,:,imid)),squeeze(zr(:,:,imid)),squeeze(t(:,:,imid)),20);
colorbar
title('Temperature')
%
figure
colormap(jet)
[M,L]=size(Y);
imid=round(L/2);
contourf(squeeze(yu(:,:,imid)),squeeze(zu(:,:,imid)),squeeze(u(:,:,imid)),10);
colorbar
title('Zonal velocity')
%
if tindex>20
 nc=netcdf(fname,'r');
 lx=30; ly=15;
 figure('units','centimeters','position', ...
          [0 0 lx ly],'paperpositionmode','auto')
 tindex=[11;15;21];
 for tndx=1:3;
  u=squeeze(nc{'u'}(tindex(tndx),N,:,:));
  v=squeeze(nc{'v'}(tindex(tndx),N,:,:));
  [vort]=vorticity(u,v,pm,pn);
  vort=vort./fp;
  if tndx==1,
   ax(1)=subplot(1,3,1);
  elseif tndx==2,
   ax(2)=subplot(1,3,2);
  elseif tndx==3,
   ax(3)=subplot(1,3,3);
  end
  colormap(map)
  contourf(xp,yp,vort,20); shading flat
  if tndx==3,
    h=colorbar('v');
    %set(h, 'Position', [.35 .2 .3 .03])
    set(h, 'Position', [.85 .35 .02 .3])
    for i=1:3
      pos=get(ax(i), 'Position');
      set(ax(i), 'Position', [.88*pos(1) pos(2) pos(3) pos(4)]);
    end
  end
  caxis([-0.8 0.8])
  axis image
  time=round(nc{'scrum_time'}(tindex(tndx))/(24*3600));
  title(['Vorticity/f - day=',num2str(time)])
 end
 close(nc)
%
% print plot series
%
 print -painter -depsc2 vort_jet.eps
%
end


return

