%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Make 1 animation from the results of the RIVER test case
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
%
floats=1;
step=2;
vname='salt';
%
% Read data
%
nc=netcdf('river_his.nc','r');
tis=nc{'scrum_time'}(:);
h=nc{'h'}(:);
x=(nc{'x_rho'}(:))/1000;
y=(nc{'y_rho'}(:))/1000;
N=length(nc('s_rho'));
mask=nc{'mask_rho'}(:);

mask(mask==0)=NaN;
[M,L]=size(x);
[I,J]=meshgrid([0:L-1],[0:M-1]);
hmax=max(max(h));
map=colormap(jet);
NCOL=length(map(:,1));

if floats==1
  nf=netcdf('floats.nc','r');
  nflt=length(nf('drifter'));
  hflt=0*(1:nflt);
end

for tndx=1:length(tis)
  s=squeeze(nc{vname}(tndx,N,:,:));
  u=squeeze(nc{'u'}(tndx,N,:,:));
  v=squeeze(nc{'v'}(tndx,N,:,:));
  pcolor(x,y,mask.*s)
  caxis([18 36]) 
  shading flat
  colorbar
  hold on
  [C1,h1]=contour(x,y,h,[0:25:300],'k');
  u=u2rho_2d(u);
  v=v2rho_2d(v);
  quiver(x(1:step:end,1:step:end),y(1:step:end,1:step:end),...
         u(1:step:end,1:step:end),v(1:step:end,1:step:end),1,'k');
  axis image
  axis([0 40 0 80]) 

  if floats==1    
    for i=1:nflt
      iflt=nf{'Xgrid'}(tndx,i);
      jflt=nf{'Ygrid'}(tndx,i);
      zflt=-nf{'depth'}(tndx,i);
      cndx=1+(NCOL-1)*zflt/hmax;
      if cndx<1 | ~isfinite(cndx)
        cndx=1;
      end
      if cndx>NCOL
        cndx=NCOL-0.01;
      end
      z1=floor(cndx);
      z2=floor(cndx)+1;
      v1=map(z1,:);
      v2=map(z2,:);
      v=(((v1-v2)*cndx+v2.*z1-v1.*z2)./(z1-z2));
      xflt=interp2(I,J,x,iflt,jflt,'cubic');
      yflt=interp2(I,J,y,iflt,jflt,'cubic');     
      hflt(i)=plot(xflt,yflt,'.');
      set(hflt(i),'Color',v)
    end
  end

  hold off
  title(['RIVER: ',vname,' - day = ',num2str(tis(tndx)/(24*3600))])

  MOV(tndx) = getframe;

end

close(nc)
if floats==1
  close(nf)
end

movie(MOV,1);



