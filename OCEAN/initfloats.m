%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This routine positions floats in ROMS input file floats.in
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
clear all; close all
tic

fname='floats.in';     % Output Floats file name
gname='grid.nc';       % Input ROMS grid file name
writeinputfloats = 1;  % writes down positions in .in file
plotfloats       = 1;  % plot floats at initial time
if plotfloats,
  coastfile='canary_i.mat'; 
  put_topo=1;
end;

nc=netcdf(gname);
lon=nc{'lon_rho'}(:);
lat=nc{'lat_rho'}(:);
h  =nc{'h'}(:);
mask=nc{'mask_rho'}(:);
[M L]=size(lon);
close(nc)

% Floats coordinates at release points
% Exemple of floats released at each grid position
% where bathymetry is shallower than 200 meters 
% (for a single depth level -10 meters).

ifloat=0;
for i=1:L;
  for j=1:M;
    if h(j,i)<200 & mask(j,i)==1,
      ifloat=ifloat+1;
      xrelease(ifloat)=lon(j,i);
      yrelease(ifloat)=lat(j,i);
    end;
  end;
end;

% Additional information required for float initialization file 
% (see floats.in.README for glossary)

Ftitle='ROMS 1.0 - Initial Drifters Locations\n';
Fgrd  =0;
Fcoor =1;
Ftype =0;
Fcount=1;
Fdt   =0.;
Fdx   =0.;
Fdy   =0.;
Fdz   =0.;
Ft0   =0.;
Depth =[-10];

% Writes information required for float initialization 
% into a formatted input file. 

if writeinputfloats==1
 fid=fopen(fname,'w');
 fprintf(fid,'1  Ftitle (a80)\n');
 fprintf(fid,Ftitle);
 fprintf(fid,'2  Ft0,   Fx0,   Fy0,     Fz0, Fgrd,Fcoor,Ftype,Fcount, Fdt,   Fdx,   Fdy,   Fdz\n');
 for p=1:length(Depth); for n=1:length(xrelease);
  fprintf(fid,' %4.1f %9.4f %7.4f %7.1f   %d   %d    %d    %d   %8.2f %7.3f %7.3f %5.1f\n',...
  [Ft0 xrelease(n) yrelease(n) Depth(p) Fgrd Fcoor Ftype Fcount Fdt Fdx Fdy Fdz]);
 end; end;
 fprintf(fid,'99 END of float input data\n');
 fclose(fid);
end;


% Plots initial floats position

if plotfloats,
  figure;
  domaxis=[min(xrelease)-0.5 max(xrelease)+0.5 ...
           min(yrelease)-0.5 max(yrelease)+0.5];
  subplot('position',[0. 0.14 1 .8])
  m_proj('mercator',...
         'lon',[domaxis(1) domaxis(2)],...
         'lat',[domaxis(3) domaxis(4)]);
  m_usercoast(coastfile,'patch',[.9 .9 .9]);
  hk=m_line(xrelease,yrelease,'Marker','+');
  m_grid('box','fancy','tickdir','in');
  put_topo=0;
  if put_topo,
    [C2,h2]=draw_topo(fname,npts,[500 1000 1500 2000],'k');
    set(h2,'LineWidth',0.5)
  end;
end;

toc

