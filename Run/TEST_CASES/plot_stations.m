close all
clear all

nc=netcdf('stations.nc','r');
nsta=2;
for ista=1:nsta;
  T=squeeze(nc{'temp'}(:,ista,:));
  D=squeeze(nc{'depth'}(2,ista,:));
  time=squeeze(nc{'scrum_time'}(:,ista))./86400;
  T(1,:)=[];time(1)=[];
  N=length(D); Nt=length(time);

  figure
  for k=1:N;
    plot(time,T(:,k)); hold on;
  end;
  hold off;
  xlabel('Time [days]');
  ylabel('Temperature [deg. C]');

  time2d=repmat(time,1,N);
  D2d=repmat(D,Nt,1);
  figure
  pcolor(time2d,D2d,T); shading flat;
  xlabel('Time [days]');
  ylabel('Depth [m]');
  axis([-Inf +Inf -100 0]);
  caxis([16 24]);colorbar;
  set(gca,'Layer','top');  
end;
close(nc);
