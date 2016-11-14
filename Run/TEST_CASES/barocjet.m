%============================================================
% Compute zeta,ubar,vbar,u,v,t for the baroclinic jet 
% using analytical functions for all variables
% (guarantees dynamical balance)
%
% Patrick Marchesiello, IRD, 2011
% adapted from barocvortex.m (Penven 2005)
%
% P1 : pressure at z=0
%
% P1=Pa+P0*tanh((y-y0)/lambda).*(1+Cper*cos(Kper*X));
%
% P0 such that u(y0)=umax
%
% => P0=-rho0*f0*umax*lambda 
%
% Perturbation function:
% find wavelength perturbation 2*pi/Kper near deformation 
% radius wavelength so that there is an integer number 
% of wave numbers in x direction
%============================================================
lambda=Ljet/2;
L=2*pi*sqrt(N2)*H/f0;  % deformation radius scale
disp(['Deformation Radius and Scale (km) : ', ...
     num2str(L*1.e-3/(2*pi)),' - ',num2str(L*1.e-3)]);
%
% Perturbation wavenumber and magnitude
%
if perturb,
 L=xmax/floor(xmax/L); % adjust L to ensure integer wavenumber 
 Kper=2*pi/L;          % perturbation wavenumber
 Cper=0.05;            % perturbation magnitude
end
%
% Surface pressure
%
P0=-rho0*f0*umax*lambda;
if perturb,
  P1=Pa+P0*tanh((Y-Y0)/lambda).*(1+Cper*cos(Kper*X));
else
  P1=Pa+P0*tanh((Y-Y0)/lambda);
end
%
% Calcul de rho at z=0
%
a=-P0*(1-exp(-H))/(g*(H-1+exp(-H)));
if perturb,
 rho1=rho0+a*tanh((Y-Y0)/lambda).*(1+Cper*cos(Kper*X));
else
 rho1=rho0+a*tanh((Y-Y0)/lambda);
end
%
% Surface elevation 
%
zeta=(P1-Pa)./(g.*rho1);
%
% Vertical grid 
%
zw=zlevs(h0,zeta,theta_s,theta_b,hc,N,'w');
zr=zlevs(h0,zeta,theta_s,theta_b,hc,N,'r');
%
% Density
%
%
xr=tridim(X,N);
yr=tridim(Y,N);
%
rho=rho0.*(1-N2.*zr./g);
rhodyn=-P0.*(1-exp(-zr-H)).*tanh((yr-Y0)/lambda)./(g*(H-1+exp(-H)));
if perturb,
 rhodyn=rhodyn.*(1+Cper*cos(Kper*xr));
end
rho(zr>-H)=rho(zr>-H)+rhodyn(zr>-H);
%
% Temperature
%
% rho=rho0+R0-TCOEF*T
%
R0=0; %30;
TCOEF=-1; %0.28;
t=(-rho+1000+R0)/TCOEF; %t=rho
%
% U and V
%
%
a=P0/(f0*rho0*lambda);
zu=0.5*(zr(:,:,1:end-1)+zr(:,:,2:end));
xu=0.5*(xr(:,:,1:end-1)+xr(:,:,2:end));
yu=0.5*(yr(:,:,1:end-1)+yr(:,:,2:end));
F=(H-1+zu+exp(-zu-H))./(H-1+exp(-H));
F(zu<-H)=0;
u=-a.*F.*(1-tanh((yu-Y0)/lambda).^2);
if perturb,
  u=u.*(1+Cper*cos(Kper*xu));
end
zv=0.5*(zr(:,1:end-1,:)+zr(:,2:end,:));
xv=0.5*(xr(:,1:end-1,:)+xr(:,2:end,:));
yv=0.5*(yr(:,1:end-1,:)+yr(:,2:end,:));
if perturb,
 F=(H-1+zv+exp(-zv-H))./(H-1+exp(-H));
 F(zv<-H)=0;
 v=Cper*Kper*lambda*a.*F.*tanh((yv-Y0)/lambda).*sin(Kper*xv);
else
 v=zeros(size(yv));
end
%
% Vitesses barotropes
%
dz=zw(2:end,:,:)-zw(1:end-1,:,:);
dzu=0.5*(dz(:,:,1:end-1)+dz(:,:,2:end));
dzv=0.5*(dz(:,1:end-1,:)+dz(:,2:end,:));
hu=squeeze(sum(dzu.*u));
hv=squeeze(sum(dzv.*v));
D_u=squeeze(sum(dzu));
D_v=squeeze(sum(dzv));
ubar=squeeze(hu./D_u);
vbar=squeeze(hv./D_v);
%
return
