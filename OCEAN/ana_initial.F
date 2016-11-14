! $Id: ana_initial.F 1620 2015-01-08 10:47:13Z marchesiello $
!
!======================================================================
! CROCO is a branch of ROMS developped at IRD and INRIA, in France
! The two other branches from UCLA (Shchepetkin et al) 
! and Rutgers University (Arango et al) are under MIT/X style license.
! CROCO specific routines (nesting) are under CeCILL-C license.
! 
! CROCO website : http://www.croco-ocean.org
!======================================================================
!
#include "cppdefs.h"

      subroutine ana_initial (tile)

      implicit none
#include "param.h"
       integer tile, trd
C$    integer omp_get_thread_num
#include "compute_tile_bounds.h"
      call ana_initial_tile (Istr,Iend,Jstr,Jend)
      return
      end
!
      subroutine ana_initial_tile (Istr,Iend,Jstr,Jend)
!
!----------------------------------------------------------------------
!  Set initial conditions for momentum and tracer variables using
!  analytical expressions.
!----------------------------------------------------------------------
!
#ifdef AGRIF
      use Agrif_Util
#endif
      implicit none
#include "param.h"
#include "grid.h"
#include "ocean2d.h"
#include "ocean3d.h"
#include "scalars.h"
#ifdef SEDIMENT
# include "sediment.h"
#endif
      integer Istr,Iend,Jstr,Jend, i,j,k, itrc
#if defined NBQ && ! defined MPI
      integer :: mynode
#endif
#ifdef SOLITON
      real fac, x,y, x0,y0, cff1,cff2,cff3
#endif
#ifdef GRAV_ADJ_SOLITON
      real rho1_exp,rho2_exp,h1_exp,amp_exp, Lt
      real d(0:N)
# endif
#ifdef BASIN
      real cff1,cff2
#endif
#ifdef INTERNAL
      real cff
#endif
#ifdef IGW
      real    cff,sig,rhom,depth
#endif 
#ifdef BIOLOGY
      real temp, SiO4
#endif
#ifdef THACKER
      real ETA0, omega, depth, Lt
#endif
#ifdef TANK
      real ETA0, depth, Lt,cff
#endif
#ifdef RIP
      integer iper
      real xper,yper,xx,yy,xs,db,eps
#endif
#ifdef GRAV_ADJ_SOLITON
      real zint,depth,dz,gam,del
#endif
!
#include "compute_auxiliary_bounds.h"
!
#if defined EW_PERIODIC && !defined MPI
# define IR_RANGE Istr,Iend
# define IU_RANGE Istr,Iend
#else
# define IR_RANGE IstrR,IendR
# define IU_RANGE Istr,IendR
#endif
#if defined NS_PERIODIC && !defined MPI
# define JR_RANGE Jstr,Jend
# define JV_RANGE Jstr,Jend
#else
# define JR_RANGE JstrR,JendR
# define JV_RANGE Jstr,JendR
#endif

#ifdef ANA_INITIAL
!
!----------------------------------------------------------------------
!  Initial conditions for free surface and 2D momentum components.
!----------------------------------------------------------------------
!
      if (nrrec.eq.0) then
!
# if defined SOLITON
#  ifdef AGRIF
        if (Agrif_Root()) then
#  endif
          x0=2.*xl/3.
          y0=el/2.
#  ifdef AGRIF
        else
          x0=2.*Agrif_Parent_xl/3.
          y0=Agrif_Parent_el/2.
        endif
#  endif
        cff1=0.395
        cff2=0.771*(cff1*cff1)
        do j=JR_RANGE
          do i=IR_RANGE
            x=xr(i,j)-x0
            y=yr(i,j)-y0
            cff3=exp(-cff1*x)
            fac=cff2*(2.*cff3/(1.+cff3*cff3))**2 
            zeta(i,j,1)=0.25*fac*(6.*y*y+3.)*exp(-0.5*y*y)
          enddo
        enddo
        do j=JR_RANGE
          do i=IU_RANGE
            x=0.5*(xr(i-1,j)+xr(i,j))-x0
            y=0.5*(yr(i-1,j)+yr(i,j))-y0
            cff3=exp(-cff1*x)
            fac=cff2 * (2.*cff3/(1.+cff3*cff3))**2
            ubar(i,j,1)=0.25*fac*(6.*y*y-9.)*exp(-0.5*y*y)
          enddo
        enddo
        do j=JV_RANGE
          do i=IR_RANGE
            x=0.5*(xr(i,j-1)+xr(i,j))-x0
            y=0.5*(yr(i,j-1)+yr(i,j))-y0
            cff3=exp(-cff1*x)
            fac=cff2 * (2.*cff3/(1.+cff3*cff3))**2
            vbar(i,j,1)=2.*fac*y*(-2.*cff1*tanh(cff1*x))
     &                                    *exp(-0.5*y*y)
          enddo
        enddo

# elif defined THACKER
        ETA0=0.1
        depth=10.
        Lt=80.e3
        omega=0.5*f(1,1) + sqrt( 0.25*f(1,1)**2 + 2*g*depth/Lt**2 )
        do j=JR_RANGE
          do i=IR_RANGE
            zeta(i,j,1)=2*ETA0*depth*(xr(i,j)/Lt - .5*ETA0/Lt)
            zeta(i,j,2)=zeta(i,j,1)
            ubar(i,j,1)=0.
            vbar(i,j,1)=-ETA0*omega*Lt
            ubar(i,j,2)=0
            vbar(i,j,2)=vbar(i,j,1)
          enddo
        enddo

# elif defined TANK
        ETA0=0.001
        Lt=10.
        do j=JR_RANGE
          do i=IR_RANGE
#  ifndef MOVING_BATHY       
#   ifndef TANKY
            zeta(i,j,1)=ETA0*cos(pi*xr(i,j)/Lt)
#   else
            zeta(i,j,1)=ETA0*cos(pi*yr(i,j)/Lt)
#   endif
#  else
            zeta(i,j,1)=0.
#  endif        
            ubar(i,j,1)=0.
            vbar(i,j,1)=0.
            zeta(i,j,2)=zeta(i,j,1)
            ubar(i,j,2)=ubar(i,j,1)
            vbar(i,j,2)=vbar(i,j,1)
          enddo
        enddo
# else
        do j=JR_RANGE
          do i=IR_RANGE
            zeta(i,j,1)=0.
            zeta(i,j,2)=0.
            ubar(i,j,1)=0.
            vbar(i,j,1)=0.
            ubar(i,j,2)=0.
            vbar(i,j,2)=0.
          enddo
        enddo
# endif

# ifdef NBQ
        do j=JR_RANGE
          do i=IR_RANGE
            zeta(i,j,1)=h(i,j)+zeta(i,j,1)
            zeta(i,j,2)=zeta(i,j,1)
          enddo
        enddo
# endif

      else  !  nrrec.ne.0.

# if defined RIP && defined RIP_TOPO_2D
!
!  Add perturbation to restart field
!
#  ifdef GRANDPOPO
        xs=85        ! inner surfzone at GPP
        db=50        ! distance from xs to sand bar
#  else
        xs=150       ! inner surfzone at Duck
        db=80        ! distance from xs to sand bar
#  endif
        eps=0.01     ! perturbation magnitude
        do j=JstrR,JendR
          do i=IstrR,IendR
#  ifdef OBC_EAST
            xx=xr(i,j)
#  else
            xx=xl-xr(i,j)
#  endif
            yy=yr(i,j);
            xper=1.
            yper=0.
            do iper=1,12
              yper= yper+eps*cos(2*pi*float(iper)*yy/(xs+db)
     &                           +2*pi*tanh(float(iper)/10))
            enddo
            ubar(i,j,1)=(1+xper)*ubar(i,j,1);
            ubar(i,j,2)=ubar(i,j,1);
            vbar(i,j,1)=(1+yper)*vbar(i,j,1);
            vbar(i,j,2)=vbar(i,j,1);
          enddo
        enddo
# endif /* RIP */

      endif  !  nrrec.eq.0.

# ifdef SOLVE3D
!
!----------------------------------------------------------------------
!  Initial conditions for momentum components [m/s].
!----------------------------------------------------------------------
!
      if (nrrec.eq.0) then
!
#  if defined THACKER
        do k=1,N
          do j=JR_RANGE
            do i=IR_RANGE
              u(i,j,k,1)=0.
              v(i,j,k,1)=-ETA0*omega*Lt
              u(i,j,k,2)=0.
              v(i,j,k,2)=v(i,j,k,1)
            enddo
          enddo
        enddo

#  else
        do k=1,N
          do j=JR_RANGE
            do i=IR_RANGE
              u(i,j,k,1)=0.
              v(i,j,k,1)=0.
              u(i,j,k,2)=0.
              v(i,j,k,2)=0.
            enddo
          enddo
        enddo
#  endif

      else   ! nrrec.ne.0.

#  if defined RIP && defined RIP_TOPO_2D
!
!  Add perturbation to restart field
!
        do k=1,N
          do j=JstrR,JendR
            do i=IstrR,IendR
#   ifdef OBC_EAST
              xx=xr(i,j)
#   else
              xx=xl-xr(i,j)
#   endif
              yy=yr(i,j);
              xper=1.
              yper=0.
              do iper=1,12
                yper= yper+eps*cos(2*pi*float(iper)*yy/(xs+db)
     &                            +2*pi*tanh(float(iper)/10))
              enddo
              u(i,j,k,1)=(1+xper)*u(i,j,k,1)
              u(i,j,k,2)=u(i,j,k,1)
              v(i,j,k,1)=(1+yper)*v(i,j,k,1)
              v(i,j,k,2)=v(i,j,k,1)
            enddo
          enddo
        enddo
#  endif /* RIP */
!
      endif   !nrrec.eq.0.
!
!----------------------------------------------------------------------
!  Set initial conditions for potential temperature [degC] and
!  salinity [PSU] and passive tracer
!----------------------------------------------------------------------
!
      if (nrrec.eq.0) then
!
#  ifdef BASIN
        cff1=(44.690/39.382)**2
        cff2=cff1*(rho0*800./g)*(5.0e-5/((42.689/44.690)**2))
        do k=1,N
          do j=JR_RANGE
            do i=IR_RANGE
              t(i,j,k,1,itemp)=cff2*exp(z_r(i,j,k)/800.)
     &                   *(0.6-0.4*tanh(z_r(i,j,k)/800.))
              t(i,j,k,2,itemp)=t(i,j,k,1,itemp)
             enddo
          enddo
        enddo

#  elif defined CANYON_A
        do k=1,N
          do j=JR_RANGE
            do i=IR_RANGE
              t(i,j,k,1,itemp)=T0
              t(i,j,k,2,itemp)=t(i,j,k,1,itemp)
            enddo
          enddo
        enddo

#  elif defined CANYON_B
        do k=1,N
          do j=JR_RANGE
            do i=IR_RANGE
              t(i,j,k,1,itemp)=3.488*exp(z_r(i,j,k)/800.)
     &          *(1.-0.666666666666*tanh(z_r(i,j,k)/800.))
              t(i,j,k,2,itemp)=t(i,j,k,1,itemp)
            enddo
          enddo
        enddo

#  elif defined SHOREFACE || defined RIP
        do k=1,N
          do j=jstrR,jendR
            do i=istrR,iendR
              t(i,j,k,1,itemp)=18.0
              t(i,j,k,2,itemp)=t(i,j,k,1,itemp)
#   ifdef SALINITY
              t(i,j,k,1,isalt)=32.0
              t(i,j,k,2,isalt)=t(i,j,k,1,isalt)
#   endif
            enddo
          enddo
        enddo

#  elif defined SWASH
        do k=1,N
          do j=JR_RANGE
            do i=IR_RANGE
              t(i,j,k,1,itemp)=20.
!              t(i,j,k,1,itemp)=20.-(zeta(i,j,1)-z_r(i,j,k))
              t(i,j,k,2,itemp)=t(i,j,k,1,itemp)
            enddo
          enddo
        enddo

#  elif defined THACKER
        do k=1,N
          do j=JR_RANGE
            do i=IR_RANGE
              t(i,j,k,1,itemp)=20.
!              t(i,j,k,1,itemp)=20.-(zeta(i,j,1)-z_r(i,j,k))
              t(i,j,k,2,itemp)=t(i,j,k,1,itemp)
            enddo
          enddo
        enddo

#  elif defined TANK
#   ifndef MOVING_BATHY
        do k=1,N
          do j=JR_RANGE
            do i=IR_RANGE
              t(i,j,k,1,itemp)=20.
              t(i,j,k,2,itemp)=t(i,j,k,1,itemp)
            enddo
          enddo
        enddo
#   else
        cff=((2.510e-4)**2)/g
        do k=1,N
          do j=JR_RANGE
            do i=IR_RANGE
              t(i,j,k,1,itemp)=rho0*(1-cff*z_r(i,j,k)) - 1000.
              t(i,j,k,2,itemp)=t(i,j,k,1,itemp)
            enddo
          enddo
        enddo
#   endif

#  elif defined EQUATOR
        do k=1,N
          do j=JstrR,JendR
            do i=IstrR,IendR
              t(i,j,k,1,itemp)=10.0
              t(i,j,k,2,itemp)=t(i,j,k,1,itemp)
#   ifdef SALINITY
              t(i,j,k,1,isalt)=35.0
              t(i,j,k,2,isalt)=t(i,j,k,1,isalt)
#   endif
            enddo
          enddo
        enddo

#  elif defined GRAV_ADJ
#   ifdef GRAV_ADJ_SOLITON
#    define HORN_PROFILE
#    ifdef HORN_PROFILE
!
!  Horn, D.A., J. Imberger, & G.N. Ivey, (2001). 
!  The degeneration of large-scale interfacial gravity waves in lakes. 
!  J. Fluid Mech., 434:181-207.
!  --> initialy setup with variable salinity in the tank experiment of 
!      Horn et al (2001); the conversion to temperature variations 
!      through linear EOS gives unrealistic range
!
        rho1_exp = R0 - 10.  ! * 0.7286 /2.
        rho2_exp = R0 + 10.  ! * 0.7286 /2.
        h1_exp = 0.7 * h(1,1)
        amp_exp = 0.0391
        Lt=6.
        !write(6,*) h(1,1),h1_exp,R0,Tcoef
        do j=JstrR,JendR
           do i=IstrR,IendR
              do k=1,N
                 d(k)=0.5*(rho1_exp+rho2_exp)               
     &              + 0.5*(rho2_exp-rho1_exp)
     &                   *tanh(exp(1.)*(-h1_exp 
     &                          -2.*amp_exp*(xr(i,j)-Lt/2.)/Lt 
     &                          +abs(z_r(i,j,k)))/(0.015/2.))   ! 0.005=dz_q   
                 t(i,j,k,1,itemp) = 13. - ( d(k) - R0 )  / Tcoef
                 t(i,j,k,2,itemp) = t(i,j,k,1,itemp)
              enddo
           enddo
        enddo
#    else
        depth=h(1,1)
        gam=0.9
        del=0.3
        dz=2*(z_r(1,1,2)-z_r(1,1,1));
        do j=JstrR,JendR
          do i=IstrR,IendR
            zint=depth*(del-1)+gam*del*depth*(2*xr(i,j)/xl - 1)
            do k=1,N
              t(i,j,k,1,itemp)=0.5*(5/Tcoef+10/Tcoef)
     &                        -0.5*(5/Tcoef-10/Tcoef)
     &                            *tanh((z_r(i,j,k)-zint)/dz);
              t(i,j,k,2,itemp)=t(i,j,k,1,itemp)
            enddo
          enddo
        enddo
#    endif /* HORN_PROFILE */ 
#   else
        do k=1,N
          do j=JstrR,JendR
            do i=IstrR,IendR
#    ifdef MPI
              if (i+iminmpi-1 .LE. LLm/2) then
#    else
              if (i .LE. LLm/2) then
#    endif            
                t(i,j,k,1,itemp)=5/Tcoef
                t(i,j,k,2,itemp)=5/Tcoef
              else
                t(i,j,k,1,itemp)=10/Tcoef
                t(i,j,k,2,itemp)=10/Tcoef           
              endif
            enddo
          enddo
        enddo
#   endif /* GRAV_ADJ_SOLITON */

#  elif defined ACOUSTIC
        do j=JstrR,JendR
          do i=IstrR,IendR
            do k=1,N
               t(i,j,k,1,itemp)=5/Tcoef
               t(i,j,k,2,itemp)=5/Tcoef
            enddo
          enddo
        enddo

#  elif defined INNERSHELF
        do k=1,N
          do j=JR_RANGE
            do i=IR_RANGE
#   ifdef INNERSHELF_EKMAN
              t(i,j,k,1,itemp)=15.
#   else
#    define Z0 (-80.)
#    define THKNSS 50.
#    define Z1 0.
#    define STRAT 1000.
              t(i,j,k,1,itemp)=14.+5.*THKNSS*log(
     &                         cosh((z_w(i,j,k  )-Z0)/THKNSS)
     &                        /cosh((z_w(i,j,k-1)-Z0)/THKNSS)
     &                            )/(z_w(i,j,k)-z_w(i,j,k-1))
     &               +((z_w(i,j,k)+z_w(i,j,k-1))/2.-Z1)/STRAT
#    undef Z0
#    undef THKNSS
#    undef Z1
#    undef STRAT
#   endif
              t(i,j,k,2,itemp)=t(i,j,k,1,itemp)
#   ifdef SALINITY
              t(i,j,k,1,isalt)=35.
              t(i,j,k,2,isalt)=t(i,j,k,1,isalt)
#   endif
            enddo
          enddo
        enddo

#  elif defined  INTERNAL
        cff=((2.e-3)**2)/g
        do k=1,N
          do j=JR_RANGE
            do i=IR_RANGE
              t(i,j,k,1,itemp)=rho0*(1-cff*z_r(i,j,k)) - 1000.
              t(i,j,k,2,itemp)=t(i,j,k,1,itemp)
            enddo
          enddo
        enddo

#  elif defined IGW
        cff=((2.e-3)**2)/g
        rhom=1027.037885
        do k=1,N
          do j=JR_RANGE
            do i=IR_RANGE
              depth = -z_r(i,j,k)         
#   ifdef EXPERIMENT1
              if(depth.le.50.) then
                sig = 25.0  
              elseif( depth.gt.50. .and. depth.le.60.) then
                sig = 26.2 + rhom*(depth-50.)*cff
                sig = sig + 1.2*(depth-60.)/10.
              else
                sig = 26.2 + rhom*(depth-50.)*cff
              endif
              t(i,j,k,1,itemp)=(R0-sig)/Tcoef
#   elif defined EXPERIMENT2	    
              if (depth.le.50.) then
               sig = 25.8
              else
               sig = 27.0
              endif
              t(i,j,k,1,itemp)=(R0-sig)/Tcoef
#   else /* default EXPERIMENT3 */
              sig = 26.2 + rhom*(depth-50.)*cff
              t(i,j,k,1,itemp)=(R0-sig)/Tcoef
#   endif
              t(i,j,k,2,itemp)=t(i,j,k,1,itemp)
            enddo
          enddo
        enddo

#  elif defined OVERFLOW
        do k=1,N
          do j=JR_RANGE
            do i=IR_RANGE
              t(i,j,k,1,itemp)=T0*(0.5-0.5*tanh(yr(i,j)/1000.-25.))
              t(i,j,k,2,itemp)=t(i,j,k,1,itemp)
            enddo
          enddo
        enddo

#   elif defined  RIVER
        do k=1,N
          do j=JR_RANGE
            do i=IR_RANGE
              t(i,j,k,1,itemp)=10.  !4.+10.*exp(z_r(i,j,k)/50.)
              t(i,j,k,1,isalt)=36.
              t(i,j,k,2,itemp)=t(i,j,k,1,itemp)
              t(i,j,k,2,isalt)=t(i,j,k,1,isalt)
            enddo
          enddo
        enddo

#  elif defined SEAMOUNT
        do k=1,N
          do j=JR_RANGE
            do i=IR_RANGE
              t(i,j,k,1,itemp)=5.*15.*exp(z_r(i,j,k)/100.)
              t(i,j,k,2,itemp)=t(i,j,k,1,itemp)
#   ifdef SALINITY
              t(i,j,k,1,isalt)=35.
              t(i,j,k,2,isalt)=t(i,j,k,1,isalt)
#   endif
            enddo
          enddo
        enddo

#  elif defined SHELFRONT
        do k=1,N
          do j=JR_RANGE
            do i=IR_RANGE
              t(i,j,k,1,itemp)=T0+2.5*tanh((yr(i,j)-50000.0)/20000.0)
              t(i,j,k,2,itemp)=t(i,j,k,1,itemp)
              t(i,j,k,1,isalt)=S0  +  tanh((yr(i,j)-50000.0)/20000.0)
              t(i,j,k,2,isalt)=t(i,j,k,1,isalt)
            enddo
          enddo
        enddo

#  elif  defined SED_TEST2 
        do k=1,N
          do j=JstrR,JendR
            do i=IstrR,IendR
              t(i,j,k,1,itemp)=20.0
              t(i,j,k,1,isalt)=0.0
              t(i,j,k,2,itemp)=t(i,j,k,1,itemp)
              t(i,j,k,2,isalt)=t(i,j,k,1,isalt)
            enddo
          enddo
        enddo

#  elif defined UPWELLING
        do k=1,N
          do j=JR_RANGE
            do i=IR_RANGE
#   define Z0 (-35.)
#   define THKNSS 6.5
#   define Z1 (-75.)
#   define STRAT 150.
             t(i,j,k,1,itemp)=14.+4.*THKNSS*log(
     &                        cosh((z_w(i,j,k )-Z0)/THKNSS)
     &                       /cosh((z_w(i,j,k-1)-Z0)/THKNSS)
     &                           )/(z_w(i,j,k)-z_w(i,j,k-1))
     &             +((z_w(i,j,k)+z_w(i,j,k-1))/2.-Z1)/STRAT
#   undef Z0
#   undef THKNSS
#   undef Z1
#   undef STRAT
              t(i,j,k,2,itemp)=t(i,j,k,1,itemp)
#   ifdef SALINITY
              t(i,j,k,1,isalt)=35.
              t(i,j,k,2,isalt)=t(i,j,k,1,isalt)
#   endif
            enddo
          enddo
        enddo

#  elif defined REGIONAL
        do k=1,N
          do j=JR_RANGE
            do i=IR_RANGE
#   define Z0 (-80.)
#   define THKNSS 50.
#   define Z1 0.
#   define STRAT 1000.
             t(i,j,k,1,itemp)=14.+5.*THKNSS*log(
     &                        cosh((z_w(i,j,k )-Z0)/THKNSS)
     &                       /cosh((z_w(i,j,k-1)-Z0)/THKNSS)
     &                           )/(z_w(i,j,k)-z_w(i,j,k-1))
     &              +((z_w(i,j,k)+z_w(i,j,k-1))/2.-Z1)/STRAT
#   undef Z0
#   undef THKNSS
#   undef Z1
#   undef STRAT
              t(i,j,k,2,itemp)=t(i,j,k,1,itemp)
#   ifdef SALINITY
              t(i,j,k,1,isalt)=35.
              t(i,j,k,2,isalt)=t(i,j,k,1,isalt)
#   endif
            enddo
          enddo
        enddo

#  elif defined JET
!
! Compute analytical t and geostrophically adjusted 
! zeta,ub,vb,u,v fields with no zonal perturbation first
!  --> store in climatological arrays
!  ... This is done both during initialization 
!      and restart procedures
!
      endif  ! nrrec.eq.0.
!
      call ana_jet_tile (Istr,Iend,Jstr,Jend,0)
!
! Compute same fields with zonal perturbation 
! --> store in initial arrays
!
      if (nrrec.eq.0) then  ! only during initialization
!
        call ana_jet_tile (Istr,Iend,Jstr,Jend,1)

#  else
        do k=1,N
          do j=JR_RANGE
            do i=IR_RANGE
              t(i,j,k,1,itemp)=20.
              t(i,j,k,2,itemp)=20.
#   ifdef SALINITY
              t(i,j,k,1,isalt)=35.
              t(i,j,k,2,isalt)=35.
#   endif
            enddo
          enddo
        enddo
#  endif
!
      endif   !  nrrec.eq.0.
!
# endif /*SOLVE3D */
#endif /* ANA_INITIAL */

#ifdef SOLVE3D
!
!--------------------------------------------------------------------
!  Initial conditions for passive tracer
!--------------------------------------------------------------------
!
# ifdef PASSIVE_TRACER 
      do k=1,N
        do j=JR_RANGE
          do i=IR_RANGE
            if (.not.got_tini(itpas)) then
#  if defined REGIONAL && !defined ANA_PSOURCE
              if (h(i,j).lt.300) then
!             if (z_r(i,j,k).lt.-800 .and.
!     &           z_r(i,j,k).gt.-1200 ) then
                t(i,j,k,1,itpas)=1
              else
                 t(i,j,k,1,itpas)=0
              endif
#  elif defined SHOREFACE || defined RIP
            if (h(i,j).lt.2) then
              t(i,j,k,1,itpas)=1.
            else
              t(i,j,k,1,itpas)=0
            endif
            t(i,j,k,2,itpas)=t(i,j,k,1,itpas)
#  else
            if (h(i,j).lt.300) then
              t(i,j,k,1,itpas)=1.
            else
              t(i,j,k,1,itpas)=0
            endif
#  endif
            endif
          enddo
        enddo
      enddo
# endif /*PASSIVE_TRACER */
!
!----------------------------------------------------------------------
!  Initial conditions for sediment tracer variables.
!----------------------------------------------------------------------
!
# ifdef SEDIMENT
      do k=1,N
        do j=JR_RANGE
          do i=IR_RANGE
            if (.not.got_tini(isand)) then
              t(i,j,k,1,isand)=Csed(1)
              t(i,j,k,2,isand)=t(i,j,k,1,isand)
            endif
            if (.not.got_tini(isilt)) then
              t(i,j,k,1,isilt)=Csed(2)
              t(i,j,k,2,isilt)=t(i,j,k,1,isilt)
            endif
          enddo
        enddo
      enddo
#  endif
!
!----------------------------------------------------------------------
!  Initial conditions for skin temperature
!----------------------------------------------------------------------
!
# ifdef SST_SKIN
      do j=JR_RANGE
        do i=IR_RANGE
          sst_skin(i,j)=t(i,j,N,1,itemp)
          dT_skin(i,j)=0.
        enddo
      enddo
# endif
!
!--------------------------------------------------------------------
!  Initial conditions for biology tracer variables.
!--------------------------------------------------------------------
!
# ifdef BIOLOGY
      do k=1,N
        do j=JR_RANGE
          do i=IR_RANGE
            if (.not.got_tini(iNO3_)) then
              temp=t(i,j,k,1,itemp)
               if (temp.lt.8.) then
                  SiO4=30.
               elseif (temp.ge.8. .and. temp.le.11.) then
                  SiO4=30.-((temp-8.)*(20./3.))
               elseif (temp.gt.11. .and. temp.le.13.) then
                  SiO4=10.-((temp-11.)*(8./2.))
               elseif (temp.gt.13. .and. temp.le.16.) then
                  SiO4=2.-((temp-13.)*(2./3.))
               elseif (temp.gt.16.) then
                 SiO4=0.
               endif
               t(i,j,k,1,iNO3_)=1.67+0.5873*SiO4+0.0144*SiO4**2
     &                               +0.0003099*SiO4**3
               t(i,j,k,2,iNO3_)=t(i,j,k,1,iNO3_)
            endif
#  ifdef PISCES
            if (.not.got_tini(iDIC_))  then
              t(i,j,k,1,iDIC_)=2150.
              t(i,j,k,2,iDIC_)=t(i,j,k,1,iDIC_)
            endif
            if (.not.got_tini(iTAL_))  then
              t(i,j,k,1,iTAL_)=2350.
              t(i,j,k,2,iTAL_)=t(i,j,k,1,iTAL_)
            endif
            if (.not.got_tini(iOXY_))  then
              t(i,j,k,1,iOXY_)=200.
              t(i,j,k,2,iOXY_)=t(i,j,k,1,iOXY_)
            endif
            if (.not.got_tini(iCAL_))  then
              t(i,j,k,1,iCAL_)=0.01
              t(i,j,k,2,iCAL_)=t(i,j,k,1,iCAL_)
            endif
            if (.not.got_tini(iPO4_))  then
             temp=t(i,j,k,1,itemp)
               if (temp.lt.8.) then
                  SiO4=30.
               elseif (temp.ge.8. .and. temp.le.11.) then
                  SiO4=30.-((temp-8.)*(20./3.))
               elseif (temp.gt.11. .and. temp.le.13.) then
                  SiO4=10.-((temp-11.)*(8./2.))
               elseif (temp.gt.13. .and. temp.le.16.) then
                  SiO4=2.-((temp-13.)*(2./3.))
               elseif (temp.gt.16.) then
                 SiO4=0.
               endif
               t(i,j,k,1,iPO4_)=(1.67+0.5873*SiO4+0.0144*SiO4**2
     &                               +0.0003099*SiO4**3)/16.
               t(i,j,k,2,iPO4_)=t(i,j,k,1,iPO4_)
            endif
            if (.not.got_tini(iPOC_))  then
              t(i,j,k,1,iPOC_)=1.e-2
              t(i,j,k,2,iPOC_)=t(i,j,k,1,iPOC_)
            endif
            if (.not.got_tini(iSIL_))  then
              t(i,j,k,1,iSIL_)=91.51
              t(i,j,k,2,iSIL_)=t(i,j,k,1,iSIL_)
            endif
            if (.not.got_tini(iPHY_))  then
              t(i,j,k,1,iPHY_)=0.01
              t(i,j,k,2,iPHY_)=t(i,j,k,1,iPHY_)
            endif
            if (.not.got_tini(iZOO_))  then
              t(i,j,k,1,iZOO_)=0.01
              t(i,j,k,2,iZOO_)=t(i,j,k,1,iZOO_)
            endif
            if (.not.got_tini(iDOC_))  then
              t(i,j,k,1,iDOC_)=5.
              t(i,j,k,2,iDOC_)=t(i,j,k,1,iDOC_)
            endif
            if (.not.got_tini(iDIA_))  then
              t(i,j,k,1,iDIA_)=0.01
              t(i,j,k,2,iDIA_)=t(i,j,k,1,iDIA_)
            endif
            if (.not.got_tini(iMES_))  then
              t(i,j,k,1,iMES_)=0.01
              t(i,j,k,2,iMES_)=t(i,j,k,1,iMES_)
            endif
            if (.not.got_tini(iBSI_))  then
              t(i,j,k,1,iBSI_)=1.5e-3
              t(i,j,k,2,iBSI_)=t(i,j,k,1,iBSI_)
            endif
            if (.not.got_tini(iFER_))  then
              t(i,j,k,1,iFER_)=6.e-4
              t(i,j,k,2,iFER_)=t(i,j,k,1,iFER_)
            endif
            if (.not.got_tini(iBFE_))  then
              t(i,j,k,1,iBFE_)=1.E-2*5E-6
              t(i,j,k,2,iBFE_)=t(i,j,k,1,iBFE_)
            endif
            if (.not.got_tini(iGOC_))  then
              t(i,j,k,1,iGOC_)=1.e-2
              t(i,j,k,2,iGOC_)=t(i,j,k,1,iGOC_)
            endif
            if (.not.got_tini(iSFE_))  then
              t(i,j,k,1,iSFE_)=1.e-2*5.E-6
              t(i,j,k,2,iSFE_)=t(i,j,k,1,iSFE_)
            endif
            if (.not.got_tini(iDFE_))  then
              t(i,j,k,1,iDFE_)=1.e-2*5.E-6
              t(i,j,k,2,iDFE_)=t(i,j,k,1,iDFE_)
            endif
            if (.not.got_tini(iDSI_))  then
              t(i,j,k,1,iDSI_)=1.e-2*0.15
              t(i,j,k,2,iDSI_)=t(i,j,k,1,iDSI_)
            endif
            if (.not.got_tini(iNFE_))  then
              t(i,j,k,1,iNFE_)=1.e-2*5.E-6
              t(i,j,k,2,iNFE_)=t(i,j,k,1,iNFE_)
            endif
            if (.not.got_tini(iNCH_))  then
              t(i,j,k,1,iNCH_)=1.e-2*12./55.
              t(i,j,k,2,iNCH_)=t(i,j,k,1,iNCH_)
            endif
            if (.not.got_tini(iDCH_))  then
              t(i,j,k,1,iDCH_)=1.e-2*12./55.
              t(i,j,k,2,iDCH_)=t(i,j,k,1,iDCH_)
            endif
            if (.not.got_tini(iNH4_))  then
              t(i,j,k,1,iNH4_)=1.e-2
              t(i,j,k,2,iNH4_)=t(i,j,k,1,iNH4_)
            endif
#  elif defined BIO_NChlPZD
            if (.not.got_tini(iChla)) then
               t(i,j,k,1,iChla)=0.08
               t(i,j,k,2,iChla)=t(i,j,k,1,iChla)
            endif
            if (.not.got_tini(iPhy1)) then
               t(i,j,k,1,iPhy1)=0.1
               t(i,j,k,2,iPhy1)=t(i,j,k,1,iPhy1)
            endif
            if (.not.got_tini(iZoo1)) then
               t(i,j,k,1,iZoo1)=0.06
               t(i,j,k,2,iZoo1)=t(i,j,k,1,iZoo1)
            endif
            if (.not.got_tini(iDet1)) then
               t(i,j,k,1,iDet1)=0.02
               t(i,j,k,2,iDet1)=t(i,j,k,1,iDet1)
            endif
#   ifdef OXYGEN
            if (.not.got_tini(iO2))  then
              t(i,j,k,1,iO2)=250.0
              t(i,j,k,2,iO2)=t(i,j,k,1,iO2)
            endif
#   endif
#  elif defined BIO_N2ChlPZD2
            if (.not.got_tini(iNH4_))  then
               t(i,j,k,1,iNH4_)=0.1
               t(i,j,k,2,iNH4_)=t(i,j,k,1,iNH4_)
            endif
            if (.not.got_tini(iChla)) then
               t(i,j,k,1,iChla)=0.08
               t(i,j,k,2,iChla)=t(i,j,k,1,iChla)
            endif
            if (.not.got_tini(iPhy1)) then
               t(i,j,k,1,iPhy1)=0.06
               t(i,j,k,2,iPhy1)=t(i,j,k,1,iPhy1)
            endif
            if (.not.got_tini(iZoo1)) then
               t(i,j,k,1,iZoo1)=0.04
               t(i,j,k,2,iZoo1)=t(i,j,k,1,iZoo1)
            endif
            if (.not.got_tini(iDet1)) then
               t(i,j,k,1,iDet1)=0.02
               t(i,j,k,2,iDet1)=t(i,j,k,1,iDet1)
            endif
            if (.not.got_tini(iDet2)) then
               t(i,j,k,1,iDet2)=0.02
               t(i,j,k,2,iDet2)=t(i,j,k,1,iDet2)
            endif
#  elif defined BIO_BioEBUS
            if (.not.got_tini(iNO2_))  then
               t(i,j,k,1,iNO2_)=0.05*exp(z_r(i,j,k)/100.) 
               t(i,j,k,2,iNO2_)=t(i,j,k,1,iNO2_)
            endif
            if (.not.got_tini(iNH4_))  then
               t(i,j,k,1,iNH4_)=0.1*exp(z_r(i,j,k)/100.) 
               t(i,j,k,2,iNH4_)=t(i,j,k,1,iNH4_)
            endif
            if (.not.got_tini(iPhy1)) then
               t(i,j,k,1,iPhy1)=0.04*exp(z_r(i,j,k)/50.) 
               t(i,j,k,2,iPhy1)=t(i,j,k,1,iPhy1)
            endif
            if (.not.got_tini(iPhy2)) then
               t(i,j,k,1,iPhy2)=0.06*exp(z_r(i,j,k)/50.) 
               t(i,j,k,2,iPhy2)=t(i,j,k,1,iPhy2)
            endif
            if (.not.got_tini(iZoo1)) then
               t(i,j,k,1,iZoo1)=0.04*exp(z_r(i,j,k)/100.) 
               t(i,j,k,2,iZoo1)=t(i,j,k,1,iZoo1)
            endif
            if (.not.got_tini(iZoo2)) then
               t(i,j,k,1,iZoo2)=0.04*exp(z_r(i,j,k)/100.) 
               t(i,j,k,2,iZoo2)=t(i,j,k,1,iZoo2)
            endif
            if (.not.got_tini(iDet1)) then
               t(i,j,k,1,iDet1)=0.02
               t(i,j,k,2,iDet1)=t(i,j,k,1,iDet1)
            endif
            if (.not.got_tini(iDet2)) then
               t(i,j,k,1,iDet2)=0.02
               t(i,j,k,2,iDet2)=t(i,j,k,1,iDet2)
            endif
            if (.not.got_tini(iDON)) then
               t(i,j,k,1,iDON)=0.5*exp(z_r(i,j,k)/100.) 
               t(i,j,k,2,iDON)=t(i,j,k,1,iDON)
            endif
            if (.not.got_tini(iO2)) then
               t(i,j,k,1,iO2)=250.*exp(z_r(i,j,k)/100.) 
               t(i,j,k,2,iO2)=t(i,j,k,1,iO2)
            endif
#   ifdef NITROUS_OXIDE
            if (.not.got_tini(iN2O)) then
              t(i,j,k,1,iN2O)=-(0.008*exp(z_r(i,j,k)/100.)-0.01)
              if (t(i,j,k,1,iN2O).lt.0.) then
                t(i,j,k,1,iN2O)=0.
              endif
              t(i,j,k,2,iN2O)=t(i,j,k,1,iN2O)
            endif
#   endif
#  endif /* PISCES */
          enddo
        enddo
      enddo
# endif /* BIOLOGY */
#endif /* SOLVE3D */
!
!--------------------------------------------------------------------
!  Exchange boundary information
!--------------------------------------------------------------------
!

#if defined EW_PERIODIC || defined NS_PERIODIC || defined  MPI
# ifdef ANA_INITIAL
      call exchange_r2d_tile (Istr,Iend,Jstr,Jend,
     &                        zeta(START_2D_ARRAY,1))
      call exchange_u2d_tile (Istr,Iend,Jstr,Jend,
     &                        ubar(START_2D_ARRAY,1))
      call exchange_v2d_tile (Istr,Iend,Jstr,Jend,
     &                        vbar(START_2D_ARRAY,1))
#  ifdef SOLVE3D
      call exchange_u3d_tile (Istr,Iend,Jstr,Jend,
     &                        u(START_2D_ARRAY,1,1))
      call exchange_v3d_tile (Istr,Iend,Jstr,Jend,
     &                        v(START_2D_ARRAY,1,1))
#  endif /* SOLVE3D */
# endif /* ANA_INITIAL */
# ifdef SOLVE3D
      do itrc=1,NT
        if (.not.got_tini(itrc)) then
#  ifdef THREE_GHOST_POINTS_TS
          call exchange_r3d_3pts_tile (Istr,Iend,Jstr,Jend,
     &                                 t(START_2D_ARRAY,1,1,itrc))
#  else
          call exchange_r3d_tile (Istr,Iend,Jstr,Jend,
     &                            t(START_2D_ARRAY,1,1,itrc))
#  endif
        endif
      enddo
# endif
#endif

#undef IR_RANGE
#undef IU_RANGE
#undef JR_RANGE
#undef JV_RANGE

      return
      end

#ifdef JET
!======================================================================
!
      subroutine ana_jet_tile (Istr,Iend,Jstr,Jend, jet_perturb)
!
!======================================================================
!
!  Set initial conditions for momentum and tracer variables using
!  analytical expressions for the JET test case
!
!----------------------------------------------------------------------
!
# ifdef AGRIF
      use Agrif_Util
# endif
      implicit none
# include "param.h"
# include "grid.h"
# include "ocean2d.h"
# include "ocean3d.h"
# include "scalars.h"
# include "mpi_roms.h"
# include "private_scratch.h"
# ifdef CLIMATOLOGY
#  include "climat.h"
# endif
      integer Istr,Iend,Jstr,Jend, i,j,k, itrc, 
     &        jet_perturb, tndx
      real cff,x,y,Fyz,
     &     rhomaxs,bgdrhos,zs1,dzs,drhos,drhosfs,
     &     rhomaxn,bgdrhon,zn1,dzn,drhon,drhosfn,
     &     drhosf,z0,z0p,dzs_a,dzn_a,zk,
     &     Ljet,cff_perturb,ierr,tanh
      real*QUAD mssh,area,cff1
      real rhoprof(0:N,2)
      real ru(PRIVATE_2D_SCRATCH_ARRAY,N),   
     &     rv(PRIVATE_2D_SCRATCH_ARRAY,N),   
     &      P(PRIVATE_2D_SCRATCH_ARRAY,N), 
     &     FC(PRIVATE_2D_SCRATCH_ARRAY),
     &    dZx(PRIVATE_2D_SCRATCH_ARRAY),
     &     rx(PRIVATE_2D_SCRATCH_ARRAY),
     &    dRx(PRIVATE_2D_SCRATCH_ARRAY)
# ifdef MPI
      include 'mpif.h'
      real*QUAD allmssh(1,NNODES),allarea(1,NNODES)
# endif
      real asymth,zz,zasym,dzasym
      asymth(zz,zasym,dzasym) = 
     &         (zz-zasym)
     &         *sqrt(1+0.5*((zz-zasym)+abs(zz-zasym))**2/dzasym**2)
     &         +zasym
!
# include "compute_auxiliary_bounds.h"
!
! ---------------------------------------------------------------------
!                     BAROCLINIC JET TEST CASE
! ---------------------------------------------------------------------
!
! --- Jet and density profile parameters ---
!
      Ljet        = 1600.e3;  ! Jet width in km
!
! southern profile
      rhomaxs=27.75     ! max density
      bgdrhos=9.8e-6    ! background stratification
      zs1=-1000
      dzs=700
      drhos=1.4         ! gives strength of Phillips mode (0 to 1.4)
! northern profile 
      rhomaxn=27.7573   ! max density
      bgdrhon=9.8e-6    ! background stratification
      zn1=-400 
      dzn=300
! surface Charney mode 
      drhosfs=1.5       ! northern anomaly  (0 for Phillips mode only)
      drhosfn=0.0       ! southern anomaly  (0 for Phillips mode only)
      z0=-300.          ! vertical penetration (-600 for strong Charney mode)
      drhosf=0.00       ! weakens Charney mode (canceled for 1.05)
      z0p=-110.         ! only used for drhosf non zero
!
! zonal perturbation
!
      if (jet_perturb .eq. 0) then
        cff_perturb = 0.0    ! zonal perturbation coefficient
        tndx=2
      else
        cff_perturb = 0.02
        tndx=1
        zeta(:,:,tndx)=0.
        call set_depth_tile (Istr,Iend,Jstr,Jend)
      endif
!
! --- Build northern and southern density profiles ---
!
! First compute background density profiles associated with
! a gentle stratification that does not depend 
! on jet side. It is there to ensure static stability. 
      do k=1,N
        zk = z_r(istr,jstrR,k)
        rhoprof(k,1)=rhomaxs-bgdrhos*(zk+h(istr,jstrR))
        rhoprof(k,2)=rhomaxn-bgdrhon*(zk+h(istr,jstrR))
      enddo
!
! Second, get main north/south contrast with a distorded 
! tanh shape of variable amplitude.Distorsion of the tanh 
! is done with the asym function that increases 
! stratification in the upper ocean
      dzs_a=1.3*dzs
      do k=1,N
        zk=asymth(z_r(istr,jstrR,k),zs1,dzs_a)
        rhoprof(k,1)=rhoprof(k,1)-drhos*(0.5+0.5*tanh((zk-zs1)/dzs))
      enddo
      dzn_a=1.3*dzn
      drhon=-(rhoprof(N,1)-rhoprof(N,2))/(0.5+0.5*tanh((zk-zn1)/dzn))
      do k=1,N
        zk=asymth(z_r(istr,jstrR,k),zn1,dzn_a)
        rhoprof(k,2)=rhoprof(k,2)-drhon*(0.5+0.5*tanh((zk-zn1)/dzn))
      enddo
      do k=1,N
        zk = z_r(istr,jstrR,k)
        rhoprof(k,1)=rhoprof(k,1)
     &             -drhosf*(exp((zk-z0p)/abs(z0p)))/(exp(1.))
        rhoprof(k,1)=rhoprof(k,1)
     &             -drhosfs*0.5*(1+tanh((zk-z0)/abs(z0)))/tanh(1.)
        rhoprof(k,2)=rhoprof(k,2)
     &             -drhosf*(exp((zk-z0p)/abs(z0p)))/(exp(1.))
        rhoprof(k,2)=rhoprof(k,2)
     &             -drhosfn*0.5*(1+tanh((zk-z0)/abs(z0)))/tanh(1.)
      enddo
!
! --- Fit southern and northern profiles ---
!
      do k=1,N
        do j=JstrR,JendR
          do i=IstrR,IendR
# ifdef MPI
            y=j-0.5+jj*Mm
            x=i-0.5+ii*Lm
# else
            y=j-0.5
            x=i-0.5
# endif
            y=y/real(MMm)-0.5
            x=x/real(LLm)
!
! Add vertical attenuation and XI perturbation
!
            y = y + cff_perturb*exp(z_r(i,j,k)/1000.)
     &                         *exp(-(x-0.5)**2/0.05)
     &                         *( 0.5*sin(2.*pi*x)
     &                           +0.5*sin(6.*pi*x) )
!
! Set Jet structure and place its center at y=pi/2
!
            y = y*pi*el/Ljet + pi/2 

            if (y.lt.0) then
              Fyz=1.0
            elseif (y.gt.pi) then
              Fyz=0.0
            else
              Fyz=1.0-(y-sin(y)*cos(y))/pi
            endif
            rho(i,j,k)=Fyz*rhoprof(k,1)+(1.-Fyz)*rhoprof(k,2)
          enddo
        enddo
      enddo
!
! Fill tracer array and make exchanges
!
      do k=1,N
        do j=JstrR,JendR
          do i=IstrR,IendR
            t(i,j,k,tndx,1)=rho(i,j,k)
          enddo
        enddo
      enddo
# if defined EW_PERIODIC || defined NS_PERIODIC || defined  MPI
#  ifdef THREE_GHOST_POINTS_TS
      call exchange_r3d_3pts_tile (Istr,Iend,Jstr,Jend,
     &                             t(START_2D_ARRAY,1,tndx,1))
#  else
      call exchange_r3d_tile (Istr,Iend,Jstr,Jend,
     &                        t(START_2D_ARRAY,1,tndx,1))
#  endif
      call exchange_r3d_tile (Istr,Iend,Jstr,Jend,
     &                        rho(START_2D_ARRAY,1))
# endif
!
! --- Compute zeta ---
!
! Get pressure using pressure gradient routine
!
      call prsgrd_tile (Istr,Iend,Jstr,Jend, ru,rv, P,
     &                  A2d(1,3,0),A2d(1,4,0), FC,dZx,rx,dRx)
!
! Compute zeta so as to cancel bottom pressure perturbation
!
      do j=Jstr,Jend
        do i=Istr,Iend
          zeta(i,j,tndx)=-P(i,j,1)/g
        enddo
      enddo
!
! Remove mean sea level (mssh)  !!! MPI DEBUG problems here !!!
!
      mssh=QuadZero
      area=QuadZero
      do j=Jstr,Jend
        do i=Istr,Iend
           cff1=1./(pm(i,j)*pn(i,j))
           mssh=mssh+cff1*zeta(i,j,tndx)
           area=area+cff1
        enddo
      enddo
      mssh=mssh/area
# ifdef MPI
      call MPI_ALLGATHER(mssh,1,MPI_DOUBLE_PRECISION,
     &                allmssh,1,MPI_DOUBLE_PRECISION,
     &                          MPI_COMM_WORLD,ierr)
      call MPI_ALLGATHER(area,1,MPI_DOUBLE_PRECISION,
     &                allarea,1,MPI_DOUBLE_PRECISION,
     &                          MPI_COMM_WORLD,ierr)
      mssh=QuadZero
      area=QuadZero
      do i=1,NNODES
        mssh=mssh+allmssh(1,i)*allarea(1,i)
        area=area+allarea(1,i)
      enddo
      mssh=mssh/area
# endif
      do j=Jstr,Jend
        do i=Istr,Iend
          zeta(i,j,tndx)=zeta(i,j,tndx)
# ifndef RVTK_DEBUG
     &                          - mssh
# endif
        enddo
      enddo
!
! Apply zeta boundary conditions
!
      if (SOUTHERN_EDGE) then
        do i=Istr,Iend                     ! Southern edge gradient BC
          zeta(i,Jstr-1,tndx)=zeta(i,Jstr,tndx)
          enddo
      endif
      if (NORTHERN_EDGE) then
        do i=istr,iend                     ! Northern edge gradient BC
          zeta(i,Jend+1,tndx)=zeta(i,Jend,tndx)
        enddo
      endif
# if defined EW_PERIODIC || defined NS_PERIODIC || defined  MPI
      call exchange_r2d_tile (istr,iend,jstr,jend,
     &                        zeta(START_2D_ARRAY,tndx))
# endif
!
! --- Get gressure gradient (ru,rv) using updated zeta field ---
!
      call set_depth_tile (Istr,Iend,Jstr,Jend)
      call prsgrd_tile (Istr,Iend,Jstr,Jend, ru,rv, P,
     &                  A2d(1,3,0),A2d(1,4,0), FC,dZx,rx,dRx)

# define WORK rho1
      do k=1,N
        do j=JstrV,Jend
          do i=Istr,Iend
            WORK(i,j,k)=rv(i,j,k)
          enddo
        enddo
      enddo
      if (NORTHERN_EDGE) then
        do k=1,N
          do i=Istr-1,Iend+1
            WORK(i,Jend+1,k)=0.
          enddo
        enddo
      endif
      if (SOUTHERN_EDGE) then
        do k=1,N
          do i=Istr-1,Iend+1
            WORK(i,JstrV-1,k)=0.
          enddo
        enddo
      endif
# if defined EW_PERIODIC || defined NS_PERIODIC || defined  MPI
      call exchange_r3d_tile (Istr,Iend,Jstr,Jend,
     &                        WORK(START_2D_ARRAY,1))
# endif
      do k=1,N
        do j=JstrV-1,Jend+1
          do i=Istr-1,Iend+1
            rv(i,j,k)=WORK(i,j,k)
          enddo
        enddo
      enddo
!
      do k=1,N
        do j=Jstr,Jend
          do i=IstrU,Iend
            WORK(i,j,k)=ru(i,j,k)
          enddo
        enddo
      enddo
      if (NORTHERN_EDGE) then
        do k=1,N
          do i=Istr-1,Iend+1
            WORK(i,Jend+1,k)=0.
          enddo
        enddo
      endif
      if (SOUTHERN_EDGE) then
        do k=1,N
          do i=Istr-1,Iend+1
            WORK(i,0,k)=0.
          enddo
        enddo
      endif
# if defined EW_PERIODIC || defined NS_PERIODIC || defined  MPI
      call exchange_r3d_tile (istr,iend,jstr,jend,
     &                        WORK(START_2D_ARRAY,1))
# endif
      do k=1,N
        do j=Jstr-1,Jend+1
          do i=IstrU-1,Iend+1
            ru(i,j,k)=WORK(i,j,k)
          enddo
        enddo
      enddo
# undef WORK
!
! --- Compute geostrophic u,v velocities ---
!
      do k=1,N
        do j=Jstr,Jend
          do i=Istr,Iend
            cff=0.5*(pm(i-1,j)+pm(i,j))
     &             *(pn(i-1,j)+pn(i,j))
     &             /(Hz(i-1,j,k)+Hz(i,j,k))/f(i,j)
            u(i,j,k,tndx) = 0.25*cff*
     &            (rv(i,j,k)+rv(i,j+1,k)+rv(i-1,j,k)+rv(i-1,j+1,k))
          enddo
        enddo
      enddo
      do k=1,N
        do j=Jstr,Jend
          do i=Istr,Iend
            cff=0.5*(pm(i,j)+pm(i,j-1))
     &             *(pn(i,j)+pn(i,j-1))
     &             /(Hz(i,j,k)+Hz(i,j-1,k))/f(i,j)
            v(i,j,k,tndx) =-0.25*cff*
     &            (ru(i,j,k)+ru(i+1,j,k)+ru(i,j-1,k)+ru(i+1,j-1,k))
          enddo
        enddo
      enddo
!
! --- Compute barotropic u velocity ---
!
      ubar(:,:,tndx)=0.
      do k=1,N
        do j=Jstr,Jend
          do i=Istr,Iend
            ubar(i,j,tndx)=ubar(i,j,tndx)+u(i,j,k,tndx)
     &                             *0.5*(Hz(i-1,j,k)+Hz(i,j,k))
          enddo
        enddo
      enddo
      do j=Jstr,Jend
        do i=Istr,Iend
          ubar(i,j,tndx) = ubar(i,j,tndx) /
     &                  ( 0.5*(zeta(i,j,tndx)+zeta(i-1,j,tndx))
     &                  + 0.5*(h(i,j)+h(i-1,j)) )
        enddo
      enddo
!
! --- Compute barotropic v velocity ---
!
      vbar(:,:,tndx)=0.
      do k=1,N
        do j=Jstr,Jend
          do i=Istr,Iend
            vbar(i,j,tndx)=vbar(i,j,tndx)+v(i,j,k,tndx)
     &                           *0.5*(Hz(i,j-1,k)+Hz(i,j,k))
          enddo
        enddo
      enddo
      do j=Jstr,Jend
        do i=Istr,Iend
          vbar(i,j,tndx) = vbar(i,j,tndx) / 
     &                ( 0.5*(zeta(i,j,tndx)+zeta(i,j-1,tndx))
     &                + 0.5*(h(i,j)+h(i,j-1)) )
        enddo
      enddo

# ifdef CLIMATOLOGY
      if (jet_perturb .eq. 0) then
!
! Fill climatological arrays. MPI exchanges are done in analytical
!
        do k=1,N
          do j=Jstr,Jend
            do i=Istr,Iend
              uclm(i,j,k) = u(i,j,k,tndx)
              vclm(i,j,k) = v(i,j,k,tndx)
              tclm(i,j,k,1) = t(i,j,k,tndx,1)
            enddo
          enddo
        enddo
        do j=Jstr,Jend
          do i=Istr,Iend
            ssh(i,j) = zeta(i,j,tndx)
          enddo
        enddo
        do j=Jstr,Jend
          do i=Istr,Iend
            ubclm(i,j) = ubar(i,j,tndx)
          enddo
        enddo
        do j=Jstr,Jend
          do i=Istr,Iend
            vbclm(i,j) = vbar(i,j,tndx)
          enddo
        enddo
      endif
# endif

      return
      end
#endif /* JET */

