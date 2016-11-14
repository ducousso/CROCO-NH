! $Id: ana_grid.F 1620 2015-01-08 10:47:13Z marchesiello $
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
#ifdef ANA_GRID
      subroutine ana_grid (tile)
      implicit none
# include "param.h"
      integer tile, trd
C$    integer omp_get_thread_num
# include "compute_tile_bounds.h"
      call ana_grid_tile (Istr,Iend,Jstr,Jend)
      return
      end

      subroutine ana_grid_tile (Istr,Iend,Jstr,Jend)
!
!  Set up model grid using analytical expressions:
!-------------------------------------------------------------------
!
!  INPUT: Grid configuration parameters
!  ------------------------------------
! Length_XI      are the physical dimensions of the computational
! Length_ETA     domain [usually measured in  meters];
!
! depth          is the maximum depth [meters, positive];
! f0,beta        are coriolis parameters which set up a beta-plane
!                                                  [1/s, 1/(m*s)].
!
!  OUTPUT: stored in common blocks, see files "scalars.h" "grid.h"
!  ---------------------------------------------------------------
!  xl,el      Physical dimensions of the computational domain 
!                                    [usually measured in  meters];  
!  h          Model bathymetry [meters, positive] at RHO-points.
!  hmin,hmax  Minimum and maximum values of depth of bathymetry [m].
!  f          Coriolis parameter (1/seconds) at RHO-points.
!  pm,pn      Coordinate transformation metric "m" [1/meters]
!                    associated with the differential distances in 
!                    XI- and ETA-directions, both are at RHO-points.
!  xp,xr      XI-coordinates [m] at PSI- and RHO-points.
!  yp,yr      ETA-coordinates [m] at PSI- and RHO-points.
!-------------------------------------------------------------------
!
# ifdef AGRIF
      use Agrif_Util
# endif
      implicit none
      integer Istr,Iend,Jstr,Jend, i,j,imax,jchn
# include "param.h"
# include "grid.h"
# include "scalars.h"
!
      real Length_XI,Length_ETA, depth, f0,beta, 
     &     cff,y, x0,y0,dx,dy, NSUB

# ifdef INTERNAL
      real cff1, ridge_width, ridge_height
# endif
# ifdef RIP
      real h0,per,xx,yy,xs,db,alpha,lambda,eps
# endif
#ifdef MPI
# define LOCALLM Lmmpi
# define LOCALMM Mmmpi
#else
# define LOCALLM Lm
# define LOCALMM Mm
#endif
!
# include "compute_extended_bounds.h"
!
# if defined BASIN
                     depth=5000.
                     f0=1.E-4 
                     beta=2.E-11
# elif defined CANYON_A || defined CANYON_B
                     depth=4000.
                     f0=1.E-4
                     beta=0.
# elif defined EQUATOR
                     depth=5000.
                     f0=0.
                     beta=2.2829e-11
# elif defined GRAV_ADJ
#  ifdef NBQ
                     depth=0.29      ! Horn et al. (2001)
#  else
                     depth=20.
#  endif
                     f0=0.
                     beta=0.
# elif defined ACOUSTIC
                     depth=128.
                     f0=0.
                     beta=0.
# elif defined INNERSHELF
                     depth=0.
                     f0=4*pi/86400*sin(-21*pi/180)
                     beta=0.
# elif defined INTERNAL
                     depth=2000.
                     f0=1.e-4
                     beta=0.
                     ridge_width=30.e3
                     ridge_height=1600.
# elif defined OVERFLOW
                     depth=40.
                     f0=0.
                     beta=0.
# elif defined SEAMOUNT
                     depth=4500.
                     beta=0.
                     f0=1.E-4
# elif defined SHELFRONT
                     depth=1660.
                     f0=1.E-4
                     beta=0.
# elif defined SOLITON
                     depth=1.
                     f0=0.
                     beta=1.
# elif defined UPWELLING
                     depth=150.
                     f0=-8.26E-5
                     beta=0.
# elif defined SED_TEST2
                     depth=10.0    
                     f0=1.E-4
                     beta=0.
# elif defined RIVER
                     depth=150.
                     f0=8.26E-5
                     beta=0.
# elif defined JET
                     depth=4000.
                     f0=1.E-4
                     beta=1.6E-11
# elif defined SHOREFACE
                     depth=15.
                     f0=0.   
                     beta=0.
# elif defined SWASH
                     depth=1.
                     f0=0.   
                     beta=0.
# elif defined RIP
                     depth=15.
                     f0=0.   
                     beta=0.
# elif defined THACKER
                     depth=10.
                     beta=0.
#  ifdef THACKER_2DV
                     f0=0.
#  else
                     f0=1.E-4
#  endif
# elif defined TANK
#  ifndef MOVING_BATHY 
                     depth=10.
#  else
                     depth=0.394
#  endif
                     f0=0.
                     beta=0.
# else
                     depth=???
                     f0=???
                     beta=???
# endif
# ifdef AGRIF
      if (Agrif_Root()) then
# endif
# if defined BASIN
                     Length_XI =3600.0e+3
                     Length_ETA=2800.0e+3
# elif defined CANYON_A || defined CANYON_B
                     Length_XI =128.0e+3
                     Length_ETA=96.0e+3
# elif defined EQUATOR
                     Length_ETA=3200.0E+3
                     Length_XI =4000.0E+3
# elif defined GRAV_ADJ
#  ifdef NBQ
#   ifdef GRAV_ADJ_SOLITON
                     Length_XI =6.0    ! Horn et al. (2001)
#   else
                     Length_XI =3.0
#   endif
                     Length_ETA=0.1
#  else
                     Length_XI =64.0E+3
                     Length_ETA=2.0E+3
#  endif
# elif defined ACOUSTIC
                     Length_XI =128.0
                     Length_ETA=2.0
# elif defined INNERSHELF
                     Length_XI =200.0e+3
                     Length_ETA=3.0e+3
# elif defined INTERNAL
!                     Length_XI =2400.0e+3
                     Length_XI =1200.0e+3
!                     Length_ETA=100.0e+3
                     Length_ETA=6.0e+3
# elif defined OVERFLOW
                     Length_XI =2.0E+3
                     Length_ETA=64.0E+3
# elif defined SEAMOUNT
                     Length_XI =512.0e+3
                     Length_ETA=512.0e+3
# elif defined SHELFRONT
                     Length_XI =20.0e+3
                     Length_ETA=200.0e+3
# elif defined SOLITON
                     Length_XI =48.
                     Length_ETA=16.
# elif defined UPWELLING
                     Length_ETA=8.E+4
                     Length_XI =LLm*Length_ETA/MMm
# elif defined SED_TEST2
                     Length_XI =25000.0
                     Length_ETA=2500.0
# elif defined RIVER
                     Length_ETA=8.E+4
                     Length_XI =LLm*Length_ETA/MMm
# elif defined JET
                     Length_XI =500.e+3 
                     Length_ETA=2000.e+3 
# elif defined SHOREFACE
                    Length_XI =1180.0 
                    Length_ETA=140.0
# elif defined SWASH
                    Length_XI =110.0 
                    Length_ETA=MMm*Length_XI/LLm
# elif defined RIP
                    Length_XI =768.0 
                    Length_ETA=768.0
# elif defined THACKER
                     Length_XI =199.e+3 
#  ifdef THACKER_2DV
                     Length_ETA=3.e+3
#  else
                     Length_ETA=199.e+3
#  endif
# elif defined TANK
#  ifndef MOVING_BATHY
#   ifndef TANKY
                     Length_XI =10.0
                     Length_ETA=0.20
#   else
                     Length_XI =0.20
                     Length_ETA=10.0
#   endif
#  else
                     Length_XI =4.0
                     Length_ETA=0.001
#  endif
# else
                     Length_XI =???
                     Length_ETA=??? 
# endif
# ifdef AGRIF
      else
!
! Child grid dimension
!
        Length_XI=
     &       Agrif_Parent_xp(Agrif_Ix()+Lm/Agrif_IRhox(),Agrif_Iy())
     &      -Agrif_Parent_xp(Agrif_Ix(),Agrif_Iy())
        Length_ETA=
     &       Agrif_Parent_yp(Agrif_Ix(),Agrif_Iy()+Mm/Agrif_IRhoy())
     &      -Agrif_Parent_yp(Agrif_Ix(),Agrif_Iy())
      endif
# endif /* AGRIF */
!
! Copy physical dimensions of the grid into globally vizible  variables
!
      xl=Length_XI
      el=Length_ETA
!
! Set grid spacings for rectangular grids
!
      dx=Length_XI/float(LLm)
      dy=Length_ETA/float(MMm)
!
! Set reference point location
!
      x0=0.
      y0=0.
# ifdef EQUATOR
      y0=y0-0.5*Length_ETA  
# endif
# if defined INTERNAL || defined THACKER
      x0=x0-0.5*Length_XI
      y0=y0-0.5*Length_ETA  
# endif

CR      write(*,'(4(A3,pe15.9,1x),I3)') 'dx=',dx, 'dy=',dy,
CR     &                                'x0=',x0, 'y0=',y0, mynode

!
! Setup rectangulag grid:  coordinates (XI,ETA) at PSI- and RHO-points.
!
      do j=JstrR,JendR
        do i=IstrR,IendR
         xp(i,j) = x0 + dx*float(i+iminmpi-1-1)
         xr(i,j) = x0 + dx*(float(i+iminmpi-1-1)+0.5)
         yp(i,j) = y0 + dy*float(j+jminmpi-1-1)
         yr(i,j) = y0 + dy*(float(j+jminmpi-1-1)+0.5)
        enddo
      enddo
!
! Compute coordinate transformation metrics at RHO-points "pm" and
! "pn"  (1/m) associated with the differential distances in XI and
! ETA, respectively.
!
      do j=JstrR,JendR
        do i=IstrR,IendR
          pm(i,j)=1./dx
          pn(i,j)=1./dy
        enddo
      enddo
!
!  Set Coriolis parameter [1/s] at RHO-points.
!
      do j=JstrR,JendR
        do i=IstrR,IendR
#  ifdef EQUATOR
          f(i,j)=f0+beta*yr(i,j)
#  else
          f(i,j)=f0+beta*(yr(i,j)-(0.5*el))
#  endif
        enddo
      enddo   
!
!  Set bathymetry [meters; positive] at RHO-points.
!
# if defined BASIN || defined EQUATOR || defined GRAV_ADJ \
                   || defined SOLITON || defined JET \
                   || defined ACOUSTIC
      do j=JstrR,JendR
        do i=IstrR,IendR
          h(i,j)=depth
        enddo
      enddo
# elif defined CANYON_A || defined CANYON_B
      do j=JstrR,JendR
        do i=IstrR,IendR
          cff=32000.-16000.*(sin(pi*xr(i,j)/Length_XI))**24
          h(i,j)=20.+0.5*(depth-20.)*(1.+tanh((yr(i,j)-cff)/10000.))
        enddo
      enddo
# elif defined INNERSHELF
      do j=JstrR,JendR
        do i=IstrR,IendR
!         --- constant slope ---
          h(i,j)=1.e-3*(xl+0.5*dx-xr(i,j))+4.
!         --- shelf/slope tanh ---
!          h(i,j)=30.+500.*(1+sinh((150.e3-xr(i,j))/40.e3)
!     &                      /cosh((150.e3-xr(i,j))/40.e3))
        enddo
      enddo
# elif defined INTERNAL
      cff1=1./(ridge_width*ridge_width)
      do j=JstrR,JendR
        do i=IstrR,IendR
          if ((xr(i,j).gt.ridge_width).or.
     &       (xr(i,j).lt.(-ridge_width))) then
            cff=0.
          else
            cff=1-(xr(i,j)*xr(i,j)*cff1);
          endif
          h(i,j)=depth-ridge_height*cff*cff;
#   ifdef INTERNALSHELF
          if (xr(i,j).ge.0.) then
            h(i,j)=depth-ridge_height
          endif
#   endif
        enddo
      enddo
# elif defined OVERFLOW
      do j=JstrR,JendR
        do i=IstrR,IendR
          h(i,j)=20.+0.5*(depth-20.)*(1.+
     &                         tanh((yr(i,j)-40000.)/5000.))
        enddo
      enddo
# elif defined SEAMOUNT
      cff=(1./50.e3)**2
      do j=JstrR,JendR
        do i=IstrR,IendR
          h(i,j)=depth*(1-0.6*exp(-cff*((xr(i,j)-0.5*xl)**2+
     &                                  (yr(i,j)-0.5*el)**2))) 
        enddo
      enddo
# elif defined SHELFRONT
      do j=JstrR,JendR
        do i=IstrR,IendR
          cff=yr(i,j)/1000.
          if (cff.lt.50.) then
            h(i,j)=50.+2.*cff
          elseif (cff.lt.60.) then
            h(i,j)=160.+1.5*(cff-50.)**2-0.1*(cff-60.0)**2
          elseif (cff.lt.100.) then
            h(i,j)=310.+30.*(cff-60.)
          elseif (cff.lt.110.) then
            h(i,j)=1660.-1.5*(cff-110.)**2
          else
            h(i,j)=1660.
          endif
        enddo
      enddo
# elif defined SED_TEST2
      do j=Jstr,Jend
        do i=Istr,Iend
          val1=user(3)            !ocean-side depth
          val2=user(4)            !nearshore depth
          val3=user(5)/3.0        ! ~ width of slope
          h(i,j)=0.5*(val1+
     &     (val1-val2)*TANH((7000.0-xr(i,j))/val3))  
        enddo
      enddo
# elif defined UPWELLING
      do j=JstrR,JendR
#  ifdef MPI
        y=dy*float(j+jj*Mm)
#  else
        y=dy*float(j)
#  endif
        if (y.gt.Length_ETA/2.) y=Length_ETA-y+dy
        cff=min(depth,84.5+66.526*tanh(0.00015*(y-0.125*Length_ETA)))
        do i=IstrR,IendR
          h(i,j)=cff
        enddo
      enddo
# elif defined RIVER
      do i=IstrR,IendR
#  ifdef MPI
        cff=(float(i +ii*Lm)-0.5)/float(LLm)
#  else
        cff=(float(i       )-0.5)/float(LLm)
#  endif
        if (cff.lt.0.05) then
          h(i,JstrR)=15.
        elseif (cff.lt.0.15) then
          h(i,JstrR)=15.+843.75*(cff-0.05)**2
        elseif (cff.lt.0.85) then
          h(i,JstrR)=15.+168.75*(cff-0.1)
        elseif (cff.lt.0.95) then
          h(i,JstrR)=150.-843.75*(cff-0.95)**2
        else
          h(i,JstrR)=150.
        endif
      enddo
      do j=JstrR+1,JendR
        do i=IstrR,IendR
          h(i,j)=h(i,JstrR)
        enddo
      enddo
# elif defined SHOREFACE
      do j=JstrR,JendR
        do i=IstrR,IendR
#  ifdef MPI
          cff=(float(i +ii*Lm) )
#  else
          cff=(float(i       ) )
#  endif
#  ifdef WET_DRY
          h(i,j)=12.0-0.0125*Length_XI/float(LLm+1)*cff ! original fonction
#  else
          h(i,j)=12.0-0.0125*Length_XI/float(LLm+1)*cff + 3.
#  endif
#  ifdef MASKING
          rmask(i,j)=1.0
          if (h(i,j).lt.0.01) then
            h(i,j)=0.01
            rmask(i,j)=0.0
            rmask(48,j)=1.0
          endif
#  endif
        enddo
      enddo
# elif defined SWASH
      do j=JstrR,JendR
        do i=IstrR,IendR
#  ifdef MPI
          cff=(float(i +ii*Lm) )
#  else
          cff=(float(i       ) )
#  endif
          h(i,j)=1.0571-0.0125*Length_XI/float(LLm+1)*cff  ! GLOBEX profil
#  ifndef WET_DRY
     &                                              + 0.6
#  endif
          if (xr(i,j).lt.16.57) h(i,j)=0.85               ! GLOBEX
        enddo
      enddo
# elif defined RIP
#  ifdef GRANDPOPO
!
!  Idealized Grand Popo Beach in Benin
!
      xs=85;        ! inner surf zone 
      db=50;        ! distance from xs to sand bar
      alpha=0.01;
      do j=JstrR,JendR
        do i=IstrR,IendR
#  ifdef OBC_EAST
          xx=xr(i,j)
#  else
          xx=Length_XI-xr(i,j)
#  endif
          h(i,j)=-4.5 
     &       -1.7*exp(-6*(((xx-xs-db)/db)**2))
     &       +3.1*(1+tanh(0.025*(xx-xs)))
     &       +0.014*(xx+log(cosh(alpha*(xx-xs))/cosh(alpha*xs))/alpha)
        enddo
      enddo
#  else
!
! Weir et al. 2011 after Lippmann et al. 1999 (idealization of Duck beach)
!
      xs=150      ! inner surf zone 
      db=80       ! distance from xs to sand bar
      alpha=0.02
      lambda=256  ! sand bar perturbation wavelength
#  ifdef RIP_TOPO_2D
      eps=0.0     ! no sand bar perturbation
#  else
      eps=0.1     ! sand bar perturbation magnitude
#  endif
      do j=JstrR,JendR
        do i=IstrR,IendR
#  ifdef OBC_EAST
          xx=xr(i,j)
#  else
          xx=Length_XI-xr(i,j)
#  endif
          yy=yr(i,j);
          h0=-1.5*exp(-5*(((xx-xs-db)/db)**2))
     &       +1.35*(1+tanh(0.0253*(xx-xs)))
     &       +0.0032*(xx+log(cosh(alpha*(xx-xs))/cosh(alpha*xs))/alpha)
          per=eps*cos(2*pi*yy/lambda)*exp(-5*(((xx-xs-db)/db)**2))
          h(i,j)=(1+per)*h0;
        enddo
      enddo
#  endif
# elif defined THACKER
      cff=80.e3
      do j=JstrR,JendR
        do i=IstrR,IendR
#  ifdef THACKER_2DV
          h(i,j)=depth*(1-(xr(i,j)/cff)**2)
#  else
          h(i,j)=depth*(1-(xr(i,j)/cff)**2 -
     &                    (yr(i,j)/cff)**2 )
#  endif
        enddo
      enddo
# elif defined TANK
#  ifndef MOVING_BATHY
      do j=JstrR,JendR
        do i=IstrR,IendR
          h(i,j)=depth
        enddo
      enddo
#  else
      cff=1./(0.03686)**2
      do j=JstrR,JendR
        do i=IstrR,IendR
          h(i,j)=depth-0.1*exp(-cff*((xr(i,j)-2.)**2+
     &                               (yr(i,j)-el*0.5)**2)) 
        enddo
      enddo
#  endif      
# else
      do j=JstrR,JendR
        do i=IstrR,IendR
          h(i,j)=???
        enddo
      enddo
# endif

!
!  Set masking at RHO-points.
!
# ifdef MASKING
#  ifdef RIVER
      do j=JstrR,JendR             ! Set mask to all-water status,
        do i=IstrR,IendR           ! then mask out 5-point wide strip
          rmask(i,j)=1.            ! of land on the west, and finally
        enddo                      ! carve a 1-point wide channel
      enddo                        ! through that strip.
      imax=8
      jchn=60
      do j=JstrR,JendR
        do i=IstrR,imax
          rmask(i,j)=0.            ! <-- strip of land
        enddo
      enddo
      j=jchn
        if (j.ge.JstrR .and. j.le.JendR) then
          do i=IstrR+3,imax
            rmask(i,jchn-20)=1.
            rmask(i,jchn)=1.                   ! <-- channel
          enddo
        endif
      do j=jchn-6,jchn
        if (j.ge.JstrR .and. j.le.JendR) then
            rmask(3,j)=1.                      ! <-- channel
        endif
      enddo
#  elif defined TANK || defined GRAV_ADJ || defined ACOUSTIC\
                     || defined INTERNAL || defined IGW
      gamma2=1.
      do j=JstrR,JendR
        do i=IstrR,IendR
          rmask(i,j)=1.
        enddo
      enddo
      if (SOUTHERN_EDGE) then
        do i=IstrR,IendR
          rmask(i,0)=0.
        enddo
      endif
      if (NORTHERN_EDGE) then
        do i=IstrR,IendR
          rmask(i,LOCALMM+1)=0.
        enddo
      endif
      if (WESTERN_EDGE) then
        do j=JstrR,JendR
          rmask(0,j)=0.
        enddo
      endif
      if (EASTERN_EDGE) then
        do j=JstrR,JendR
          rmask(LOCALLM+1,j)=0.
        enddo
      endif
#  elif defined SWASH
      gamma2=1.
      do j=JstrR,JendR
        do i=IstrR,IendR
          rmask(i,j)=1.
        enddo
      enddo
      if (SOUTHERN_EDGE) then
        do i=IstrR,IendR
          rmask(i,0)=0.
        enddo
      endif
      if (NORTHERN_EDGE) then
        do i=IstrR,IendR
          rmask(i,LOCALMM+1)=0.
        enddo
      endif
      if (EASTERN_EDGE) then
        do j=JstrR,JendR
          rmask(LOCALLM+1,j)=0.
        enddo
      endif
#  endif
# endif
!
#else
      subroutine ana_grid_empty
#endif /* ANA_GRID */
      return
      end

