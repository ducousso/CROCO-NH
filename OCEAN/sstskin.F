! $Id: sstskin.F 1458 2014-02-03 15:01:25Z gcambon $
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
#ifdef SST_SKIN

      subroutine sstskin (tile)
      implicit none
      integer tile, trd, omp_get_thread_num
# include "param.h"
# include "compute_tile_bounds.h"
      trd=omp_get_thread_num()
      call sstskin_tile (Istr,Iend,Jstr,Jend)
      return
      end
!
      subroutine sstskin_tile (Istr,Iend,Jstr,Jend)
!
!======================================================================
!  This routine computes a sea surface skin temperature using 
!  the prognostic scheme of Zeng and Beljaars (2005). 
!
!  The scheme is based on one-dimensional heat transfer equations
!  in the molecular sublayer (cool skin) and diurnal layer (warm skin) 
!  of the ocean. The cool skin is a layer a few millimeters thick that 
!  driven by the exchange of heat and moisture to the atmosphere as 
!  well as the emission of infrared radiation. the warm layer is a 
!  few centimeters thick and is driven by the absorption of sunlight.
!
!  The scheme consists of two equations predicting the difference 
!  of temperature between top and bottom temperatures of these layers. 
!  The skin temperature (SST_skin) is at the top of the cool skin 
!  and the bulk temperature (model SST) is at the bottom of the 
!  warm layer (the subskin temperature is in between).
!
!  An implicit time stepping is used to compute the warm layer 
!  temperature difference dtw. dtw is provided as input at time index 
!  nrhs (present time) and the routine computes dtw and sst_skin at
!  nnew (future time). The result will be used at the next model time 
!  step in the bulk_flux routine (prestep3D). It will thus correspond 
!  to present time (nrhs) of that step.
!
!  REFERENCE:
!  Zeng, X., and A. Beljaars (2005), A prognostic scheme of sea 
!  surface skin temperature for modeling and data assimilation, 
!  Geophys. Res. Lett., 32, L14605  
!
!  Patrick Marchesiello 2012: implementation in CROCO based on
!                             WRF routine module_sf_sstskin.F
!======================================================================
!
      IMPLICIT NONE
# include "param.h"
# include "grid.h"
# include "ocean3d.h" 
# include "forces.h"
# include "mixing.h"
# include "scalars.h"
      INTEGER    i,j, Istr,Iend,Jstr,Jend
      REAL       nu, visc, diff
      PARAMETER (nu=.3, visc=1.e-6, diff=1.4e-7)
      REAL       sw, q, q2, qn, qn1, fs, f1, phi, usw,
     &           db, zeta, ds,
     &           cff1, cff2, cff3, cff4, cff5,
     &           tb, dtc, dtw, ts, dtwo, alw
!
!----------------------------------------------------------------------
!
!  Input arguments
!     (all fluxes are positive downwards)
!     q       ! LH + SH + LW (degC m/s), + down
!     sw      ! Net shortwave flux (degC m/s), + down
!     usw     ! oceanic friction velocity (m/s)
!     tb      ! Bulk temperature (deg C)
!     dtwo    ! Warm layer temp. diff. from previous time (deg C)
!     db      ! depth of bulk temperature (m; positive)
!     nu      ! exponent of warm layer T profile:
!             !    T=Tds-(Tds-Tdb)*((z+ds)/(-db+ds)).^nu;
!     visc    ! molecular kinematic viscosity
!     diff    ! molecular thermal conductivity
!  Local variables
!     qn      ! Q + R_s - R(-d) heat flux in warm layer
!     zeta    ! db / L  (L is Monin-Obukhov length)
!     ds      ! cool skin layer depth (m; positive)
!     alw     ! thermal expansion coefficient
!     fs      ! fraction of solar radiation absorbed in the cool sublayer
!     f1      ! fraction of solar radiation absorbed in the warm layer
!     phi     ! stability function
!  Output variables
!     dtw     ! Warm layer temp. diff. (deg C)
!     dtc     ! Cool skin temp. diff. (deg C)
!     ts      ! Skin temperature (deg C)
!
!----------------------------------------------------------------------
!
      do j=Jstr,Jend
        do i=Istr,Iend 
!
          tb   = t(i,j,N,nrhs,itemp)
          db   = z_w(i,j,N)-z_r(i,j,N)
          q    = stflx(i,j,itemp)-srflx(i,j)
          sw   = srflx(i,j)
          usw  = max(0.01,
     &           sqrt(sqrt( (0.5*(sustr(i,j)+sustr(i+1,j)))**2
     &                     +(0.5*(svstr(i,j)+svstr(i,j+1)))**2)))
          dtwo = dT_skin(i,j)
!
!------------------------------------------------
! cool skin/sublayer (eq. 4 of Z&B-2005)
!------------------------------------------------
!
          alw=1.e-5*max(tb,1.)
          cff4=16.*g*alw*visc**3/diff**2
          cff5=cff4/usw**4
          q2=max(1./(rho0*Cp),-q)             ! avoid iterative procedure 
                                              ! between zs and fs (impact <0.03C)
          ds=6./(1.+(cff5*q2)**0.75)**0.333   ! sublayer thickness (m)
          ds=ds*visc/usw                      ! --> eq 6 of Z&B-2005
          fs=0.065+11.*ds
     &            -(6.6e-5/ds)                ! fract. of solar rad. absorbed 
     &            *(1.-exp(-ds/8.e-4))        ! in sublayer
          fs=max(fs,0.01)
          dtc=ds*(q+sw*fs)/diff               ! cool skin temp. diff
          dtc=min(dtc,0.)                     ! --> eq. 4 of Z&B-2005
!
!------------------------------------------------
! warm layer  (eq. 11 of Z&B-2005)
!------------------------------------------------
!
          alw=1.e-5*max(tb,1.)
          f1=1.-0.27*exp(-2.8*db)-0.45*exp(-0.07*db)
          qn=q+sw*f1
          if(qn.lt.0.) then                          
            cff1=sqrt(5.*db*g*alw/nu)    ! allow warm layer to subsist after
            qn1=sqrt(dtwo)*usw**2/cff1   ! sunset by changing heat flux in
            qn=max(qn,qn1)               ! Monin-Obukhov length (eq. 12 Z&B-2005)
          endif
          cff2=vonKar*g*alw
          zeta=db*cff2*qn/usw**3         ! db/LMO
          if(zeta.gt.0.) then
            phi=1.+5.*zeta
          else
            phi=1./sqrt(1.-16.*zeta)
          endif
          cff3=vonKar*usw/(db*phi)
!
! implicit time stepping (eq. 11 of Z&B-2005)
!   dtw(n+1)-dtw(n) = dt*RHS(dtw(n+1))
!
          dtw=(dtwo + (nu+1.)/nu*(q+sw*f1)*dt/db)  ! warm layer temp. diff
     &                      /(1.+(nu+1.)*cff3*dt)  ! --> eq. 11 of Z&B-2005
          dtw=max(0.,dtw)
! get skin temperature
          ts = tb + dtw + dtc
!
!------------------------------------------------
! store skin temperature in shared global arrays
!------------------------------------------------
!
          sst_skin(i,j) = ts
          dT_skin(i,j)  = dtw          

        enddo
      enddo

# if defined EW_PERIODIC || defined NS_PERIODIC || defined  MPI
        call exchange_r2d_tile (Istr,Iend,Jstr,Jend,
     &                          sst_skin(START_2D_ARRAY))
# endif

#else
      subroutine sst_skin_empty
#endif /* SST_SKIN */
      return
      end

