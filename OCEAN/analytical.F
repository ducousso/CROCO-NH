! $Id: analytical.F 1619 2015-01-07 13:53:03Z marchesiello $
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
!
!  ANALYTICAL PACKAGE:
!----------------------------------------------------------------------
!
!  This package is used to provide various analytical fields to the
!  model when appropriate.
!
!  Routines:
!
!  ana_bmflux     Analytical kinematic bottom momentum flux.
!  ana_btflux     Analytical kinematic bottom flux of tracer
!                            type variables.
!  ana_hmorph     Analytical dynamic bathymetry
!  ana_bsedim     Analytical bottom sediment grain size
!                            and density.
!  ana_smflux     Analytical kinematic surface momentum flux
!                            (wind stress).
!  ana_srflux     Analytical kinematic surface shortwave
!                            radiation.
!  ana_ssh        Analytical sea surface height climatology.      
!  ana_sst        Analytical sea surface temperature and dQdSST  
!                            which are used during heat flux correction.
!  ana_sss        Analytical sea surface salinity which is used 
!                            during salt flux correction.
!  ana_stflux     Analytical kinematic surface flux of tracer type
!                            variables.
!  ana_tclima     Analytical tracer climatology fields.     
!  ana_uclima     Analytical tracer climatology fields.  
!  ana_nbq_clima  Analytical NBQ variable climatology fields
!  ana_wwave      Analytical wind induced wave amplitude,
!                            direction and period.
!  ana_sediment   Analytical sediment
!  ana_psource    Analytical point source
!  ana_bry        Analytical boundary forcing.
!  ana_nbq_bry    Analytical boundary forcing for NBQ variables
!  ana_bry_bio    Analytical boundary forcing for BIO variables
!
!----------------------------------------------------------------------
!
#if !defined OPENMP
      integer function omp_get_thread_num()
      omp_get_thread_num=0
      return
      end
      integer function omp_get_num_threads()
      omp_get_num_threads=1
      return
      end
#endif
!
!======================================================================
!                   subroutine ana_bmflux
!======================================================================
!
#ifdef ANA_BMFLUX
      subroutine ana_bmflux_tile (Istr,Iend,Jstr,Jend)
!
!----------------------------------------------------------------------
!  This routine sets kinematic bottom momentum flux (bottom stress)
! "bustr" and "bvstr" [m^2/s^2] using an analytical expression.
!----------------------------------------------------------------------
!
      implicit none
# include "param.h"
# include "grid.h"
# include "forces.h"
# include "scalars.h"
      integer Istr,Iend,Jstr,Jend, i,j
!
# include "compute_auxiliary_bounds.h"
!
      do j=JstrR,JendR
        do i=Istr,IendR
          bustr(i,j)=0.
        enddo
      enddo
      do j=Jstr,JendR
        do i=IstrR,IendR
          bvstr(i,j)=0.
        enddo
      enddo
      return
      end
#endif /* ANA_BMFLUX */ 
!
!======================================================================
!                   subroutine ana_btflux
!======================================================================
!
#ifdef SOLVE3D
# if defined ANA_BTFLUX || defined ANA_BSFLUX || defined ANA_BPFLUX 
      subroutine ana_btflux_tile (Istr,Iend,Jstr,Jend, itrc)
!
!----------------------------------------------------------------------
!  This routine sets kinematic bottom flux of tracer type variables
!  [tracer units m/s].
!
!  On Input:
!     itrc      Tracer type array index.
!----------------------------------------------------------------------
!
      implicit none
#  include "param.h"
#  include "grid.h"
#  include "forces.h"
#  include "scalars.h"
      integer itrc, Istr,Iend,Jstr,Jend, i,j
!
#  include "compute_auxiliary_bounds.h"
!
      if (itrc.eq.itemp) then
!
!----------------------------------------------------------------------
! Set kinematic bottom heat flux [degC m/s] at horizontal RHO-points.
!----------------------------------------------------------------------
!
        do j=JstrR,JendR
          do i=IstrR,IendR
            btflx(i,j,itemp)=0.
          enddo
        enddo
!
#  ifdef SALINITY
      elseif (itrc.eq.isalt) then
!
!----------------------------------------------------------------------
!  Set kinematic bottom salt flux (m/s) at horizontal RHO-points,
!  scaling by bottom salinity is done in STEP3D.
!----------------------------------------------------------------------
!
        do j=JstrR,JendR
          do i=IstrR,IendR
            btflx(i,j,isalt)=0.
          enddo
        enddo
#  endif /* SALINITY */
      else
!
!----------------------------------------------------------------------
!  Set kinematic surface flux of additional tracers, 
! for example sediments, bio..., to zero
!----------------------------------------------------------------------
!
        do j=JstrR,JendR
          do i=IstrR,IendR
            btflx(i,j,itrc)=0.
          enddo
        enddo
      endif
      return
      end
# endif /* ANA_BTFLUX */
#endif /* SOLVE3D */
!
!======================================================================
!                   subroutine ana_hmorph
!======================================================================
!
#ifdef ANA_MOVING_BATHY
      subroutine ana_hmorph (tile)
      implicit none
# include "param.h"
      integer tile
#ifdef  ALLOW_SINGLE_BLOCK_MODE
C$    integer  trd, omp_get_thread_num
#endif
# include "compute_tile_bounds.h"
      call ana_hmorph_tile (Istr,Iend,Jstr,Jend)
      return
      end 
      
      subroutine ana_hmorph_tile (Istr,Iend,Jstr,Jend)
!
!----------------------------------------------------------------------
!  This routine sets bathymetry increment
!  dh using an analytical expression:
!  example for SEAMOUNT test case
!----------------------------------------------------------------------
!
      implicit none
# include "param.h"
# include "grid.h"
# include "forces.h"
# include "scalars.h"
      integer Istr,Iend,Jstr,Jend, i,j
      real cff,cff1
!
# include "compute_auxiliary_bounds.h"
!
# if defined SEAMOUNT 
      cff=(1./50.e3)**2
      cff1=0.5*xl+time*1.
      ! 0.1m/s translation
      ! starting for mid-basin 
      ! for example purpoe
      ! has to be coherent with initial bathymetry
      ! definition in ana_grid.F  
      do j=JstrR,JendR
        do i=IstrR,IendR
          dh(i,j)=4500*(1-0.6*exp(-cff*((xr(i,j)-cff1  )**2+
     &                                  (yr(i,j)-0.5*el)**2)))
     &                                                 -h(i,j)
        enddo
      enddo
# elif defined TANK
      cff=1./(0.03686)**2
      cff1=2.+0.001*sin(2.*pi/10.5*time)
      do j=JstrR,JendR
        do i=IstrR,IendR
          dh(i,j)=0.394-0.1*exp(-cff*((xr(i,j)-cff1)**2+
     &                                (yr(i,j)-el*0.5)**2)) 
     &                                              -h(i,j)
        enddo
      enddo

# else
        dh(:,:) =??? 
# endif
      return
      end
#endif /* ANA_MOVING_BATHY */ 
!
!======================================================================
!                   subroutine ana_bsedim
!======================================================================
!
#if defined ANA_BSEDIM && defined BBL
      subroutine ana_bsedim (tile)
      implicit none
# include "param.h"
      integer tile
# ifdef  ALLOW_SINGLE_BLOCK_MODE
C$    integer  trd, omp_get_thread_num
# endif
# include "compute_tile_bounds.h"
      call ana_bsedim_tile   (Istr,Iend,Jstr,Jend)
      return
      end
      subroutine ana_bsedim_tile (Istr,Iend,Jstr,Jend)
!
!----------------------------------------------------------------------
!  This routine sets initial bottom sediment grain diameter size [m]
!  and density used in the bottom boundary formulation [kg/m^3].
!----------------------------------------------------------------------
!
      implicit none
# include "param.h"
# include "bbl.h"
# include "grid.h"
# include "scalars.h"
      integer Istr,Iend,Jstr,Jend, i,j
!
# include "compute_extended_bounds.h"
!
# if defined SED_TEST2 || defined REGIONAL
!
! taucb=critical threshold stress for initiation of motion 
! (=bedload for coarse grains). 
! critical suspension stress: ustar_crit=0.8*w_set 
!
! determine taucb from Shields curve, fit provided by 
! Soulsby & Whitehouse 1997, Threshold of sediment motion 
! in coastal environments, Proc. Pacific Coasts and Ports 
! '97 Conf., pp 149--154, Univ Canterbury, Nw Zealand.
!
! visk=1.3e-3/rhow; (kinem. visc., nu=mu/rhow)
! D=d50*(g*(Sdens/rhow-1)/(visk^2))^0.33333 
! thetcr=0.3./(1+1.2*D) + 0.055*(1-exp(-0.02*D))
! taucb=thetcr.*(g*(sdens-rhow).*d50);
!
! Souslby's (1997) estimate of settling velocity
!   w_set = visk*(sqrt(10.36^2+1.049*D^3)-10.36)/d50 [m/s]
! with D as above 
!
      do j=JstrR,JendR
        do i=IstrR,IendR
          Ssize(i,j)=1.5e-4  ! d50 [m]
          Sdens(i,j)=2650.0  ! rho sediment [kg/m^3]
          taucb(i,j)=0.16    ! critical bedload stress [N/m^2]
          w_set(i,j)=0.013   ! analytical settling velocity [m/s]
          Hripple(i,j)=0.01   ! analytical initial ripple height [m]
          Lripple(i,j)=0.10   ! analytical initial ripple length [m]
        enddo
      enddo
# else
      !ANA_BSEDIM: no values provided for SSIZE and SDENS.
      do j=JstrR,JendR
        do i=IstrR,IendR
          Ssize(i,j)=1.5e-4  ! d50 [m]
          Sdens(i,j)=2650.0  ! rho sediment [kg/m^3]
          taucb(i,j)=0.16    ! critical bedload stress [N/m^2]
          w_set(i,j)=0.013   ! analytical settling velocity [m/s]
          Hripple(i,j)=0.01   ! analytical initial ripple height [m]
          Lripple(i,j)=0.10   ! analytical initial ripple length [m]
        enddo
      enddo
# endif
      return
      end
#endif /* ANA_BSEDIM && BBL */
!
!======================================================================
!                   subroutine ana_smflux
!======================================================================
!
#ifdef ANA_SMFLUX
      subroutine ana_smflux_tile (Istr,Iend,Jstr,Jend)
!
!  Sets kinematic surface momentum flux (wind stress) "sustr" and "svstr"
!  [m^2/s^2] using an analytical expression.
!
# ifdef AGRIF
      use Agrif_UTIL
# endif
      implicit none
# include "param.h"
# include "ocean2d.h"
# include "grid.h"
# include "forces.h"
# include "scalars.h"
      integer Istr,Iend,Jstr,Jend, i,j
      real Ewind, Nwind, dircoef, windamp, wcos,wsin
      real cff1, cff2
!     data windamp /0./ 
      data Ewind, Nwind, dircoef /0., 0., 0./
!     save windamp
!
#include "compute_extended_bounds.h"
!
!  Set kinematic surface momentum flux (wind stress) component in the
!  XI-direction (m^2/s^2) at horizontal U-points.
!

      windamp = 0.

# ifdef BASIN
      cff1=0.0001 * 0.5*(1.+tanh((time-6.*86400.)/(3.*86400.)))
      cff2=2.*pi/el
      do j=JstrR,JendR
        do i=IstrR,IendR
          sustr(i,j)=-cff1*cos(cff2*yr(i,j))
       enddo
      enddo
# elif defined CANYON_A || defined CANYON_B
      do j=JstrR,JendR
        do i=IstrR,IendR
          sustr(i,j)=0.0001*0.5*sin(2.*pi*tdays/10.)*
     &               (1.-tanh((yr(i,j)-0.5*el)/10000.))
        enddo
      enddo
# elif defined EQUATOR
      do j=JstrR,JendR
        do i=IstrR,IendR
          sustr(i,j)=-0.05/rho0
        enddo
      enddo
# elif defined SED_TEST2
      do j=JstrR,JendR
        do i=IstrR,IendR
          windamp=0.5+
     &            0.5*TANH((time-user(9))/user(10))
          sustr(i,j)=windamp*user(1)
        enddo
      enddo
# elif defined UPWELLING
      if (tdays.le.2.) then
        windamp=-0.1*sin(pi*tdays/4.)/rho0
      else
        windamp=-0.1/rho0
      endif
      do j=JstrR,JendR
        do i=IstrR,IendR
          sustr(i,j)=windamp
        enddo
      enddo
# elif defined SHOREFACE
#  if !defined MRL_WCI
      windamp=tanh(24.0*dt*sec2day*float(iic-ntstart))
      wcos = cos(10.0/180.0*pi)
      do j=jstr-1,jend+1
        do i=istr-1,iend+1
          cff2  = h(i,j)+zeta(i,j,kstp)
          cff1  = 0.05/sqrt(9.8*cff2)  ! 0.6 is epsilon_b/rho
          sustr(i,j)=cff1*wcos*windamp
        enddo
      enddo
#  else
      do j=JstrR,JendR
        do i=IstrR,IendR
          sustr(i,j)=0.
        enddo
      enddo
#  endif
# else
      do j=JstrR,JendR
        do i=IstrR,IendR
          sustr(i,j)=0.
        enddo
      enddo
# endif
!
!  Set kinematic surface momentum flux (wind stress) component in the
!  ETA-direction (m^2/s^2) at horizontal V-points.
!
# if defined SED_TEST2
      do j=JstrR,JendR
        do i=IstrR,IendR
          windamp=0.5+
     &            0.5*TANH((time-user(9))/user(10))
          svstr(i,j)= windamp*user(2)
        enddo
      enddo
# elif defined INNERSHELF
      do j=JstrR,JendR
        do i=IstrR,IendR
          svstr(i,j)=0.07/rho0
        enddo
      enddo
# elif defined SHOREFACE 
#  if !defined MRL_WCI
      windamp=tanh(24.0*dt*sec2day*float(iic-ntstart))
      wsin = sin(10.0/180.0*pi)
      do j=jstr-1,jend+1
        do i=istr-1,iend+1
          cff2  = h(i,j)+zeta(i,j,kstp)
          cff1  = 0.05/sqrt(9.8*cff2)  ! 0.6 is \epsilon_b/\rho
          svstr(i,j)=cff1*wsin*windamp
        enddo
      enddo
#  else
      do j=JstrR,JendR
        do i=IstrR,IendR
          svstr(i,j)=0.
        enddo
      enddo
#  endif
# else
      do j=JstrR,JendR
        do i=IstrR,IendR
          svstr(i,j)=0.
        enddo
      enddo
# endif
      return
      end
#endif /* ANA_SMFLUX */
!
!======================================================================
!                   subroutine ana_srflux
!======================================================================
!
#ifdef ANA_SRFLUX
      subroutine ana_srflux_tile (Istr,Iend,Jstr,Jend)
!
!----------------------------------------------------------------------
!  This subroutine sets kinematic surface solar shortwave radiation
!  flux "srflx" (degC m/s) using an analytical expression.
!----------------------------------------------------------------------
!
      implicit none
# include "param.h"
# include "grid.h"
# include "forces.h"
# include "scalars.h"
      integer Istr,Iend,Jstr,Jend, i,j
!
# include "compute_auxiliary_bounds.h"
!
!  Set kinematic surface solar shortwave radiation [degC m/s] at
!  horizontal RHO-points.
!
      do j=JstrR,JendR
        do i=IstrR,IendR
          srflx(i,j)=0.
# ifdef DIURNAL_INPUT_SRFLX
          srflxbio(i,j)=0.
# endif
        enddo
      enddo
      return
      end
#endif /* ANA_SRFLUX */
!
!======================================================================
!                   subroutine ana_ssh
!======================================================================
!
#if defined ANA_SSH && defined ZCLIMATOLOGY
      subroutine ana_ssh (tile)
      implicit none
# include "param.h"
      integer tile
# ifdef  ALLOW_SINGLE_BLOCK_MODE
C$    integer  trd, omp_get_thread_num
# endif
# include "compute_tile_bounds.h"
      call ana_ssh_tile (Istr,Iend,Jstr,Jend)
      return
      end 
!
      subroutine ana_ssh_tile (Istr,Iend,Jstr,Jend)
!
!----------------------------------------------------------------------
!  This routine sets analytical sea surface height climatology [m].
!----------------------------------------------------------------------
!
      implicit none
# include "param.h"
# include "grid.h"
# include "ocean2d.h"
# include "climat.h"
# include "scalars.h"
# include "coupling.h"
      integer Istr,Iend,Jstr,Jend, i,j
# ifdef INTERNAL
      real U0,omega,kwave,ETA0
# endif
# if defined RIP || defined SHOREFACE
      real surelev,omega1,omega2
      real tide_ampli_moy,tide_ampli_var
      real*QUAD mssh,area,cff1
#  ifdef MPI
      include 'mpif.h'
      integer ierr
      real*QUAD allmssh(1,NNODES),allarea(1,NNODES)
#  endif
# endif
!
# include "compute_auxiliary_bounds.h"
!
!  Set sea surface height (meters).
!
# if defined INTERNAL
      U0=0.02
      omega=2.*pi/(12.4*3600)
      kwave=((omega*omega)-(f(1,1)*f(1,1)))/(g*h(1,1))
      ETA0=kwave*h(1,1)*U0/omega
      do j=JstrR,JendR
        do i=IstrR,IendR
          ssh(i,j)=ETA0*sin(omega*time-kwave*(xr(i,j)))
        enddo
      enddo
# elif defined JET
!
!    --> climatology computed in ana_initial
!
# elif (defined RIP || defined SHOREFACE) && defined ANA_TIDES
!
! Remove mean sea level (mssh) 
!  !!! not OPENMP compliant and MPI DEBUG problems !!!
!
      mssh=QuadZero
      area=QuadZero
      do j=Jstr,Jend
        do i=Istr,Iend
          cff1=1./(pm(i,j)*pn(i,j))
          mssh=mssh+cff1*Zt_avg1(i,j)
          area=area+cff1
        enddo
      enddo
      mssh=mssh/area
#   ifdef MPI
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
#   endif
      omega1=2.*pi/(14.*86400.)
      omega2=2.*pi/(12.*3600. )
      tide_ampli_moy=1.
      tide_ampli_var=30./70.
      surelev=1.
      do j=JstrR,JendR
        do i=IstrR,IendR
          ssh(i,j)=zeta(i,j,knew)-mssh
     &                +tide_ampli_moy*(1.+tide_ampli_var
     &                *sin(time*omega1))
     &                *sin(time*omega2)
     &                    +time*surelev/31536000.
        enddo
      enddo
# else
      do j=JstrR,JendR
        do i=IstrR,IendR
          ssh(i,j)=0.
        enddo
      enddo
# endif

#   ifdef WET_DRY
      do j=JstrR,JendR
        do i=IstrR,IendR
          if (ssh(i,j) .lt. Dcrit(i,j)-h(i,j)) then
            ssh(i,j)=Dcrit(i,j)-h(i,j)
          endif
        enddo
      enddo
#   endif

# ifdef NBQ
      do j=JstrR,JendR
        do i=IstrR,IendR
          ssh(i,j)=h(i,j)+ssh(i,j)
        enddo
      enddo
# endif

# if defined EW_PERIODIC || defined NS_PERIODIC || defined  MPI
      call exchange_r2d_tile (istr,iend,jstr,jend, ssh(START_2D_ARRAY))
# endif
      return
      end
#endif /* ANA_SSH && ZCLIMATOLOGY */
!
!======================================================================
!                   subroutine ana_sst
!======================================================================
!
#if defined ANA_SST && defined QCORRECTION
      subroutine ana_sst_tile (Istr,Iend,Jstr,Jend)
!
!----------------------------------------------------------------------
!  This routine sets sea surface temperature SST[Celsius] and surface
!  net heat flux sensitivity dQdSTT to sea surface temperature using
!  analytical expressions. dQdSTT is usually computed in units of
!  [Watts/m^2/degC]. It needs to be scaled to [m/s] by dividing by
!  rho0*Cp.  These forcing fields are used when the heat flux
!  correction is activated:
!
!       Q_model ~ Q + dQdSST * (T_model - SST)
!----------------------------------------------------------------------
!
      implicit none
# include "param.h"
# include "grid.h"
# include "forces.h"
# include "scalars.h"
      integer Istr,Iend,Jstr,Jend, i,j
# if defined EQUATOR
      real y1,y2,sst1,sst2
# endif
!
# include "compute_auxiliary_bounds.h"
!
# if defined EQUATOR
! SST = 25C and lineraly decreases to 10C 1200km from the Equator.
      y1=1200.E+3
      y2=1500.E+3
      sst1=10.
      sst2=25.
      do j=JstrR,JendR
        do i=IstrR,IendR
          sst(i,j)=sst1
          if ((yr(i,j).gt.-y1).and.(yr(i,j).lt.y1)) then
            sst(i,j)=sst2
          else
            if ((yr(i,j).gt.-y2).and.(yr(i,j).lt.-y1)) then
              sst(i,j)=((sst2-sst1)*yr(i,j)-sst1*y1+y2*sst2)/(y2-y1)
            endif
            if ((yr(i,j).gt.y1).and.(yr(i,j).lt.y2)) then
              sst(i,j)=((sst2-sst1)*yr(i,j)+sst1*y1-y2*sst2)/(y1-y2)
            endif
          endif
          dqdt(i,j)=-50.0/(rho0*Cp)
        enddo
      enddo
# else
      do j=JstrR,JendR
        do i=IstrR,IendR
          sst(i,j)=???
          dqdt(i,j)=???
        enddo
      enddo
# endif
      return
      end
#endif /* ANA_SST && QCORRECTION */

!
!======================================================================
!                   subroutine ana_sss
!======================================================================
!
#if defined SALINITY && defined SFLX_CORR && defined ANA_SSS
      subroutine ana_sss_tile (Istr,Iend,Jstr,Jend)
!
!----------------------------------------------------------------------
!  This routine sets sea surface salinity SSS[PSU] using
!  analytical expressions. This forcing field is used when the
!  salt flux correction is activated:
!
!    SSSFLX_model ~ SSS*(E-P) + CST * (SSS_model - SSS)
!
!  we use DQDSST for CST.... 
!
!----------------------------------------------------------------------
!
      implicit none
# include "param.h"
# include "grid.h"
# include "forces.h"
# include "scalars.h"
      integer Istr,Iend,Jstr,Jend, i,j
!
# include "compute_auxiliary_bounds.h"
!
      do j=JstrR,JendR
        do i=IstrR,IendR
          sss(i,j)=???
        enddo
      enddo
      return
      end
#endif /* SALINITY && SFLX_CORR && ANA_SSS */
!
!======================================================================
!                   subroutine ana_stflux
!======================================================================
!
#if defined ANA_STFLUX || defined ANA_SSFLUX
      subroutine ana_stflux_tile (Istr,Iend,Jstr,Jend, itrc)
!
!----------------------------------------------------------------------
!  This routine sets kinematic surface flux of tracer type variables
!  "stflx" (tracer units m/s) using analytical expressions.
!
!  On Input:
!     itrc      Tracer type array index.
!----------------------------------------------------------------------
!
      implicit none
# include "param.h"
# include "grid.h"
# include "forces.h"
# include "scalars.h"
      integer itrc, Istr,Iend,Jstr,Jend, i,j 
c
# include "compute_auxiliary_bounds.h"
c
      if (itrc.eq.itemp) then
!
!  Set kinematic surface heat flux [degC m/s] at horizontal
!  RHO-points.
!
        do j=JstrR,JendR
          do i=IstrR,IendR
            stflx(i,j,itemp)=0.
          enddo
        enddo
!
#ifdef SALINITY
      elseif (itrc.eq.isalt) then
!
!  Set kinematic surface freshwater flux (m/s) at horizontal
!  RHO-points, scaling by surface salinity is done in STEP3D.
!
        do j=JstrR,JendR
          do i=IstrR,IendR
            stflx(i,j,isalt)=0.
          enddo
        enddo
#endif /* SALINITY */
      else
!
!  Set kinematic surface flux of additional tracers, if any.
!
        do j=JstrR,JendR
          do i=IstrR,IendR
            stflx(i,j,itrc)=0.
          enddo
        enddo
      endif
      return
      end
#endif /* ANA_STFLUX || ANA_SSFLUX */
!
!======================================================================
!                   subroutine ana_tclima
!======================================================================
!
#if defined TCLIMATOLOGY
      subroutine ana_tclima (tile)
      implicit none
# include"param.h"
      integer tile
#ifdef  ALLOW_SINGLE_BLOCK_MODE
C$    integer  trd, omp_get_thread_num
#endif
#include "compute_tile_bounds.h"
      call ana_tclima_tile   (Istr,Iend,Jstr,Jend)
      return
      end
      subroutine ana_tclima_tile (Istr,Iend,Jstr,Jend)
!
!----------------------------------------------------------------------
!  This routine sets analytical ACTIVE (T&S) tracer climatology fields.
!----------------------------------------------------------------------
!
      implicit none
# include "param.h"
# include "grid.h"
# include "climat.h"
# include "ocean3d.h"
# include "scalars.h"
# include "sediment.h"
      integer Istr,Iend,Jstr,Jend, i,j,k, itrc
      real    cff,cff1
# ifdef IGW
      real    sig,rhom,depth
# endif  
!
# include "compute_auxiliary_bounds.h"
!
# ifdef ANA_TCLIMA
#  ifdef JET
!
!        --> climatology computed in ana_initial
!
#  elif defined IGW
      cff=((2.e-3)**2)/g
      rhom=1027.037885
      do k=1,N
        do j=JstrR,JendR
          do i=IstrR,IendR
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
            tclm(i,j,k,itemp)=(R0-sig)/Tcoef
#   elif defined EXPERIMENT2	    
            if (depth.le.50.) then
             sig = 25.8
            else
             sig = 27.0
            endif
            tclm(i,j,k,itemp)=(R0-sig)/Tcoef
#   else /* default EXPERIMENT3 */
            sig = 26.2 + rhom*(depth-50.)*cff
            tclm(i,j,k,itemp)=(R0-sig)/Tcoef
#   endif
          enddo
        enddo
      enddo
#  else
      do k=1,N
        do j=JstrR,JendR
          do i=IstrR,IendR
            tclm(i,j,k,itemp)=t(i,j,k,1,itemp)
#  ifdef SALINITY
            tclm(i,j,k,isalt)=t(i,j,k,1,isalt)
#  endif /* SALINITY */
          enddo
        enddo
      enddo
#  endif /* JET */
# endif

# ifdef BIOLOGY
#  define temp cff
#  define SiO4 cff1
      do k=1,N
        do j=JstrR,JendR
          do i=IstrR,IendR
#  ifdef ANA_TCLIMA
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
            tclm(i,j,k,iNO3_)=1.67+0.5873*SiO4+0.0144*SiO4**2
     &                            +0.0003099*SiO4**3
#   ifdef PISCES
            tclm(i,j,k,iDIC_)=2150.
            tclm(i,j,k,iTAL_)=2350.
            tclm(i,j,k,iOXY_)=200.
            tclm(i,j,k,iCAL_)=0.01
            tclm(i,j,k,iPO4_)=tclm(i,j,k,iNO3_)/16.
            tclm(i,j,k,iPOC_)=0.01
            tclm(i,j,k,iSIL_)=91.51
            tclm(i,j,k,iPHY_)=0.01
            tclm(i,j,k,iZOO_)=0.01
            tclm(i,j,k,iDOC_)=5.
            tclm(i,j,k,iDIA_)=0.01
            tclm(i,j,k,iMES_)=0.01
            tclm(i,j,k,iBSI_)=1.5e-3
            tclm(i,j,k,iFER_)=6.e-4
            tclm(i,j,k,iBFE_)=1.e-2*5.e-6
            tclm(i,j,k,iGOC_)=0.01
            tclm(i,j,k,iSFE_)=1.e-2*5.e-6
            tclm(i,j,k,iDFE_)=1.e-2*5.e-6
            tclm(i,j,k,iDSI_)=1.e-2*0.15
            tclm(i,j,k,iNFE_)=1.e-2*5.e-6
            tclm(i,j,k,iNCH_)=1.e-2*12./55.
            tclm(i,j,k,iDCH_)=1.e-2*12./55.
            tclm(i,j,k,iNH4_)=1.e-2
#   elif defined BIO_NChlPZD
            tclm(i,j,k,iChla)=0.08
            tclm(i,j,k,iPhy1)=0.1
            tclm(i,j,k,iZoo1)=0.06
            tclm(i,j,k,iDet1)=0.02
#   elif defined BIO_N2ChlPZD2
            tclm(i,j,k,iNH4_)=0.1
            tclm(i,j,k,iChla)=0.08
            tclm(i,j,k,iPhy1)=0.06
            tclm(i,j,k,iZoo1)=0.04
            tclm(i,j,k,iDet1)=0.02
            tclm(i,j,k,iDet2)=0.02
#   elif defined BIO_BioEBUS
            tclm(i,j,k,iNO2_)=0.05*exp(z_r(i,j,k)/100.)
            tclm(i,j,k,iNH4_)=0.1*exp(z_r(i,j,k)/100.)
            tclm(i,j,k,iPhy1)=0.04*exp(z_r(i,j,k)/50.)
            tclm(i,j,k,iPhy2)=0.06*exp(z_r(i,j,k)/50.)
            tclm(i,j,k,iZoo1)=0.04*exp(z_r(i,j,k)/100.)
            tclm(i,j,k,iZoo2)=0.04*exp(z_r(i,j,k)/100.)
            tclm(i,j,k,iDet1)=0.02
            tclm(i,j,k,iDet2)=0.02
            tclm(i,j,k,iDON) =0.5*exp(z_r(i,j,k)/100.) 
#     ifdef NITROUS_OXIDE
            tclm(i,j,k,iN2O)=-(0.008*exp(z_r(i,j,k)/100.)-0.01)
            if (tclm(i,j,k,iN2O).lt.0.) then
              tclm(i,j,k,iN2O)=0.
            endif
#     endif
#   endif
#  else
            if (.not.got_tclm(iNO3_)) then
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
              tclm(i,j,k,iNO3_)=1.67+0.5873*SiO4+0.0144*SiO4**2
     &                              +0.0003099*SiO4**3
            endif
#   ifdef PISCES
            if (.not.got_tclm(iDIC_)) tclm(i,j,k,iDIC_)=2150.
            if (.not.got_tclm(iTAL_)) tclm(i,j,k,iTAL_)=2350.
            if (.not.got_tclm(iOXY_)) tclm(i,j,k,iOXY_)=200.
            if (.not.got_tclm(iCAL_)) tclm(i,j,k,iCAL_)=0.01
            if (.not.got_tclm(iPO4_)) then
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
              tclm(i,j,k,iPO4_)=(1.67+0.5873*SiO4+0.0144*SiO4**2
     &                               +0.0003099*SiO4**3)/16.
            endif
            if (.not.got_tclm(iPOC_)) tclm(i,j,k,iPOC_)=0.01
            if (.not.got_tclm(iSIL_)) tclm(i,j,k,iSIL_)=91.51
            if (.not.got_tclm(iPHY_)) tclm(i,j,k,iPHY_)=0.01
            if (.not.got_tclm(iZOO_)) tclm(i,j,k,iZOO_)=0.01
            if (.not.got_tclm(iDOC_)) tclm(i,j,k,iDOC_)=5.
            if (.not.got_tclm(iDIA_)) tclm(i,j,k,iDIA_)=0.01
            if (.not.got_tclm(iMES_)) tclm(i,j,k,iMES_)=0.01
            if (.not.got_tclm(iBSI_)) tclm(i,j,k,iBSI_)=0.0015
            if (.not.got_tclm(iFER_)) tclm(i,j,k,iFER_)=6.e-4
            if (.not.got_tclm(iBFE_)) tclm(i,j,k,iBFE_)=5.e-8
            if (.not.got_tclm(iGOC_)) tclm(i,j,k,iGOC_)=0.01
            if (.not.got_tclm(iSFE_)) tclm(i,j,k,iSFE_)=5.e-8
            if (.not.got_tclm(iDFE_)) tclm(i,j,k,iDFE_)=5.e-8
            if (.not.got_tclm(iDSI_)) tclm(i,j,k,iDSI_)=0.0015
            if (.not.got_tclm(iNFE_)) tclm(i,j,k,iNFE_)=5.e-8
            if (.not.got_tclm(iNCH_)) tclm(i,j,k,iNCH_)=1.e-2*12./55.
            if (.not.got_tclm(iDCH_)) tclm(i,j,k,iDCH_)=1.e-2*12./55.
            if (.not.got_tclm(iNH4_)) tclm(i,j,k,iNH4_)=0.01
#   elif defined BIO_NChlPZD
            if (.not.got_tclm(iChla)) tclm(i,j,k,iChla)=0.08
            if (.not.got_tclm(iPhy1)) tclm(i,j,k,iPhy1)=0.1
            if (.not.got_tclm(iZoo1)) tclm(i,j,k,iZoo1)=0.06
            if (.not.got_tclm(iDet1)) tclm(i,j,k,iDet1)=0.02
#   elif defined BIO_N2ChlPZD2
            if (.not.got_tclm(iNH4_)) tclm(i,j,k,iNH4_)=0.1
            if (.not.got_tclm(iChla)) tclm(i,j,k,iChla)=0.08
            if (.not.got_tclm(iPhy1)) tclm(i,j,k,iPhy1)=0.06
            if (.not.got_tclm(iZoo1)) tclm(i,j,k,iZoo1)=0.04
            if (.not.got_tclm(iDet1)) tclm(i,j,k,iDet1)=0.02
            if (.not.got_tclm(iDet2)) tclm(i,j,k,iDet2)=0.02  
#   elif defined BIO_BioEBUS
            if (.not.got_tclm(iNO2_)) tclm(i,j,k,iNO2_)=0.05*
!            if (.not.got_tclm(iNO2_)) tclm(i,j,k,iNO2_)=0.6*
     &                                  exp(z_r(i,j,k)/100.)
            if (.not.got_tclm(iNH4_)) tclm(i,j,k,iNH4_)=0.1*
     &                                  exp(z_r(i,j,k)/100.)
            if (.not.got_tclm(iPhy1)) tclm(i,j,k,iPhy1)=0.04*
     &                                  exp(z_r(i,j,k)/50.)
            if (.not.got_tclm(iPhy2)) tclm(i,j,k,iPhy2)=0.06*
     &                                  exp(z_r(i,j,k)/50.)
            if (.not.got_tclm(iZoo1)) tclm(i,j,k,iZoo1)=0.04*
     &                                  exp(z_r(i,j,k)/100.)
            if (.not.got_tclm(iZoo2)) tclm(i,j,k,iZoo2)=0.04*
     &                                  exp(z_r(i,j,k)/100.)
            if (.not.got_tclm(iDet1)) tclm(i,j,k,iDet1)=0.02
            if (.not.got_tclm(iDet2)) tclm(i,j,k,iDet2)=0.02
            if (.not.got_tclm(iDON))  tclm(i,j,k,iDON)=0.5*    
!            if (.not.got_tclm(iDON))  tclm(i,j,k,iDON)=2.*
     &                                  exp(z_r(i,j,k)/100.)
            if (.not.got_tclm(iO2)) tclm(i,j,k,iO2)=250.*
     &                                  exp(z_r(i,j,k)/100.)
#      ifdef CARBON	
            if (.not.got_tclm(iTCO2)) tclm(i,j,k,iTCO2)=2100.
            if (.not.got_tclm(iTALK)) tclm(i,j,k,iTALK)=2300.   
#      endif	    	    	       	    	    	    
#      ifdef NITROUS_OXIDE
            if (.not.got_tclm(iN2O)) then 
               tclm(i,j,k,iN2O)=-(0.008*exp(z_r(i,j,k)/100.)-0.01)
!	       tclm(i,j,k,iN2O)=-(0.01*exp(z_r(i,j,k)/100.)-0.01)
               if (tclm(i,j,k,iN2O).lt.0.) then
                 tclm(i,j,k,iN2O)=0.
               endif
            endif    
#      endif
#      ifdef HYDROGEN_SULFIDE
            if (.not.got_tclm(iH2S)) tclm(i,j,k,iH2S)=0.
#      endif	    	        	    
#   endif 
#  endif /* ANA_TCLIMA */
          enddo
        enddo
      enddo
#  undef SiO4
#  undef temp
# endif /* BIOLOGY */

# ifdef SEDIMENT
      do k=1,N
        do j=JstrR,JendR
          do i=IstrR,IendR
#  ifdef ANA_TCLIMA
            tclm(i,j,k,isand)=Csed(1)
            tclm(i,j,k,isilt)=Csed(2)
#  else   
            if (.not.got_tclm(isand)) then
              tclm(i,j,k,isand)=Csed(1)
            endif
            if (.not.got_tclm(isilt)) then
              tclm(i,j,k,isilt)=Csed(2)
            endif
#  endif
          enddo
        enddo
      enddo
# endif /* SEDIMENT */

# ifdef PASSIVE_TRACER
      do k=1,N
        do j=JstrR,JendR
          do i=IstrR,IendR
#  ifdef ANA_TCLIMA
            tclm(i,j,k,itpas)=0.0
#  else
            if (.not.got_tclm(itpas)) then
              tclm(i,j,k,itpas)=0.0
            endif
#  endif
          enddo
        enddo
      enddo
# endif

# if defined EW_PERIODIC || defined NS_PERIODIC || defined MPI
      do itrc=1,NT
#  ifndef ANA_TCLIMA
        if (.not.got_tclm(itrc)) then
#  endif
#  ifdef THREE_GHOST_POINTS_TS
          call exchange_r3d_3pts_tile (Istr,Iend,Jstr,Jend,
     &                                 tclm(START_2D_ARRAY,1,itrc))
#  else
          call exchange_r3d_tile (Istr,Iend,Jstr,Jend,
     &                            tclm(START_2D_ARRAY,1,itrc))
#  endif
#  ifndef ANA_TCLIMA
        endif
#  endif
      enddo
# endif

      return
      end
#endif /* TCLIMATOLOGY */
!
!======================================================================
!                   subroutine ana_uclima
!======================================================================
!
#if defined ANA_M2CLIMA && defined M2CLIMATOLOGY ||\
   (defined ANA_M3CLIMA && defined M3CLIMATOLOGY)
      subroutine ana_uclima (tile)
      implicit none
# include "param.h"
      integer tile
#ifdef  ALLOW_SINGLE_BLOCK_MODE
C$    integer  trd, omp_get_thread_num
#endif
# include "compute_tile_bounds.h"
      call ana_uclima_tile   (Istr,Iend,Jstr,Jend)
      return
      end
!
      subroutine ana_uclima_tile (Istr,Iend,Jstr,Jend)
!
!----------------------------------------------------------------------
!  This routine sets analytical momentum climatology fields.
!----------------------------------------------------------------------
!
      implicit none
# include "param.h"
# include "grid.h"
# include "ocean2d.h"
# include "ocean3d.h"
# include "climat.h"
# include "scalars.h"
      integer Istr,Iend,Jstr,Jend, i,j,k
# ifdef INTERNAL
      real U0,omega,kwave,V0
# endif
      real ramp
!
# include "compute_auxiliary_bounds.h"
!
# ifdef EW_PERIODIC
#  define IU_RANGE Istr,Iend
#  define IV_RANGE Istr,Iend
# else
#  define IU_RANGE Istr,IendR
#  define IV_RANGE IstrR,IendR
# endif

# ifdef NS_PERIODIC
#  define JU_RANGE Jstr,Jend
#  define JV_RANGE Jstr,Jend
# else
#  define JU_RANGE JstrR,JendR
#  define JV_RANGE Jstr,JendR
# endif
!
# if defined ANA_M2CLIMA && defined M2CLIMATOLOGY
#  if defined INTERNAL
      U0=0.02
      omega=2.*pi/(12.4*3600)
      kwave=sqrt(((omega*omega)-(f(1,1)*f(1,1)))/(g*h(1,1)))
      V0=f(1,1)*U0/omega
      do j=JU_RANGE
        do i=IU_RANGE
          ubclm(i,j)=U0*sin(omega*time-kwave*0.5*(xr(i,j)+xr(i-1,j)))
        enddo
      enddo
      do j=JV_RANGE
        do i=IV_RANGE
          vbclm(i,j)=V0*cos(omega*time-kwave*0.5*(xr(i,j)+xr(i,j-1)))
        enddo
      enddo
#  elif defined JET
!
!       --> climatology computed in ana_initial
!
#  else
      do j=JstrR,JendR
        do i=IstrR,IendR
          ubclm(i,j)=0.
          vbclm(i,j)=0.
        enddo
      enddo
#  endif
#  if defined EW_PERIODIC || defined NS_PERIODIC || defined MPI
      call exchange_u2d_tile (Istr,Iend,Jstr,Jend,  ubclm)
      call exchange_v2d_tile (Istr,Iend,Jstr,Jend,  vbclm)
#  endif
# endif

# if defined ANA_M3CLIMA && defined M3CLIMATOLOGY && defined SOLVE3D
#  if defined JET
!
!       --> climatology computed in ana_initial
!
#  else
      do k=1,N
        do j=JstrR,JendR
          do i=IstrR,IendR
            uclm(i,j,k)=0.
            vclm(i,j,k)=0.
          enddo
        enddo
      enddo
#  endif
#  if defined EW_PERIODIC || defined NS_PERIODIC || defined MPI
      call exchange_u3d_tile (Istr,Iend,Jstr,Jend,  uclm)
      call exchange_v3d_tile (Istr,Iend,Jstr,Jend,  vclm)
#  endif
# endif
# undef IU_RANGE
# undef JU_RANGE
# undef IV_RANGE
# undef JV_RANGE
      return
      end
#endif /* ANA_M2CLIMA && M2CLIMATOLOGY || (ANA_M3CLIMA && M3CLIMATOLOGY) */
!
!======================================================================
!                   subroutine ana_nbq_clima
!======================================================================
!
#ifdef NBQCLIMATOLOGY 
      subroutine ana_nbq_clima (tile)
      implicit none
# include "param.h"
      integer tile
# ifdef  ALLOW_SINGLE_BLOCK_MODE
C$    integer  trd, omp_get_thread_num
# endif
# include "compute_tile_bounds.h"
      call ana_nbq_clima_tile (Istr,Iend,Jstr,Jend)
      return
      end
!
      subroutine ana_nbq_clima_tile (Istr,Iend,Jstr,Jend)
!
!----------------------------------------------------------------------
!  This routine sets analytical NBQ climatology fields.
!----------------------------------------------------------------------
!
      implicit none
# include "param.h"
# include "grid.h"
# include "ocean3d.h"
# include "climat.h"
# include "scalars.h"
      integer Istr,Iend,Jstr,Jend, i,j,k
!
# include "compute_auxiliary_bounds.h"
!
# ifdef EW_PERIODIC
#  define IU_RANGE Istr,Iend
#  define IV_RANGE Istr,Iend
# else
#  define IU_RANGE Istr,IendR
#  define IV_RANGE IstrR,IendR
# endif

# ifdef NS_PERIODIC
#  define JU_RANGE Jstr,Jend
#  define JV_RANGE Jstr,Jend
# else
#  define JU_RANGE JstrR,JendR
#  define JV_RANGE Jstr,JendR
# endif
!
      do k=1,N
        do j=JU_RANGE
          do i=IU_RANGE
            unbqclm(i,j,k)=0.5*rho0*(Hz(i,j,k)+Hz(i-1,j,k)*uclm(i,j,k)
          enddo
        enddo
        do j=JV_RANGE
          do i=IV_RANGE
            vnbqclm(i,j,k)=0.5*rho0*(Hz(i,j,k)+Hz(i,j-1,k)*vclm(i,j,k)
          enddo
        enddo
        do j=JstrR,JendR
          do i=IstrR,IendR
            wnbqclm(i,j,k)=0.
            rnbqclm(i,j,k)=0.
          enddo
        enddo
      enddo

# if defined EW_PERIODIC || defined NS_PERIODIC || defined MPI
      call exchange_u3d_tile (Istr,Iend,Jstr,Jend,  unbqclm)
      call exchange_v3d_tile (Istr,Iend,Jstr,Jend,  vnbqclm)
      call exchange_r3d_tile (Istr,Iend,Jstr,Jend,  wnbqclm)
      call exchange_r3d_tile (Istr,Iend,Jstr,Jend,  rnbqclm)
# endif
# undef IU_RANGE
# undef JU_RANGE
# undef IV_RANGE
# undef JV_RANGE
      return
      end
#endif /*  NBQCLIMATOLOGY */
!
!======================================================================
!                   subroutine ana_wwave
!======================================================================
!
#if defined ANA_WWAVE
      subroutine ana_wwave (tile)
      implicit none
# include "param.h"
      integer tile
# ifdef  ALLOW_SINGLE_BLOCK_MODE
C$    integer  trd, omp_get_thread_num
# endif
# include "compute_tile_bounds.h"
      call ana_wwave_tile   (Istr,Iend,Jstr,Jend)
      return
      end
!
      subroutine ana_wwave_tile (Istr,Iend,Jstr,Jend)
!
!----------------------------------------------------------------------
!  This routine sets wind induced wave amplitude, direction
!  and period used in the bottom boundary layer formulation.
!----------------------------------------------------------------------
!
      implicit none
      real wamp,wdir,wprd,
     &     ho,khd,kh,kw,kr,ks,wday,ramp,cff,co,gamw,
     &     cff1,cff2,Hrms,cfrq,cdir,cw,ch,cr,sbc,dd,cgo,
     &     dsup,Btg,cosw,sinw,eps,nw,wh
      parameter (eps=1.0e-10)
# include "param.h"
# include "grid.h"
# include "forces.h"
# include "ocean3d.h"
# include "scalars.h"
# ifdef WKB_WWAVE
#  include "wkb_wwave.h"
# endif
      integer Istr,Iend,Jstr,Jend, i,j,k
!
# include "compute_extended_bounds.h" 
!
!  Set wind induced wave amplitude (m), direction (radians) and
!  period (s) at RHO-points.
!
# if defined SED_TEST2
      do j=JstrR,JendR
        do i=IstrR,IendR
          Awave(i,j)=1.0
          Dwave(i,j)=270.*deg2rad
          Pwave(i,j)=10.
        enddo
      enddo
# elif defined SHOREFACE
      wamp = 0.5            ! wave amplitude (m)
      wdir = -10.           ! wave direction (deg)
      wprd = 8.3            ! wave period (s)

      cfrq = 2.0*pi/wprd    ! peak wave frequency (rad/s)
      cdir = wdir*deg2rad   ! wave direction (rad)
      ho=h(1,1)             ! offshore depth
#   ifdef OBC_EAST
      ho=h(LLm,1)
#   endif
      khd = ho*cfrq*cfrq/g
      kh  = sqrt(    khd*khd + khd/(1.0 + khd*(0.6666666666
     &               +khd*(0.3555555555 + khd*(0.1608465608
     &               +khd*(0.0632098765 + khd*(0.0217540484
     &                               +khd*0.0065407983)))))) )
      co=sqrt(g/kh*ho*tanh(kh))
      cgo=co*0.5*(1.+2.*kh/sinh(2.*kh))
      do j=jstrR,jendR
        do i=istrR,iendR
          dsup=0.0
          do k=1,4
            dd = h(i,j) + z_w(i,j,N) + dsup
            khd = dd*cfrq*cfrq/g
            kh = sqrt(    khd*khd + khd/(1.0 + khd*(0.6666666666
     &                    +khd*(0.3555555555 + khd*(0.1608465608
     &                    +khd*(0.0632098765 + khd*(0.0217540484
     &                                    +khd*0.0065407983)))))) )
            kw=kh/max(dd,eps)
            cw=sqrt(g/kw*tanh(kh))
            nw=0.5*(1.+2.*kh/sinh(2.*kh))  ! n=Cg/C
            ks=sqrt(cgo/(2.*nw*cw))        ! shoaling coefficient
            kr=sqrt(cos(cdir)/cos(cdir))   ! refraction coefficient
            cosw=cos(cdir)
            sinw=sin(cdir)
            gamw=0.31
            cff1=gamw*dd
            cff2=2.0*wamp*ks*kr
            wh = min(cff1,cff2)
            dsup=-0.125*(wh**2)*kw/sinh(2.*kw*dd) ! wave set-up
          enddo
!
!  Fill global arrays
!
          whrm(i,j)=2.*wh  ! Hrms = 2A
          wfrq(i,j)=sqrt(g*kw*tanh(kw*dd))
          wdrx(i,j)=cosw
          wdre(i,j)=sinw
#  ifdef WET_DRY
          IF (h(i,j) + z_w(i,j,N).le.Dcrit(i,j)+eps) THEN
            whrm(i,j)=0.
            wfrq(i,j)=0.
            wdrx(i,j)=0.
            wdre(i,j)=0.
          ENDIF
#  endif
        enddo
      enddo
# else
      do j=JstrR,JendR
        do i=IstrR,IendR
          Awave(i,j)=0.7         ! wamp (m)
          Pwave(i,j)=8.3         ! period (s)
          Dwave(i,j)=-10.        ! offshore wave angle (deg)
        enddo
      enddo
#  ifdef MRL_WCI
      Hrms=2*Awave(1,1)          ! offshore RMS wave height (m)
      cfrq=2.0*pi/Pwave(1,1)     ! peak wave frequency (rad/s)
      cdir=Dwave(1,1)*deg2rad    ! wave direction rad
!
!  Fill global arrays
!
      do j=jstr-1,jend+1
        do i=istr-1,iend+1
          whrm(i,j)=Hrms
          wfrq(i,j)=cfrq
          wdrx(i,j)=cos(cdir)
          wdre(i,j)=sin(cdir)
        enddo
      enddo
#   endif
# endif
      return
      end
#endif /* ANA_WWAVE */
!
!======================================================================
!                   subroutine ana_sediment
!======================================================================
!
#if defined SEDIMENT
      subroutine ana_sediment (tile)
      implicit none
# include "param.h"
      integer tile
# ifdef  ALLOW_SINGLE_BLOCK_MODE
C$    integer  trd, omp_get_thread_num
# endif
# include "compute_tile_bounds.h"
      call ana_sediment_tile (Istr,Iend,Jstr,Jend)
      return
      end
!
      subroutine ana_sediment_tile (Istr,Iend,Jstr,Jend)
!
!----------------------------------------------------------------------
!  This routine sets sediment ripple and bed parameters
!  from values found in sediment.in:
!
!    Hrip    initial ripple height [m]
!    Lrip    initial ripple length [m]
!    Bthk    initial thicknesses of bed layers [m]
!    Bpor    initial porosity of bed layers [m]
!    Bfrac   volume fraction of each size class in each bed layer 
!
!----------------------------------------------------------------------
!
      implicit none
# include "param.h"
# include "grid.h"
# include "scalars.h"
# include "sediment.h"
# include "bbl.h"
      real    cff1,cff2,cff3,cff4
      integer Istr,Iend,Jstr,Jend, i,j, ilay, ised
!
# undef DEBUG 
# ifdef DEBUG
      integer ick,jck
      parameter (ick=60, jck=35)
# endif

# include "compute_extended_bounds.h"

      do j=JstrR,JendR
        do i=IstrR,IendR
# ifdef BBL
#  ifndef ANA_SEDIMENT
          if(.not.got_inibed(1)) then
#  endif
            Hripple(i,j)=Hrip
#  ifndef ANA_SEDIMENT
          endif
          if(.not.got_inibed(2)) then
#  endif
            Lripple(i,j)=Lrip
#  ifndef ANA_SEDIMENT
          endif
#  endif
# endif /* BBL */
          do ilay=1,NLAY
# ifndef ANA_SEDIMENT
            if(.not.got_inised(1)) then
# endif
              bed_thick(i,j,ilay)=Bthk(ilay)

# ifndef ANA_SEDIMENT
            endif
            if(.not.got_inised(2)) then
# endif
              bed_poros(i,j,ilay)=Bpor(ilay)
# ifndef ANA_SEDIMENT
            endif
            if(.not.got_inised(3)) then
# endif
            do ised=1,NST
              bed_frac(i,j,ilay,ised)=Bfr(ilay,ised)
            enddo

            bed_age(i,j,ilay)=time
            bot_thick(i,j) = 0.003
            do ised=1,NST
              bed_mass(i,j,ilay,1,ised)=
     &                     bed_thick(i,j,ilay)*
     &                     Srho(ised)*
     &                     (1.0-bed_poros(i,j,ilay))*
     &                     bed_frac(i,j,ilay,ised)
# ifdef MASKING
     &                                 *rmask(i,j)
# endif
              bed_mass(i,j,ilay,2,ised)=bed_mass(i,j,ilay,1,ised)
            enddo
# ifndef ANA_SEDIMENT
          endif
# endif
          enddo
          cff1=1.0
          cff2=1.0
          cff3=1.0
          cff4=1.          
          do ised=1,NST
            cff1=cff1*tau_ce(ised)**bed_frac(i,j,1,ised)
            cff2=cff2*Sd(ised)**bed_frac(i,j,1,ised)
            cff3=cff3*wsed(ised)**bed_frac(i,j,1,ised)
            cff4=cff4*Srho(ised)**bed_frac(i,j,1,ised)
          enddo
          taucb(i,j)=cff1                ! [m2/s2]
          Ssize(i,j)=MIN(cff2,Zob)
          w_set(i,j)=cff3                ! [m/s]
          Sdens(i,j)=MAX(cff4,1050.)
# ifdef DEBUG
          if(j.eq.jck.and.i.eq.ick) then
            write(6,*) '********** ANA_SEDIMENT ***********'
            do ised=1,NST
              write(6,*) 'Sd(ised)',Sd(ised)
              do ilay=1,NLAY
                write(6,*) 'i,j,ilay,ised',i,j,ilay,ised
                write(6,*) 'bed_frac',bed_frac(i,j,ilay,ised)
              enddo
            enddo
            do ilay=1,NLAY
              write(6,*) 'bed_thick',bed_thick(i,j,ilay)
              write(6,*) 'bed_por',bed_poros(i,j,ilay)
            enddo
          endif
# endif
# ifdef MOVING_BATHY
          bed_thick_tot(i,j,2)=0.
          do ilay=1,NLAY
            bed_thick_tot(i,j,2)=bed_thick_tot(i,j,2)+
     &                           bed_thick(i,j,ilay)
          enddo
          bed_thick_tot(i,j,1)=bed_thick_tot(i,j,2)
# endif
        enddo
      enddo

# if defined EW_PERIODIC || defined NS_PERIODIC || defined MPI
      do ised=1,NST
        do ilay=1,NLAY
          call exchange_r2d_tile (Istr,Iend,Jstr,Jend,
     &                 bed_frac(START_2D_ARRAY,ilay,ised))
          call exchange_r2d_tile (Istr,Iend,Jstr,Jend,
     &                 bed_mass(START_2D_ARRAY,ilay,1,ised))
          call exchange_r2d_tile (Istr,Iend,Jstr,Jend,
     &                 bed_mass(START_2D_ARRAY,ilay,2,ised))
         enddo
      enddo
      do ilay=1,NLAY
         call exchange_r2d_tile (Istr,Iend,Jstr,Jend,
     &                 bed_poros(START_2D_ARRAY,ilay))
         call exchange_r2d_tile (Istr,Iend,Jstr,Jend,
     &                 bed_thick(START_2D_ARRAY,ilay))
         call exchange_r2d_tile (Istr,Iend,Jstr,Jend,
     &                 bed_age(START_2D_ARRAY,ilay))
      enddo
         call exchange_r2d_tile (Istr,Iend,Jstr,Jend,
     &                 bot_thick(START_2D_ARRAY))
#  ifdef MOVING_BATHY
         call exchange_r2d_tile (Istr,Iend,Jstr,Jend,
     &                 bed_thick_tot(START_2D_ARRAY,1))
         call exchange_r2d_tile (Istr,Iend,Jstr,Jend,
     &                 bed_thick_tot(START_2D_ARRAY,2))
#  endif
# endif
# if defined EW_PERIODIC || defined NS_PERIODIC || defined MPI
         call exchange_r2d_tile (Istr,Iend,Jstr,Jend,
     &                 taucb(START_2D_ARRAY))
         call exchange_r2d_tile (Istr,Iend,Jstr,Jend,
     &                 Ssize(START_2D_ARRAY))
         call exchange_r2d_tile (Istr,Iend,Jstr,Jend,
     &                 w_set(START_2D_ARRAY))
         call exchange_r2d_tile (Istr,Iend,Jstr,Jend,
     &                 Sdens(START_2D_ARRAY))
# endif
      return
      end
#endif /* SEDIMENT */
!
!======================================================================
!                   subroutine ana_psource
!======================================================================
!
#if defined PSOURCE && defined ANA_PSOURCE && defined SOLVE3D
!
!----------------------------------------------------------------------
!  Set analytical tracer and mass point sources and sinks
!----------------------------------------------------------------------
!
      subroutine ana_psource_tile (Istr,Iend,Jstr,Jend) 
      implicit none          
# include "param.h"
# include "scalars.h"
# include "sources.h"
# include "ocean3d.h"
# include "grid.h"
!
      integer is, k, Istr,Iend,Jstr,Jend, i,j
      real cff, cff1, cff2, ramp, Hs
      real    xno3,temp,SiO4,zsrc
# include "compute_auxiliary_bounds.h" 

      if (iic.eq.ntstart) then

!
! Set-up nondimensional shape Qshape, must add to unity
!
# if defined RIVER
#  define EXP_SHAPE
#  ifdef CST_SHAPE
       cff=1./float(N)
       do k=1,N                         ! Uniform vertical
         do is=1,Nsrc                   ! distribution
           Qshape(is,k)=cff
         enddo
       enddo
#  elif defined EXP_SHAPE
        do is=1,Nsrc                   ! Exponential vertical
          Hs=h(Isrc(is),Jsrc(is))      ! distribution
          cff=5.            ! Hs/z0  (z0 surface layer depth)
          cff1=cff/(1-exp(-cff))
          cff2=0.
          do k=1,N
            Qshape(is,k)=cff1*exp(z_r(Isrc(is),Jsrc(is),k)*cff/Hs)*
     &        (z_w(Isrc(is),Jsrc(is),k)-z_w(Isrc(is),Jsrc(is),k-1))/Hs
            cff2=cff2+Qshape(is,k)
          enddo
          do k=1,N
            Qshape(is,k)=Qshape(is,k)/cff2
          enddo
        enddo
#  elif defined AL_SHAPE
        do is=1,Nsrc                   ! Set-up nondimensional shape
          do k=1,10
            Qshape(is,k)=0.0
          enddo
          do k=11,14
            Qshape(is,k)=0.05
          enddo
          Qshape(is,15)=0.1           ! These most add to unity!
          Qshape(is,16)=0.1           ! These most add to unity!
          Qshape(is,17)=0.1           ! These most add to unity!
          Qshape(is,18)=0.1           ! These most add to unity!
          Qshape(is,19)=0.2
          Qshape(is,20)=0.2
        enddo
#  endif

# elif defined REGIONAL
        do is=1,Nsrc                   ! Exponential vertical
#  ifdef MPI
         i=Isrc_mpi(is,mynode)
         j=Jsrc_mpi(is,mynode)
#  else
         i=Isrc(is)
         j=Jsrc(is)
#  endif
          Hs=h(i,j)      ! distribution
          cff=5.            ! Hs/z0  (z0 surface layer depth)
          cff1=cff/(1-exp(-cff))
          cff2=0.
          do k=1,N
            Qshape(is,k)=cff1*exp(z_r(i,j,k)*cff/Hs)*
     &        (z_w(i,j,k)-z_w(i,j,k-1))/Hs
            cff2=cff2+Qshape(is,k)
          enddo
          do k=1,N
            Qshape(is,k)=Qshape(is,k)/cff2
          enddo
        enddo

# else
      ERROR ###  CPP-key 'ANA_PSOURCE' is defined, but no code
      ERROR ###  is provided to set up Dsrc, Isrc, Jsrc, Lsrc.
# endif /* REGIONAL */

      endif   ! iic.eq.ntstart

!
! Set-up vertically integrated mass transport [m3/s] of point
! sources (these may be time-dependent; positive in the positive U-
! or V-direction and vice-versa) and vertically distribute them
! according to mass transport profile chosen above.
!
# if defined RIVER
      ramp=1 !TANH(dt*sec2day*float(iic-ntstart))
      do is=1,Nsrc
        Qbar(is)=ramp*Qbar(is)
      enddo
# elif defined REGIONAL
      ramp=1 !TANH(dt*sec2day*float(iic-ntstart))
      do is=1,Nsrc
        Qbar(is)=ramp*Qbar(is)
      enddo
# else
      ERROR ###  CPP-key 'ANA_PSOURCE' is defined, but no code
      ERROR ###  is provided to set up Qbar(is) analytically.
# endif
      do is=1,Nsrc
        do k=1,N
          Qsrc(is,k)=Qbar(is)*Qshape(is,k)
        enddo
      enddo
!
!  Set-up tracer (tracer units) point Sources/Sinks.
!
# if defined RIVER
      do k=1,N
        do is=1,Nsrc
          Tsrc(is,k,itemp)=Tsrc0(is,itemp)
!          Tsrc(is,k,itemp)=4.+10.*exp(z_r(Isrc(is),Jsrc(is),k)/50.)
          Tsrc(is,k,isalt)=Tsrc0(is,isalt)
#  if defined PASSIVE_TRACER
          Tsrc(is,k,itpas)=Tsrc0(is,itpas)
#  endif
        enddo
      enddo
# elif defined REGIONAL
      do k=1,N
        do is=1,Nsrc
#  if defined PSOURCE_NCFILE_TS
          if (.not.got_tsrc(itemp)) then
             Tsrc0(is,itemp)=5.
          endif
          if (.not.got_tsrc(isalt)) then
             Tsrc0(is,itemp)=2.
          endif
#  endif
          Tsrc(is,k,itemp)=Tsrc0(is,itemp)
          Tsrc(is,k,isalt)=Tsrc0(is,isalt)
#  if defined PASSIVE_TRACER
#      if defined PSOURCE_NCFILE_TS
          if (.not.got_tsrc(itpas)) then
             Tsrc0(is,itpas)=1.
          endif
#      endif  /* PSOURCE_NCFILE_TS    */  
          Tsrc(is,k,itpas)=Tsrc0(is,itpas)
#  endif
#  if defined BIOLOGY
!     Fill with analytical value  if not find in netcdf file
          temp=Tsrc0(is,itemp)
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
          xno3=1.67+0.5873*SiO4+0.0144*SiO4**2
     &         +0.0003099*SiO4**3
#    if defined PSOURCE_NCFILE_TS
          if(.not.got_tsrc(iNO3_)) then 
            Tsrc0(is,iNO3_)=xno3
          endif
#    endif  /* PSOURCE_NCFILE_TS    */  
          Tsrc(is,k,iNO3_)=Tsrc0(is,iNO3_)

#    ifdef PISCES
#      if defined PSOURCE_NCFILE_TS
          if (.not.got_tsrc(iCAL_)) then 
            Tsrc0(is,iCAL_)=0.01   
          endif
          if (.not.got_tsrc(iPOC_)) then 
            Tsrc0(is,iPOC_)=0.01
          endif
          if (.not.got_tsrc(iPHY_)) then 
            Tsrc0(is,iPHY_)=0.01
          endif
          if (.not.got_tsrc(iZOO_)) then
            Tsrc0(is,iZOO_)=0.01
          endif
          if (.not.got_tsrc(iDIA_)) then
            Tsrc0(is,iDIA_)=0.01
          endif
          if (.not.got_tsrc(iBSI_)) then
            Tsrc0(is,iBSI_)=1.5e-3
          endif
          if (.not.got_tsrc(iBFE_)) then 
            Tsrc0(is,iBFE_)=1.e-2*5.e-6
          endif
          if (.not.got_tsrc(iGOC_)) then
            Tsrc0(is,iGOC_)=0.01
          endif
          if (.not.got_tsrc(iSFE_)) then
            Tsrc0(is,iSFE_)=1.e-2*5.e-6
          endif
          if (.not.got_tsrc(iDFE_)) then
            Tsrc0(is,iDFE_)=1.e-2*5.e-6
          endif
          if (.not.got_tsrc(iDSI_)) then
            Tsrc0(is,iDSI_)=1.e-2*0.15
          endif
          if (.not.got_tsrc(iNFE_)) then
            Tsrc0(is,iNFE_)=1.e-2*5e-6
          endif
          if (.not.got_tsrc(iNCH_)) then
            Tsrc0(is,iNCH_)=1.e-2*12./55.
          endif
          if (.not.got_tsrc(iDCH_)) then
            Tsrc0(is,iDCH_)=1.e-2*12./55.
          endif
          if (.not.got_tsrc(iNH4_)) then
            Tsrc0(is,iNH4_)=1.e-2
          endif
#      endif   /* PSOURCE_NCFILE_TS    */  
          Tsrc(is,k,iCAL_)=Tsrc0(is,iCAL_)
          Tsrc(is,k,iPOC_)=Tsrc0(is,iPOC_)
          Tsrc(is,k,iPHY_)=Tsrc0(is,iPHY_)
          Tsrc(is,k,iZOO_)=Tsrc0(is,iZOO_)
          Tsrc(is,k,iDIA_)=Tsrc0(is,iDIA_)
          Tsrc(is,k,iBSI_)=Tsrc0(is,iBSI_)
          Tsrc0(is,iBFE_)= Tsrc(is,k,iBFE_)
          Tsrc(is,k,iGOC_)=Tsrc0(is,iGOC_)
          Tsrc(is,k,iSFE_)=Tsrc0(is,iSFE_)
          Tsrc(is,k,iDFE_)=Tsrc0(is,iDFE_)
          Tsrc(is,k,iDSI_)=Tsrc0(is,iDSI_)
          Tsrc(is,k,iNFE_)=Tsrc0(is,iNFE_)
          Tsrc(is,k,iNCH_)=Tsrc0(is,iNCH_)
          Tsrc(is,k,iDCH_)=Tsrc0(is,iDCH_)
          Tsrc(is,k,iNH4_)=Tsrc0(is,iNH4_)
#    elif defined BIO_NChlPZD
#      if defined PSOURCE_NCFILE_TS
          if (.not.got_tsrc(iChla)) then
            Tsrc0(is,iChla)=0.08  
          endif
          if (.not.got_tsrc(iPhy1)) then
            Tsrc0(is,iPhy1)=0.1
          endif
          if (.not.got_tsrc(iZoo1)) then
            Tsrc0(is,iZoo1)=0.06
          endif
          if (.not.got_tsrc(iDet1)) then
            Tsrc0(is,iDet1)=0.02
          endif
#      endif   /* PSOURCE_NCFILE_TS    */        
          Tsrc(is,k,iChla)=Tsrc0(is,iChla)
          Tsrc(is,k,iPhy1)=Tsrc0(is,iPhy1)
          Tsrc(is,k,iZoo1)=Tsrc0(is,iZoo1)
          Tsrc(is,k,iDet1)=Tsrc0(is,iDet1)
          
#    elif defined BIO_N2ChlPZD2
#      if defined PSOURCE_NCFILE_TS
          if (.not.got_tsrc(iNH4_)) then
            Tsrc0(is,iNH4_)=0.1  
          endif
          if (.not.got_tsrc(iChla)) then
            Tsrc0(is,iChla)=0.08
          endif
          if (.not.got_tsrc(iPhy1)) then
            Tsrc0(is,iPhy1)=0.06
            
          endif
          if (.not.got_tsrc(iZoo1)) then
            Tsrc0(is,iZoo1)=0.04
            
          endif
          if (.not.got_tsrc(iDet1)) then
            Tsrc0(is,iDet1)=0.02
            
          endif
          if (.not.got_tsrc(iDet2)) then
            Tsrc0(is,iDet2)=0.02
          endif
#      endif
          Tsrc(is,k,iNH4_)= Tsrc0(is,iNH4_)
          Tsrc(is,k,iChla)=Tsrc0(is,iChla)
          Tsrc(is,k,iPhy1)=Tsrc0(is,iPhy1)
          Tsrc(is,k,iZoo1)=Tsrc0(is,iZoo1)
          Tsrc(is,k,iDet1)=Tsrc0(is,iDet1)
          Tsrc(is,k,iDet2)=Tsrc0(is,iDet2)
!----------------------------------------------------------------------
#    elif defined BIO_BioEBUS
          zsrc=z_r(Isrc(is),Jsrc(is),k)
#      if defined PSOURCE_NCFILE_TS
          if (.not.got_tsrc(iNO2_)) then
            Tsrc0(is,iNO2_)=  0.05
          endif
          if (.not.got_tsrc(iNH4_)) then
            Tsrc0(is,iNH4_)= 0.1
          endif
          if (.not.got_tsrc(iPhy1)) then
            Tsrc0(is,iPhy1)= 0.04
          endif
          if (.not.got_tsrc(iPhy2)) then
            Tsrc0(is,iPhy2)= 0.06
          endif
          if (.not.got_tsrc(iZoo1)) then
            Tsrc0(is,iZoo1)= 0.04
          endif
          if (.not.got_tsrc(iZoo2)) then
            Tsrc0(is,iZoo2)= 0.
          endif
          if (.not.got_tsrc(iDet1)) then
            Tsrc0(is,iDet1)=0.02
          endif
          if (.not.got_tsrc(iDet2)) then
            Tsrc0(is,iDet2)=0.02
          endif
          if (.not.got_tsrc(iDON)) then
            Tsrc0(is,iDON)= 0.5
          endif
#        ifdef NITROUS_OXIDE
           if (.not.got_tsrc(iN2O)) then
             Tsrc0(is,iN2O)= 0.002 ! not used ...
           endif
#        endif 
#      endif  /* PSOURCE_NCFILE_TS  */
          Tsrc(is,k,iNO2_)= Tsrc0(is,iNO2_)*exp(zsrc/100.)
          Tsrc(is,k,iNH4_)= Tsrc0(is,iNH4_)*exp(zsrc/100.)
          Tsrc(is,k,iPhy1)=Tsrc0(is,iPhy1)*exp(zsrc/50.)
          Tsrc(is,k,iPhy2)=Tsrc0(is,iPhy2)*exp(zsrc/50.)
          Tsrc(is,k,iZoo1)=Tsrc0(is,iZoo1)*exp(zsrc/100.)
          Tsrc(is,k,iZoo2)=Tsrc0(is,iZoo2)*exp(zsrc/100.)
          Tsrc(is,k,iDet1)=Tsrc0(is,iDet1)
          Tsrc(is,k,iDet2)=Tsrc0(is,iDet2)
          Tsrc(is,k,iDON)=Tsrc0(is,iDON)*exp(zsrc/100.)
          Tsrc(is,k,iN2O)=-(0.008*exp(zsrc/100.)-0.01)
          if (Tsrc(is,k,iN2O).lt.0.) Tsrc(is,k,iN2O)=0.
#    endif /* PISCES or BIO_NChlPZD or BIO_N2ChlPZD2 or BIO_BioEBUS*/
!----------------------------------------------------------------------
#  endif /* BIOLOGY */
        enddo
      enddo
# else    /*   REGIONAL   */

          ERROR ###  CPP-key 'ANA_PSOURCE' is defined, but no code
          ERROR ###  is provided to set up Tsrc(is) analytically.
# endif
      return
      end
#endif /* PSOURCE && ANA_PSOURCE SOLVE3D*/
!
!======================================================================
!                   subroutine ana_bry
!======================================================================
!
#ifdef ANA_BRY
      subroutine ana_bry (tile)
      implicit none
# include "param.h"
      integer tile
# ifdef  ALLOW_SINGLE_BLOCK_MODE
C$    integer  trd, omp_get_thread_num
# endif
# include "compute_tile_bounds.h"
      call ana_bry_tile   (Istr,Iend,Jstr,Jend)
      return
      end
!
      subroutine ana_bry_tile (Istr,Iend,Jstr,Jend) 
!
!----------------------------------------------------------------------
!  Set analytical boundary forcing
!----------------------------------------------------------------------
!
      implicit none          
      integer Istr,Iend,Jstr,Jend, i,j,k, itrc
# include "param.h"
# include "grid.h"
# include "scalars.h"
# include "boundary.h"
!
# include "compute_auxiliary_bounds.h"
!
# ifdef OBC_WEST
      if (WESTERN_EDGE) then
#  ifdef Z_FRC_BRY
        do j=JstrR,JendR
#   ifdef SWASH
           zetabry_west(j)=0.2*sin(2*pi*time/20.)      ! long waves of 20 sec period
!           zetabry_west(j)=0.07*sin(0.420*2*pi*time)   ! GLOBEX B3
!     &                    +0.03*sin(0.462*2*pi*time)   ! but with 1rst order waves
#   else
          zetabry_west(j)=0.
#   endif
#   ifdef NBQ
     &                   +h(IstrR,j)
#   endif
        enddo
#  endif
#  ifdef M2_FRC_BRY
        do j=JstrR,JendR
          ubarbry_west(j)=0.
          vbarbry_west(j)=0.
        enddo
#  endif
#  if defined SOLVE3D && (defined M3_FRC_BRY || defined T_FRC_BRY)
        do k=1,N
          do j=JstrR,JendR
#   ifdef M3_FRC_BRY
            ubry_west(j,k)=0.
            vbry_west(j,k)=0.
#   endif
#   ifdef T_FRC_BRY
            do itrc=1,NT
              tbry_west(j,k,itrc)=0.
            enddo
#   endif
          enddo
        enddo
#  endif    /* SOLVE3D && (M3_FRC_BRY || T_FRC_BRY)*/
      endif
# endif /* OBC_WEST */
!
# ifdef OBC_EAST
      if (EASTERN_EDGE) then
#  ifdef Z_FRC_BRY
        do j=JstrR,JendR
          zetabry_east(j)=0.
#   ifdef NBQ
     &                   +h(IendR,j)
#   endif
        enddo
#  endif
#  ifdef M2_FRC_BRY
        do j=JstrR,JendR
          ubarbry_east(j)=0.
          vbarbry_east(j)=0.
        enddo
#  endif
#  if defined SOLVE3D && (defined M3_FRC_BRY || defined T_FRC_BRY)
        do k=1,N
          do j=JstrR,JendR
#   ifdef M3_FRC_BRY
            ubry_east(j,k)=0.
            vbry_east(j,k)=0.
#   endif
#   ifdef T_FRC_BRY
            do itrc=1,NT
              tbry_east(j,k,itrc)=0.
            enddo
#   endif
          enddo
        enddo
#  endif    /* SOLVE3D && (M3_FRC_BRY || T_FRC_BRY)*/
      endif
# endif /* OBC_EAST */
!
# ifdef OBC_SOUTH
      if (SOUTHERN_EDGE) then
#  ifdef Z_FRC_BRY
        do i=IstrR,IendR
          zetabry_south(i)=0.
#   ifdef NBQ
     &                   +h(i,JstrR)
#   endif
        enddo
#  endif
#  ifdef M2_FRC_BRY
        do i=IstrR,IendR
          ubarbry_south(i)=0.
          vbarbry_south(i)=0.
        enddo
#  endif
#  if defined SOLVE3D && (defined M3_FRC_BRY || defined T_FRC_BRY)
        do k=1,N
          do i=IstrR,IendR
#   ifdef M3_FRC_BRY
            ubry_south(i,k)=0.
            vbry_south(i,k)=0.
#   endif
#   ifdef T_FRC_BRY
            do itrc=1,NT
              tbry_south(i,k,itrc)=0.
            enddo
#   endif
          enddo
        enddo
#  endif    /* SOLVE3D && (M3_FRC_BRY || T_FRC_BRY)*/
      endif
# endif /* OBC_SOUTH */
!
# ifdef OBC_NORTH
      if (NORTHERN_EDGE) then
#  ifdef Z_FRC_BRY
        do i=IstrR,IendR
          zetabry_north(i)=0.
#   ifdef NBQ
     &                   +h(i,JendR)
#   endif
        enddo
#  endif
#  ifdef M2_FRC_BRY
        do i=IstrR,IendR
          ubarbry_north(i)=0.
          vbarbry_north(i)=0.
        enddo
#  endif
#  if defined SOLVE3D && (defined M3_FRC_BRY || defined T_FRC_BRY)
        do k=1,N
          do i=IstrR,IendR
#   ifdef M3_FRC_BRY
            ubry_north(i,k)=0.
            vbry_north(i,k)=0.
#   endif
#   ifdef T_FRC_BRY
            do itrc=1,NT
              tbry_north(i,k,itrc)=0.
            enddo
#   endif
          enddo
        enddo
#  endif    /* SOLVE3D && (M3_FRC_BRY || T_FRC_BRY)*/
      endif
# endif /* OBC_NORTH */
!
      return
      end
#endif /* defined ANA_BRY */
!
!======================================================================
!                   subroutine ana_nbq_bry
!======================================================================
!
#ifdef NBQ_FRC_BRY
      subroutine ana_nbq_bry (tile)
      implicit none
# include "param.h"
      integer tile
# ifdef  ALLOW_SINGLE_BLOCK_MODE
C$    integer  trd, omp_get_thread_num
# endif
# include "compute_tile_bounds.h"
      call ana_nbq_bry_tile (Istr,Iend,Jstr,Jend)
      return
      end
!
      subroutine ana_nbq_bry_tile (Istr,Iend,Jstr,Jend) 
!
!----------------------------------------------------------------------
!  Set analytical boundary forcing
!----------------------------------------------------------------------
!
      implicit none          
      integer Istr,Iend,Jstr,Jend, i,j,k, itrc
# include "param.h"
# include "grid.h"
# include "scalars.h"
# include "boundary.h"
# include "climat.h"
# include "ocean3d.h"
!
# include "compute_auxiliary_bounds.h"
!
# ifdef OBC_WEST
      if (WESTERN_EDGE) then
        do k=1,N
          do j=JstrR,JendR
#  ifdef M3CLIMATOLOGY
            unbqbry_west(j,k)=0.5*rho0*(Hz(0,j,k)+Hz(1,j,k))
     &                                          *uclm(1,j,k)
#  elif defined M3_FRC_BRY
            unbqbry_west(j,k)=0.5*rho0*(Hz(0,j,k)+Hz(1,j,k))
     &                                       *ubry_west(j,k)
#  else
            unbqbry_west(j,k)=0.
#  endif
            wnbqbry_west(j,k)=0.
            rnbqbry_west(j,k)=0.
#  ifdef W_FRC_BRY
            wbry_west(j,k)=0.
#  endif
          enddo
          do j=JstrR+1,JendR
#  ifdef M3CLIMATOLOGY
            vnbqbry_west(j,k)=0.5*rho0*(Hz(0,j,k)+Hz(0,j-1,k))
     &                                            *vclm(0,j,k)
#  elif defined M3_FRC_BRY
            vnbqbry_west(j,k)=0.5*rho0*(Hz(0,j,k)+Hz(0,j-1,k))
     &                                         *vbry_west(j,k)
#  else
            vnbqbry_west(j,k)=0.
#  endif
          enddo
        enddo
      endif
# endif /* OBC_WEST */
!
# ifdef OBC_EAST
      if (EASTERN_EDGE) then
        do k=1,N
          do j=JstrR,JendR
#  ifdef M3CLIMATOLOGY
            unbqbry_east(j,k)=0.5*rho0*(Hz(IendR,j,k)+Hz(IendR-1,j,k))
     &                                                *uclm(IendR,j,k)
#  elif defined M3_FRC_BRY
            unbqbry_east(j,k)=0.5*rho0*(Hz(IendR,j,k)+Hz(IendR-1,j,k))
     &                                                 *ubry_east(j,k)
#  else
            unbqbry_east(j,k)=0.
#  endif
            wnbqbry_east(j,k)=0.
            rnbqbry_east(j,k)=0.
#  ifdef W_FRC_BRY
            wbry_east(j,k)=0.
#  endif
          enddo
          do j=JstrR+1,JendR
#  ifdef M3CLIMATOLOGY
            vnbqbry_east(j,k)=0.5*rho0*(Hz(IendR,j,k)+Hz(IendR-1,j-1,k))
     &                                                  *vclm(IendR,j,k)
#  elif defined M3_FRC_BRY
            vnbqbry_east(j,k)=0.5*rho0*(Hz(IendR,j,k)+Hz(IendR-1,j-1,k))
     &                                                   *vbry_east(j,k)
#  else
            vnbqbry_east(j,k)=0.
#  endif
          enddo
        enddo
      endif
# endif /* OBC_EAST */
!
# ifdef OBC_SOUTH
      if (SOUTHERN_EDGE) then
        do k=1,N
          do i=IstrR,IendR
#  ifdef M3CLIMATOLOGY
            vnbqbry_south(i,k)=0.5*rho0*(Hz(i,0,k)+Hz(i,1,k))
     &                                           *vclm(i,1,k)
#  elif defined M3_FRC_BRY
            vnbqbry_south(i,k)=0.5*rho0*(Hz(i,0,k)+Hz(i,1,k))
     &                                       *vbry_south(i,k)
#  else
            vnbqbry_south(j,k)=0.
#  endif
            wnbqbry_south(i,k)=0.
            rnbqbry_south(i,k)=0.
#  ifdef W_FRC_BRY
            wbry_south(i,k)=0.
#  endif
          enddo
          do i=IstrR+1,IendR
#  ifdef M3CLIMATOLOGY
            unbqbry_south(i,k)=0.5*rho0*(Hz(i,0,k)+Hz(i-1,0,k))
     &                                             *uclm(i,0,k)
#  elif defined M3_FRC_BRY
            unbqbry_south(i,k)=0.5*rho0*(Hz(i,0,k)+Hz(i-1,0,k))
     &                                         *ubry_south(i,k)
#  else
            unbqbry_south(i,k)=0.
#  endif
          enddo
        enddo
      endif
# endif /* OBC_SOUTH */
!
# ifdef OBC_NORTH
      if (NORTHERN_EDGE) then
        do k=1,N
          do i=IstrR,IendR
#  ifdef M3CLIMATOLOGY
            vnbqbry_north(i,k)=0.5*rho0*(Hz(i,JendR,k)+Hz(i,JendR-1,k))
     &                                                 *vclm(i,JendR,k)
#  elif defined M3_FRC_BRY
            vnbqbry_north(i,k)=0.5*rho0*(Hz(i,JendR,k)+Hz(i,JendR-1,k))
     &                                                 *vbry_north(i,k)
#  else
            vnbqbry_north(j,k)=0.
#  endif
            wnbqbry_north(i,k)=0.
            rnbqbry_north(i,k)=0.
#  ifdef W_FRC_BRY
            wbry_north(i,k)=0.
#  endif
          enddo
          do i=IstrR+1,IendR
#  ifdef M3CLIMATOLOGY
            unbqbry_north(i,k)=0.5*rho0*(Hz(i,JendR,k)+Hz(i-1,JendR,k))
     &                                                 *uclm(i,JendR,k)
#  elif defined M3_FRC_BRY
            unbqbry_north(i,k)=0.5*rho0*(Hz(i,JendR,k)+Hz(i-1,JendR,k))
     &                                                 *ubry_north(i,k)
#  else
            unbqbry_north(i,k)=0.
#  endif
          enddo
        enddo
      endif
# endif /* OBC_NORTH */
!
      return
      end
#endif /* defined NBQ_FRC_BRY */
!
!======================================================================
!                   subroutine ana_bry_bio
!======================================================================
!
#if defined BIOLOGY && defined T_FRC_BRY
!
!----------------------------------------------------------------------
!  Set analytical boundary forcing for biological tracers
!----------------------------------------------------------------------
!
      subroutine ana_bry_bio (tile)
      implicit none
      integer tile
# include "param.h"
# ifdef  ALLOW_SINGLE_BLOCK_MODE
C$    integer  trd, omp_get_thread_num
# endif
# include "compute_tile_bounds.h"
      call ana_bry_bio_tile (Istr,Iend,Jstr,Jend)
      return
      end
!
      subroutine ana_bry_bio_tile (Istr,Iend,Jstr,Jend)
      implicit none
      integer Istr,Iend,Jstr,Jend, i,j,k, itrc
      real    xno3,temp,SiO4,zbry
# include "param.h"
# include "boundary.h"
# include "scalars.h"
# include "ocean3d.h"
!
# include "compute_auxiliary_bounds.h"
!
# ifdef OBC_WEST
      if (WESTERN_EDGE) then
        do k=1,N
          do j=JstrR,JendR
            temp=tbry_west(j,k,itemp)
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
            xno3=1.67+0.5873*SiO4+0.0144*SiO4**2
     &           +0.0003099*SiO4**3
            if (.not.got_tbry(iNO3_)) tbry_west(j,k,iNO3_)=xno3
#  ifdef PISCES
            if (.not.got_tbry(iCAL_)) tbry_west(j,k,iCAL_)=0.01   
            if (.not.got_tbry(iPOC_)) tbry_west(j,k,iPOC_)=0.01
            if (.not.got_tbry(iPHY_)) tbry_west(j,k,iPHY_)=0.01
            if (.not.got_tbry(iZOO_)) tbry_west(j,k,iZOO_)=0.01
            if (.not.got_tbry(iDIA_)) tbry_west(j,k,iDIA_)=0.01
            if (.not.got_tbry(iBSI_)) tbry_west(j,k,iBSI_)=1.5e-3
            if (.not.got_tbry(iBFE_)) tbry_west(j,k,iBFE_)=1.e-2*5.e-6
            if (.not.got_tbry(iGOC_)) tbry_west(j,k,iGOC_)=0.01
            if (.not.got_tbry(iSFE_)) tbry_west(j,k,iSFE_)=1.e-2*5.e-6
            if (.not.got_tbry(iDFE_)) tbry_west(j,k,iDFE_)=1.e-2*5.e-6
            if (.not.got_tbry(iDSI_)) tbry_west(j,k,iDSI_)=1.e-2*0.15
            if (.not.got_tbry(iNFE_)) tbry_west(j,k,iNFE_)=1.e-2*5e-6
            if (.not.got_tbry(iNCH_)) tbry_west(j,k,iNCH_)=1.e-2*12./55.
            if (.not.got_tbry(iDCH_)) tbry_west(j,k,iDCH_)=1.e-2*12./55.
            if (.not.got_tbry(iNH4_)) tbry_west(j,k,iNH4_)=1.e-2
#  elif defined BIO_NChlPZD
            if (.not.got_tbry(iChla)) tbry_west(j,k,iChla)=0.08  
            if (.not.got_tbry(iPhy1)) tbry_west(j,k,iPhy1)=0.1
            if (.not.got_tbry(iZoo1)) tbry_west(j,k,iZoo1)=0.06
            if (.not.got_tbry(iDet1)) tbry_west(j,k,iDet1)=0.02
#  elif defined BIO_N2ChlPZD2
            if (.not.got_tbry(iNH4_)) tbry_west(j,k,iNH4_)=0.1  
            if (.not.got_tbry(iChla)) tbry_west(j,k,iChla)=0.08
            if (.not.got_tbry(iPhy1)) tbry_west(j,k,iPhy1)=0.06
            if (.not.got_tbry(iZoo1)) tbry_west(j,k,iZoo1)=0.04
            if (.not.got_tbry(iDet1)) tbry_west(j,k,iDet1)=0.02
            if (.not.got_tbry(iDet2)) tbry_west(j,k,iDet2)=0.02
#  elif defined BIO_BioEBUS
            zbry=z_r(Istr,j,k)
            if (.not.got_tbry(iNO2_)) tbry_west(j,k,iNO2_)=0.05*
     &                               exp(zbry/100.)
            if (.not.got_tbry(iNH4_)) tbry_west(j,k,iNH4_)=0.1*
     &                               exp(zbry/100.)
            if (.not.got_tbry(iPhy1)) tbry_west(j,k,iPhy1)=0.04*
     &                               exp(zbry/50.)
            if (.not.got_tbry(iPhy2)) tbry_west(j,k,iPhy2)=0.06*
     &                               exp(zbry/50.)
            if (.not.got_tbry(iZoo1)) tbry_west(j,k,iZoo1)=0.04*
     &                               exp(zbry/100.)
            if (.not.got_tbry(iZoo2)) tbry_west(j,k,iZoo2)=0.04*
     &                               exp(zbry/100.)
            if (.not.got_tbry(iDet1)) tbry_west(j,k,iDet1)=0.02
            if (.not.got_tbry(iDet2)) tbry_west(j,k,iDet2)=0.02
            if (.not.got_tbry(iDON))  tbry_west(j,k,iDON) =0.5*
     &                               exp(zbry/100.)
#   ifdef NITROUS_OXIDE
            if (.not.got_tbry(iN2O)) tbry_west(j,k,iN2O)=-(0.008*
     &                              exp(zbry/100.)-0.01)
            if (tbry_west(j,k,iN2O).lt.0.) tbry_west(j,k,iN2O)=0.  
#   endif
#  endif /* PISCES or BIO_NChlPZD or BIO_N2ChlPZD2 or BIO_BioEBUS */
          enddo
        enddo
      endif
# endif /* OBC_WEST */   
!
# ifdef OBC_EAST
      if (EASTERN_EDGE) then
        do k=1,N
          do j=JstrR,JendR
            temp=tbry_east(j,k,itemp)
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
            xno3=1.67+0.5873*SiO4+0.0144*SiO4**2
     &           +0.0003099*SiO4**3
            if (.not.got_tbry(iNO3_)) tbry_east(j,k,iNO3_)=xno3
#  ifdef PISCES
            if (.not.got_tbry(iCAL_)) tbry_east(j,k,iCAL_)=0.01   
            if (.not.got_tbry(iPOC_)) tbry_east(j,k,iPOC_)=0.01
            if (.not.got_tbry(iPHY_)) tbry_east(j,k,iPHY_)=0.01
            if (.not.got_tbry(iZOO_)) tbry_east(j,k,iZOO_)=0.01
            if (.not.got_tbry(iDIA_)) tbry_east(j,k,iDIA_)=0.01
            if (.not.got_tbry(iBSI_)) tbry_east(j,k,iBSI_)=1.5e-3
            if (.not.got_tbry(iBFE_)) tbry_east(j,k,iBFE_)=1.e-2*5.e-6
            if (.not.got_tbry(iGOC_)) tbry_east(j,k,iGOC_)=0.01
            if (.not.got_tbry(iSFE_)) tbry_east(j,k,iSFE_)=1.e-2*5.e-6
            if (.not.got_tbry(iDFE_)) tbry_east(j,k,iDFE_)=1.e-2*5.e-6
            if (.not.got_tbry(iDSI_)) tbry_east(j,k,iDSI_)=1.e-2*0.15
            if (.not.got_tbry(iNFE_)) tbry_east(j,k,iNFE_)=1.e-2*5e-6
            if (.not.got_tbry(iNCH_)) tbry_east(j,k,iNCH_)=1.e-2*12./55.
            if (.not.got_tbry(iDCH_)) tbry_east(j,k,iDCH_)=1.e-2*12./55.
            if (.not.got_tbry(iNH4_)) tbry_east(j,k,iNH4_)=1.e-2
#  elif defined BIO_NChlPZD
            if (.not.got_tbry(iChla)) tbry_east(j,k,iChla)=0.08   
            if (.not.got_tbry(iPhy1)) tbry_east(j,k,iPhy1)=0.1
            if (.not.got_tbry(iZoo1)) tbry_east(j,k,iZoo1)=0.06
            if (.not.got_tbry(iDet1)) tbry_east(j,k,iDet1)=0.02
#  elif defined BIO_N2ChlPZD2
            if (.not.got_tbry(iNH4_)) tbry_east(j,k,iNH4_)=0.1   
            if (.not.got_tbry(iChla)) tbry_east(j,k,iChla)=0.08
            if (.not.got_tbry(iPhy1)) tbry_east(j,k,iPhy1)=0.06
            if (.not.got_tbry(iZoo1)) tbry_east(j,k,iZoo1)=0.04
            if (.not.got_tbry(iDet1)) tbry_east(j,k,iDet1)=0.02
            if (.not.got_tbry(iDet2)) tbry_east(j,k,iDet2)=0.02
#  elif defined BIO_BioEBUS
            zbry=z_r(IendR,j,k)
            if (.not.got_tbry(iNO2_)) tbry_east(j,k,iNO2_)=0.05*
     &                               exp(zbry/100.)
            if (.not.got_tbry(iNH4_)) tbry_east(j,k,iNH4_)=0.1*
     &                               exp(zbry/100.)
            if (.not.got_tbry(iPhy1)) tbry_east(j,k,iPhy1)=0.04*
     &                               exp(zbry/50.)
            if (.not.got_tbry(iPhy2)) tbry_east(j,k,iPhy2)=0.06*
     &                               exp(zbry/50.)
            if (.not.got_tbry(iZoo1)) tbry_east(j,k,iZoo1)=0.04*
     &                               exp(zbry/100.)
            if (.not.got_tbry(iZoo2)) tbry_east(j,k,iZoo2)=0.04*
     &                               exp(zbry/100.)
            if (.not.got_tbry(iDet1)) tbry_east(j,k,iDet1)=0.02
            if (.not.got_tbry(iDet2)) tbry_east(j,k,iDet2)=0.02
            if (.not.got_tbry(iDON))  tbry_east(j,k,iDON) =0.5*
     &                               exp(zbry/100.)
#   ifdef NITROUS_OXIDE
            if (.not.got_tbry(iN2O)) tbry_east(j,k,iN2O)=-(0.008*
     &                               exp(zbry/100.)-0.01)
             if (tbry_east(j,k,iN2O).lt.0.) tbry_east(j,k,iN2O)=0.
#   endif
#  endif /* PISCES or BIO_NChlPZD or BIO_N2ChlPZD2 or BIO_BioEBUS */
           enddo
         enddo
      endif
# endif /* OBC_EAST */   
!
# ifdef OBC_NORTH
       if (NORTHERN_EDGE) then
         do k=1,N
           do j=IstrR,IendR
             temp=tbry_north(j,k,itemp)
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
             xno3=1.67+0.5873*SiO4+0.0144*SiO4**2
     &            +0.0003099*SiO4**3
             if (.not.got_tbry(iNO3_)) tbry_north(j,k,iNO3_)=xno3
#  ifdef PISCES
             if (.not.got_tbry(iCAL_)) tbry_north(j,k,iCAL_)=0.01   
             if (.not.got_tbry(iPOC_)) tbry_north(j,k,iPOC_)=0.01
             if (.not.got_tbry(iPHY_)) tbry_north(j,k,iPHY_)=0.01
             if (.not.got_tbry(iZOO_)) tbry_north(j,k,iZOO_)=0.01
             if (.not.got_tbry(iDIA_)) tbry_north(j,k,iDIA_)=0.01
             if (.not.got_tbry(iBSI_)) tbry_north(j,k,iBSI_)=1.5e-3
             if (.not.got_tbry(iBFE_)) tbry_north(j,k,iBFE_)=1.e-2*5.e-6
             if (.not.got_tbry(iGOC_)) tbry_north(j,k,iGOC_)=0.01
             if (.not.got_tbry(iSFE_)) tbry_north(j,k,iSFE_)=1.e-2*5.e-6
             if (.not.got_tbry(iDFE_)) tbry_north(j,k,iDFE_)=1.e-2*5.e-6
             if (.not.got_tbry(iDSI_)) tbry_north(j,k,iDSI_)=1.e-2*0.15
             if (.not.got_tbry(iNFE_)) tbry_north(j,k,iNFE_)=1.e-2*5e-6
             if (.not.got_tbry(iNCH_)) tbry_north(j,k,iNCH_)=1.e-2*12./55.
             if (.not.got_tbry(iDCH_)) tbry_north(j,k,iDCH_)=1.e-2*12./55.
             if (.not.got_tbry(iNH4_)) tbry_north(j,k,iNH4_)=1.e-2
#  elif defined BIO_NChlPZD
             if (.not.got_tbry(iChla)) tbry_north(j,k,iChla)=0.08   
             if (.not.got_tbry(iPhy1)) tbry_north(j,k,iPhy1)=0.1
             if (.not.got_tbry(iZoo1)) tbry_north(j,k,iZoo1)=0.06
             if (.not.got_tbry(iDet1)) tbry_north(j,k,iDet1)=0.02
#  elif defined BIO_N2ChlPZD2
             if (.not.got_tbry(iNH4_)) tbry_north(j,k,iNH4_)=0.1   
             if (.not.got_tbry(iChla)) tbry_north(j,k,iChla)=0.08
             if (.not.got_tbry(iPhy1)) tbry_north(j,k,iPhy1)=0.06
             if (.not.got_tbry(iZoo1)) tbry_north(j,k,iZoo1)=0.04
             if (.not.got_tbry(iDet1)) tbry_north(j,k,iDet1)=0.02
             if (.not.got_tbry(iDet2)) tbry_north(j,k,iDet2)=0.02
#  elif defined BIO_BioEBUS
             zbry=z_r(j,JendR,k)
             if (.not.got_tbry(iNO2_)) tbry_north(j,k,iNO2_)=0.05*
     &                                             exp(zbry/100.)
             if (.not.got_tbry(iNH4_)) tbry_north(j,k,iNH4_)=0.1*
     &                                             exp(zbry/100.)
             if (.not.got_tbry(iPhy1)) tbry_north(j,k,iPhy1)=0.04*
     &                                              exp(zbry/50.)
             if (.not.got_tbry(iPhy2)) tbry_north(j,k,iPhy2)=0.06*
     &                                              exp(zbry/50.)
             if (.not.got_tbry(iZoo1)) tbry_north(j,k,iZoo1)=0.04*
     &                                             exp(zbry/100.)
             if (.not.got_tbry(iZoo2)) tbry_north(j,k,iZoo2)=0.04*
     &                                             exp(zbry/100.)
             if (.not.got_tbry(iDet1)) tbry_north(j,k,iDet1)=0.02
             if (.not.got_tbry(iDet2)) tbry_north(j,k,iDet2)=0.02
             if (.not.got_tbry(iDON))  tbry_north(j,k,iDON) =0.5*
     &                               exp(zbry/100.)
#   ifdef NITROUS_OXIDE
            if (.not.got_tbry(iN2O)) tbry_north(j,k,iN2O)=-(0.008*
     &                                 exp(zbry/100.)-0.01)
            if (tbry_north(j,k,iN2O).lt.0.) tbry_north(j,k,iN2O)=0.
#   endif
#  endif /* PISCES or BIO_NChlPZD or BIO_N2ChlPZD2 or BIO_BioEBUS */
           enddo
         enddo
      endif
# endif /* OBC_NORTH */   

# ifdef OBC_SOUTH
       if (SOUTHERN_EDGE) then
         do k=1,N
           do j=IstrR,IendR
             temp=tbry_south(j,k,itemp)
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
             xno3=1.67+0.5873*SiO4+0.0144*SiO4**2
     &            +0.0003099*SiO4**3 
             if (.not.got_tbry(iNO3_)) tbry_south(j,k,iNO3_)=xno3
#  ifdef PISCES
             if (.not.got_tbry(iCAL_)) tbry_south(j,k,iCAL_)=0.01   
             if (.not.got_tbry(iPOC_)) tbry_south(j,k,iPOC_)=0.01
             if (.not.got_tbry(iPHY_)) tbry_south(j,k,iPHY_)=0.01
             if (.not.got_tbry(iZOO_)) tbry_south(j,k,iZOO_)=0.01
             if (.not.got_tbry(iDIA_)) tbry_south(j,k,iDIA_)=0.01
             if (.not.got_tbry(iBSI_)) tbry_south(j,k,iBSI_)=1.5e-3
             if (.not.got_tbry(iBFE_)) tbry_south(j,k,iBFE_)=1.e-2*5.e-6
             if (.not.got_tbry(iGOC_)) tbry_south(j,k,iGOC_)=0.01
             if (.not.got_tbry(iSFE_)) tbry_south(j,k,iSFE_)=1.e-2*5.e-6
             if (.not.got_tbry(iDFE_)) tbry_south(j,k,iDFE_)=1.e-2*5.e-6
             if (.not.got_tbry(iDSI_)) tbry_south(j,k,iDSI_)=1.e-2*0.15
             if (.not.got_tbry(iNFE_)) tbry_south(j,k,iNFE_)=1.e-2*5e-6
             if (.not.got_tbry(iNCH_)) tbry_south(j,k,iNCH_)=
     &                                                    1.e-2*12./55.
             if (.not.got_tbry(iDCH_)) tbry_south(j,k,iDCH_)=
     &                                                    1.e-2*12./55.
             if (.not.got_tbry(iNH4_)) tbry_south(j,k,iNH4_)=1.e-2
#  elif defined BIO_NChlPZD
             if (.not.got_tbry(iChla)) tbry_south(j,k,iChla)=0.08   
             if (.not.got_tbry(iPhy1)) tbry_south(j,k,iPhy1)=0.1
             if (.not.got_tbry(iZoo1)) tbry_south(j,k,iZoo1)=0.06
             if (.not.got_tbry(iDet1)) tbry_south(j,k,iDet1)=0.02
#  elif defined BIO_N2ChlPZD2
             if (.not.got_tbry(iNH4_)) tbry_south(j,k,iNH4_)=0.1 
             if (.not.got_tbry(iChla)) tbry_south(j,k,iChla)=0.08
             if (.not.got_tbry(iPhy1)) tbry_south(j,k,iPhy1)=0.06
             if (.not.got_tbry(iZoo1)) tbry_south(j,k,iZoo1)=0.04
             if (.not.got_tbry(iDet1)) tbry_south(j,k,iDet1)=0.02
             if (.not.got_tbry(iDet2)) tbry_south(j,k,iDet2)=0.02
#  elif defined BIO_BioEBUS
             zbry=z_r(j,JstrR,k)
            if (.not.got_tbry(iNO2_)) tbry_south(j,k,iNO2_)=0.05*
     &                                     exp(zbry/100.)
            if (.not.got_tbry(iNH4_)) tbry_south(j,k,iNH4_)=0.1*
     &                                     exp(zbry/100.)
            if (.not.got_tbry(iPhy1)) tbry_south(j,k,iPhy1)=0.04*
     &                                     exp(zbry/50.)
            if (.not.got_tbry(iPhy2)) tbry_south(j,k,iPhy2)=0.06*
     &                                     exp(zbry/50.)
            if (.not.got_tbry(iZoo1)) tbry_south(j,k,iZoo1)=0.04*
     &                                     exp(zbry/100.)
            if (.not.got_tbry(iZoo2)) tbry_south(j,k,iZoo2)=0.04*
     &                                     exp(zbry/100.) 
            if (.not.got_tbry(iDet1)) tbry_south(j,k,iDet1)=0.02
            if (.not.got_tbry(iDet2)) tbry_south(j,k,iDet2)=0.02
            if (.not.got_tbry(iDON))  tbry_south(j,k,iDON) =0.5*
     &                                     exp(zbry/100.)
#   ifdef NITROUS_OXIDE
            if (.not.got_tbry(iN2O)) tbry_south(j,k,iN2O)=-(0.008*
     &                              exp(zbry/100.)-0.01)
             if (tbry_south(j,k,iN2O).lt.0.) then
               tbry_south(j,k,iN2O)=0.  
             endif
#   endif
#  endif /* PISCES or BIO_NChlPZD or BIO_N2ChlPZD2 or BIO_BioEBUS */
           enddo
         enddo
      endif
# endif /* OBC_SOUTH */   
      return
      end
#else 
      subroutine empty_analytical
      return
      end
#endif /* BIOLOGY && defined T_FRC_BRY */


