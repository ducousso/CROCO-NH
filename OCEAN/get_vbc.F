! $Id: get_vbc.F 1615 2014-12-17 13:27:07Z rblod $
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

      subroutine get_vbc
!
!--------------------------------------------------------------------
!  This subroutine reads various forcing fields fom NetCDF files and
! save them as globally accessable arrays (declared in common blocks
! in file "forces.h"):
!
!   sustrg    kinematic surface momentum flux (wind stress) in
!                 the XI-direction [m^2/s^2].
!   svstrg    kinematic surface momentum flux (wind stress) in
!                 the ETA-direction [m^2/s^2].
!   srflxg    kinematic surface shortwave solar radiation flux
!                 [degC m/s].
!   stflxg    kinematic surface flux of tracer type variables
!                 [temperature: degC m/s; salinity: PSU m/s].   
!-------------------------------------------------------------------
!
      implicit none
#include "param.h"
#include "sources.h"
!

!---------------------------------------------------------------
! River discharge, temperature and salinity
!---------------------------------------------------------------
!
#ifdef PSOURCE_NCFILE
        call get_psource      
# ifdef PSOURCE_NCFILE_TS
        call get_psource_ts
# endif
#endif
!
#ifndef OW_COUPLING
!
!---------------------------------------------------------------
! Initial wave periode, amplitude, direction and dissipation
!---------------------------------------------------------------
!
# if (defined BBL || defined MRL_WCI) &&\
     (!defined ANA_WWAVE && !defined WKB_WWAVE)
#  if defined  AGRIF && defined BBL
      if (Agrif_lev_sedim.EQ.0) call get_wwave
#  else
      call get_wwave
#  endif
# endif
#endif /* !OW_COUPLING */
!
#ifndef OA_COUPLING
!
!---------------------------------------------------------------
! Kinematic surface momentum flux (wind stress) components
!  "sustrg" and "svstrg" [m^2/s^2].
!---------------------------------------------------------------
!
# ifndef ANA_SMFLUX
#  if !defined BULK_FLUX || (defined BULK_FLUX && !defined BULK_SMFLUX)
      call get_smflux
#  endif
# endif
!
# ifdef SOLVE3D
!
!---------------------------------------------------------------
! Kinematic surface temperature (heat) flux [degC m/s].
!---------------------------------------------------------------
!
#  if defined BULK_FLUX && !defined ANA_BULK
#   ifdef ONLINE
      ! Load data for ONLINE bulk forcing
      call get_bulk_online
#   else
      ! Load data for OFFLINE bulk forcing
      call get_bulk
#   endif
#  elif !defined ANA_STFLUX
      call get_stflux (itemp)
#  endif
!
!---------------------------------------------------------------
! Flux correction to surface net heat flux.
!---------------------------------------------------------------
!
#  if defined QCORRECTION && !defined ANA_SST
      call get_sst
#  endif
!
!---------------------------------------------------------------
! Kinematic surface freshwater flux (E-P) flux [PSU m/s].
!---------------------------------------------------------------
!
#  ifndef BULK_EP
#   if defined SALINITY && !defined ANA_SSFLUX
      call get_stflux (isalt)
#   endif
#  endif /* NON BULK_EP */
!
!---------------------------------------------------------------
! Flux correction to surface salt flux.
!---------------------------------------------------------------
!
#  if defined SALINITY && defined SFLX_CORR && !defined ANA_SSS
      call get_sss
#  endif
!
!---------------------------------------------------------------
! Kinematic surface solar shortwave radiation flux [degC m/s].
!---------------------------------------------------------------
!
! If BULK_FLUX is defined, radsw copies to srflx in get_bulk
!
#  ifndef BULK_FLUX
#   if defined LMD_SKPP || defined LMD_BKPP || defined GLS_MIXING
#    ifndef ANA_SRFLUX
      call get_srflux
#    endif
#   endif
#  endif /* !BULK_FLUX */
# endif /* SOLVE3D */
#endif /* !OA_COUPLING */

      return
      end
!
!====================================================================
!                   subroutine set_vbc
!====================================================================
!
      subroutine set_vbc (tile)
      implicit none
#include "param.h"
#include "private_scratch.h"
      integer tile, trd, omp_get_thread_num 
#include "compute_tile_bounds.h"
!
      trd=omp_get_thread_num()
      call set_vbc_tile (Istr,Iend,Jstr,Jend, 
     &         A2d(1,1,trd),A2d(1,2,trd),A2d(1,3,trd))
      return
      end

      subroutine set_vbc_tile (Istr,Iend,Jstr,Jend, wrk,wrk1,wrk2)
!
!----------------------------------------------------------------
!
!  This subroutine sets the vertical boundary conditions for 
!  momentum and tracers.
!
!  On Output:  stored in common blocks (see include file forces.h)
!
!  sustr       kinematic surface momentum flux (wind stress) in
!                                      the XI-direction (m^2/s^2).
!  svstr       kinematic surface momentum flux (wind stress) in
!                                     the ETA-direction (m^2/s^2).
!  srflx       kinematic surface shortwave solar radiation flux
!                                                      (degC m/s).
!  stflx       kinematic surface flux of tracer type variables
!                      (temperature: degC m/s; salinity: PSU m/s).
!  btflx       Kinematic bottom flux of tracer type variables
!                      (temperature: degC m/s; salinity: PSU m/s).
!  bustr       kinematic bottom momentum flux in
!                                      the XI-direction (m^2/s^2).
!  bvstr       kinematic bottom momentum flux in
!                                     the ETA-direction (m^2/s^2).
!----------------------------------------------------------------
!
#ifdef AGRIF
      use Agrif_Util
#endif
      implicit none
#include "param.h"
#include "grid.h"
#include "ocean3d.h"
#include "forces.h"
#include "scalars.h"
#include "climat.h"
#include "sources.h"
#include "mpi_roms.h"
      integer i,j,is, Istr,Iend,Jstr,Jend
      real cff,cff1
      real wrk(PRIVATE_2D_SCRATCH_ARRAY)
      real wrk1(PRIVATE_2D_SCRATCH_ARRAY)
      real wrk2(PRIVATE_2D_SCRATCH_ARRAY)
!
#include "compute_auxiliary_bounds.h"
!
#ifndef OA_COUPLING
!
!---------------------------------------------------------------
! Kinematic surface momentum flux (wind stress) components
! "sustr" and "svstr" [m^2/s^2].
!---------------------------------------------------------------
!
# ifdef ANA_SMFLUX
      call ana_smflux_tile (Istr,Iend,Jstr,Jend)
# else
#  if !defined BULK_FLUX || (defined BULK_FLUX && !defined BULK_SMFLUX)
      call set_smflux_tile (Istr,Iend,Jstr,Jend)
#  endif
# endif

# ifdef SOLVE3D
!
!---------------------------------------------------------------
! Kinematic surface temperature (heat) flux [degC m/s] and
!           surface solar shortwave radiation flux [degC m/s]
!           surface freshwater (E-P) flux [PSU m/s].
!
! --- This sets the input variables for Bulk formulation ---
!  --- The Bulk routine is actually called from step.F ---
!---------------------------------------------------------------
!
#  ifdef BULK_FLUX
#   ifdef ONLINE
      call set_bulk_tile_online (Istr,Iend,Jstr,Jend)
#   else
      call set_bulk_tile (Istr,Iend,Jstr,Jend)
#   endif
#  else
!
!---------------------------------------------------------------
! Kinematic surface temperature (heat) flux [degC m/s].
!---------------------------------------------------------------
!
#   ifdef ANA_STFLUX
      call ana_stflux_tile (Istr,Iend,Jstr,Jend, itemp)
#   else
      call set_stflux_tile (Istr,Iend,Jstr,Jend, itemp)
#   endif
!
!---------------------------------------------------------------
! Kinematic surface solar shortwave radiation flux [degC m/s].
!---------------------------------------------------------------
!
#   if defined LMD_SKPP || defined LMD_BKPP || defined GLS_MIXING
#    ifdef ANA_SRFLUX
      call ana_srflux_tile (Istr,Iend,Jstr,Jend)
#    else
      call set_srflux_tile (Istr,Iend,Jstr,Jend)
#    endif
#   endif
#  endif /* BULK_FLUX */
!
!---------------------------------------------------------------
! Flux correction to surface net heat flux.
!---------------------------------------------------------------
!
#  ifdef QCORRECTION
#   ifdef ANA_SST
      call ana_sst_tile (Istr,Iend,Jstr,Jend)
#   else
      call set_sst_tile (Istr,Iend,Jstr,Jend)
#   endif
#   ifndef BULK_FLUX
!
! If BULK_FLUX is defined, correction is done in bulk_flux
!
      do j=JstrR,JendR
        do i=IstrR,IendR
          stflx(i,j,itemp)=stflx(i,j,itemp)+
     &                  dqdt(i,j)*(t(i,j,N,nrhs,itemp)-sst(i,j))
        enddo
      enddo
#   endif
#  endif /* QCORRECTION */
!
!---------------------------------------------------------------
! Kinematic surface freshwater flux (E-P) flux [PSU m/s].
!---------------------------------------------------------------
!
#  if defined SALINITY && !defined BULK_EP
#   ifdef ANA_SSFLUX
      call ana_stflux_tile (Istr,Iend,Jstr,Jend, isalt)
#   else
      call set_stflux_tile (Istr,Iend,Jstr,Jend, isalt) 
#   endif
!
! Multiply flux by surface salinity.
!
      do j=JstrR,JendR
        do i=IstrR,IendR
          stflx(i,j,isalt)=stflx(i,j,isalt)*t(i,j,N,nrhs,isalt)
        enddo
      enddo
#  endif /* !BULK_EP */
!
!--------------------------------------------------------------
! Flux correction to surface salt flux.
!--------------------------------------------------------------
!
#  ifdef SFLX_CORR
#   ifdef ANA_SSS
      call ana_sss_tile (Istr,Iend,Jstr,Jend)
#   else
      call set_sss_tile (Istr,Iend,Jstr,Jend) 
#   endif
#   ifndef BULK_FLUX
!
! If BULK_FLUX is defined, correction is done in bulk_flux
!
      do j=JstrR,JendR
        do i=IstrR,IendR
          stflx(i,j,isalt)=stflx(i,j,isalt)+
     &                  dqdt(i,j)*(t(i,j,N,nrhs,isalt)-sss(i,j))
        enddo
      enddo
#   endif
#  endif /* SFLX_CORR */
!
!---------------------------------------------------------------
! Diurnal modulation of surface solar shortwave radiation flux
!---------------------------------------------------------------
!
#  if defined LMD_SKPP || defined LMD_BKPP || defined GLS_MIXING
#   ifdef ANA_DIURNAL_SW
      call ana_diurnal_sw_tile (Istr,Iend,Jstr,Jend)
#   endif
#  endif
!
!---------------------------------------------------------------
! Ensure that drying water does not receive heat/salt fluxes
!  to avoid unrealistic T,S values
!---------------------------------------------------------------
!
#  ifdef WET_DRY
      do j=JstrR,JendR
        do i=IstrR,IendR
          stflx(i,j,itemp)=stflx(i,j,itemp)*rmask_wet(i,j)
          srflx(i,j)=srflx(i,j)*rmask_wet(i,j)
#   ifdef SALINITY
          stflx(i,j,isalt)=stflx(i,j,isalt)*rmask_wet(i,j)
#   endif
        enddo
      enddo
#  endif
# endif /* SOLVE3D */
#endif /* !OA_COUPLING */

#ifdef SOLVE3D
!
!---------------------------------------------------------------
!  Kinematic bottom temperature (heat) flux [degC m/s].
!  (Analytical bottom heat flux is usually set to zero.)
!---------------------------------------------------------------
!
# ifdef ANA_BTFLUX
      call ana_btflux_tile (Istr,Iend,Jstr,Jend, itemp) 
# else
      call set_btflux_tile (Istr,Iend,Jstr,Jend, itemp)
# endif
!
!---------------------------------------------------------------
! Kinematic bottom salt flux [PSU m/s].
! (Aanalytical bottom salt flux is usually set to zero.)
!---------------------------------------------------------------
!
# ifdef SALINITY
#  ifdef ANA_BSFLUX
      call ana_btflux_tile (Istr,Iend,Jstr,Jend, isalt)
#  else
      call set_btflux_tile (Istr,Iend,Jstr,Jend, isalt)
#  endif
!
!---------------------------------------------------------------
! Multiply flux by bottom salinity.
!---------------------------------------------------------------
!
      do j=JstrR,JendR
        do i=IstrR,IendR
          btflx(i,j,isalt)=btflx(i,j,isalt)*t(i,j,1,nrhs,isalt)
        enddo
      enddo
# endif /* SALINITY */
#endif   /* SOLVE3D */
!
!---------------------------------------------------------------
! Kinematic bottom momentum flux [m^2/s^2]
!
! if BBL is defined, bottom flux (bustr,bvstr) is computed in
! subroutine bbl (called in step after call to set_vbc).
!---------------------------------------------------------------
!
#if defined BBL && defined AGRIF
      IF (Agrif_Fixed().LT.Agrif_lev_sedim) THEN
#endif

#if (!defined BBL || defined AGRIF)
# ifdef ANA_BMFLUX
      call ana_bmflux_tile (Istr,Iend,Jstr,Jend)
# elif defined SOLVE3D
!
!  Set bottom stress using logarithmic or linear 
!  and/or quadratic formulation. 
!
      if (Zob.ne.0.) then
        do j=JstrV-1,Jend
          do i=IstrU-1,Iend
            cff=vonKar/LOG((z_r(i,j,1)-z_w(i,j,0))/Zob)
            wrk(i,j)=MIN(Cdb_max,MAX(Cdb_min,cff*cff))
          enddo
        enddo
        do j=Jstr,Jend
          do i=IstrU,Iend
            cff=0.25*(v(i  ,j,1,nrhs)+v(i  ,j+1,1,nrhs)+
     &                v(i-1,j,1,nrhs)+v(i-1,j+1,1,nrhs))
            bustr(i,j)=0.5*(wrk(i-1,j)+wrk(i,j))*u(i,j,1,nrhs)*
     &                 SQRT(u(i,j,1,nrhs)*u(i,j,1,nrhs)+cff*cff)
          enddo
        enddo
        do j=JstrV,Jend
          do i=Istr,Iend
            cff=0.25*(u(i,j  ,1,nrhs)+u(i+1,j,1,nrhs)+
     &                u(i,j-1,1,nrhs)+u(i+1,j-1,1,nrhs))
            bvstr(i,j)=0.5*(wrk(i,j-1)+wrk(i,j))*v(i,j,1,nrhs)*
     &                 SQRT(cff*cff+v(i,j,1,nrhs)*v(i,j,1,nrhs))
          enddo
        enddo
      elseif (rdrg2.gt.0.) then
        do j=JstrV,Jend
          do i=Istr,Iend

            cff=0.25*(v(i,j,1,nrhs)+v(i,j+1,1,nrhs)+v(i-1,j,1,nrhs)+
     &                                            v(i-1,j+1,1,nrhs))
            bustr(i,j)=u(i,j,1,nrhs)*(rdrg2*sqrt(
     &                         u(i,j,1,nrhs)*u(i,j,1,nrhs)+cff*cff
     &                                                            ))
          enddo
        enddo
        do j=Jstr,Jend
          do i=IstrU,Iend
            cff=0.25*(u(i,j,1,nrhs)+u(i+1,j,1,nrhs)+u(i,j-1,1,nrhs)+
     &                                            u(i+1,j-1,1,nrhs))
            bvstr(i,j)=v(i,j,1,nrhs)*(rdrg2*sqrt(
     &                         cff*cff+v(i,j,1,nrhs)*v(i,j,1,nrhs)
     &                                                            ))
          enddo
        enddo
      else
        do j=Jstr,Jend
          do i=Istr,Iend
            bustr(i,j)=rdrg*u(i,j,1,nrhs)
          enddo
        enddo
        do j=Jstr,Jend
          do i=Istr,Iend
            bvstr(i,j)=rdrg*v(i,j,1,nrhs)
          enddo
        enddo
      endif

#  ifdef LIMIT_BSTRESS
!
!  From J. Warner's code:
!  Set limiting factor for bottom stress. The bottom stress is adjusted
!  to not change the direction of momentum.  It only should slow down
!  to zero.  The value of 0.75 is arbitrary limitation assigment.
!
      cff=0.75/dt
      do j=Jstr,Jend
        do i=IstrU,Iend
          cff1=cff*0.5*(Hz(i-1,j,1)+Hz(i,j,1))
          bustr(i,j)=SIGN(1.D0, bustr(i,j))* 
     &               MIN(ABS(bustr(i,j)),
     &                   ABS(u(i,j,1,nrhs))*cff1)
        enddo
      enddo
      do j=JstrV,Jend
        do i=Istr,Iend
          cff1=cff*0.5*(Hz(i,j-1,1)+Hz(i,j,1))
          bvstr(i,j)=SIGN(1.D0, bvstr(i,j))*
     &               MIN(ABS(bvstr(i,j)),
     &                   ABS(v(i,j,1,nrhs))*cff1)
        enddo
      enddo
#  endif

!
! Set boundary conditions
!
#  ifndef EW_PERIODIC
      IF (EASTERN_EDGE) THEN
        DO j=Jstr,Jend
          bustr(Iend+1,j)=bustr(Iend,j)
        END DO
        DO j=JstrV,Jend
          bvstr(Iend+1,j)=bvstr(Iend,j)
        END DO
      END IF
      IF (WESTERN_EDGE) THEN
        DO j=Jstr,Jend
          bustr(IstrU-1,j)=bustr(IstrU,j)
        END DO
        DO j=JstrV,Jend
          bvstr(Istr-1,j)=bvstr(Istr,j)
        END DO
      END IF
#  endif
#  ifndef NS_PERIODIC
      IF (NORTHERN_EDGE) THEN
        DO i=IstrU,Iend
          bustr(i,Jend+1)=bustr(i,Jend)
        END DO
        DO i=Istr,Iend
          bvstr(i,Jend+1)=bvstr(i,Jend)
        END DO
      END IF
      IF (SOUTHERN_EDGE) THEN
        DO i=IstrU,Iend
          bustr(i,Jstr-1)=bustr(i,Jstr)
        END DO
        DO i=Istr,Iend
          bvstr(i,JstrV-1)=bvstr(i,JstrV)
        END DO
      END IF
#  endif
#  if !defined EW_PERIODIC && !defined NS_PERIODIC
      IF (SOUTHERN_EDGE.and.WESTERN_EDGE) THEN
        bustr(Istr,Jstr-1)=0.5*(bustr(Istr+1,Jstr-1)+bustr(Istr,Jstr))
        bvstr(Istr-1,Jstr)=0.5*(bvstr(Istr,Jstr)+bvstr(Istr-1,Jstr+1))
      END IF
      IF (SOUTHERN_EDGE.and.EASTERN_EDGE) THEN
        bustr(Iend+1,Jstr-1)=0.5*(bustr(Iend+1,Jstr)+bustr(Iend,Jstr-1))
        bvstr(Iend+1,Jstr)=0.5*(bvstr(Iend+1,Jstr+1)+bvstr(Iend,Jstr))
      END IF
      IF (NORTHERN_EDGE.and.WESTERN_EDGE) THEN
        bustr(Istr,Jend+1)=0.5*(bustr(Istr,Jend)+bustr(Istr+1,Jend+1))
        bvstr(Istr-1,Jend+1)=0.5*(bvstr(Istr-1,Jend)+bvstr(Istr,Jend+1))
      END IF
      IF (NORTHERN_EDGE.and.EASTERN_EDGE) THEN
        bustr(Iend+1,Jend+1)=0.5*(bustr(Iend+1,Jend)+bustr(Iend,Jend+1))
        bvstr(Iend+1,Jend+1)=0.5*(bvstr(Iend+1,Jend)+bvstr(Iend,Jend+1))
      END IF
#  endif
#  if defined EW_PERIODIC || defined NS_PERIODIC || defined MPI
      call exchange_u2d_tile (Istr,Iend,Jstr,Jend,bustr(START_2D_ARRAY))
      call exchange_v2d_tile (Istr,Iend,Jstr,Jend,bvstr(START_2D_ARRAY))
#  endif
# endif /* SOLVE3D */
#endif /* BBL */

#if defined BBL && defined AGRIF
      ENDIF
#endif

#ifdef SOLVE3D
# if defined EW_PERIODIC || defined NS_PERIODIC || defined MPI
      call exchange_u2d_tile (Istr,Iend,Jstr,Jend,sustr(START_2D_ARRAY))
      call exchange_v2d_tile (Istr,Iend,Jstr,Jend,svstr(START_2D_ARRAY))
      call exchange_r2d_tile (Istr,Iend,Jstr,Jend,
     &                                      stflx(START_2D_ARRAY,itemp))
      call exchange_r2d_tile (Istr,Iend,Jstr,Jend,srflx(START_2D_ARRAY))
#  ifdef SALINITY
      call exchange_r2d_tile (Istr,Iend,Jstr,Jend,
     &                                      stflx(START_2D_ARRAY,isalt))
#  endif
# endif
#endif
#ifdef PSOURCE_NCFILE
      call set_psource_tile(Istr,Iend,Jstr,Jend)  
      do is=1,Nsrc
        Qbar(is)=qbardir(is)*Qbar(is)
      enddo
# ifdef PSOURCE_NCFILE_TS
      call set_psource_ts_tile(Istr,Iend,Jstr,Jend)  
# endif
#endif
#if defined PSOURCE & defined ANA_PSOURCE
C$OMP BARRIER
C$OMP MASTER
      call ana_psource_tile (Istr,Iend,Jstr,Jend)
C$OMP END MASTER
#endif
#ifndef OW_COUPLING
# if (defined BBL || defined MRL_WCI) &&\
     (!defined ANA_WWAVE && !defined WKB_WWAVE)
      call set_wwave_tile(Istr,Iend,Jstr,Jend)  
# endif
#endif

      return
      end
