! $Id: cpl_prism_get.F 1574 2014-07-01 15:13:38Z gcambon $
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
      SUBROUTINE cpl_prism_get (rcvtime)
!     !---------------------------------------------------------------------
!     !              ***  ROUTINE cpl_prism_get  ***
!     !
!     ! ** Purpose : - 
!     !           
!     !
!     !----------------------------------------------------------------------
#include "cppdefs.h"
#if defined OA_COUPLING || defined OW_COUPLING
# ifdef OA_MCT
      USE mod_prism             ! OASIS3 prism module
# else 
      USE mod_prism_get_proto   ! OASIS3 prism module for receiving
# endif
# ifdef AGRIF
      use Agrif_Util
# endif
      IMPLICIT NONE

#include "cppdefs.h"

#include "param.h"
#include "forces.h"
#include "mpi_roms.h"
#include "ocean3d.h"
#include "scalars.h"
#include "grid.h"

      INTEGER    :: rcvtime, i, j, io, jo
      INTEGER    :: iok_taux, iok_tauy
      INTEGER    :: iok_srflx, iok_itemp , iok_isalt
      INTEGER    :: iok_wdry                                   ! OW_COUPLING 
      INTEGER    :: iok_wt0m1, iok_whs, iok_wcdir, iok_wsdir  ! OW_COUPLING 
      INTEGER    :: iok_wbhd, iok_wubr, iok_wfoc, iok_wlm      ! OW_COUPLING 
      INTEGER    :: iok_wtwox, iok_wtwoy, iok_wtaux, iok_wtauy ! OW_COUPLING 
      INTEGER    :: imin_halo, imax_halo, jmin_halo, jmax_halo
      INTEGER    :: level
      CHARACTER*1 clevel

      real, parameter :: eps=1.e-20
      real,dimension(GLOBAL_2D_ARRAY) :: tab_t_tmp
# if (!defined OA_GRID_UV)
      real,dimension(GLOBAL_2D_ARRAY) :: tab_u_tmp
      real,dimension(GLOBAL_2D_ARRAY) :: tab_v_tmp
# endif

# ifdef AGRIF
      level=Agrif_Fixed()
# else
      level=0
# endif
      write(clevel,'(i1.1)') level
!      
      if (level==0) then
         MPI_master_only 
     & write(*,*) 'CPL-ROMS : Get for parent at time', rcvtime
      else		
         MPI_master_only 
     & write(*,*) 'CPL-ROMS : Get for child:', level,
     &  'at time', rcvtime
      endif
!
      imin_halo = 1  
      imax_halo = Lmmpi  
      jmin_halo = 1  
      jmax_halo = Mmmpi  

      if (WEST_INTER) then
         imin_halo = imin_halo - 1
      endif 
      if (EAST_INTER) then
         imax_halo = imax_halo + 1
      endif 
      if (SOUTH_INTER) then     
         jmin_halo = jmin_halo - 1
      endif
      if (NORTH_INTER) then
         jmax_halo = jmax_halo + 1
      endif 
!
      iok_srflx = 0
      iok_isalt = 0
      iok_itemp = 0
      iok_taux = 0
      iok_tauy = 0
!
!     add for OW_COUPLING
      iok_wdry = 0
      iok_wt0m1 = 0
      iok_whs = 0
      iok_wcdir = 0
      iok_wsdir = 0
      iok_wbhd = 0
      iok_wubr = 0
      iok_wfoc = 0
      iok_wlm = 0
      iok_wtwox = 0
      iok_wtwoy = 0
      iok_wtaux = 0
      iok_wtauy = 0
!     add end for OW_COUPLING
!
!      print *,'CROCO, number of received fields = ',krcv
      DO i = 1, krcv
# ifdef OA_COUPLING
!-------------------------------------------------
         if (srcv_clname(i)=='RRMSRFL'//clevel) then
            call cpl_prism_getvar(srcv_nid(i), srcv_clname(i),
     &  tab_t_tmp, rcvtime, iok_srflx) 
            if (iok_srflx /= 0) then
!              Transmit to ROMS field
               call exchange_r2d_tile(1,Lmmpi,1,Mmmpi,tab_t_tmp(START_2D_ARRAY))
               srflx = tab_t_tmp / (rho0*Cp) 
            endif
         end if  
!-------------------------------------------------
         if (srcv_clname(i)=='RRMEVPR'//clevel) then
            call cpl_prism_getvar(srcv_nid(i), srcv_clname(i),
     &  tab_t_tmp, rcvtime, iok_isalt) 
            if (iok_isalt /= 0) then
!              Transmit to ROMS field : stflx(:,:,isalt)
!              Changing Unit from [kg/m^2/s] (<--> [mm/s]) to [PSU m/s]  --> coeff= 1/1000.
               call exchange_r2d_tile(1,Lmmpi,1,Mmmpi,tab_t_tmp(START_2D_ARRAY))
               stflx(:,:,isalt) = tab_t_tmp * t(:,:,N,nstp,isalt) / 1000.
            endif
         end if
!-------------------------------------------------
         if (srcv_clname(i)=='RRMSTFL'//clevel) then
            call cpl_prism_getvar(srcv_nid(i), srcv_clname(i),
     &  tab_t_tmp, rcvtime, iok_itemp) 
            if (iok_itemp /= 0)  then
!              Transmit to ROMS field : stflx(:,:,itemp)
               call exchange_r2d_tile(1,Lmmpi,1,Mmmpi,tab_t_tmp(START_2D_ARRAY))
               stflx(:,:,itemp) = tab_t_tmp / (rho0*Cp)
            endif     
         end if
!-------------------------------------------------
         if (srcv_clname(i)=='RRMTAUX'//clevel) then
            call cpl_prism_getvar(srcv_nid(i), srcv_clname(i),
     &  tab_t_tmp, rcvtime, iok_taux) 
            if (iok_taux /= 0) then  
#  if (!defined OA_GRID_UV)  
               call exchange_r2d_tile(1,Lmmpi,1,Mmmpi,tab_t_tmp(START_2D_ARRAY))
!              Interpolate TAUX on grid T --> U
               do jo=jmin_halo-1,jmax_halo+1
               do io=imin_halo,imax_halo+1
                  tab_u_tmp(io,jo)=0.5*(tab_t_tmp(io-1,jo)+tab_t_tmp(io,jo))
               enddo
               enddo
!              Transmit to ROMS field : sustr
               sustr  = tab_u_tmp / rho0
#  else
!              Transmit to ROMS field : sustr
               call exchange_u2d_tile (1,Lmmpi,1,Mmmpi,tab_t_tmp(START_2D_ARRAY))
               sustr  = tab_t_tmp / rho0
#  endif   
            endif
         end if
!-------------------------------------------------
         if (srcv_clname(i)=='RRMTAUY'//clevel) then
            call cpl_prism_getvar(srcv_nid(i), srcv_clname(i),
     &  tab_t_tmp, rcvtime, iok_tauy) 
            if (iok_tauy /= 0) then 
#  if (!defined OA_GRID_UV) 
               call exchange_r2d_tile(1,Lmmpi,1,Mmmpi,tab_t_tmp(START_2D_ARRAY))
!              Interpolate TAUY on grid T --> U
               do jo=jmin_halo,jmax_halo+1
               do io=imin_halo-1,imax_halo+1
                  tab_v_tmp(io,jo)=0.5*(tab_t_tmp(io,jo-1)+tab_t_tmp(io,jo))
               enddo
               enddo
!              Transmit to ROMS field : svstr
               svstr = tab_v_tmp(:,:) / rho0
#  else
!              Transmit to ROMS field : svstr
               call exchange_v2d_tile (1,Lmmpi,1,Mmmpi,tab_t_tmp(START_2D_ARRAY))
               svstr = tab_t_tmp(:,:) / rho0                
#  endif 
            endif
         end if
# endif
# ifdef OW_COUPLING
!        print *,'entering get ow_coupling'
!        print *,'srcv_clname(i)=',srcv_clname(i)
!-------------------------------------------------
!------ OW_COUPLING - WAVE VARIABLES -------------
!-------------------------------------------------
!        mask to manage wet-drying
!        not tested/performed yet
         if (srcv_clname(i)=='RRM_DRY'//clevel) then
            call cpl_prism_getvar(srcv_nid(i), srcv_clname(i),
     &  tab_t_tmp, rcvtime, iok_wdry)
!            if (iok_wdry /= 0)  then
!!              Transmit to ROMS field : stflx(:,:,itemp)
!               call exchange_r2d_tile(1,Lmmpi,1,Mmmpi,tab_t_tmp(START_2D_ARRAY))
!               stflx(:,:,itemp) = tab_t_tmp / (rho0*Cp)
!            endif
         end if
!-------------------------------------------------
!        wave_t0m1 : mean period (s)
         if (srcv_clname(i)=='RRMT0M1'//clevel) then
            call cpl_prism_getvar(srcv_nid(i), srcv_clname(i),
     &  tab_t_tmp, rcvtime, iok_wt0m1)
            if (iok_wt0m1 /= 0)  then
!              Transmit to ROMS field : wfrq(:,:)
               call exchange_r2d_tile(1,Lmmpi,1,Mmmpi,tab_t_tmp(START_2D_ARRAY))
               wfrq = rmask * (2*pi) / max(tab_t_tmp, eps) 
            endif
         end if
!-------------------------------------------------
!        wave_hs : HS significant wave height (m) 
         if (srcv_clname(i)=='RRM__HS'//clevel) then
            call cpl_prism_getvar(srcv_nid(i), srcv_clname(i),
     &  tab_t_tmp, rcvtime, iok_whs)
            if (iok_whs /= 0)  then
!              Transmit to ROMS field : whrm(:,:)
               call exchange_r2d_tile(1,Lmmpi,1,Mmmpi,tab_t_tmp(START_2D_ARRAY))
               whrm = rmask * tab_t_tmp * 0.70710678
            endif
         end if
!-------------------------------------------------
!        wave_cdir : cos of mean direction 
         if (srcv_clname(i)=='RRMCDIR'//clevel) then
            call cpl_prism_getvar(srcv_nid(i), srcv_clname(i),
     &  tab_t_tmp, rcvtime, iok_wcdir)
            if (iok_wcdir /= 0)  then
!              Transmit to ROMS field : wdrx(:,:)
               call exchange_r2d_tile(1,Lmmpi,1,Mmmpi,tab_t_tmp(START_2D_ARRAY))
               wdrx = rmask * tab_t_tmp
            endif
         end if
!-------------------------------------------------
!        wave_sdir : sin of mean direction
         if (srcv_clname(i)=='RRMSDIR'//clevel) then
            call cpl_prism_getvar(srcv_nid(i), srcv_clname(i),
     &  tab_t_tmp, rcvtime, iok_wsdir)
            if (iok_wsdir /= 0)  then
!              Transmit to ROMS field : wdre(:,:)
               call exchange_r2d_tile(1,Lmmpi,1,Mmmpi,tab_t_tmp(START_2D_ARRAY))
               wdre = rmask * tab_t_tmp 
            endif
         end if
!-------------------------------------------------
!        wave_bhd : wave-induced Bernoulli head pressure (bhd in N.m-1)
!        not tested/performed yet
         if (srcv_clname(i)=='RRM_BHD'//clevel) then
            call cpl_prism_getvar(srcv_nid(i), srcv_clname(i),
     &  tab_t_tmp, rcvtime, iok_wbhd)
!            if (iok_wbhd /= 0)  then
!!              Transmit to ROMS field : stflx(:,:,itemp)
!               call exchange_r2d_tile(1,Lmmpi,1,Mmmpi,tab_t_tmp(START_2D_ARRAY))
!               stflx(:,:,itemp) = tab_t_tmp / (rho0*Cp)
!            endif
         end if
!-------------------------------------------------
!        wave_ubr : rms amplitude of orbital bottom velocity of the waves (m/s)
!        not tested/performed yet
         if (srcv_clname(i)=='RRM_UBR'//clevel) then
            call cpl_prism_getvar(srcv_nid(i), srcv_clname(i),
     &  tab_t_tmp, rcvtime, iok_wubr)
!            if (iok_wubr /= 0)  then
!!              Transmit to ROMS field : stflx(:,:,itemp)
!               call exchange_r2d_tile(1,Lmmpi,1,Mmmpi,tab_t_tmp(START_2D_ARRAY))
!               stflx(:,:,itemp) = tab_t_tmp / (rho0*Cp)
!            endif
         end if
!-------------------------------------------------
!        wave_phioc : Wave-to-ocean TKE flux (phioc in W.m-2)
!        not tested/performed yet
         if (srcv_clname(i)=='RRM_FOC'//clevel) then
            call cpl_prism_getvar(srcv_nid(i), srcv_clname(i),
     &  tab_t_tmp, rcvtime, iok_wfoc)
!            if (iok_wfoc /= 0)  then
!!              Transmit to ROMS field : stflx(:,:,itemp)
!               call exchange_r2d_tile(1,Lmmpi,1,Mmmpi,tab_t_tmp(START_2D_ARRAY))
!               stflx(:,:,itemp) = tab_t_tmp / (rho0*Cp)
!            endif
         end if
!-------------------------------------------------
!        wave_wlm : mean length wave (m)
!        not tested/performed yet
         if (srcv_clname(i)=='RRM__LM'//clevel) then
            call cpl_prism_getvar(srcv_nid(i), srcv_clname(i),
     &  tab_t_tmp, rcvtime, iok_wlm)
!            if (iok_wlm /= 0)  then
!!              Transmit to ROMS field : stflx(:,:,itemp)
!               call exchange_r2d_tile(1,Lmmpi,1,Mmmpi,tab_t_tmp(START_2D_ARRAY))
!               stflx(:,:,itemp) = tab_t_tmp / (rho0*Cp)
!            endif
         end if
!-------------------------------------------------
!        wave_tauox : x-component of the wave-ocean momentum flux (tauox in m2.s-2)
!        not tested/performed yet
         if (srcv_clname(i)=='RRMTWOX'//clevel) then
            call cpl_prism_getvar(srcv_nid(i), srcv_clname(i),
     &  tab_t_tmp, rcvtime, iok_wtwox)
!            if (iok_wtwox /= 0) then
!#if (!defined OA_GRID_UV)  
!               call exchange_r2d_tile(1,Lmmpi,1,Mmmpi,tab_t_tmp(START_2D_ARRAY))
!!              Interpolate TWOX on grid T --> U
!               do jo=jmin_halo-1,jmax_halo+1
!               do io=imin_halo,imax_halo+1
!                  tab_u_tmp(io,jo)=0.5*(tab_t_tmp(io-1,jo)+tab_t_tmp(io,jo))
!               enddo
!               enddo
!!              Transmit to ROMS field : sustr
!               sustr  = tab_u_tmp / rho0
!#else
!!              Transmit to ROMS field : sustr
!               call exchange_u2d_tile (1,Lmmpi,1,Mmmpi,tab_t_tmp(START_2D_ARRAY))
!               sustr  = tab_t_tmp / rho0
!#endif   
!            endif
         end if
!-------------------------------------------------
!        wave_tauoy : y-component of the wave-ocean momentum flux (tauoy in m2.s-2)
!        not tested/performed yet
         if (srcv_clname(i)=='RRMTWOY'//clevel) then
            call cpl_prism_getvar(srcv_nid(i), srcv_clname(i),
     &  tab_t_tmp, rcvtime, iok_wtwoy)
!            if (iok_wtwoy /= 0) then
!#if (!defined OA_GRID_UV) 
!               call exchange_r2d_tile(1,Lmmpi,1,Mmmpi,tab_t_tmp(START_2D_ARRAY))
!!              Interpolate TWOY on grid T --> U
!               do jo=jmin_halo,jmax_halo+1
!               do io=imin_halo-1,imax_halo+1
!                  tab_v_tmp(io,jo)=0.5*(tab_t_tmp(io,jo-1)+tab_t_tmp(io,jo))
!               enddo
!               enddo
!!              Transmit to ROMS field : svstr
!               svstr = tab_v_tmp(:,:) / rho0
!#else
!!              Transmit to ROMS field : svstr
!               call exchange_v2d_tile (1,Lmmpi,1,Mmmpi,tab_t_tmp(START_2D_ARRAY))
!               svstr = tab_t_tmp(:,:) / rho0
!#endif 
!            endif
         end if
!-------------------------------------------------
!        wave_tauwx : Net wave-supported stress, x-component (tauwix in m2.s-2)
!        not tested/performed yet
         if (srcv_clname(i)=='RRMTWSX'//clevel) then
            call cpl_prism_getvar(srcv_nid(i), srcv_clname(i),
     &  tab_t_tmp, rcvtime, iok_wtaux)
!            if (iok_wtaux /= 0) then
!#if (!defined OA_GRID_UV)  
!               call exchange_r2d_tile(1,Lmmpi,1,Mmmpi,tab_t_tmp(START_2D_ARRAY))
!!              Interpolate TWOX on grid T --> U
!               do jo=jmin_halo-1,jmax_halo+1
!               do io=imin_halo,imax_halo+1
!                  tab_u_tmp(io,jo)=0.5*(tab_t_tmp(io-1,jo)+tab_t_tmp(io,jo))
!               enddo
!               enddo
!!              Transmit to ROMS field : sustr
!               sustr  = tab_u_tmp / rho0
!#else
!!              Transmit to ROMS field : sustr
!               call exchange_u2d_tile (1,Lmmpi,1,Mmmpi,tab_t_tmp(START_2D_ARRAY))
!               sustr  = tab_t_tmp / rho0
!#endif   
!            endif
         end if
!-------------------------------------------------
!        wave_tauwy : Net wave-supported stress, y-component (tauwiy in m2.s-2)
!        not tested/performed yet
         if (srcv_clname(i)=='RRMTWSY'//clevel) then
            call cpl_prism_getvar(srcv_nid(i), srcv_clname(i),
     &  tab_t_tmp, rcvtime, iok_wtauy)
!            if (iok_wtauy /= 0) then
!#if (!defined OA_GRID_UV) 
!               call exchange_r2d_tile(1,Lmmpi,1,Mmmpi,tab_t_tmp(START_2D_ARRAY))
!!              Interpolate TWOY on grid T --> U
!               do jo=jmin_halo,jmax_halo+1
!               do io=imin_halo-1,imax_halo+1
!                  tab_v_tmp(io,jo)=0.5*(tab_t_tmp(io,jo-1)+tab_t_tmp(io,jo))
!               enddo
!               enddo
!!              Transmit to ROMS field : svstr
!               svstr = tab_v_tmp(:,:) / rho0
!#else
!!              Transmit to ROMS field : svstr
!               call exchange_v2d_tile (1,Lmmpi,1,Mmmpi,tab_t_tmp(START_2D_ARRAY))
!               svstr = tab_t_tmp(:,:) / rho0
!#endif 
!            endif
         end if
!-------------------------------------------------
# endif
      END DO 
!-------------------------------------------------
!
# ifdef OA_COUPLING
      if  (iok_itemp /= 0) then
         stflx(:,:,itemp) = stflx(:,:,itemp) + srflx
      endif
# endif
!	
#else 
!---------------------------------------------------------------------------------------
!     Dummy subroutine                                                           NO Coupling
!---------------------------------------------------------------------------------------
#endif 
      END SUBROUTINE cpl_prism_get
      
