! $Id: step.F 1618 2014-12-18 14:39:51Z rblod $
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
!
!====================================================================
!                         subroutine step
!====================================================================
!
#include "cppdefs.h"
      subroutine step()
      implicit none
#include "param.h"
#include "scalars.h"
#include "zoom.h"
#include "grid.h"
#include "coupling.h"
#include "ocean3d.h"
#include "ocean2d.h"
#include "mpi_roms.h"
       
#ifdef AGRIF
      IF (agrif_fixed().NE.sortedint(nbtimes)) return
      nbtimes = nbtimes + 1
#endif

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#ifdef SOLVE3D   
 
#if defined OA_COUPLING || defined OW_COUPLING
!--Get phase of OASIS3  
       if ((iif==-1).and.(oasis_time < oasis_runtime)) then 
         print *, 'oasis time before get = ', oasis_time
!         call cpl_prism_put (oasis_time) ! moved to the end of the step       
         call cpl_prism_get (oasis_time)
!         oasis_time = oasis_time + dt   ! moved to the end of the step 
       endif 
#endif /* OA_COUPLING */

       iif = iif+1
      IF (iif .EQ. 0) then
#endif      
C$OMP PARALLEL
        call prestep3D_thread()
C$OMP END PARALLEL
#ifdef SOLVE3D 
      endif       
      IF ((iif.GT.0).AND.(iif.LT.(nfast+1))) THEN
#endif
C$OMP PARALLEL
        call step2d_thread()
C$OMP END PARALLEL
#ifdef SOLVE3D
      ENDIF
      IF (iif .EQ. (nfast+1)) then
#endif      
C$OMP PARALLEL
        call step3D_uv_thread()
C$OMP END PARALLEL
#ifdef SOLVE3D
      endif
      IF (iif .EQ. (nfast + 2)) then
#endif      
C$OMP PARALLEL
        call step3D_t_thread()
C$OMP END PARALLEL
#ifdef SOLVE3D
        iif = -1
        nbstep3d = nbstep3d + 1
      endif
#if defined OA_COUPLING || defined OW_COUPLING
!--Get phase of OASIS3  
       if ((iif==-1).and.(oasis_time < oasis_runtime)) then
         print *, 'oasis time before put = ', oasis_time
         call cpl_prism_put (oasis_time)        
         oasis_time = oasis_time + dt
       endif
#endif /* OA_COUPLING */
#endif
      return
      end
!
!====================================================================
!                subroutine prestep3D_thread
!====================================================================
!
      subroutine prestep3D_thread()
#ifdef AGRIF
      use Agrif_Util
# ifdef XIOS
# include "zoom.h"
# endif
#endif
      implicit none
#include "param.h"
#include "scalars.h"
#include "ncscrum.h"
#ifdef NBQ
# include "nbq.h"
#endif
#ifdef FLOATS
# include "floats.h"
#endif
#include "ocean2d.h"
#include "ocean3d.h"
#if defined RVTK_DEBUG && defined  BULK_FLUX
# include "forces.h"
#endif
#ifdef STATIONS
# include "sta.h"
#endif
#if defined WKB_WWAVE
# include "wkb_wwave.h"
      integer iif_wave,winterp
#endif
      integer range
      integer ntrds,trd,
     &        my_first,my_last, tile, my_iif
      integer itrc

#ifdef FLOATS
       integer chunk_size_flt, Lstr,Lend, flt_str
       common /floats_step/ flt_str
#endif
#ifdef STATIONS
       integer chunk_size_sta, Mcpstr,Mcpend, sta_str
       common /sta_step/ sta_str
#endif
      integer omp_get_num_threads, omp_get_thread_num 

      ntrds=omp_get_num_threads()
      trd=omp_get_thread_num()
C$OMP BARRIER
      range=(NSUB_X*NSUB_E+ntrds-1)/ntrds
      my_first=trd*range
      my_last=min(my_first + range-1, NSUB_X*NSUB_E-1)

C$OMP MASTER
#ifdef AUTOTILING
      call begin_timing
#endif
      time=time_start+dt*float(iic-ntstart)
      tdays=time*sec2day

#ifdef SOLVE3D
      nstp=1+mod(iic-ntstart,2)
      nrhs=nstp
      nnew=3
#endif

#ifdef AGRIF
# ifdef XIOS
!      if( nbcoarse == 1 )then
        call iom_swap( "roms" )
!      endif
# endif
#endif

#ifdef FLOATS
      nfp1=MOD(nfp1+1,NFT+1)  ! Shift time indices
      nf  =MOD(nf  +1,NFT+1)  ! for floats
      nfm1=MOD(nfm1+1,NFT+1)
      nfm2=MOD(nfm2+1,NFT+1)
      nfm3=MOD(nfm3+1,NFT+1)
      flt_str=0
#endif
#ifdef STATIONS
      sta_str=0
#endif

!
! Model input block: read forcing/climatology data from netCDF files.
!------ ----- ------ ---- ------------------- ---- ---- ------ ------
!
      if (synchro_flag) then
#ifdef AGRIF
        IF (Agrif_Root()) THEN
#endif
#if defined SOLVE3D && defined TCLIMATOLOGY && !defined ANA_TCLIMA
            call get_tclima
#endif
#if defined SOLVE3D && defined M3CLIMATOLOGY && !defined ANA_M3CLIMA \
 || (defined M2CLIMATOLOGY && !defined ANA_M2CLIMA)
            call get_uclima
#endif
#if defined ZCLIMATOLOGY && !defined ANA_SSH
            call get_ssh
#endif
#ifdef FRC_BRY
# ifndef ANA_BRY
            call get_bry
#  ifdef BIOLOGY
            call get_bry_bio
#  endif
# endif
#endif
#if defined WKB_WWAVE && !defined ANA_BRY_WKB
            call get_bry_wkb
#endif
#ifdef AGRIF
        ENDIF
#endif
        call get_vbc
        synchro_flag=.false.
      endif  !<-- synchro_flag
C$OMP END MASTER
C$OMP BARRIER

      if (may_day_flag.ne.0) return  !-->  EXIT

#ifdef SOLVE3D

      do tile=my_first,my_last
# ifdef AGRIF
        IF (Agrif_Root()) THEN
# endif
# ifdef TCLIMATOLOGY
          call ana_tclima (tile)   ! analytical values are given
#  ifndef ANA_TCLIMA
          call set_tclima (tile)   ! if data are missing from clim files
#  endif
# endif
# if defined M2CLIMATOLOGY || defined M3CLIMATOLOGY
#  if defined ANA_M2CLIMA || defined ANA_M3CLIMA
          call ana_uclima (tile)
#  else
          call set_uclima (tile)
#  endif
# endif
# ifdef ZCLIMATOLOGY
#  ifdef ANA_SSH
          call ana_ssh (tile)
#  else
          call set_ssh (tile)
#  endif
# endif
# ifdef FRC_BRY
#  ifdef ANA_BRY
          call ana_bry (tile)
#  else
          call set_bry (tile)
#  endif
#  ifdef BIOLOGY
          call ana_bry_bio (tile)  ! analytical values are given
          call set_bry_bio (tile)  ! if data is missing from bry files
#  endif
# endif
# if defined WKB_WWAVE && !defined ANA_BRY_WKB
          call set_bry_wkb (tile)  
# endif

# ifdef NBQCLIMATOLOGY
          call ana_nbq_clima (tile)
# elif defined NBQ_FRC_BRY
          call ana_nbq_bry (tile)
# endif

# ifdef AGRIF
        ENDIF
# endif
# ifdef ANA_WWAVE
        call ana_wwave (tile)
# endif
# ifdef ANA_MOVING_BATHY
        call ana_hmorph (tile)
# endif
        call rho_eos (tile)
        call set_HUV (tile)
# if defined BIOLOGY
        call bio_diag (tile)
# else
        call diag(tile)
# endif
      enddo
C$OMP BARRIER
      do tile=my_first,my_last
        call set_vbc (tile)
# if defined  BBL  
#   ifdef AGRIF
        IF (Agrif_Fixed().GE.Agrif_lev_sedim) THEN
#   endif
        call bblm (tile)
#  ifdef AGRIF
        ENDIF
#  endif
# endif
# if defined SSH_TIDES || defined UV_TIDES
#  ifdef AGRIF
        IF (Agrif_Root()) call clm_tides (tile)
#  else
        call clm_tides (tile)
#  endif
# endif
      enddo
C$OMP BARRIER

# ifdef BULK_FLUX
      do tile=my_first,my_last
        call bulk_flux (tile)   
      enddo
C$OMP BARRIER
#  ifdef RVTK_DEBUG
C$OMP MASTER
      call check_tab2d(uwndg(:,:,1),'uwndg1 prestep3d','u')
      call check_tab2d(uwndg(:,:,2),'uwndg2 prestep3d','u')
      call check_tab2d(vwndg(:,:,1),'vwndg1 prestep3d','v')
      call check_tab2d(vwndg(:,:,2),'vwndg2 prestep3d','v')
      call check_tab2d(sustr(:,:),'sustr prestep3d','u')
      call check_tab2d(svstr(:,:),'sustr prestep3d','v')
      call check_tab2d(tair(:,:),'tair prestep3d','r')
      call check_tab2d(rhum(:,:),'rhum prestep3d','r')
      call check_tab2d(prate(:,:),'prate prestep3d','r')
      call check_tab2d(radlw(:,:),'radlw prestep3d','r')
      call check_tab2d(radsw(:,:),'radsw prestep3d','r')
      call check_tab2d(wspd(:,:),'wspd prestep3d','r')
      call check_tab2d(uwnd(:,:),'uwnd prestep3d','r')
      call check_tab2d(vwnd(:,:),'vwnd prestep3d','r')
C$OMP END MASTER
C$OMP BARRIER
#  endif
# endif /* BULK_FLUX */

      if (may_day_flag.ne.0) return  !-->  EXIT

# if defined WKB_WWAVE && (defined WKB_UNSTEADY || defined MRL_CEW)
      wkb_agrif_done=.FALSE.
#  ifdef MRL_CEW
      do tile=my_tile_range
        call wkb_cew_prep (tile)
      enddo
C$OMP BARRIER
C$OMP SINGLE
      wint=0
C$OMP END SINGLE
      do winterp=1,interp_max
C$OMP SINGLE
        wint=wint+1
        if (wint.gt.2) wint=1
C$OMP END SINGLE
        do tile=my_tile_range
          call wkb_uvfield (tile, winterp)
        enddo
C$OMP BARRIER
      enddo
#  endif
      do iif_wave=1,ndtfast         ! WKB ray equation w/ barotropic time stepping
C$OMP SINGLE
        wstp=wnew
        wnew=wstp+1
        if (wnew.ge.3) wnew=1
C$OMP END SINGLE

#  ifdef MRL_CEW
        do tile=my_tile_range
          call wkb_cew_finalize (tile)
        enddo
C$OMP BARRIER
#  endif
        do tile=my_tile_range
          call wkb_wwave (tile)
        enddo
C$OMP BARRIER
      enddo
#  if defined RVTK_DEBUG || defined RVTK_DEBUG_ADVANCED
C$OMP BARRIER
C$OMP MASTER
      call check_tab2d(wac(:,:,wnew),'wac in step ','r')
C$OMP END MASTER
#  endif
# endif

# ifdef MRL_WCI 
      do tile=my_tile_range
        call  mrl_wci (tile)
      enddo
C$OMP BARRIER
# endif

      do tile=my_first,my_last
# if defined ANA_VMIX
        call ana_vmix (tile)
# elif defined LMD_MIXING
        call lmd_vmix (tile)
# elif defined BVF_MIXING
        call bvf_mix (tile)
# endif

# ifdef NBQ_CONS2
        call cons_nbq(300)
# endif
# ifdef NBQ_CONS3
        call cons_nbq(100)
# endif

        call omega (tile)

# ifdef NBQ_CONSOUT
        call consout_nbq(200)
# endif

# ifdef VIS_COEF_3D
        call hvisc_coef (tile)
# endif
      enddo
C$OMP BARRIER

      do tile=my_first,my_last
        call prsgrd (tile)
        call rhs3d (tile)
# ifdef NBQ
        call rhs3d_w_nh (tile)
# endif
        call pre_step3d (tile)
# ifdef GLS_MIXING
        call gls_prestep (tile)
# endif
# ifdef AGRIF
        if (.Not.Agrif_Root()) then
          call uv3dpremix (tile)
        endif
# endif
      enddo
C$OMP BARRIER

      do tile=my_first,my_last
# if defined UV_VIS2 || defined UV_VIS4 
        call uv3dmix (tile)
# endif
# if defined SPONGE_VIS2 && !defined UV_VIS2
        call uv3dmix_spg (tile)
# endif
# ifdef SST_SKIN
        call sstskin (tile)
# endif
# ifdef AVERAGES
        call set_avg (tile)
#  if defined DIAGNOSTICS_TS
        call set_diags_avg (tile)
#  endif
#  if defined DIAGNOSTICS_UV
        call set_diagsM_avg (tile)
#  endif
#  ifdef DIAGNOSTICS_BIO
        call set_bio_diags_avg (tile)
#  endif
# endif
      enddo

C$OMP BARRIER
C$OMP MASTER

#ifdef RVTK_DEBUG
      do itrc=1,NT
      call check_tab3d(t(:,:,:,nnew,itrc),'t prestep3d','r')
      enddo
      call check_tab3d(u(:,:,:,nnew),'u prestep3d','u')
      call check_tab3d(v(:,:,:,nnew),'v prestep3d','v')
#endif

      nrhs=3
      nnew=3-nstp
!
! Output block: write restart/history files.   
!
      call output

C$OMP END MASTER
C$OMP BARRIER
#endif  /* SOLVE3D */

      if (may_day_flag.ne.0) return  !-->  EXIT


      return
      end
!
!====================================================================
!                   subroutine step2D_thread
!====================================================================
!
      subroutine step2D_thread()

#ifdef AGRIF
      use Agrif_Util
#endif
      implicit none
#include "param.h"
#include "scalars.h"
#include "ncscrum.h"
#include "ocean2d.h"
#ifdef FLOATS
# include "floats.h"
#endif
#ifdef STATIONS
# include "sta.h"
#endif
       integer range
      integer ntrds,trd,
     &        my_first,my_last, tile, my_iif

#ifdef FLOATS
       integer chunk_size_flt, Lstr,Lend, flt_str
       common /floats_step/ flt_str
#endif
#ifdef STATIONS
       integer chunk_size_sta, Mcpstr,Mcpend, sta_str
       common /sta_step/ sta_str
#endif
      integer omp_get_num_threads, omp_get_thread_num 

      ntrds=omp_get_num_threads()
      trd=omp_get_thread_num()
C$OMP BARRIER
      range=(NSUB_X*NSUB_E+ntrds-1)/ntrds
      my_first=trd*range
      my_last=min(my_first + range-1, NSUB_X*NSUB_E-1)

!
! Solve the 2D primitive equations for the barotropic mode.
!-------------------------------------------------------------
! Note that no actual fast-time-step is performed during the
! auxiliary (nfast+1)th step. It is needed to finalize the fast-time
! averaging procedure, if any, and to compute the total depth of
! water column, as well as the new vertical coordinate transformation
! profiles, z_r, z_w, because the free surface elevation has been
! changed. All these operations are done during predictor time step.
! Thus, there is no need for the corrector step during the auxiliary
! time step.
!

#ifndef SOLVE3D
# if defined WKB_WWAVE && defined MRL_CEW
        wstp=wnew
        wnew=wstp+1
        if (wnew.ge.3) wnew=1
        do tile=my_tile_range
          call wkb_wwave (tile)
        enddo
C$OMP BARRIER
# endif
# ifdef MRL_WCI
        do tile=my_tile_range
          call mrl_wci (tile)
        enddo
C$OMP BARRIER
# endif
#endif

C$OMP MASTER
        kstp=knew                   ! This might look a bit silly,
        knew=kstp+1                 ! since both branches of this
        if (knew.gt.4) knew=1       ! if statement are identical.
C$OMP END MASTER
C$OMP BARRIER
!
! Model input block for 2D configurations only !!!
!
#ifndef SOLVE3D
# ifdef AGRIF
        IF (Agrif_Root()) THEN
# endif
          do tile=my_first,my_last
# if defined M2CLIMATOLOGY
#  if defined ANA_M2CLIMA
            call ana_uclima (tile)
#  else
            call set_uclima (tile)
#  endif
# endif
# ifdef ZCLIMATOLOGY
#  ifdef ANA_SSH
            call ana_ssh (tile)
#  else
            call set_ssh (tile)
#  endif
# endif
# if  defined Z_FRC_BRY  || defined M2_FRC_BRY
#  ifdef ANA_BRY
            call ana_bry (tile)
#  else
            call set_bry (tile)
#  endif
# endif
          enddo
# ifdef AGRIF
        ENDIF
# endif
C$OMP BARRIER
        do tile=my_first,my_last
          call set_vbc (tile)
# ifdef BBL
          call bblm (tile)
# endif
# if defined SSH_TIDES || defined UV_TIDES
#   ifdef AGRIF
          IF (Agrif_Root()) call clm_tides (tile)
#   else
          call clm_tides (tile)
#   endif
# endif
# ifdef AVERAGES
          call set_avg (tile)
# endif
# if defined UV_VIS2 && defined UV_VIS_SMAGO

        call hvisc_coef (tile)
# endif
        enddo
C$OMP BARRIER
C$OMP MASTER
        call output
C$OMP END MASTER
C$OMP BARRIER

#endif  /* !defined SOLVE3D */
        
                  ! This might look a bit silly,
                  ! since both branches of this
                  ! if statement are identical.
                  ! Nevertheless, it makes sense,
                  ! since mpc will reverse one of
                  ! these loops to make zig-zag
                  ! tile-processing sequence.               
        if (mod(knew,2).eq.0) then
          do tile=my_first,my_last
            call     step2d (tile)
          enddo
        else
          do tile=my_first,my_last
            call     step2d (tile)
          enddo
        endif

#if defined AGRIF && defined AGRIF_2WAY
        if (.not.agrif_root()) then
            call update2d()
        endif
#endif

#ifdef RVTK_DEBUG
C$OMP BARRIER
C$OMP MASTER
       call check_tab2d(zeta(:,:,knew),'zeta step2d','r')
       call check_tab2d(ubar(:,:,knew),'ubar step2d','u')
       call check_tab2d(vbar(:,:,knew),'vbar step2d','v')
C$OMP END MASTER       
#endif

      return
      end
!
!====================================================================
!                  subroutine step3D_uv_thread
!====================================================================
!      
      subroutine step3D_uv_thread()
#ifdef AGRIF
      use Agrif_Util
#endif
      implicit none
#include "param.h"
#include "scalars.h"
#include "ncscrum.h"
#ifdef NBQ
# include "nbq.h"
#endif
#ifdef FLOATS
# include "floats.h"
#endif
#ifdef STATIONS
# include "sta.h"
#endif
#include "zoom.h"
#include "ocean3d.h"
#include "ocean2d.h"
#include "coupling.h"
#ifdef AUTOTILING
#include "autotiling.h"
#endif
       integer range
      integer ntrds,trd,
     &        my_first,my_last, tile, my_iif
      integer i,j,k

#ifdef FLOATS
       integer chunk_size_flt, Lstr,Lend, flt_str
       common /floats_step/ flt_str
#endif
#ifdef STATIONS
       integer chunk_size_sta, Mcpstr,Mcpend, sta_str
       common /sta_step/ sta_str
#endif
      integer omp_get_num_threads, omp_get_thread_num 
      integer itrc
      
      real t1,t2,t3

      ntrds=omp_get_num_threads()
      trd=omp_get_thread_num()
C$OMP BARRIER
      range=(NSUB_X*NSUB_E+ntrds-1)/ntrds
      my_first=trd*range
      my_last=min(my_first + range-1, NSUB_X*NSUB_E-1)

C$OMP BARRIER

#ifdef SOLVE3D
      do tile=my_first,my_last
        call set_depth (tile)
      enddo

C$OMP BARRIER

      do tile=my_first,my_last
        call set_HUV2 (tile)
      enddo
C$OMP BARRIER

      do tile=my_first,my_last

# ifdef NBQ_CONS2
        call cons_nbq(301)
# endif
# ifdef NBQ_CONS3
        call cons_nbq(101)
# endif

        call omega (tile)

# ifdef NBQ_CONSOUT
        call consout_nbq(201)
# endif
        call rho_eos (tile)

      enddo

C$OMP BARRIER

      do tile=my_first,my_last
        call prsgrd (tile)
        call rhs3d (tile)
# ifdef NBQ
        call rhs3d_w_nh (tile)
# endif
        call step3d_uv1 (tile)
# ifdef NBQ
        call step3d_w (tile)
# endif
      enddo
C$OMP BARRIER

      do tile=my_first,my_last
        call step3d_uv2 (tile)
# ifdef DIF_COEF_3D 
        call hdiff_coef (tile)
# endif
      enddo
C$OMP BARRIER

#endif /* SOLVE3D */
!
#if defined AGRIF && defined AGRIF_2WAY
!
! Update the outer domain after the last child step
! in case of 2-way nesting.
!
C$OMP BARRIER 
C$OMP MASTER
      if (.Not.Agrif_Root()) then
        call Agrif_update_uv_np1
      endif
C$OMP END MASTER
#endif /*AGRIF && AGRIF_2WAY*/

      return
      end
      
!
!====================================================================
!                  subroutine step3D_t_thread
!====================================================================
!      
      subroutine step3D_t_thread()
#ifdef AGRIF
      use Agrif_Util
#endif
      implicit none
#include "param.h"
#include "scalars.h"
#include "ncscrum.h"
#ifdef NBQ
# include "nbq.h"
#endif
#ifdef FLOATS
# include "floats.h"
#endif
#ifdef STATIONS
# include "sta.h"
#endif
#include "zoom.h"
#include "ocean3d.h"
#include "ocean2d.h"
#include "coupling.h"
#include "grid.h"
#ifdef AUTOTILING
#include "autotiling.h"
#endif
       integer range
      integer ntrds,trd,
     &        my_first,my_last, tile, my_iif
      integer i,j,k

#ifdef FLOATS
       integer chunk_size_flt, Lstr,Lend, flt_str
       common /floats_step/ flt_str
#endif
#ifdef STATIONS
       integer chunk_size_sta, Mcpstr,Mcpend, sta_str
       common /sta_step/ sta_str
#endif
      integer omp_get_num_threads, omp_get_thread_num 
      integer itrc
      
      real t1,t2,t3

      ntrds=omp_get_num_threads()
      trd=omp_get_thread_num()
C$OMP BARRIER
      range=(NSUB_X*NSUB_E+ntrds-1)/ntrds
      my_first=trd*range
      my_last=min(my_first + range-1, NSUB_X*NSUB_E-1)

C$OMP BARRIER

#ifdef SOLVE3D

      do tile=my_first,my_last

# ifdef NBQ_CONS2
        call cons_nbq(302)
# endif
# ifdef NBQ_CONS3
        call cons_nbq(102)
# endif

        call omega (tile)

# ifdef NBQ_CONSOUT
        call consout_nbq(202)
# endif

# ifdef GLS_MIXING
        call step3d_gls (tile)
# endif
        call step3d_t (tile)
# ifdef SEDIMENT
#  ifdef AGRIF
        IF (Agrif_Fixed().GE.Agrif_lev_sedim)
     &  call sediment (tile)
#  else
        call sediment (tile)
#  endif
# endif
# ifdef AGRIF
        if (.Not.Agrif_Root()) then
          call t3dpremix (tile)
        endif
# endif
      enddo
C$OMP BARRIER

      do tile=my_first,my_last
# if defined TS_DIF2 || defined TS_DIF4 
        call t3dmix (tile)
# endif
# if defined SPONGE_DIF2
        call t3dmix_spg (tile)
# endif
      enddo
C$OMP BARRIER

#endif /* SOLVE3D */

#ifdef FLOATS
      chunk_size_flt=32
      do while (flt_str.lt.nfloats) 
C$OMP CRITICAL
        Lstr=flt_str+1
        flt_str=Lstr+chunk_size_flt-1
C$OMP END CRITICAL
        Lend=min(Lstr+chunk_size_flt-1,nfloats)
        call step_floats (Lstr,Lend)
      enddo
c        call step_floats (1,nfloats) ! serial version for debugging
#endif /*FLOATS*/

#ifdef STATIONS
      chunk_size_sta=32
      do while (sta_str.lt.nstas)
C$OMP CRITICAL
        Mcpstr=sta_str+1
        sta_str=Mcpstr+chunk_size_sta-1
C$OMP END CRITICAL
        Mcpend=min(Mcpstr+chunk_size_sta-1,nstas)
        call step_sta (Mcpstr,Mcpend)
      enddo
#endif /*STATIONS*/
!
#if defined AGRIF && defined AGRIF_2WAY
!
! Update the outer domain after the last child step
! in case of 2-way nesting.
!
C$OMP BARRIER 
C$OMP MASTER
      if ((.Not.Agrif_Root()).and.
     &    (nbcoarse == Agrif_Irhot())) then
        call Agrif_update_np1
      endif
C$OMP END MASTER
#endif /*AGRIF && AGRIF_2WAY*/

C$OMP MASTER
#ifdef AUTOTILING
      call end_timing
      if (iic-lastiic == nbsampling) call auto_tiling
#endif

      iic=iic + 1

#ifdef AGRIF      
      nbcoarse = 1 + mod(nbcoarse, Agrif_IRhot())
#endif      
C$OMP END MASTER
C$OMP BARRIER

#if defined RVTK_DEBUG && defined SOLVE3D
C$OMP BARRIER
C$OMP MASTER
      do itrc=1,NT
      call check_tab3d(t(:,:,:,nnew,itrc),'t step3d','r')
      enddo
      call check_tab3d(u(:,:,:,nnew),'u step3d','u')
      call check_tab3d(v(:,:,:,nnew),'v step3d','v')    
C$OMP END MASTER      
#endif

      return
      end      
