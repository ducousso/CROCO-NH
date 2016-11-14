! $Id: climat.h 1458 2014-02-03 15:01:25Z gcambon $
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
/*  This is include file "climat.h"
-----------------------------------
 Free surface climatology:
 ==== ======= ============
   ssh        sea surface height climatology at current time-step.
   Znudgcof   inverse relaxation time [1/sec] for nudging toward
                               free surface climatological fields.
   sshg       two-time-level array to hold climatological data for 
                                                     free surface.
   tssh       time of read in sea surface height climatology.
*/
#if defined ZCLIMATOLOGY || defined AGRIF
      real ssh(GLOBAL_2D_ARRAY)
      common /climat_ssh/ssh
#endif
#ifdef ZCLIMATOLOGY
# ifdef ZNUDGING
      real Znudgcof(GLOBAL_2D_ARRAY)
      common /climat_Znudgcof/Znudgcof
# endif
# ifndef ANA_SSH
      real sshg(GLOBAL_2D_ARRAY,2)
      common /climat_sshg/sshg

      real    ssh_time(2)
      real    ssh_cycle
      integer itssh, ssh_ncycle, ssh_rec, ssh_tid, ssh_id 
      common /climat_zdat1/ ssh_time
      common /climat_zdat2/ ssh_cycle
      common /climat_zdat3/ 
     &        itssh, ssh_ncycle, ssh_rec, ssh_tid, ssh_id

#   undef SSH_DATA
# endif /* !ANA_SSH */
#endif

/*
 Temperature and salinity climatology:
 =========== === ======== ============
   tclm       climatology for tracer variables at current time-step.
   Tnudgcof   inverse relaxation time [1/sec] for nudging toward
                                       tracer climatological fields.
   tclima     two-time-level array to hold climatological data for
                                               tracer variables.
   ttclm      time of read in climatology for tracer type variables.
*/
#ifdef SOLVE3D
# if defined TCLIMATOLOGY || (defined AGRIF && !defined T_FRC_BRY)
      real tclm(GLOBAL_2D_ARRAY,N,NT)
      common /climat_tclm/tclm
# endif
# ifdef TCLIMATOLOGY
#  ifdef TNUDGING
      real Tnudgcof(GLOBAL_2D_ARRAY,N,NT)
      common /climat_Tnudgcof/Tnudgcof
#  endif
#  ifndef ANA_TCLIMA
      real tclima(GLOBAL_2D_ARRAY,N,2,NT)
      common /climat_tclima/tclima

      real tclm_time(2,NT)
      real tclm_cycle(NT)
      integer ittclm(NT), tclm_ncycle(NT), tclm_rec(NT), 
     &        tclm_tid(NT), tclm_id(NT)
      logical got_tclm(NT)

      common /climat_tdat/  tclm_time,       tclm_cycle,
     &        ittclm,       tclm_ncycle,     tclm_rec,
     &                      tclm_tid,        tclm_id,
     &                                       got_tclm

#   undef TCLIMA_DATA
#  endif /* !ANA_TCLIMA */
# endif /* TCLIMATOLOGY */
#endif /* SOLVE3D */
/*
 barotropic and baroclinic velocity climatology:
 ========== === ========== ======== ===========
   ubclm     climatology for bar. u-velocity at current time-step.
   vbclm     climatology for bar. v-velocity at current time-step.
   uclm      climatology for u-velocity at current time-step.
   vclm      climatology for v-velocity at current time-step.

   ubclima   two-time-level array to hold climatological data
   vbclima
   uclima
   vclima
*/
#if defined M2CLIMATOLOGY || (defined AGRIF && !defined M2_FRC_BRY)
      real ubclm(GLOBAL_2D_ARRAY)
      real vbclm(GLOBAL_2D_ARRAY)
      common /climat_ubclm/ubclm /climat_vbclm/vbclm 
#endif
#if defined SOLVE3D && (defined M3CLIMATOLOGY || \
                        (defined AGRIF && !defined M3_FRC_BRY))
      real uclm(GLOBAL_2D_ARRAY,N)
      real vclm(GLOBAL_2D_ARRAY,N)
      common /climat_uclm/uclm /climat_vclm/vclm
#endif
#ifdef M2CLIMATOLOGY
# ifdef M2NUDGING
      real M2nudgcof(GLOBAL_2D_ARRAY)
      common /climat_M2nudgcof/M2nudgcof
# endif
# ifndef ANA_M2CLIMA
      real ubclima(GLOBAL_2D_ARRAY,2)
      real vbclima(GLOBAL_2D_ARRAY,2)
      common /climat_ubclima/ubclima /climat_vbclima/vbclima
# endif
#endif
!
#if defined SOLVE3D && defined M3CLIMATOLOGY
#   ifdef M3NUDGING
      real M3nudgcof(GLOBAL_2D_ARRAY)
      common /climat_M3nudgcof/M3nudgcof
#   endif
#   ifndef ANA_M3CLIMA
      real uclima(GLOBAL_2D_ARRAY,N,2)
      real vclima(GLOBAL_2D_ARRAY,N,2)
      common /climat_uclima/uclima /climat_vclima/vclima
#   endif
#endif
!
#if defined M2CLIMATOLOGY || defined M3CLIMATOLOGY
      real     uclm_time(2)
      real     uclm_cycle
      integer ituclm, uclm_ncycle, uclm_rec, uclm_tid,
     &        ubclm_id, vbclm_id, uclm_id, vclm_id
      common /climat_udat1/  uclm_time
      common /climat_udat2/  uclm_cycle
      common /climat_udat3/
     &             ituclm,   uclm_ncycle, uclm_rec,
     &             uclm_tid, ubclm_id,    vbclm_id,
     &             uclm_id,  vclm_id
!
#endif

#ifdef ZONAL_NUDGING
# define GLOBAL_1D_ETA 0:Mm+1
      real zetazon(GLOBAL_1D_ETA), 
     &     ubzon(GLOBAL_1D_ETA), 
     &     vbzon(GLOBAL_1D_ETA),
     &     uzon(GLOBAL_1D_ETA,N), 
     &     vzon(GLOBAL_1D_ETA,N), 
     &     tzon(GLOBAL_1D_ETA,N,NT)
      common /climat_zetazon/zetazon
      common /climat_ubzon/ubzon
      common /climat_vbzon/vbzon
      common /climat_uzon/uzon
      common /climat_vzon/vzon
      common /climat_tzon/tzon
      real sshzon(GLOBAL_1D_ETA), 
     &     ubclmzon(GLOBAL_1D_ETA), 
     &     vbclmzon(GLOBAL_1D_ETA),
     &     uclmzon(GLOBAL_1D_ETA,N), 
     &     vclmzon(GLOBAL_1D_ETA,N), 
     &     tclmzon(GLOBAL_1D_ETA,N,NT)
      common /climat_sshzon/sshzon
      common /climat_ubclmzon/ubclmzon
      common /climat_vbclmzon/vbclmzon
      common /climat_uclmzon/uclmzon
      common /climat_vclmzon/vclmzon
      common /climat_tclmzon/tclmzon
# undef GLOBAL_1D_ETA
#endif

#if defined NBQ && (defined NBQCLIMATOLOGY || \
                   (defined AGRIF && !defined NBQ_FRC_BRY))
      real unbqclm(GLOBAL_2D_ARRAY,N)
      real vnbqclm(GLOBAL_2D_ARRAY,N)
      real wnbqclm(GLOBAL_2D_ARRAY,N)
      real rnbqclm(GLOBAL_2D_ARRAY,N)
      common /climat_unbqclm/unbqclm 
      common /climat_vnbqclm/vnbqclm
      common /climat_wnbqclm/wnbqclm 
      common /climat_rnbqclm/rnbqclm
#endif


