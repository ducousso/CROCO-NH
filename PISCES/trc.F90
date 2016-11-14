#include "cppdefs.h"

MODULE trc
   !!======================================================================
   !!                      ***  MODULE  trc  ***
   !! Passive tracers   :  module for tracers defined
   !!======================================================================
   !! History :    -   !  1996-01  (M. Levy)  Original code
   !!              -   !  1999-07  (M. Levy)  for LOBSTER1 or NPZD model
   !!              -   !  2000-04  (O. Aumont, M.A. Foujols)  HAMOCC3 and P3ZD
   !!             1.0  !  2004-03  (C. Ethe)  Free form and module
   !!----------------------------------------------------------------------
   !! NEMO/TOP 1.0 , LOCEAN-IPSL (2005) 
   !! $Id: trc.F90 1542 2009-07-27 11:57:49Z cetlod $ 
   !! Software governed by the CeCILL licence (modipsl/doc/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
#if defined key_top || defined key_pisces
   !!----------------------------------------------------------------------
   !!   'key_top'                                                TOP models
   !!----------------------------------------------------------------------
   USE par_pisces
   USE ocean2pisces
   
   IMPLICIT NONE
   PUBLIC

   PUBLIC   trc_alloc   ! called by nemogcm.F90

#include "ocean2pisces.h90"

   !! information for outputs
   !! --------------------------------------------------
   TYPE, PUBLIC :: PTRACER                                                            !: Passive tracer type
       CHARACTER(len = 20)  :: clsname  !: short name
       CHARACTER(len = 80)  :: cllname  !: long name
       CHARACTER(len = 20)  :: clunit   !: unit
   END TYPE PTRACER
# ifndef AGRIF
   CHARACTER(len = 20), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:)    ::  ctrcnm         !: tracer name 
   CHARACTER(len = 80), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:)    ::  ctrcnl         !: trccer field long name
   CHARACTER(len = 20), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:)    ::  ctrcnu         !: tracer unit
# else
   CHARACTER(len = *), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:)    ::  ctrcnm         !: tracer name 
   CHARACTER(len = *), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:)    ::  ctrcnl         !: trccer field long name
   CHARACTER(len = *), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:)    ::  ctrcnu         !: tracer unit
# endif

   TYPE, PUBLIC :: DIAG                                                               !: passive trcacer ddditional diagnostic type
      CHARACTER(len = 20)  :: sname    !: short name
      CHARACTER(len = 80)  :: lname    !: long name
      CHARACTER(len = 20)  :: units    !: unit
   END TYPE DIAG

   
   !! passive tracers fields (before,now,after)
   !! --------------------------------------------------
!   REAL(wp), PUBLIC ::   trai                         !: initial total tracer

   REAL(wp), PUBLIC, DIMENSION(:,:,:,:), SAVE, ALLOCATABLE ::   tra   !: traceur concentration for next time step

   
   !! passive tracers restart (input and output)
   !! ------------------------------------------  
   INTEGER , PUBLIC  ::  ndttrc     !: frequency of step on passive tracers
   INTEGER , PUBLIC  ::  nittrc000  !: first time step of passive tracers model
   LOGICAL , PUBLIC  ::  ln_rsttr     !: boolean term for restart i/o for passive tracers (namelist)
   LOGICAL , PUBLIC  ::  lrst_trc   !: logical to control the trc restart write
   INTEGER , PUBLIC  ::  nutwrs     !: output FILE for passive tracers restart
   INTEGER , PUBLIC  ::  nutrst     !: logical unit for restart FILE for passive tracers
   INTEGER , PUBLIC  ::  nrsttr     !: control of the time step ( 0 or 1 ) for pass. tr.
   CHARACTER(len=50) ::  cn_trcrst_in  !: suffix of pass. tracer restart name (input)
   CHARACTER(len=50) ::  cn_trcrst_out !: suffix of pass. tracer restart name (output)
   
   !! information for outputs
   !! --------------------------------------------------
   INTEGER , PUBLIC ::   nwritetrc   !: time step frequency for concentration outputs (namelist)
   
# if defined key_trc_diaadd && ! defined key_iomput

   !! additional 2D/3D outputs namelist
   !! --------------------------------------------------
   REAL(wp)           , PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,  :) ::   trc2d         !: additional 2d outputs array 
   REAL(wp)           , PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:,:) ::   trc3d         !: additional 3d outputs array 
   CHARACTER(len = 20), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:)       ::   ctrc2d        !: 2d field short name
   CHARACTER(len = 80), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:)       ::   ctrc2l        !: 2d field long name
   CHARACTER(len = 20), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:)       ::   ctrc2u        !: 2d field unit
   CHARACTER(len = 20), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:)       ::   ctrc3d        !: 3d field short name
   CHARACTER(len = 80), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:)       ::   ctrc3l        !: 3d field long name
   CHARACTER(len = 20), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:)       ::   ctrc3u        !: 3d field unit
   LOGICAL            , PUBLIC                                        ::  ln_diatrc      !: boolean term for additional diagnostic
   INTEGER            , PUBLIC                                        ::  nn_writedia    !: frequency of additional outputs
#endif
   
   !! passive tracers data read and at given time_step
   !! --------------------------------------------------
# if defined key_dtatrc
   INTEGER , PUBLIC, DIMENSION(jptra) ::   numtr   !: logical unit for passive tracers data
# endif
   !!----------------------------------------------------------------------
   !! NEMO/TOP 3.3.1 , NEMO Consortium (2010)
   !! $Id: trc.F90 5385 2015-06-09 13:50:42Z cetlod $
   !! Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
CONTAINS

   INTEGER FUNCTION trc_alloc()
      !!-------------------------------------------------------------------
      !!                    *** ROUTINE trc_alloc ***
      !!-------------------------------------------------------------------
      !
      ALLOCATE( tra(PRIV_3D_BIOARRAY, jptra), STAT = trc_alloc  )  

      IF( trc_alloc /= 0 )   CALL ctl_warn('trc_alloc: failed to allocate arrays')
      !
   END FUNCTION trc_alloc

#else
   !!----------------------------------------------------------------------
   !!  Empty module :                                     No passive tracer
   !!----------------------------------------------------------------------
#endif

   !!======================================================================
END MODULE trc
