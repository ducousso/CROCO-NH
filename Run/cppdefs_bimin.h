! $Id: cppdefs.h 1628 2015-01-10 13:53:00Z marchesiello $
!
!======================================================================
! ROMS_AGRIF is a branch of ROMS developped at IRD and INRIA, in France
! The two other branches from UCLA (Shchepetkin et al) 
! and Rutgers University (Arango et al) are under MIT/X style license.
! ROMS_AGRIF specific routines (nesting) are under CeCILL-C license.
! 
! ROMS_AGRIF website : http://www.romsagrif.org
!======================================================================
!
/*
   This is "cppdefs.h": MODEL CONFIGURATION FILE
   ==== == ============ ===== ============= ====
*/
#define REGIONAL        /* REGIONAL Applications */


#if defined REGIONAL
/*
!====================================================================
!               REGIONAL (realistic) Configurations
!==================================================================== 
!
!----------------------
! BASIC OPTIONS
!----------------------
!
*/
                      /* Configuration Name */
# define BIMIN

# define ICE_NOFLUX

                      /* Parallelization */
# undef  OPENMP
# define  MPI
                      /* Nesting */
# undef AGRIF
# undef AGRIF_2WAY
                      /* OA Coupling via OASIS (MPI) */
# undef  OA_COUPLING
                      /* Open Boundary Conditions */
# undef  XIOS
                      /* I/O server */
# undef  TIDES
# define OBC_EAST
# define OBC_WEST
# define OBC_NORTH
# define OBC_SOUTH
                      /* Applications */
# undef  BIOLOGY
# undef  FLOATS
# undef  STATIONS
# undef  PASSIVE_TRACER
# undef  SEDIMENT
# undef  BBL
/*!
!-------------------------------------------------
! PRE-SELECTED OPTIONS
!
! ADVANCED OPTIONS ARE IN CPPDEFS_DEV.H
!-------------------------------------------------
*/
                      /* Parallelization */
# ifdef MPI
#  define  PARALLEL_FILES
# endif
# undef  AUTOTILING
# undef  ETALON_CHECK
                      /* Grid configuration */
# define CURVGRID
# define SPHERICAL
# define MASKING
# undef  WET_DRY
# define  NEW_S_COORD
                      /* Model dynamics */
# define SOLVE3D
# define NHMG
# define UV_COR
# define UV_ADV
                      /* Equation of State */
# define SALINITY
# define NONLIN_EOS
# define SPLIT_EOS
                      /* Lateral Tracer Advection (default UP3) */
# undef  TS_HADV_UP3
# define TS_HADV_RSUP3
# undef  TS_HADV_UP5
# undef  TS_HADV_C4
# undef  TS_HADV_WENO5
                      /* Lateral Explicit Momentum Mixing */
# undef  UV_VIS2
# ifdef UV_VIS2
#  define UV_VIS_SMAGO
# endif
                      /* Lateral Explicit Tracer Mixing */
# undef  TS_DIF2
# undef  TS_DIF4
# undef  TS_MIX_S
                      /* Sponge layers for UV and TS */
# define SPONGE
                      /* Semi-implicit Vertical Tracer/Mom Advection */
# define  VADV_ADAPT_IMP
                      /* Vertical Mixing */
# undef  BODYFORCE
# undef  BVF_MIXING
# define LMD_MIXING
# undef  GLS_MIXING
# ifdef LMD_MIXING
#  define LMD_SKPP
#  define LMD_BKPP
#  define LMD_RIMIX
#  define LMD_CONVEC
#  undef  LMD_DDMIX
#  define LMD_NONLOCAL
#  define MLCONVEC
# endif
# ifdef GLS_MIXING
#  define GLS_KKL
#  undef  GLS_KOMEGA
#  undef  GLS_KEPSILON
#  undef  GLS_GEN
#  undef  KANTHA_CLAYSON
#  undef  CRAIG_BANNER
#  undef  CANUTO_A
#  undef  ZOS_HSIG
# endif
                      /* Surface Forcing */
# undef  BULK_FLUX
# ifdef BULK_FLUX
#  define BULK_FAIRALL
#  define BULK_LW
#  define BULK_EP
#  define BULK_SMFLUX
#  undef  SST_SKIN
#  undef  ANA_DIURNAL_SW
#  undef  ONLINE
#  undef  ERA_ECMWF
#  undef  RELATIVE_WIND
# else
#  define QCORRECTION
#  define SFLX_CORR
#  undef ANA_DIURNAL_SW
# endif
                      /* Lateral Forcing */
# undef CLIMATOLOGY
# ifdef CLIMATOLOGY
#  define ZCLIMATOLOGY
#  define M2CLIMATOLOGY
#  define M3CLIMATOLOGY
#  define TCLIMATOLOGY
#  define ZNUDGING
#  define M2NUDGING
#  define M3NUDGING
#  define TNUDGING
#  undef  ROBUST_DIAG
# endif

# define  FRC_BRY
# ifdef FRC_BRY
#  define Z_FRC_BRY
#  define M2_FRC_BRY
#  define M3_FRC_BRY
#  ifdef NHMG
#   define W_FRC_BRY
#  endif
#  define T_FRC_BRY
# endif
                      /* Bottom Forcing */
# define ANA_BSFLUX
# define ANA_BTFLUX
                      /* Point Sources - Rivers */
# undef PSOURCE
# undef PSOURCE_NCFILE
# ifdef PSOURCE_NCFILE                    
#   define PSOURCE_NCFILE_TS
# endif
                      /* Open Boundary Conditions */
# ifdef TIDES
#  define SSH_TIDES
#  define UV_TIDES
#  undef  POT_TIDES
#  define TIDERAMP
#  define OBC_M2FLATHER
# else
#  undef  OBC_M2SPECIFIED
#  undef  OBC_M2FLATHER
#  define OBC_M2CHARACT
#  undef  OBC_M2ORLANSKI
# endif
# define OBC_M3ORLANSKI
# define OBC_TORLANSKI
# undef  OBC_M3SPECIFIED
# undef  OBC_TSPECIFIED
                      /* Input/Output & Diagnostics */
# define AVERAGES
# define AVERAGES_K
# undef  DIAGNOSTICS_TS
# undef  DIAGNOSTICS_UV
# ifdef DIAGNOSTICS_TS
#  undef DIAGNOSTICS_TS_ADV
#  define DIAGNOSTICS_TS_MLD
# endif

# define DIAGNOSTICS_VRT
# define  DIAGNOSTICS_EK
# ifdef DIAGNOSTICS_EK
#  undef DIAGNOSTICS_EK_FULL
#  define DIAGNOSTICS_EK_MLD
# endif

# undef OUTPUTS_SURFACE



/*
!           Applications:
!---------------------------------
! Biology, floats, Stations, 
! Passive tracer, Sediments, BBL
!---------------------------------
!
   Quasi-monotone lateral advection scheme (WENO5)
   for passive/biology/sediment tracers 
*/
# if defined PASSIVE_TRACER || defined BIOLOGY || defined SEDIMENT
#  define BIO_HADV_WENO5
# endif
                      /*   Choice of Biology models   */
# ifdef BIOLOGY
#  undef  PISCES
#  undef BIO_NChlPZD
#  undef  BIO_N2ChlPZD2
#  define BIO_BioEBUS
                      /*   Biology options    */
#  ifdef PISCES
#   define DIURNAL_INPUT_SRFLX
#   define key_trc_pisces
#   define key_passivetrc
#  endif
#  ifdef BIO_NChlPZD
#   define  OXYGEN
#  endif
#  ifdef BIO_BioEBUS
#   define NITROUS_OXIDE
#  endif
                      /*   Biology diagnostics    */
#  define DIAGNOSTICS_BIO
#  if defined DIAGNOSTICS_BIO && defined PISCES
#   define key_trc_diaadd
#   define key_trc_dia3d
#  endif
# endif
                      /*   Lagrangian floats model    */
# ifdef FLOATS
#  undef  FLOATS_GLOBAL_ATTRIBUTES
#  undef  IBM
#  undef  RANDOM_WALK
#  ifdef RANDOM_WALK
#   define DIEL_MIGRATION
#   define RANDOM_VERTICAL
#   define RANDOM_HORIZONTAL
#  endif
# endif
                      /*   Stations recording    */
# ifdef STATIONS
#  define ALL_SIGMA
# endif
                      /*   Sediment dynamics model     */
# ifdef SEDIMENT
#  define ANA_SEDIMENT
#  undef  BED_ARMOR
#  undef  ANA_SPFLUX
#  undef  ANA_BPFLUX
# endif
                      /*   Bottom Boundary Layer model     */
# ifdef BBL
#  define ANA_WWAVE
# endif

#endif /* END OF CONFIGURATION CHOICE */

#include "cppdefs_dev.h"
#include "set_global_definitions.h"

