! $Id: cppdefs.h 1628 2015-01-10 13:53:00Z marchesiello $
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
/*
   This is "cppdefs.h": MODEL CONFIGURATION FILE
   ==== == ============ ===== ============= ====
*/
#undef  BASIN           /* Basin Example */
#undef  CANYON_A        /* Canyon_A Example */
#undef  CANYON_B        /* Canyon_B Example */
#undef  EQUATOR         /* Equator Example  */
#undef  ACOUS           /* Acoustic wave test case */
#undef  GRAV_ADJ        /* Graviational Adjustment Example */
#undef  KH_INST         /* Kelvin-Helmholtz Instability Example */
#undef  INNERSHELF      /* Inner Shelf Example */
#undef  RIVER           /* River run-off Example */
#undef  OVERFLOW        /* Graviational/Overflow Example */
#undef  SEAMOUNT        /* Seamount Example */
#undef  SHELFRONT       /* Shelf Front Example */
#undef  SOLITON         /* Equatorial Rossby Wave Example */
#undef  UPWELLING       /* Upwelling Example */
#undef  VORTEX          /* Baroclinic Vortex Example */
#undef  INTERNAL        /* Internal Tide Example */
#undef  IGW             /* COMODO Internal Tide Example */
#undef  JET             /* Baroclinic Jet Example */
#undef  RIP             /* Rip Current Test Case */
#undef  SHOREFACE       /* Shoreface Test Case on a Planar Beach */
#undef  SWASH           /* Swash Test Case on a Planar Beach */
#undef  THACKER         /* Thacker wetting-drying Example */
#undef  TANK            /* Tank Example */
#undef  S2DV            /* 2D Vertical Section Application */
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
# define BENGUELA_LR
                      /* Parallelization */
# undef  OPENMP
# undef  MPI
                      /* Nesting */
# undef  AGRIF
# undef  AGRIF_2WAY
                      /* OA and OW Coupling via OASIS (MPI) */
# undef  OA_COUPLING
# undef  OW_COUPLING
                      /* I/O server */
# undef  XIOS
                      /* Open Boundary Conditions */
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
#  undef  PARALLEL_FILES
# endif
# undef  AUTOTILING
# undef  ETALON_CHECK
                      /* Grid configuration */
# define CURVGRID
# define SPHERICAL
# define MASKING
# undef  WET_DRY
# undef  NEW_S_COORD
                      /* Model dynamics */
# define SOLVE3D
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
# undef  VADV_ADAPT_IMP
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
#  define ANA_DIURNAL_SW
# endif
                      /* Lateral Forcing */
# define CLIMATOLOGY
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

# undef  FRC_BRY
# ifdef FRC_BRY
#  define Z_FRC_BRY
#  define M2_FRC_BRY
#  define M3_FRC_BRY
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
#   undef  DIURNAL_INPUT_SRFLX
#   define key_pisces
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
#   define key_iomput
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
/*
!
!==========================================================
!              IDEALIZED CONFIGURATIONS
!==========================================================
!
*/
#elif defined BASIN
/*
!                       Basin Example
!                       ===== =======
*/
# undef OPENMP
# undef MPI
# define UV_ADV
# define UV_COR
# define UV_VIS2
# define SOLVE3D
# define TS_DIF2
# define BODYFORCE
# define ANA_GRID
# define ANA_INITIAL
# define ANA_SMFLUX
# define ANA_STFLUX
# define ANA_BTFLUX

#elif defined CANYON_A
/*
!                       First Canyon Example
!                       ===== ====== =======
*/
# undef OPENMP
# undef MPI
# define UV_ADV
# define UV_COR
# define SOLVE3D
# define EW_PERIODIC
# define ANA_GRID
# define ANA_INITIAL
# define ANA_SMFLUX
# define ANA_STFLUX
# define ANA_BTFLUX

#elif defined CANYON_B
/*
!                       Second Canyon Example
!                       ====== ====== =======
*/
# undef OPENMP
# undef MPI
# define UV_ADV
# define UV_COR
# define SOLVE3D
# define EW_PERIODIC
# define ANA_GRID
# define ANA_INITIAL
# define ANA_SMFLUX
# define ANA_STFLUX
# define ANA_BTFLUX
# define ANA_VMIX

#elif defined EQUATOR
/*
!                       Equator Example
!                       ======= =======
! Boccaletti, G., R.C. Pacanowski, G.H. Philander and A.V. Fedorov, 2004,
! The Thermal Structure of the Upper Ocean, J.Phys.Oceanogr., 34, 888-902.
*/
# undef OPENMP
# undef MPI
# define UV_ADV
# define UV_COR
# define UV_VIS2
# define SOLVE3D
# define SALINITY
# define TS_DIF2
# define ANA_GRID
# define ANA_INITIAL
# define ANA_SMFLUX
# define ANA_STFLUX
# define ANA_SRFLUX
# define ANA_SSFLUX
# define ANA_BTFLUX
# define ANA_BSFLUX
# define QCORRECTION
# define ANA_SST
# define LMD_SKPP /* problem with MPI in Xi direction */
# define LMD_MIXING
# define LMD_RIMIX
# define LMD_CONVEC

#elif defined KH_INST 
/*
!                       Kelvin-Helmholtz Instability Example
!                       ================ =========== =======
!
*/
# undef  OPENMP
# define MPI
# define NBQ
# ifdef NBQ
#  undef  NBQ_IMP
# endif
# define UV_VIS2
# define UV_MIX_S
# define UV_VIS_SMAGO
# define SOLVE3D
# define NEW_S_COORD
# define UV_ADV
# define TS_HADV_WENO5
# define TS_VADV_WENO5
# define ANA_GRID
# define ANA_INITIAL
# define ANA_SMFLUX
# define ANA_STFLUX
# define ANA_BTFLUX
# define EW_PERIODIC

#elif defined ACOUS 
/*
!                       ACOUSTIC WAVE TESTCASE 
!                       ======================
*/
# undef  OPENMP
# define MPI
# define NBQ
# ifdef NBQ
#  define NBQ_IMP
# else
# endif
# undef  UV_VIS2
# define SOLVE3D
# define NEW_S_COORD
# undef  UV_ADV
# undef  TS_HADV_WENO5
# undef  TS_VADV_WENO5
# define ANA_GRID
# define ANA_INITIAL
# define ANA_SMFLUX
# define ANA_STFLUX
# define ANA_BTFLUX

#elif defined GRAV_ADJ
/*
!                       Gravitational Adjustment Example
!                       ============= ========== =======
!
!  Soliton case GRAV_ADJ_SOLITON (non-hydro test) is setup from:
!  Horn, D.A., J. Imberger, & G.N. Ivey, (2001). 
!  The degeneration of large-scale interfacial gravity waves in lakes. 
!  J. Fluid Mech., 434:181-207. 
!
*/
# undef  OPENMP
# define MPI
# define NBQ
# ifdef NBQ
#  define GRAV_ADJ_SOLITON
#  define NBQ_IMP
# else
#  define UV_VIS2
# endif
# define SOLVE3D
# define NEW_S_COORD
# define UV_ADV
# define TS_HADV_WENO5
# define TS_VADV_WENO5
# define ANA_GRID
# define ANA_INITIAL
# define ANA_SMFLUX
# define ANA_STFLUX
# define ANA_BTFLUX

#elif defined INNERSHELF
/*
!                       Inner Shelf Example
!                       ===== ===== =======
*/
# undef  OPENMP
# undef  MPI
# define INNERSHELF_EKMAN
# define INNERSHELF_APG
# define SOLVE3D
# define UV_COR
# define ANA_GRID
# define ANA_INITIAL
# define AVERAGES
# define ANA_SSFLUX
# define ANA_SRFLUX
# define ANA_STFLUX
# define ANA_BSFLUX
# define ANA_BTFLUX
# define ANA_SMFLUX
# define NS_PERIODIC
# define OBC_WEST
# define SPONGE
# ifndef INNERSHELF_EKMAN
#  define UV_ADV
#  define SALINITY
#  define NONLIN_EOS
#  define SPLIT_EOS
#  define LMD_MIXING
#  ifdef LMD_MIXING
#   define LMD_SKPP
#   define LMD_BKPP
#   define LMD_RIMIX
#   define LMD_CONVEC
#  else
#   define GLS_MIXING
#   define GLS_KKL
#  endif
# endif

#elif defined INTERNAL
/*
!                       Internal Tide Example
!                       ======== ==== =======
!
! Di Lorenzo, E, W.R. Young and S.L. Smith, 2006, Numerical and anlytical estimates of M2
! tidal conversion at steep oceanic ridges, J. Phys. Oceanogr., 36, 1072-1084.  
*/
# undef  ETALON_CHECK
# undef  OPENMP
# undef  MPI
# define SOLVE3D
# define UV_COR
# define UV_ADV
# define BODYTIDE
# define ANA_GRID
# undef  INTERNALSHELF
# define ANA_INITIAL
# define ANA_BTFLUX
# define ANA_SMFLUX
# define ANA_SRFLUX
# define ANA_STFLUX
# define ANA_VMIX
# define EW_PERIODIC
# define NS_PERIODIC
# undef  OBC_EAST
# undef  OBC_WEST
# undef  SPONGE
# undef  ANA_SSH
# undef  ANA_M2CLIMA
# undef  ANA_M3CLIMA
# undef  ANA_TCLIMA
# undef  ZCLIMATOLOGY
# undef  M2CLIMATOLOGY
# undef  M3CLIMATOLOGY
# undef  TCLIMATOLOGY
# undef  M2NUDGING
# undef  M3NUDGING
# undef  TNUDGING

#elif defined S2DV 
/*
!                  2DV Sections 
!                  ============
!
*/

# undef  EXPERIMENT3
# undef  OPENMP
# undef  MPI
# undef  NBQ
# ifdef NBQ
#  define NBQ_IMP
# endif
# define NEW_S_COORD
# define SALINITY
# define TIDES
# define TIDERAMP
# define SSH_TIDES
# define UV_TIDES
# define SOLVE3D 
# define UV_ADV
# define UV_COR
# define UV_VIS2
# undef  VADV_ADAPT_IMP
# define SPHERICAL
# define CURVGRID
# undef  ANA_INITIAL
# define ANA_VMIX
# define ANA_SMFLUX
# define ANA_STFLUX
# define ANA_SRFLUX
# define ANA_SSFLUX
# define ANA_BTFLUX
# define ANA_BSFLUX
# define NS_PERIODIC
# undef  OBC_EAST
# define OBC_WEST
# define SPONGE
# define ANA_SSH
# define ANA_M2CLIMA
# define ANA_M3CLIMA
# undef  ANA_TCLIMA
# define ZCLIMATOLOGY
# define M2CLIMATOLOGY
# define M3CLIMATOLOGY
# define TCLIMATOLOGY
# define M2NUDGING
# define M3NUDGING
# define TNUDGING
# undef  ONLINE_ANALYSIS

#elif defined IGW
/*
!                  COMODO Internal Tide Example
!                  ====== ======== ==== =======
!
! Pichon, A., 2007: Tests academiques de maree, Rapport interne n 21 du 19 octobre 2007, 
! Service Hydrographique et Oceanographique de la Marine. 
*/

# define EXPERIMENT3
# undef  OPENMP
# undef  MPI
# undef  NBQ
# ifdef NBQ
#  define NBQ_IMP
# endif
# define NEW_S_COORD
# define TIDES
# define TIDERAMP
# define SSH_TIDES
# define UV_TIDES
# define SOLVE3D 
# define UV_ADV
# define UV_COR
# define UV_VIS2
# undef  VADV_ADAPT_IMP
# define SPHERICAL
# define CURVGRID
# define ANA_INITIAL
# define ANA_VMIX
# define ANA_SMFLUX
# define ANA_STFLUX
# define ANA_SRFLUX
# define ANA_SSFLUX
# define ANA_BTFLUX
# define ANA_BSFLUX
# define NS_PERIODIC
# define OBC_EAST
# define OBC_WEST
# undef  SPONGE
# define ANA_SSH
# define ANA_M2CLIMA
# define ANA_M3CLIMA
# define ANA_TCLIMA
# define ZCLIMATOLOGY
# define M2CLIMATOLOGY
# define M3CLIMATOLOGY
# define TCLIMATOLOGY
# define M2NUDGING
# define M3NUDGING
# define TNUDGING
# undef  ONLINE_ANALYSIS

#elif defined RIVER
/*
!                       River run-off test problem
!                       ==========================
*/
# undef OPENMP
# undef MPI
# define SOLVE3D
# define UV_ADV
# define UV_COR
# define M2FILTER_FLAT
# define NONLIN_EOS
# define SPLIT_EOS
# define SALINITY
# define ANA_GRID
# define MASKING
# define ANA_INITIAL
# define ANA_SMFLUX
# define ANA_STFLUX
# define ANA_SSFLUX
# define ANA_SRFLUX
# define ANA_BTFLUX
# define ANA_BSFLUX
# define LMD_MIXING
# define LMD_SKPP
# define LMD_BKPP
# define LMD_RIMIX
# define LMD_CONVEC
# define PSOURCE
# define ANA_PSOURCE
# define NS_PERIODIC
# define FLOATS
# ifdef FLOATS
#   define RANDOM_WALK
#   ifdef RANDOM_WALK
#      define DIEL_MIGRATION
#      define RANDOM_VERTICAL
#      define RANDOM_HORIZONTAL
#   endif
# endif

#elif defined SEAMOUNT
/*
!                       Seamount Example
!                       ======== =======
*/
# undef OPENMP
# undef MPI
# define UV_ADV
# define UV_COR
# define SOLVE3D
# define SALINITY
# define NONLIN_EOS
# define SPLIT_EOS
# define ANA_GRID
# define ANA_INITIAL
# define ANA_SMFLUX
# define ANA_STFLUX
# define ANA_SSFLUX
# define ANA_SRFLUX
# define ANA_BTFLUX
# define ANA_BSFLUX

# elif defined SHELFRONT
/*
!                       Shelf Front Example
!                       ===== ===== =======
*/
# undef OPENMP
# undef MPI
# define UV_ADV
# define UV_COR
# define SOLVE3D
# define SALINITY
# define ANA_GRID
# define ANA_INITIAL
# define ANA_SMFLUX
# define ANA_STFLUX
# define ANA_SSFLUX
# define ANA_SRFLUX
# define ANA_BTFLUX
# define ANA_BSFLUX
# define EW_PERIODIC

#elif defined SOLITON
/*
!                       Equatorial Rossby Wave Example
!                       ========== ====== ==== =======
*/
# undef OPENMP
# undef MPI
# define UV_COR
# define UV_ADV
# define ANA_GRID
# define ANA_INITIAL
# define AVERAGES
# define EW_PERIODIC
# define ANA_SMFLUX

# elif defined OVERFLOW
/*
!                       Gravitational/Overflow Example
!                       ====================== =======
*/
# undef OPENMP
# undef MPI
# define UV_ADV
# define UV_COR
# define UV_VIS2
# define TS_DIF2
# define TS_MIX_GEO
# define SOLVE3D
# define ANA_GRID
# define ANA_INITIAL
# define ANA_SMFLUX
# define ANA_STFLUX
# define ANA_BTFLUX

#elif defined UPWELLING
/*
!                       Upwelling Example
!                       ========= =======
*/
# undef OPENMP
# undef MPI
# define SOLVE3D
# define UV_COR
# define UV_ADV
# define ANA_GRID
# define ANA_INITIAL
# define AVERAGES
# define SALINITY
# define NONLIN_EOS
# define SPLIT_EOS
# define ANA_SSFLUX
# define ANA_SRFLUX
# define ANA_STFLUX
# define ANA_BSFLUX
# define ANA_BTFLUX
# define ANA_SMFLUX
# define LMD_MIXING
# define LMD_SKPP
# define LMD_BKPP
# define LMD_RIMIX
# define LMD_CONVEC
# define EW_PERIODIC

#elif defined VORTEX
/*
!                       Baroclinic Vortex Example (TEST AGRIF)
!                       ========== ====== ======= ===== ======
*/
# undef  OPENMP
# undef  MPI
# define AGRIF
# define AGRIF_2WAY
# undef  NBQ
# define SOLVE3D
# define UV_COR
# define UV_ADV
# define ANA_STFLUX
# define ANA_SMFLUX
# define ANA_BSFLUX
# define ANA_BTFLUX
# define ANA_VMIX
# define OBC_EAST
# define OBC_WEST
# define OBC_NORTH
# define OBC_SOUTH
# define SPONGE
# define ZCLIMATOLOGY
# define M2CLIMATOLOGY
# define M3CLIMATOLOGY
# define TCLIMATOLOGY
# define ZNUDGING
# define M2NUDGING
# define M3NUDGING
# define TNUDGING

#elif defined JET
/*
!                       Baroclinic JET Example
!                       ========== === =======
*/
# define ANA_JET
# undef  MPI
# undef  AGRIF
# undef  AGRIF_2WAY
# define SOLVE3D
# define UV_COR
# define UV_ADV
# define UV_VIS2
# ifdef ANA_JET
#  define ANA_GRID
#  define ANA_INITIAL
# endif
# define ANA_STFLUX
# define ANA_SMFLUX
# define ANA_BSFLUX
# define ANA_BTFLUX
# define ANA_VMIX
# define EW_PERIODIC
# define CLIMATOLOGY
# ifdef CLIMATOLOGY
#  define ZCLIMATOLOGY
#  define M2CLIMATOLOGY
#  define M3CLIMATOLOGY
#  define TCLIMATOLOGY
#  define ZNUDGING
#  define M2NUDGING
#  define M3NUDGING
#  define TNUDGING
#  define ROBUST_DIAG
#  define ZONAL_NUDGING
#  ifdef ANA_JET
#   define ANA_SSH
#   define ANA_M2CLIMA
#   define ANA_M3CLIMA
#   define ANA_TCLIMA
#  endif
# endif
# define LMD_MIXING 
# ifdef  LMD_MIXING
#  undef  ANA_VMIX
#  define ANA_SRFLUX
#  undef  LMD_KPP
#  define LMD_RIMIX
#  define LMD_CONVEC
# endif 

#elif defined SHOREFACE
/*
!                       PLANAR BEACH Example
!                       ====== ===== =======
!
!   Uchiyama, Y., McWilliams, J.C. and Shchepetkin, A.F. (2010): 
!      Wave-current interaction in an oceanic circulation model with a 
!      vortex force formalism: Application to the surf zone.
!      Ocean Modelling Vol. 34:1-2, pp.16-35.
*/
# undef  OPENMP
# undef  MPI
# define SOLVE3D
# define UV_ADV
# undef  MASKING
# define NEW_S_COORD
# define ANA_GRID
# define ANA_INITIAL
# define ANA_SMFLUX
# define ANA_STFLUX
# define ANA_SSFLUX
# define ANA_SRFLUX
# define ANA_SST
# define ANA_BTFLUX
# define NS_PERIODIC
# define OBC_WEST
# define SPONGE
# define WET_DRY
# define MRL_WCI
# ifdef MRL_WCI
#  define WKB_WWAVE
#  undef  WKB_UNSTEADY
#  define MRL_CEW
#  define WKB_OBC_WEST
#  define WAVE_ROLLER
#  define WAVE_FRICTION
#  define WAVE_STREAMING
#  define WAVE_RAMP
# endif
# define LMD_MIXING
# define LMD_SKPP
# define LMD_BKPP
# undef  BBL
# undef  SEDIMENT
# ifdef SEDIMENT
#  define TCLIMATOLOGY
#  define TNUDGING
#  define ANA_TCLIMA
# endif

#elif defined RIP
/*
!                       Rip Current Example
!                       === ======= =======
!
!   Weir, B., Uchiyama, Y.. (2010): 
!      A vortex force analysis of the interaction of rip 
!      currents and surface gravity wave
!      JGR Vol. 116
!
!  Default is idealized Duck Beach with 3D topography
!  RIP_TOPO_2D: Logshore uniform topography
!  BISCA: realistic case with Input files
!  GRANDPOPO: idealized Grand Popo Beach in Benin, 
!              longshore uniform
*/
# undef  BISCA
# undef  RIP_TOPO_2D
# undef  GRANDPOPO
# ifdef GRANDPOPO
#  define RIP_TOPO_2D
# endif
!
# undef  OPENMP
# undef  MPI
# define SOLVE3D
# define UV_ADV
# undef  VADV_ADAPT_IMP
# define UV_VIS2
# define UV_VIS_SMAGO
# define LMD_MIXING
# define LMD_SKPP
# define LMD_BKPP
# define NEW_S_COORD
# ifndef BISCA
#  define ANA_GRID
# endif
# define ANA_INITIAL
# define ANA_SMFLUX
# define ANA_STFLUX
# define ANA_SSFLUX
# define ANA_SRFLUX
# define ANA_SST
# define ANA_BTFLUX
# undef  ANA_TIDES
# if !defined BISCA && !defined ANA_TIDES
#  define NS_PERIODIC
# else
#  define OBC_NORTH
#  define OBC_SOUTH
# endif
# define OBC_WEST
# define SPONGE
# ifdef ANA_TIDES
#  define ANA_SSH
#  define ANA_M2CLIMA
#  define ANA_M3CLIMA
#  define ZCLIMATOLOGY
#  define M2CLIMATOLOGY
#  define M3CLIMATOLOGY
#  define M2NUDGING
#  define M3NUDGING
# endif
# define WET_DRY
# define MRL_WCI
# ifdef MRL_WCI
#  define WKB_WWAVE
#  define WKB_OBC_WEST
#  define WAVE_ROLLER
#  define WAVE_FRICTION
#  define WAVE_STREAMING
#  define MRL_CEW
#  ifdef RIP_TOPO_2D
#   define WAVE_RAMP
#  endif
# endif
# ifdef BISCA
#  define BBL
# endif
# undef SEDIMENT
# ifdef SEDIMENT
#  define ANA_SEDIMENT
#  undef  BED_ARMOR
#  undef  ANA_SPFLUX
#  undef  ANA_BPFLUX
# endif
# undef  DIAGNOSTICS_UV

#elif defined SWASH
/*
!                       SWASH PLANAR BEACH Example
!                       ===== ====== ===== =======
!
*/
# undef  OPENMP
# undef  MPI
# define SOLVE3D
# undef  NBQ
# define UV_ADV
# define UV_VIS2
# define UV_VIS_SMAGO
# define MASKING
# define NEW_S_COORD
# define ANA_GRID
# define ANA_INITIAL
# define ANA_SMFLUX
# define ANA_STFLUX
# define ANA_SSFLUX
# define ANA_SRFLUX
# define ANA_SST
# define ANA_BTFLUX
# define OBC_WEST
# define SPONGE
# define FRC_BRY
# define ANA_BRY
# define Z_FRC_BRY
# define M2_FRC_BRY
# define M3_FRC_BRY
# define T_FRC_BRY
# define WET_DRY

#elif defined THACKER
/*
!                       Thacker Example
!                       ======= =======
!
! Thacker, W., (1981), Some exact solutions to the nonlinear 
! shallow-water wave equations. 
! J. Fluid Mech., 107, 499–508.
*/
# undef  OPENMP
# undef  MPI
# define THACKER_2DV
# define SOLVE3D
# define UV_COR
# define UV_ADV
# undef  UV_VIS2
# define WET_DRY
# define NEW_S_COORD
# define ANA_GRID
# define ANA_INITIAL
# define ANA_BTFLUX
# define ANA_SMFLUX
# define ANA_SRFLUX
# define ANA_STFLUX

#elif defined TANK
/*
!                       Tank Example
!                       ======= =======
!
! Chen, X.J., 2003. A fully hydrodynamic model for three-dimensional, 
! free-surface flows. 
! Int. J. Numer. Methods Fluids 42, 929–952.
*/
# undef  MPI
# define NBQ
# ifdef NBQ
#  undef  NBQ_IMP
# endif
# define SOLVE3D
# undef  MASKING
# undef  UV_ADV
# define NEW_S_COORD
# define ANA_GRID
# define ANA_INITIAL
# define ANA_BTFLUX
# define ANA_SMFLUX
# define ANA_SRFLUX
# define ANA_STFLUX


#endif /* END OF CONFIGURATION CHOICE */

#include "cppdefs_dev.h"
#include "set_global_definitions.h"

