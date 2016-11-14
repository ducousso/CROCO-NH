! $Id: param.h 1619 2015-01-07 13:53:03Z marchesiello $
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
!----------------------------------------------------------------------
! Dimensions of Physical Grid and array dimensions
!----------------------------------------------------------------------
!
! LLm,MMm  Number of the internal points of the PHYSICAL grid.
!          in the XI- and ETA-directions [physical side boundary
!          points and peroodic ghost points (if any) are excluded].
!
! Lm,Mm    Number of the internal points [see above] of array
!          covering a Message Passing subdomain. In the case when
!          no Message Passing partitioning is used, these two are
!          the same as LLm,MMm. 
!
! N        Number of vertical levels.
!
      integer  LLm,Lm,MMm,Mm,N, LLm0,MMm0
#if defined AGRIF
      integer LLmm2, MMmm2
#endif
#if defined BASIN
      parameter (LLm0=60,   MMm0=50,   N=10)
#elif defined CANYON_A
      parameter (LLm0=65,   MMm0=48,   N=16)
#elif defined CANYON_B
      parameter (LLm0=66,   MMm0=48,   N=16)
#elif defined EQUATOR
      parameter (LLm0=40,   MMm0=32,   N=32)   ! 100 km resolution
#elif defined GRAV_ADJ
# ifdef NBQ
#  ifdef GRAV_ADJ_SOLITON
!     parameter (LLm0=100,  MMm0=1,    N=60)   !   3 cm resolution
#  elif GRAV_ADJ_ACOUSTIC
      parameter (LLm0=64,   MMm0=1,    N=64)   !   2  m resolution
#  else
!     parameter (LLm0=600,  MMm0=1,    N=60)   !   5 mm resolution
      parameter (LLm0=300,  MMm0=1,    N=30)   !  10 mm resolution
#  endif
# else
!     parameter (LLm0=32,   MMm0=4,    N=10)   !   2 km resolution
      parameter (LLm0=128,  MMm0=4,    N=40)   ! 500  m resolution
!     parameter (LLm0=512,  MMm0=4,   N=160)   ! 125  m resolution
# endif
#elif defined INNERSHELF
      parameter (LLm0=200,  MMm0=3,    N=60)
#elif defined INTERNAL
!     parameter (LLm0=120,  MMm0=10,   N=40)   !  10 km resolution
!     parameter (LLm0=800,  MMm0=4,    N=40)   ! 1.5 km resolution
      parameter (LLm0=1600, MMm0=4,    N=40)   ! .75 km resolution
#elif defined IGW
!      parameter (LLm0=878, MMm0=3,    N=80)   !   1 km resolution  
       parameter (LLm0=878, MMm0=3,    N=40)
!      parameter (LLm0=878, MMm0=3,    N=20)
#elif defined OVERFLOW
      parameter (LLm0=4,    MMm0=128,  N=10)
#elif defined RIVER
      parameter (LLm0=40,   MMm0=80,   N=20)
#elif defined SEAMOUNT
      parameter (LLm0=64,   MMm0=64,   N=20)
#elif defined SHELFRONT
      parameter (LLm0=4,    MMm0=40,   N=10)
#elif defined SOLITON
      parameter (LLm0=96,   MMm0=32,   N=10)
#elif defined UPWELLING
      parameter (LLm0=16,   MMm0=64,   N=16)
#elif defined VORTEX
!     parameter (LLm0=360,  MMm0=360,  N=10)   !  5 km resolution
!     parameter (LLm0=180,  MMm0=180,  N=10)   ! 10 km resolution
!     parameter (LLm0=90,   MMm0=90,   N=10)   ! 20 km resolution
      parameter (LLm0=60,   MMm0=60,   N=10)   ! 30 km resolution
#elif defined JET
# ifdef ANA_JET
!     parameter (LLm0=250,  MMm0=1000,N=100)   !  2 km resolution
!     parameter (LLm0=100,  MMm0=400,  N=80)   !  5 km resolution
!     parameter (LLm0= 50,  MMm0=200,  N=60)   ! 10 km resolution
      parameter (LLm0= 25,  MMm0=100,  N=40)   ! 20 km resolution
# else
!     parameter (LLm0=300,  MMm0=500,  N=30)   !  2 km resolution
!     parameter (LLm0=120,  MMm0=200,  N=30)   !  5 km resolution
!     parameter (LLm0=60,   MMm0=100,  N=30)   ! 10 km resolution
      parameter (LLm0=30,   MMm0=50,   N=30)   ! 20 km resolution
# endif
#elif defined SHOREFACE
      parameter (LLm0=59,   MMm0=7,    N=20)   ! Planar Beach 20m
#elif defined RIP
# ifdef BISCA
      parameter (LLm0= 86,  MMm0= 92,  N=20)   ! 10 m Bisca Rip
# else
!     parameter (LLm0=256,  MMm0=256,  N=20)   !  3 m resolution 
!     parameter (LLm0= 96,  MMm0= 96,  N=20)   !  8 m resolution
      parameter (LLm0= 48,  MMm0= 48,  N=20)   ! 16 m resolution
# endif
#elif defined THACKER
# ifdef THACKER_2DV
      parameter (LLm0=199,  MMm0=1,    N=5 )   !  1 km resolution
# else
      parameter (LLm0=199,  MMm0=199,  N=5 )   !  1 km resolution
# endif
#elif defined TANK
# if ! defined MOVING_BATHY
      parameter (LLm0=50,   MMm0=1,    N=50)   ! 20 cm resolution
!     parameter (LLm0=10,   MMm0=1,    N=10)   !  1  m resolution
# else
      parameter (LLm0=4000,   MMm0=1,  N=30)  !  1 mm resolution
# endif
#elif defined REGIONAL
#  if   defined USWC0
      parameter (LLm0=62,   MMm0=126,  N=40)   ! US_West grid15 L0
#  elif defined USWC1
      parameter (LLm0=60,   MMm0=96,   N=40)   ! US_West grid15 L1
#  elif defined USWC2
      parameter (LLm0=60,   MMm0=120,  N=40)   ! US_West grid15 L2
#  elif defined USWC155
      parameter (LLm0=83,   MMm0=168,  N=20)   ! US_West USWC155 L1
#  elif defined CANARY
!     parameter (LLm0=97,   MMm0=159,  N=32)   ! Canary
#  elif defined FINISTERE
      parameter (LLm0=78,   MMm0=100,  N=16)   ! Finistere
#  elif defined RIA
      parameter (LLm0=77,   MMm0=96,   N=28)   ! RIA
#  elif defined PERU
      parameter (LLm0=39,   MMm0=32,   N=20)   ! Peru test
#  elif defined SAFE
      parameter (LLm0=111,  MMm0=96,   N=32)   ! SAFE
#  elif defined PACIFIC
      parameter (LLm0=170,  MMm0=60,   N=30)   ! Pacific
#  elif defined  CORAL
      parameter (LLm0=81,   MMm0=77,   N=32)   ! CORAL sea
#  elif defined  BENGUELA_LR
      parameter (LLm0=41,   MMm0=42,   N=32)   ! BENGUELA_LR
#  elif defined  BENGUELA_HR
      parameter (LLm0=83,   MMm0=85,   N=32)   ! BENGUELA_HR
#  elif defined  BENGUELA_VHR
      parameter (LLm0=167,  MMm0=170,  N=32)   ! BENGUELA_VHR
#  else
      parameter (LLm0=94,   MMm0=81,   N=40)
#  endif
#else
      parameter (LLm0=xx, MMm0=xx, N=xx)
#endif
      
#ifdef AGRIF
      common /scrum_physical_grid/ LLm,Lm,LLmm2,MMm,Mm,MMmm2
#else
      parameter (LLm=LLm0,  MMm=MMm0)
#endif

!
!----------------------------------------------------------------------
! MPI related variables
!----------------------------------------------------------------------
!
      integer Lmmpi,Mmmpi,iminmpi,imaxmpi,jminmpi,jmaxmpi
      common /comm_setup_mpi1/ Lmmpi,Mmmpi
      common /comm_setup_mpi2/ iminmpi,imaxmpi,jminmpi,jmaxmpi
!
! Domain subdivision parameters
! ====== =========== ==========
!
! NPP            Maximum allowed number of parallel threads;
! NSUB_X,NSUB_E  Number of SHARED memory subdomains in XI- and
!                                                ETA-directions;
! NNODES        Total number of MPI processes (nodes);
! NP_XI,NP_ETA  Number of MPI subdomains in XI- and ETA-directions;
!
      integer NSUB_X, NSUB_E, NPP
#ifdef MPI
      integer NP_XI, NP_ETA, NNODES     
      parameter (NP_XI=1, NP_ETA=4,  NNODES=NP_XI*NP_ETA)
      parameter (NPP=1)
      parameter (NSUB_X=1, NSUB_E=1)
#elif defined OPENMP
      parameter (NPP=4)
# ifdef AUTOTILING
      common/distrib/NSUB_X, NSUB_E
# else
      parameter (NSUB_X=1, NSUB_E=NPP)
# endif
#else
      parameter (NPP=1)
# ifdef AUTOTILING
      common/distrib/NSUB_X, NSUB_E
# else
      parameter (NSUB_X=1, NSUB_E=NPP)
# endif
#endif

!
!----------------------------------------------------------------------
! Number maximum of weights for the barotropic mode
!----------------------------------------------------------------------
!
      integer NWEIGHT
#ifdef NBQ
      parameter (NWEIGHT=100000)
#else
      parameter (NWEIGHT=1000)
#endif

!
!----------------------------------------------------------------------
! Tides, Wetting-Drying, Point sources, Floast, Stations
!----------------------------------------------------------------------
!

#if defined SSH_TIDES || defined UV_TIDES
      integer Ntides             ! Number of tides
                                 ! ====== == =====
# ifdef IGW
      parameter (Ntides=1)
# else
      parameter (Ntides=8)
# endif
#endif
#ifdef WET_DRY
      real D_wetdry             ! Critical Depth for Drying cells
                                ! ======== ===== === ====== =====
# ifdef THACKER
      parameter (D_wetdry=0.01)
# else
      parameter (D_wetdry=0.10)
# endif
#endif
#if defined PSOURCE || defined PSOURCE_NCFILE
      integer Msrc               ! Number of point sources
      parameter (Msrc=10)        ! ====== == ===== =======
#endif
#ifdef FLOATS
       integer Mfloats           ! Maximum number of floats
       parameter (Mfloats=32000) ! ======= ====== == ======
#endif
#ifdef STATIONS
       integer NS                ! Number of output stations
       parameter (NS=5)          ! ====== == ====== ========
       integer Msta              ! Maximum number of stations
       parameter (Msta=1000)     ! ======= ====== == ========
#endif

!
!----------------------------------------------------------------------
! Derived dimension parameters
!----------------------------------------------------------------------
!
      integer stdout, Np, padd_X,padd_E
#ifdef AGRIF
      common/scrum_deriv_param/padd_X,padd_E
#endif
      parameter (stdout=6, Np=N+1)
#ifndef AGRIF
# ifdef MPI
      parameter (Lm=(LLm+NP_XI-1)/NP_XI, Mm=(MMm+NP_ETA-1)/NP_ETA)
# else
      parameter (Lm=LLm, Mm=MMm)
# endif
      parameter (padd_X=(Lm+2)/2-(Lm+1)/2)
      parameter (padd_E=(Mm+2)/2-(Mm+1)/2)
#endif

#if defined AGRIF || defined AUTOTILING
      integer NSA, N2d,N3d,N1dXI,N1dETA
      parameter (NSA=28)
      common /scrum_private_param/ N2d,N3d,N1dXI,N1dETA
#else
      integer NSA, N2d,N3d, size_XI,size_ETA
      integer se,sse, sz,ssz
      parameter (NSA=28)
# ifdef ALLOW_SINGLE_BLOCK_MODE
      parameter (size_XI=6+Lm, size_ETA=6+Mm)
# else
      parameter (size_XI=7+(Lm+NSUB_X-1)/NSUB_X)
      parameter (size_ETA=7+(Mm+NSUB_E-1)/NSUB_E)
# endif
      parameter (sse=size_ETA/Np, ssz=Np/size_ETA)
      parameter (se=sse/(sse+ssz), sz=1-se)
      parameter (N2d=size_XI*(se*size_ETA+sz*Np))
      parameter (N3d=size_XI*size_ETA*Np)
#endif

!
!----------------------------------------------------------------------
! I/O : flag for type sigma vertical transformation
!----------------------------------------------------------------------
!
#ifdef NEW_S_COORD
      real Vtransform          
      parameter (Vtransform=2) 
#else
      real Vtransform
      parameter (Vtransform=1)
#endif

!
!----------------------------------------------------------------------
! Number of tracers 
!----------------------------------------------------------------------
!
#ifdef SOLVE3D
      integer   NT, itemp
      integer   ntrc_salt, ntrc_pas, ntrc_bio, ntrc_sed 
!
      parameter (itemp=1)
# ifdef SALINITY 
      parameter (ntrc_salt=1)
# else
      parameter (ntrc_salt=0)
# endif
# ifdef PASSIVE_TRACER
      parameter (ntrc_pas=1)
# else
      parameter (ntrc_pas=0)
# endif
# ifdef BIOLOGY
#  ifdef PISCES
      parameter (ntrc_bio=24)
#  elif defined BIO_NChlPZD
#   ifdef OXYGEN
      parameter (ntrc_bio=6)
#   else
      parameter (ntrc_bio=5)
#   endif
#  elif defined BIO_N2ChlPZD2
      parameter (ntrc_bio=7)
#  elif defined BIO_BioEBUS 
#   ifdef NITROUS_OXIDE
      parameter (ntrc_bio=12)
#   else
      parameter (ntrc_bio=11)
#   endif  
#  endif 
# else
      parameter (ntrc_bio=0)
# endif /* BIOLOGY */
!
# ifdef SEDIMENT
! NGRAV          Number of gravel classes
! NSAND          Number of sand classes
! NMUD           Number of mud classes  
! NST            Number of sediment (tracer) size classes
! NLAY           Number of layers in sediment bed
!
      integer    NGRAV, NSAND, NMUD, NST, NLAY
      parameter (NGRAV=0, NSAND=2, NMUD=0, 
     &           NST=NGRAV+NSAND+NMUD, NLAY=2)
      parameter (ntrc_sed=NST)
# else
      parameter (ntrc_sed=0)
# endif /* SEDIMENT */

! Total number of tracers
!
      parameter (NT=itemp+ntrc_salt+ntrc_pas+ntrc_bio+ntrc_sed) 

# if defined BBL && defined AGRIF
      integer Agrif_lev_sedim
      parameter (Agrif_lev_sedim=0)
# endif

#endif /* SOLVE3D */

!
!----------------------------------------------------------------------
! Tracer identification indices
!----------------------------------------------------------------------
!
#if defined SOLVE3D && !defined F90CODE
      integer   ntrc_diats, ntrc_diauv, ntrc_diabio
# ifdef BIOLOGY
     &          , itrc_bio
# endif
# ifdef SEDIMENT
     &          , itrc_sed
# endif
# ifdef SALINITY
     &          , isalt
# endif
# ifdef PASSIVE_TRACER
     &          , itpas
# endif
!
# ifdef BIOLOGY
#  ifdef PISCES
     &          , iDIC_, iTAL_, iOXY_, iCAL_, iPO4_
     &          , iPOC_, iSIL_, iPHY_, iZOO_, iDOC_
     &          , iDIA_, iMES_, iBSI_, iFER_
     &          , iBFE_, iGOC_, iSFE_, iDFE_, iDSI_
     &          , iNFE_, iNCH_, iDCH_, iNO3_, iNH4_
#   ifdef DIAGNOSTICS_BIO
#    ifdef key_trc_dia3d
     &          , Nhi,Nco3,Naksp,Netot,Nprorca
     &          , Nprorca2,Npronew,Npronew2
     &          , Nprorca3,Nprorca4,Nprorca5
     &          , Ngraztot1,Ngraztot2,Nnitrifo2
     &          , Npronewo2,Nprorego2,Nremino2
     &          , Nmicroo2,Nmesoo2,Nfixo2
#    endif
#    ifdef key_trc_diaadd
     &          , Nfld,Nflu16,Nkgco2,Natcco2,Nsinking
     &          , Nsinking2,Nsinkfer,Nsinkfer2,Nsinksil
     &          , Nsinkcal,Nzmeu,Nirondep,Nnitrpot
#    endif
#   endif
     &          , NumFluxTerms,NumVSinkTerms,NumGasExcTerms
#  elif defined BIO_NChlPZD
     &          , iNO3_, iChla, iPhy1, iZoo1
     &          , iDet1
#   ifdef OXYGEN
     &          , iO2
#   endif
     &          , NFlux_NewProd, NFlux_Grazing, NFlux_SlopFeed
     &          , NFlux_Pmort, NFlux_Zmetab, NFlux_Zmort, NFlux_ReminD
     &          , NumFluxTermsN
#   ifdef OXYGEN
     &          , OGain_NewProd, OLoss_Zmetab
     &          , OLoss_ReminD, NumFluxTermsO, OFlux_GasExc
     &          , NumGasExcTerms, NumFluxTerms  
#   else 
     &          , NumGasExcTerms
     &          , NumFluxTerms
#   endif
     &          , NFlux_VSinkP1, NFlux_VSinkD1
     &          , NumVSinkTerms
#  elif defined BIO_N2ChlPZD2
     &          , iNO3_, iNH4_, iChla, iPhy1, iZoo1
     &          , iDet1, iDet2
     &          , NFlux_NewProd
     &          , NFlux_RegProd
     &          , NFlux_Nitrific 
     &          , NFlux_Grazing
     &          , NFlux_SlopFeed
     &          , NFlux_Pmort 
     &          , NFlux_Zmetab
     &          , NFlux_Zmort
     &          , NFlux_ReminD1, NFlux_ReminD2
     &          , NumFluxTermsN, NumFluxTerms, NumGasExcTerms
     &          , NFlux_VSinkP1
     &          , NFlux_VSinkD1, NFlux_VSinkD2
     &          , NumVSinkTerms

#  elif defined BIO_BioEBUS
     &          , iNO3_, iNO2_, iNH4_, iPhy1, iPhy2, iZoo1, iZoo2
     &          , iDet1, iDet2, iDON, iO2
#    ifdef NITROUS_OXIDE
     &          , iN2O
#    endif 
     &          , NFlux_lightlimitP1, NFlux_lightlimitP2
     &          , NFlux_templimitP1, NFlux_templimitP2
     &          , NFlux_NO3limitP1, NFlux_NO2limitP1
     &          , NFlux_NH4limitP1, NFlux_NO3limitP2
     &          , NFlux_NO2limitP2, NFlux_NH4limitP2
     &          , NFlux_ProdNO3P1, NFlux_ProdNO3P2
     &          , NFlux_ProdNO2P1, NFlux_ProdNO2P2
     &          , NFlux_Nitrif1, NFlux_Nitrif2, NFlux_ProdNH4P1
     &          , NFlux_ProdNH4P2, NFlux_P1Z1Grazing
     &          , NFlux_P2Z1Grazing, NFlux_P1mort, NFlux_P2mort
     &          , NFlux_P1Z2Grazing, NFlux_P2Z2Grazing
     &          , NFlux_Z1Z2Grazing, NFlux_Z1metab, NFlux_Z1mort
     &          , NFlux_Z2metab, NFlux_Z2mort, NFlux_HydrolD1
     &          , NFlux_ReminOxyD1, NFlux_Denitr1D1
     &          , NFlux_Denitr2D1
     &          , NFlux_HydrolD2, NFlux_ReminOxyD2
     &          , NFlux_Denitr1D2, NFlux_Denitr2D2
     &          , NFlux_ReminOxyDON
     &          , NFlux_Denitr1DON, NFlux_Denitr2DON
     &          , NFlux_NO2anammox
     &          , NFlux_NH4anammox, O2Flux_GasExc, NumFluxTermsN
#    ifdef NITROUS_OXIDE
     &          , NFlux_paramN2O, N2OFlux_GasExc
#    endif 
     &          , NumFluxTerms, NumGasExcTerms
     &          , NFlux_VSinkP2, NFlux_VSinkD1
     &          , NFlux_VSinkD2, NumVSinkTerms
#  endif  
# endif   /* BIOLOGY */

# ifdef SEDIMENT
     &          , isand, isilt
# endif

!
! ================  Parameters  =====================
!

# ifdef SALINITY 
      parameter (isalt=itemp+1)
# endif
# ifdef PASSIVE_TRACER
      parameter (itpas=itemp+ntrc_salt+1)
# endif

!
! ===  BIOLOGY  ===
!
# ifdef BIOLOGY
#  ifdef PISCES
      parameter (itrc_bio=itemp+ntrc_salt+ntrc_pas+1)
      parameter (iDIC_=itrc_bio, iTAL_=iDIC_+1,
     &            iOXY_=iDIC_+2,  iCAL_=iDIC_+3,  iPO4_=iDIC_+4,
     &            iPOC_=iDIC_+5,  iSIL_=iDIC_+6,  iPHY_=iDIC_+7,
     &            iZOO_=iDIC_+8,  iDOC_=iDIC_+9,  iDIA_=iDIC_+10,
     &            iMES_=iDIC_+11, iBSI_=iDIC_+12, iFER_=iDIC_+13,
     &            iBFE_=iDIC_+14, iGOC_=iDIC_+15, iSFE_=iDIC_+16,
     &            iDFE_=iDIC_+17, iDSI_=iDIC_+18, iNFE_=iDIC_+19,
     &            iNCH_=iDIC_+20, iDCH_=iDIC_+21, iNO3_=iDIC_+22,
     &            iNH4_=iDIC_+23)
#   ifdef key_trc_dia3d
       parameter (Nhi       = 1,
     &            Nco3      = 2,
     &            Naksp     = 3,
     &            Netot     = 4,
     &            Nprorca   = 5,
     &            Nprorca2  = 6,
     &            Npronew   = 7,
     &            Npronew2  = 8,
     &            Nprorca3  = 9,
     &            Nprorca4  = 10,
     &            Nprorca5  = 11,
     &            Ngraztot1 = 12,
     &            Ngraztot2 = 13,
     &            Nnitrifo2 = 14,
     &            Npronewo2 = 15,
     &            Nprorego2 = 16,
     &            Nremino2  = 17,
     &            Nmicroo2  = 18,
     &            Nmesoo2   = 19,
     &            Nfixo2    = 20,
     &            NumFluxTerms   = Nfixo2)
#   else
       parameter (NumFluxTerms = 0)
#   endif
#   ifdef key_trc_diaadd
       parameter (Nfld      = 1,
     &            Nflu16    = 2,
     &            Nkgco2    = 3,
     &            Natcco2   = 4,
     &            Nsinking  = 5,
     &            Nsinking2 = 6,
     &            Nsinkfer  = 7,
     &            Nsinkfer2 = 8,
     &            Nsinksil  = 9,
     &            Nsinkcal  = 10,
     &            Nzmeu     = 11,
     &            Nirondep  = 12,
     &            Nnitrpot  = 13,
     &            NumGasExcTerms = 0,
     &            NumVSinkTerms = Nnitrpot)
#   else
       parameter (NumGasExcTerms = 0, NumVSinkTerms = 0)
#   endif

#  elif defined BIO_NChlPZD
#   ifdef OXYGEN
      parameter (itrc_bio=itemp+ntrc_salt+ntrc_pas+1)
#   else
      parameter (itrc_bio=itemp+ntrc_salt+ntrc_pas+1)
#   endif
      parameter (iNO3_=itrc_bio, iChla=iNO3_+1,  
     &           iPhy1=iNO3_+2,
     &           iZoo1=iNO3_+3, 
     &           iDet1=iNO3_+4)
#   ifdef OXYGEN
      parameter (iO2=iNO3_+5)
#   endif
      parameter (NFlux_NewProd  = 1,
     &           NFlux_Grazing  = 2,
     &           NFlux_SlopFeed = 3,
     &           NFlux_Pmort    = 4,
     &           NFlux_Zmetab   = 5,
     &           NFlux_Zmort    = 6,
     &           NFlux_ReminD   = 7,
     &           NumFluxTermsN  = 7,
#   ifdef OXYGEN
     &           OGain_NewProd  = 8, 
     &           OLoss_Zmetab   = 9,
     &           OLoss_ReminD   = 10,
     &           NumFluxTermsO  = 3,
     &           OFlux_GasExc   = 1,
     &           NumGasExcTerms = 1,
     &           NumFluxTerms   = 10,
#   else
     &           NumGasExcTerms = 0,
     &           NumFluxTerms   = 7,
#   endif
     &           NFlux_VSinkP1  = 1,
     &           NFlux_VSinkD1  = 2,
     &           NumVSinkTerms  = 2)

#  elif defined BIO_N2ChlPZD2
      parameter (ntrc_bio=7,itrc_bio=itemp+ntrc_salt+ntrc_pas+1) 
      parameter (iNO3_=itrc_bio, iNH4_=iNO3_+1, iChla=iNO3_+2,   
     &           iPhy1=iNO3_+3,
     &           iZoo1=iNO3_+4,
     &           iDet1=iNO3_+5, iDet2=iNO3_+6)
      parameter (NFlux_NewProd  = 1,
     &           NFlux_RegProd  = 2,
     &           NFlux_Nitrific = 3,
     &           NFlux_Grazing  = 4,
     &           NFlux_SlopFeed = 5,
     &           NFlux_Pmort    = 6,
     &           NFlux_Zmetab   = 7,
     &           NFlux_Zmort    = 8,
     &           NFlux_ReminD1  = 9,
     &           NFlux_ReminD2  = 10,
     &           NumFluxTermsN  = 10,
     &           NumFluxTerms   = 10,
     &           NumGasExcTerms = 0,
     &           NFlux_VSinkP1  = 1,
     &           NFlux_VSinkD1  = 2,
     &           NFlux_VSinkD2  = 3,
     &           NumVSinkTerms  = 3)

#  elif defined BIO_BioEBUS 
#   ifdef NITROUS_OXIDE
      parameter (itrc_bio=itemp+ntrc_salt+ntrc_pas+1)
#   else
      parameter (itrc_bio=itemp+ntrc_salt+ntrc_pas+1)
#   endif  
    
      parameter (iNO3_=itrc_bio, iNO2_=iNO3_+1, iNH4_=iNO3_+2,
     &           iPhy1=iNO3_+3,  iPhy2=iNO3_+4,
     &           iZoo1=iNO3_+5,  iZoo2=iNO3_+6,
     &           iDet1=iNO3_+7,  iDet2=iNO3_+8,
     &		 iDON=iNO3_+9,   iO2=iNO3_+10)

#   ifdef NITROUS_OXIDE
      parameter (iN2O=iNO3_+11)
#   endif
  
      parameter(  NFlux_lightlimitP1=1
     &          , NFlux_lightlimitP2=2
     &          , NFlux_templimitP1=3
     &          , NFlux_templimitP2=4
     &          , NFlux_NO3limitP1=5
     &          , NFlux_NO2limitP1=6
     &          , NFlux_NH4limitP1=7
     &          , NFlux_NO3limitP2=8
     &          , NFlux_NO2limitP2=9
     &          , NFlux_NH4limitP2=10
     &          , NFlux_ProdNO3P1=11
     &          , NFlux_ProdNO3P2=12
     &          , NFlux_ProdNO2P1=13
     &          , NFlux_ProdNO2P2=14
     &          , NFlux_Nitrif1=15
     &          , NFlux_Nitrif2=16
     &          , NFlux_ProdNH4P1=17
     &          , NFlux_ProdNH4P2=18
     &          , NFlux_P1Z1Grazing=19
     &          , NFlux_P2Z1Grazing=20
     &          , NFlux_P1mort=21
     &          , NFlux_P2mort=22
     &          , NFlux_P1Z2Grazing=23
     &          , NFlux_P2Z2Grazing=24
     &          , NFlux_Z1Z2Grazing=25
     &          , NFlux_Z1metab=26
     &          , NFlux_Z1mort=27
     &          , NFlux_Z2metab=28
     &          , NFlux_Z2mort=29
     &          , NFlux_HydrolD1=30
     &          , NFlux_ReminOxyD1=31 			
     &          , NFlux_Denitr1D1=32
     &          , NFlux_Denitr2D1=33
     &          , NFlux_HydrolD2=34
     &          , NFlux_ReminOxyD2=35
     &          , NFlux_Denitr1D2=36
     &          , NFlux_Denitr2D2=37
     &          , NFlux_ReminOxyDON=38
     &          , NFlux_Denitr1DON=39
     &          , NFlux_Denitr2DON=40
     &          , NFlux_NO2anammox=41
     &          , NFlux_NH4anammox=42
#   ifdef NITROUS_OXIDE     
     &          , NFlux_paramN2O=43
     &          , NumFluxTermsN=NFlux_paramN2O
#   else 
     &          , NumFluxTermsN=NFlux_NH4anammox
#   endif      
     &          , NumFluxTerms=NumFluxTermsN
     &          , O2Flux_GasExc=1
#   ifdef NITROUS_OXIDE
     &          , N2OFlux_GasExc=2
     &          , NumGasExcTerms=2
#   else
     &          , NumGasExcTerms=1
#   endif
     &          , NFlux_VSinkP2=1
     &          , NFlux_VSinkD1=2
     &          , NFlux_VSinkD2=3
     &          , NumVSinkTerms=3)
#  endif  /* PISCES ... */

!
! ===  BIOLOGY DIAGS ===
!

#  if defined BIO_NChlPZD || defined BIO_N2ChlPZD2 || defined PISCES \
                          || defined BIO_BioEBUS
#   ifdef DIAGNOSTICS_BIO
      parameter (ntrc_diabio=NumFluxTerms+
     &                       NumGasExcTerms+NumVSinkTerms)
#   else
      parameter (ntrc_diabio=0)
#   endif
#  else
      parameter (ntrc_diabio=0)
#  endif
# else
      parameter (ntrc_diabio=0)
# endif /* BIOLOGY */

!
! === SEDIMENTS ===
!

# ifdef SEDIMENT
      parameter (itrc_sed=itemp+ntrc_salt+ntrc_pas+ntrc_bio+1)
      parameter (isand=itrc_sed, isilt=isand+1)
# endif

!
! ===  u,v and tracer equations Diagnostics  ===
!
# ifdef DIAGNOSTICS_TS
#  ifdef DIAGNOSTICS_TS_MLD
      parameter (ntrc_diats=15*NT)
#  else
      parameter (ntrc_diats=7*NT)
#  endif
# else
      parameter (ntrc_diats=0)
# endif
# ifdef DIAGNOSTICS_UV
      parameter (ntrc_diauv=16)
# else
      parameter (ntrc_diauv=0)
# endif

#endif /*SOLVE3D */


