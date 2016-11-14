! $Id: ncscrum.h 1588 2014-08-04 16:26:01Z marchesiello $
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
! This is include file "ncscrum.h".
! ==== == ======= ==== ============
!
!===================================================================
! indices in character array "vname", which holds variable names
!                                                and attributes.
! indxTime        time
! indxZ           free-surface
! indxUb,indxVb   vertically integrated 2D U,V-momentum components
!
! indxU,indxV     3D U- and V-momenta.
! indxT,indxS,.., indxZoo  tracers (temperature, salinity,
!                 biological tracers.
! indxSAND,indxGRAV...
! indxMUD         gravel,sand & mud sediment tracers
! indxO,indeW     omega vertical mass flux and true vertical velocity
! indxR           density anomaly
! indxAOU  	  Apparent Oxygen Utilization
! indxWIND10      surface wind speed 10 m
! indxpCO2        partial pressure of CO2 in the ocean
! indxVisc        Horizontal viscosity coefficients
! indxDiff        Horizontal diffusivity coefficients
! indxAkv,indxAkt,indxAks  vertical viscosity/diffusivity coefficients
! indxAkk,indxAkp vertical diffusion coefficients for TKE and GLS
! indxHbl         depth of planetary boundary layer in KPP model
! indxHbbl        depth of bottom planetary boundary layer in KPP model
! indxHel         depth of euphotic layer
! indxChC         Chlorophyll/Carbon ratio
! indxTke         Turbulent kinetic energy
! indxGls         Generic length scale
! indxLsc         vertical mixing length scale
! indexHm         time evolving bathymetry
!
! indxSSH         observed sea surface height (from climatology)
! indxSUSTR,indxSVSTR  surface U-, V-momentum stress (wind forcing)
! indxShflx       net surface heat flux.
! indxShflx_rsw   shortwave radiation flux
! indxSwflx       surface fresh water flux
! indxSST         sea surface temperature
! indxdQdSST      Q-correction coefficient dQdSST
! indxSSS         sea surface salinity
! indxQBAR         river runoff
!
! indxAi          fraction of cell covered by ice
! indxUi,indxVi   U,V-components of sea ice velocity
! indxHi,indxHS   depth of ice cover and depth of snow cover
! indxTIsrf       temperature of ice surface
!
! indxBSD,indxBSS bottom sediment grain Density and Size 
!                 to be read from file if(!defined ANA_BSEDIM, 
!                 && !defined SEDIMENT) 
!
! indxBTHK,       sediment bed thickness, porosity, size class fractions 
! indxBPOR,indxBFRA
!
! indxWWA          wind induced wave Amplitude
! indxWWD          wind induced wave Direction
! indxWWP          wind induced wave Period
! indxWEB          wave dissipation by breakig,
!                 
! ** WKB model ** or ** OW COUPLING **
! indxHRM,indxFRQ  RMS wave height and frequency
! indxWAC,indxWKX,indxWKE   wave action density, XI/ETA-dir wavenumber
!
! indxSUP          wave set-up 
! indxUST2D        vertically integrated stokes velocity in xi  direction
! indxVST2D        vertically integrated stokes velocity in eta direction 
! indxUST          stokes velocity in xi  direction
! indxVST          stokes velocity in eta direction 
! indxWST          vertical stokes velocity 
! indxAkb          eddy viscosity  due to wave breaking
! indxAkw          eddy difusivity due to wave breaking  
! indxKVF          vortex force
! indxCALP         surface pressure correction
! indxKAPS         Bernoulli head
!
! ** DIAGNOSTICS_UV **
!  indxMXadv,indxMYadv,indxMXadv : xi-, eta-, and s- advection terms
!  indxMCor                      : Coriolis term,
!  indxMPrsgrd                   : Pressure gradient force term
!  indxMHmix, indxMVmix          : horizontal and vertical mixinig terms 
!  indxMrate                     : tendency term
!  indxMBtcr                     : forth-order truncation error
!  indxMswd, indxMbdr            : surface wind & bed shear stresses (m2/s2)
!  indxMvf, indxMbrk             : vortex force & breaking body force terms
!  indxMStCo                     : Stokes-Coriolis terms
!  indxMVvf                      : vertical vortex force terms (in prsgrd.F)
!  indxMPrscrt                   : pressure correction terms (in prsgrd.F)
!  indxMsbk, indxMbwf            : surface breaking & bed wave friction (m2/s2)
!  indxMfrc                      : near-bed frictional wave streaming as body force (m2/s2)
!
! ** DIAGNOSTICS_TS **
!  indxTXadv,indxTYadv,indxTVadv : xi-, eta-, and s- advection terms
!  indxTHmix,indxTVmix           : horizontal and vertical mixinig terms
!  indxTbody                     : body force term
!  indxTrate                     : tendency term

!=======================================================================
! Output file codes
      integer filetype_his, filetype_avg 
     &       ,filetype_dia, filetype_dia_avg 
     &       ,filetype_diaM, filetype_diaM_avg
     &       ,filetype_diabio, filetype_diabio_avg
      parameter (filetype_his=1, filetype_avg=2, 
     &           filetype_dia=3, filetype_dia_avg=4,
     &           filetype_diaM=5, filetype_diaM_avg=6,
     &           filetype_diabio=7,filetype_diabio_avg=8)
!
      integer iloop, indextemp
      integer indxTime, indxZ, indxUb, indxVb
      parameter (indxTime=1, indxZ=2, indxUb=3, indxVb=4)
#ifdef MOVING_BATHY
      integer indxHm
      parameter (indxHm=5)
#endif
#ifdef SOLVE3D
      integer indxU, indxV, indxT
      parameter (indxU=6, indxV=7, indxT=8)

# ifdef SALINITY
      integer indxS
      parameter (indxS=indxT+1)
# endif
# ifdef PASSIVE_TRACER
      integer indxTPAS
      parameter (indxTPAS=indxT+ntrc_salt+1)
# endif
# ifdef BIOLOGY
#  ifdef PISCES
      integer indxDIC, indxTAL, indxOXY, indxCAL, indxPO4,
     &        indxPOC, indxSIL, indxPHY, indxZOO, indxDOC,
     &        indxDIA, indxMES, indxBSI, indxFER, indxBFE,
     &        indxGOC, indxSFE, indxDFE, indxDSI, indxNFE,
     &        indxNCH, indxDCH, indxNO3, indxNH4
      parameter (indxDIC =indxT+ntrc_salt+ntrc_pas+1,
     &           indxTAL =indxDIC+1, indxOXY=indxDIC+2,
     &           indxCAL=indxDIC+3, indxPO4=indxDIC+4,
     &           indxPOC=indxDIC+5, indxSIL=indxDIC+6,
     &           indxPHY =indxDIC+7, indxZOO=indxDIC+8,
     &           indxDOC =indxDIC+9, indxDIA=indxDIC+10,
     &           indxMES =indxDIC+11, indxBSI=indxDIC+12,
     &           indxFER =indxDIC+13, indxBFE=indxDIC+14,
     &           indxGOC =indxDIC+15, indxSFE=indxDIC+16,
     &           indxDFE =indxDIC+17, indxDSI=indxDIC+18,
     &           indxNFE =indxDIC+19, indxNCH=indxDIC+20,
     &           indxDCH =indxDIC+21, indxNO3=indxDIC+22,
     &           indxNH4 =indxDIC+23)
#  elif defined BIO_NChlPZD
      integer indxNO3, indxChla,
     &        indxPhy1,indxZoo1,
     &        indxDet1
#   ifdef OXYGEN
     &      , indxO2
#   endif
      parameter (indxNO3 =indxT+ntrc_salt+ntrc_pas+1,
     &           indxChla=indxNO3+1,
     &           indxPhy1=indxNO3+2,
     &           indxZoo1=indxNO3+3,
     &           indxDet1=indxNO3+4)
#   ifdef OXYGEN
      parameter (indxO2=indxNO3+5)
#   endif
#  elif defined BIO_N2ChlPZD2
      integer indxNO3, indxNH4, indxChla,
     &        indxPhy1, indxZoo1,
     &        indxDet1, indxDet2
      parameter (indxNO3 =indxT+ntrc_salt+ntrc_pas+1,
     &           indxNH4 =indxNO3+1, indxChla=indxNO3+2,
     &           indxPhy1=indxNO3+3,
     &           indxZoo1=indxNO3+4,
     &           indxDet1=indxNO3+5, indxDet2=indxNO3+6)
#  elif defined BIO_BioEBUS
      integer indxNO3, indxNO2, indxNH4,
     &        indxPhy1, indxPhy2, indxZoo1, indxZoo2,
     &        indxDet1, indxDet2, indxDON, indxO2
      parameter (indxNO3 =indxT+ntrc_salt+ntrc_pas+1, 
     &           indxNO2 =indxNO3+1,      
     &           indxNH4 =indxNO3+2, 
     &           indxPhy1=indxNO3+3, indxPhy2=indxNO3+4,
     &           indxZoo1=indxNO3+5, indxZoo2=indxNO3+6,
     &           indxDet1=indxNO3+7, indxDet2=indxNO3+8,
     &           indxDON =indxNO3+9, indxO2  =indxNO3+10)
#     ifdef NITROUS_OXIDE 
      integer indxN2O
      parameter (indxN2O=indxNO3+11)      
#     endif
#  endif
# endif /* BIOLOGY */
# ifdef SEDIMENT
      integer, dimension(NGRAV) ::  indxGRAV
     & =(/(iloop,iloop=indxT+ntrc_salt+ntrc_pas+ntrc_bio+1,
     &  indxT+ntrc_salt+ntrc_pas+ntrc_bio+NGRAV)/)
      integer, dimension(NSAND) :: indxSAND 
     & =(/(iloop,iloop=indxT+ntrc_salt+ntrc_pas+ntrc_bio+1+NGRAV,
     &  indxT+ntrc_salt+ntrc_pas+ntrc_bio+NGRAV+NSAND)/)
      integer, dimension(NMUD)   :: indxMUD
     & =(/(iloop,iloop=indxT+ntrc_salt+ntrc_pas+ntrc_bio+1+NGRAV+NSAND,
     &  indxT+ntrc_salt+ntrc_pas+ntrc_bio+NGRAV+NSAND+NMUD)/)
# endif

# if(!defined ANA_BSEDIM  && !defined SEDIMENT)
      integer indxBSD, indxBSS
      parameter (indxBSD=indxT+ntrc_salt+ntrc_pas+ntrc_bio+1,
     &           indxBSS=101)
# endif

# ifdef DIAGNOSTICS_TS
      integer indxTXadv,indxTYadv,indxTVadv, 
     &        indxTHmix,indxTVmix,indxTForc,indxTrate
#  if defined DIAGNOSTICS_TS_MLD
     &      , indxTXadv_mld,indxTYadv_mld,indxTVadv_mld,
     &        indxTHmix_mld,indxTVmix_mld,indxTForc_mld,indxTrate_mld,
     &        indxTentr_mld
#  endif
      parameter (indxTXadv=indxT+ntrc_salt+ntrc_pas+ntrc_bio+ntrc_sed+1,
     &           indxTYadv=indxTXadv+NT,
     &           indxTVadv=indxTYadv+NT,
     &           indxTHmix=indxTVadv+NT,
     &           indxTVmix=indxTHmix+NT, 
     &           indxTForc=indxTVmix+NT,
     &           indxTrate=indxTForc+NT
#  if defined DIAGNOSTICS_TS_MLD
     &          ,indxTXadv_mld=indxTrate+NT,
     &           indxTYadv_mld=indxTXadv_mld+NT,
     &           indxTVadv_mld=indxTYadv_mld+NT,
     &           indxTHmix_mld=indxTVadv_mld+NT,
     &           indxTVmix_mld=indxTHmix_mld+NT,
     &           indxTForc_mld=indxTVmix_mld+NT,
     &           indxTrate_mld=indxTForc_mld+NT,
     &           indxTentr_mld=indxTrate_mld+NT  
#  endif
     &                                         )
# endif
# ifdef DIAGNOSTICS_UV
      integer indxMXadv,indxMYadv,indxMVadv,indxMCor,
     &        indxMPrsgrd,indxMHmix,indxMVmix,indxMrate
      parameter (indxMXadv=indxT+ntrc_salt+ntrc_pas+ntrc_bio+ntrc_sed
     &                                                  +ntrc_diats+1,
     &           indxMYadv=indxMXadv+2,
     &           indxMVadv=indxMYadv+2,
     &           indxMCor=indxMVadv+2,
     &           indxMPrsgrd=indxMCor+2,
     &           indxMHmix=indxMPrsgrd+2,
     &           indxMVmix=indxMHmix+2, 
     &           indxMrate=indxMVmix+2)
# endif
# if defined BIOLOGY && defined DIAGNOSTICS_BIO
      integer indxbioFlux, indxbioVSink  
#  if (defined BIO_NChlPZD && defined OXYGEN) || defined BIO_BioEBUS
     &        , indxGasExcFlux
#  endif
      parameter (indxbioFlux=indxT+ntrc_salt+ntrc_pas+ntrc_bio+ntrc_sed
     &                                        +ntrc_diats+ntrc_diauv+1)
      parameter (indxbioVSink=indxbioFlux+NumFluxTerms)

#  if (defined BIO_NChlPZD && defined OXYGEN) || defined BIO_BioEBUS
      parameter (indxGasExcFlux=indxbioFlux+NumFluxTerms+NumVSinkTerms)
#  endif
# endif /* BIOLOGY && DIAGNOSTICS_BIO */

      integer indxO, indxW, indxR, indxVisc, indxDiff, indxAkv, indxAkt
      parameter (indxO=indxT+ntrc_salt+ntrc_pas+ntrc_bio+ntrc_sed
     &                      +ntrc_diats+ntrc_diauv+ntrc_diabio+1,
     &           indxW=indxO+1, indxR=indxO+2, indxVisc=indxO+3,
     &           indxDiff=indxO+4,indxAkv=indxO+5, indxAkt=indxO+6)
# ifdef BIOLOGY
#  ifdef BIO_BioEBUS
      integer indxAOU, indxWIND10
      parameter (indxAOU=indxAkt+1,
     &           indxWIND10=indxAkt+2)      
#     ifdef CARBON
      integer indxpCO2
      parameter (indxpCO2=indxAkt+3)       
#     endif
#  endif
# endif
/*------------------------------------------------------------------------*/
# ifdef SALINITY
      integer indxAks
      parameter (indxAks=indxAkt+4)
# endif
# ifdef LMD_SKPP
      integer indxHbl
      parameter (indxHbl=indxAkt+5)
# endif
# ifdef LMD_BKPP
      integer indxHbbl
      parameter (indxHbbl=indxAkt+6)
# endif
# ifdef GLS_MIXING
      integer indxAkk
      parameter (indxAkk=indxAkt+7)
      integer indxAkp
      parameter (indxAkp=indxAkt+8)
      integer indxTke
      parameter (indxTke=indxAkt+9)
      integer indxGls
      parameter (indxGls=indxAkt+10)
      integer indxLsc
      parameter (indxLsc=indxAkt+11)
# endif
#endif

      integer indxSSH
#if defined BIOLOGY && !defined PISCES
      integer indxHel
# if (defined BIO_NChlPZD || defined BIO_N2ChlPZD2)
     &      , indxChC
#  ifdef OXYGEN
     &      , indxU10, indxKvO2, indxO2sat
#  endif
# endif
#endif /* BIOLOGY*/
#ifdef SOLVE3D
# if defined BIOLOGY && !defined PISCES
      parameter (indxHel=indxAkt+12)
#  if (defined BIO_NChlPZD || defined BIO_N2ChlPZD2)
      parameter (indxChC=indxHel+1)
#   ifdef OXYGEN
      parameter (indxU10=indxChC+1)
      parameter (indxKvO2=indxU10+1)
      parameter (indxO2sat=indxKvO2+1)
      parameter (indxSSH=indxO2sat+1)
#   else
      parameter (indxSSH=indxChC+1)
#   endif
#  else
      parameter (indxSSH=indxHel+1)
#  endif
# else
      parameter (indxSSH=indxAkt+12)
# endif
#else
# if defined BIOLOGY && !defined PISCES
      parameter (indxHel=indxVb+1)
#  ifdef BIO_NChlPZD
      parameter (indxChC=indxHel+1)
#   ifdef OXYGEN
      parameter (indxU10=indxChC+1)
      parameter (indxKvO2=indxU10+1)
      parameter (indxO2sat=indxKvO2+1)
      parameter (indxSSH=indxO2sat+1)
#   else
      parameter (indxSSH=indxChC+1)
#   endif
#  endif
# else
      parameter (indxSSH=indxVb+1)
# endif
#endif /* SOLVE3D */
      integer indxSUSTR, indxSVSTR
      parameter (indxSUSTR=indxSSH+1, indxSVSTR=indxSSH+2)
      integer indxTime2
      parameter (indxTime2=indxSSH+3)
#ifdef SOLVE3D
      integer indxShflx, indxShflx_rsw
      parameter (indxShflx=indxSSH+4)
# ifdef SALINITY
      integer indxSwflx
      parameter (indxSwflx=indxShflx+1, indxShflx_rsw=indxShflx+2)
# else
      parameter (indxShflx_rsw=indxShflx+1)
# endif
      integer indxSST, indxdQdSST
      parameter (indxSST=indxShflx_rsw+1, indxdQdSST=indxShflx_rsw+2)
# if defined SALINITY && defined SFLX_CORR
      integer indxSSS
      parameter (indxSSS=indxSST+2)
# endif
# ifdef BULK_FLUX
      integer indxWSPD,indxTAIR,indxRHUM,indxRADLW,indxRADSW,
     &        indxPRATE,indxUWND,indxVWND
      parameter (indxWSPD=indxSST+3,  indxTAIR=indxSST+4,
     &           indxRHUM=indxSST+5,  indxRADLW=indxSST+6,
     &           indxRADSW=indxSST+7, indxPRATE=indxSST+8,
     &           indxUWND=indxSST+9,  indxVWND=indxSST+10)
      integer indxShflx_rlw,indxShflx_lat,indxShflx_sen 
      parameter (indxShflx_rlw=indxSST+12,
     &           indxShflx_lat=indxSST+13, indxShflx_sen=indxSST+14)
# endif
#endif /* SOLVE3D */

      integer indxWstr
      parameter (indxWstr=indxSUSTR+21)
      integer indxUWstr
      parameter (indxUWstr=indxSUSTR+22)
      integer indxVWstr
      parameter (indxVWstr=indxSUSTR+23)
      integer indxBostr
      parameter (indxBostr=indxSUSTR+24)
#ifdef SOLVE3D
# ifdef SEDIMENT
      integer indxSed, indxBTHK, indxBPOR 
      parameter (indxSed=indxSUSTR+28,
     &           indxBTHK=indxSed, indxBPOR=indxSed+1)
      integer, dimension(NST) :: indxBFRA    
     & =(/(iloop,iloop=indxSed+2,indxSed+1+NST)/)
#  ifdef SUSPLOAD
      integer, dimension(NST):: indxDFLX
     & =(/(iloop,iloop=indxSed+2+NST,indxSed+1+2*NST)/)
      integer, dimension(NST) ::indxEFLX
     & =(/(iloop,iloop=indxSed+2+2*NST,indxSed+1+3*NST)/)
#  endif
#  ifdef BEDLOAD
      integer, dimension(NST):: indxBDLU
     & =(/(iloop,iloop=indxSed+2+3*NST,indxSed+1+4*NST)/)
      integer, dimension(NST) ::indxBDLV
     & =(/(iloop,iloop=indxSed+2+4*NST,indxSed+1+5*NST)/)
#  endif
# endif
# ifdef SST_SKIN
      integer indxSST_skin
      parameter (indxSST_skin=indxSUSTR+31)
# endif 
#endif /* SOLVE3D */

#ifdef BBL
      integer indxBBL, indxAbed, indxHrip, indxLrip, indxZbnot, 
     &        indxZbapp, indxBostrw
# ifdef SEDIMENT 
      parameter (indxBBL=indxSUSTR+32+6*NST,
# else
      parameter (indxBBL=indxSUSTR+32, 
# endif
     &           indxAbed  =indxBBL,    indxHrip  =indxAbed+1,
     &           indxLrip  =indxAbed+2, indxZbnot =indxAbed+3, 
     &           indxZbapp =indxAbed+4, indxBostrw=indxAbed+5)
# ifndef ANA_WWAVE
      integer indxWWA,indxWWD,indxWWP,indxWEB
      parameter (indxWWA=indxAbed+6, indxWWD=indxWWA+1, 
     &           indxWWP=indxWWA+2
#  ifdef MRL_WCI
     &          ,indxWEB=indxWWA+3
#  endif
     &                             )                                            
# endif
# ifndef ANA_BSEDIM
# endif
#else /* BBL */
      integer indxWWA,indxWWD,indxWWP,indxWEB
      parameter (indxWWA=indxSUSTR+32, indxWWD=indxWWA+1, 
     &           indxWWP=indxWWA+2
#  ifdef MRL_WCI
     &          ,indxWEB=indxWWA+3
#  endif
     &                             )                                            
#endif  /* BBL */

#if defined MRL_WCI || defined OW_COUPLING
      integer indxSUP, indxUST2D,indxVST2D
# ifdef SEDIMENT 
      parameter (indxSUP=indxSUSTR+44+6*NST,
# else
      parameter (indxSUP  =indxSUSTR+44,
# endif      
     &           indxUST2D =indxSUP+1, indxVST2D=indxSUP+2)
# ifdef SOLVE3D
      integer indxUST,indxVST,indxWST,indxAkb,indxAkw,indxKVF,
     &        indxCALP,indxKAPS
      parameter (indxUST=indxSUP+3, indxVST=indxSUP+4,
     &           indxWST=indxSUP+5, indxAkb=indxSUP+6,
     &           indxAkw=indxSUP+7, indxKVF=indxSUP+8,
     &           indxCALP=indxSUP+9, indxKAPS=indxSUP+10)
# endif
# ifdef DIAGNOSTICS_UV
      integer indxMvf,indxMbrk,indxMStCo,indxMVvf,
     &        indxMPrscrt,indxMsbk,indxMbwf,indxMfrc 
      parameter (indxMvf=indxKAPS+1,indxMbrk=indxMvf+2,
     &           indxMStCo=indxMvf+4,indxMVvf=indxMvf+6,
     &           indxMPrscrt=indxMvf+8,indxMsbk=indxMvf+10,
     &           indxMbwf=indxMvf+12,indxMfrc=indxMvf+14) 
# endif
# if defined WKB_WWAVE || defined OW_COUPLING
      integer indxHRM,indxFRQ,indxWAC, indxWKX,indxWKE, indxEPB
     &       ,indxEPD,indxWAR,indxEPR
      parameter (indxHRM=indxSUP+30,
     &           indxFRQ=indxHRM+1, indxWAC=indxHRM+2,
     &           indxWKX=indxHRM+3, indxWKE=indxHRM+4,
     &           indxEPB=indxHRM+5, indxEPD=indxHRM+6,
     &           indxWAR=indxHRM+7, indxEPR=indxHRM+8 )
# endif
#endif  /* MRL_WCI */

#ifdef PSOURCE_NCFILE
      integer indxQBAR
      parameter (indxQBAR=indxSUSTR+80)
# ifdef PSOURCE_NCFILE_TS
      integer indxTsrc
      parameter (indxTsrc=indxSUSTR+81)
# endif
#endif /* PSOURCE_NCFILE */
#ifdef DIURNAL_INPUT_SRFLX
      integer indxShflx_rswbio
      parameter (indxShflx_rswbio=indxSUSTR+82)
#endif
#ifdef ICE
      integer indxAi
      parameter (indxAi=????)
      integer indxUi, indxVi, indxHi, indxHS, indxTIsrf
      parameter (indxUi=indxAi+1, indxVi=indxAi+2, indxHi=indxAi+3,
     &                         indxHS=indxAi+4, indxTIsrf=indxAi+5)
#endif
!
!
!===================================================================
!
!===================================================================
!
! Grid Type Codes:  r2dvar....w3hvar are codes for array types.
! ==== ==== ======  The codes are set according to the rule:
!                     horiz_grid_type+4*vert_grid_type
!    where horiz_grid_type=0,1,2,3 for RHO-,U-,V-,PSI-points
!    respectively and vert_grid_type=0 for 2D fields; 1,2 for
!    3D-RHO- and W-vertical points.

!
      integer r2dvar, u2dvar, v2dvar, p2dvar, r3dvar,
     &                u3dvar, v3dvar, p3dvar, w3dvar, b3dvar
      parameter (r2dvar=0, u2dvar=1, v2dvar=2, p2dvar=3,
     & r3dvar=4, u3dvar=5, v3dvar=6, p3dvar=7, w3dvar=8,b3dvar=12) 

!            Horizontal array dimensions in netCDF files.
! xi_rho     WARNING!!! In MPI code in the case of PARALLEL_FILES 
! xi_u       _and_ NON-Periodicity in either XI- or ETA-direction,
! eta_rho    these depend on corresonding MPI-node indices ii,jj
! eta_v      and therefore become live variables, which are placed
!            into common block below rather than defined here as
!            parameters. 
!
! Note (P. Marchesiello): 
!   the remark above is now extended to periodic conditions, i.e., 
!   if PARALLEL_FILES is defined, netCDF files array dimensions are 
!   always set in MPI-Setup and depend on MPI-nodes. After rejoining 
!   the parallel files (ncjoin), the resulting global netCDF file has
!   the same dimension as it would have if PARALLEL_FILES was undefined.
!
      integer xi_rho,xi_u, eta_rho,eta_v    
#ifndef AGRIF
# if defined MPI && defined PARALLEL_FILES
!#  ifdef EW_PERIODIC
!      parameter (xi_rho=Lm,     xi_u=Lm)
!#  endif
!#  ifdef NS_PERIODIC
!      parameter (eta_rho=Mm,    eta_v=Mm)
!#  endif
# else
      parameter (xi_rho=LLm+2,  xi_u=xi_rho-1,
     &           eta_rho=MMm+2, eta_v=eta_rho-1)
# endif
#else
# if defined MPI && defined PARALLEL_FILES
!#  ifdef EW_PERIODIC
!      common/netCDFhorizdim1/xi_rho,xi_u
!#  endif
!#  ifdef NS_PERIODIC
!      common/netCDFhorizdim2/eta_rho,eta_v
!#  endif
# else
      common/netCDFhorizdim/xi_rho,xi_u, eta_rho,eta_v
# endif
#endif /* AGRIF */
!
!====================================================================
! Naming conventions for indices, variable IDs, etc...
!
! prefix ncid_  means netCDF ID for netCDF file
!        nrec_  record number in netCDF file since initialization
!        nrpf_  maximum number of records per file  (output netCDF
!                                                       files only)
! prefix/ending rst_/_rst refers to restart  netCDF file
!               his_/_his           history
!               avg_/_avg           averages
!                    _frc           forcing
!                    _clm           climatology
!                    _qbar          river runoff
!
! endings refer to:  ___Time  time [in seconds]
!                    ___Tstep time step numbers and record numbers
!   all objects      ___Z     free-surface
!   with these       ___Ub    vertically integrated 2D U-momentum
!   endings are      ___Vb    vertically integrated 2D V-momentum
!   either
!     netCDF IDs,    ___U     3D U-momentum
!     if occur with  ___V     3D V-momentum
!     prefices rst/  ___T(NT) tracers
!     /his/avg       ___R     density anomaly
!   or               ___O     omega vertical velocity
!     parameter      ___W     true vertical velocity
!     indices, if
!     occur with     ___Akv   vertical viscosity coefficient
!     prefix indx    ___Akt   vertical T-diffusion coefficient
!     (see above).   ___Aks   vertical S-diffusion coefficient
!                    ___Hbl   depth of mixed layer LMD_SKPP.
!
! Sizes of unlimited time dimensions in netCDF files:
!
!   ntsms   surface momentum stress in current forcing file.
!   ntbulk   bulk formulation in current forcing file.
!   ntsrf   shortwave radiation flux in current forcing file.
!   ntssh   sea surface height in current climatology file.
!   ntsst   sea surface temperature in current forcing file.
!   ntsss   sea surface salinity in current forcing file.
!   ntstf   surface flux of tracers in current forcing file.
!   nttclm  tracer variables in current climatology file.
!   ntuclm  momentum variables in current climatology file.
!   ntww    wind induced wave data in current forcing file.
!   ntbulkn bulk formula variables in current forcing file.
!   ntqbar   river runoff in current forcing file.
!
! vname    character array for variable names and attributes;
!=================================================================
!
      integer ncidfrc, ncidbulk, ncidclm,  ntsms ,
     &        ntsrf,  ntssh,  ntsst, ntsss, ntuclm,
     &        ntbulk, ncidqbar, ntqbar, ntww
#ifdef SOLVE3D
      integer nttclm(NT), ntstf(NT), nttsrc(NT)
#endif
      integer ncidrst, nrecrst,  nrpfrst
     &      , rstTime, rstTime2, rstTstep, rstZ,    rstUb,  rstVb
#ifdef SOLVE3D
     &                         , rstU,    rstV
      integer rstT(NT)
# ifdef SEDIMENT
      integer rstSed(NST+2)
# endif	
#endif
#ifdef BBL
      integer rstBBL(2)
#endif
#if defined WKB_WWAVE || defined OW_COUPLING
      integer rstWKB(3),hisWKB(9)
      common /ncvars/ rstWKB,hisWKB
#endif
#ifdef MRL_WCI
      integer hisSUP, hisUST2D, hisVST2D
      common /ncvars/ hisSUP, hisUST2D, hisVST2D
# ifdef SOLVE3D
      integer hisUST, hisVST, hisAkb, hisAkw, hisKVF,
     &        hisCALP, hisKAPS, hisWST
      common /ncvars/ hisUST, hisVST, hisAkb, hisAkw, hisKVF,
     &        hisCALP, hisKAPS, hisWST
# endif
#endif

      integer  ncidhis, nrechis,  nrpfhis
     &      , hisTime, hisTime2, hisTstep, hisZ,    hisUb,  hisVb
     &      , hisBostr, hisWstr, hisUWstr, hisVWstr
     &      , hisShflx, hisSwflx, hisShflx_rsw
# ifdef MOVING_BATHY
     &      , hisHm
# endif
# ifdef BBL
     &      , hisBBL(6)
# endif
#ifdef SOLVE3D
     &      , hisU,   hisV,   hisR,    hisHbl, hisHbbl
     &      , hisO,   hisW,   hisVisc, hisDiff
     &      , hisAkv, hisAkt, hisAks
# ifdef GLS_MIXING
     &      , hisAkk, hisAkp, hisTke, hisGls, hisLsc
# endif
# ifdef BULK_FLUX
     &      , hisShflx_rlw
     &      , hisShflx_lat,   hisShflx_sen
# endif
# ifdef SST_SKIN
     &      , hisSST_skin
# endif
# ifdef BIOLOGY
     &      , hisHel
#  ifdef BIO_NChlPZD
     &      , hisChC
#   ifdef OXYGEN
     &      , hisU10, hisKvO2, hisO2sat
#   endif
#  elif defined BIO_BioEBUS 
      integer hisAOU, hisWIND10
#  endif
# endif  /* BIOLOGY */
      integer hisT(NT)
# ifdef SEDIMENT
      integer hisSed(NST+2
#  ifdef SUSPLOAD
     &      +2*NST
#   endif   
#  ifdef BEDLOAD
     &      +2*NST
#   endif   
     & )
# endif
# if defined DIAGNOSTICS_TS 
      integer nciddia, nrecdia, nrpfdia
     &      , diaTime, diaTime2, diaTstep
     &      , diaTXadv(NT), diaTYadv(NT), diaTVadv(NT)
     &      , diaTHmix(NT), diaTVmix(NT)
     &      , diaTForc(NT), diaTrate(NT)
#  if defined DIAGNOSTICS_TS_MLD
     &      , diaTXadv_mld(NT), diaTYadv_mld(NT), diaTVadv_mld(NT)
     &      , diaTHmix_mld(NT), diaTVmix_mld(NT)
     &      , diaTForc_mld(NT), diaTrate_mld(NT), diaTentr_mld(NT)
#  endif
# endif
# ifdef DIAGNOSTICS_UV
        integer nciddiaM, nrecdiaM, nrpfdiaM
     &      , diaTimeM,diaTime2M, diaTstepM
     &      , diaMXadv(2), diaMYadv(2), diaMVadv(2)
     &      , diaMCor(2), diaMPrsgrd(2), diaMHmix(2)
     &      , diaMVmix(2), diaMrate(2)
#  ifdef MRL_WCI
     &      , diaMvf(2), diaMbrk(2), diaMStCo(2)
     &      , diaMVvf(2), diaMPrscrt(2), diaMsbk(2)
     &      , diaMbwf(2), diaMfrc(2)
#  endif
# endif
# ifdef DIAGNOSTICS_BIO
      integer nciddiabio, nrecdiabio, nrpfdiabio
     &      , diaTimebio, diaTime2bio, diaTstepbio
     &      , diabioFlux(NumFluxTerms)
     &      , diabioVSink(NumVSinkTerms)
     &      , diabioGasExc(NumGasExcTerms)
# endif
#elif defined DIAGNOSTICS_UV && defined MRL_WCI
     &      , diaMvf(2), diaMbrk(2), diaMStCo(2)
     &      , diaMVvf(2), diaMPrscrt(2), diaMsbk(2)
     &      , diaMbwf(2), diaMfrc(2)
#endif /* SOLVE3D */
#ifdef AVERAGES
      integer ncidavg, nrecavg,  nrpfavg
     &      , avgTime, avgTime2, avgTstep, avgZ, avgUb,  avgVb
     &      , avgBostr, avgWstr, avgUwstr, avgVwstr
     &      , avgShflx, avgSwflx, avgShflx_rsw
# ifdef SOLVE3D
     &      , avgU,   avgV,   avgR,    avgHbl, avgHbbl
     &      , avgO,   avgW,   avgVisc, avgDiff
     &      , avgAkv, avgAkt, avgAks
# ifdef GLS_MIXING
     &      , avgAkk, avgAkp, avgTke, avgGls, avgLsc
# endif
# ifdef BIOLOGY
     &      , avgHel
#  ifdef BIO_NChlPZD
     &      , avgChC
#   ifdef OXYGEN
     &      , avgU10, avgKvO2, avgO2sat
#   endif
#  elif defined BIO_BioEBUS
      integer avgAOU, avgWIND10
#  endif
# endif  /* BIOLOGY */
      integer avgT(NT)
#  ifdef BULK_FLUX
      integer avgShflx_rlw
     &      , avgShflx_lat,   avgShflx_sen
#  endif
#  ifdef SST_SKIN
      integer avgSST_skin
#  endif
#  ifdef SEDIMENT
      integer avgSed(NST+2)
#  endif
# endif
# ifdef BBL
      integer avgBBL(6)
# endif
# if defined WKB_WWAVE || defined OW_COUPLING
      integer avgWKB(9)
# endif
# ifdef MRL_WCI
      integer avgSUP, avgUST2D, avgVST2D
#  ifdef SOLVE3D
      integer avgUST, avgVST, avgAkb, avgAkw, avgKVF,
     &        avgCALP, avgKAPS, avgWST
#  endif
# endif
# ifdef SOLVE3D
#  if defined DIAGNOSTICS_TS
      integer nciddia_avg, nrecdia_avg, nrpfdia_avg
     &      , diaTime_avg, diaTime2_avg, diaTstep_avg
     &      , diaTXadv_avg(NT), diaTYadv_avg(NT), diaTVadv_avg(NT)
     &      , diaTHmix_avg(NT), diaTVmix_avg(NT)
     &      , diaTForc_avg(NT), diaTrate_avg(NT)
#   if defined DIAGNOSTICS_TS_MLD
     &      , diaTXadv_mld_avg(NT), diaTYadv_mld_avg(NT)
     &      , diaTVadv_mld_avg(NT)
     &      , diaTHmix_mld_avg(NT), diaTVmix_mld_avg(NT)
     &      , diaTForc_mld_avg(NT), diaTrate_mld_avg(NT)
     &      , diaTentr_mld_avg(NT)
#   endif
#  endif
#  ifdef DIAGNOSTICS_UV
       integer nciddiaM_avg, nrecdiaM_avg, nrpfdiaM_avg
     &      , diaTimeM_avg, diaTime2M_avg, diaTstepM_avg
     &      , diaMXadv_avg(2), diaMYadv_avg(2), diaMVadv_avg(2)
     &      , diaMCor_avg(2), diaMPrsgrd_avg(2), diaMHmix_avg(2)
     &      , diaMVmix_avg(2), diaMrate_avg(2)
#  endif
#  ifdef DIAGNOSTICS_BIO
      integer nciddiabio_avg, nrecdiabio_avg, nrpfdiabio_avg
     &      , diaTimebio_avg, diaTime2bio_avg, diaTstepbio_avg
     &      , diabioFlux_avg(NumFluxTerms)
     &      , diabioVSink_avg(NumVSinkTerms)
     &      , diabioGasExc_avg(NumGasExcTerms)
#  endif
# endif /* SOLVE3D */
# ifdef MRL_WCI
     &      , diaMvf_avg(2), diaMbrk_avg(2), diaMStCo_avg(2)
     &      , diaMVvf_avg(2), diaMPrscrt_avg(2), diaMsbk_avg(2)
     &      , diaMbwf_avg(2), diaMfrc_avg(2)
# endif
#endif /* AVERAGES */
 
#ifdef SOLVE3D
# define NWRTHIS 500+NT
#else
# define NWRTHIS 90
#endif
      logical wrthis(NWRTHIS)
#ifdef AVERAGES
     &      , wrtavg(NWRTHIS)
#endif
#if defined DIAGNOSTICS_TS 
     &      , wrtdia3D(NT+1)
     &      , wrtdia2D(NT+1)
# ifdef AVERAGES
     &      , wrtdia3D_avg(NT+1)
     &      , wrtdia2D_avg(NT+1)
# endif
#endif
#if defined DIAGNOSTICS_UV
     &      , wrtdiaM(3)
# ifdef AVERAGES
     &      , wrtdiaM_avg(3)
# endif
#endif
#ifdef DIAGNOSTICS_BIO
     &      , wrtdiabioFlux(NumFluxTerms+1)
     &      , wrtdiabioVSink(NumVSinkTerms+1)
     &      , wrtdiabioGasExc(NumGasExcTerms+1)
# ifdef AVERAGES
     &      , wrtdiabioFlux_avg(NumFluxTerms+1)
     &      , wrtdiabioVSink_avg(NumVSinkTerms+1)
     &      , wrtdiabioGasExc_avg(NumGasExcTerms+1)
# endif
#endif
	
      common/incscrum/
     &        ncidfrc, ncidbulk,ncidclm, ntsms, ntsrf, ntssh, ntsst
     &      , ntuclm, ntsss, ntbulk, ncidqbar, ntqbar, ntww 
#if defined MPI && defined PARALLEL_FILES
!# ifndef EW_PERIODIC
     &      , xi_rho,  xi_u
!# endif
!# ifndef NS_PERIODIC
     &      , eta_rho, eta_v
!# endif
#endif
#ifdef SOLVE3D
     &                        ,  nttclm, ntstf, nttsrc
#endif
     &      , ncidrst, nrecrst,  nrpfrst
     &      , rstTime, rstTime2, rstTstep, rstZ,    rstUb,  rstVb
#ifdef SOLVE3D
     &                         , rstU,    rstV,   rstT
# ifdef SEDIMENT
     &                         , rstSed
# endif
#endif
#ifdef BBL
     &                         , rstBBL
#endif
     &      , ncidhis, nrechis,  nrpfhis
     &      , hisTime, hisTime2, hisTstep, hisZ,    hisUb,  hisVb
     &      , hisBostr, hisWstr, hisUWstr, hisVWstr
     &      , hisShflx, hisSwflx, hisShflx_rsw
# ifdef MOVING_BATHY
     &      , hisHm
# endif
#ifdef SOLVE3D
     &      , hisU,    hisV,     hisT,    hisR
     &      , hisO,    hisW,     hisVisc, hisDiff
     &      , hisAkv,  hisAkt,   hisAks
     &      , hisHbl,  hisHbbl
# ifdef GLS_MIXING
     &      , hisAkk, hisAkp, hisTke, hisGls, hisLsc
# endif
# ifdef BULK_FLUX
     &      , hisShflx_rlw
     &      , hisShflx_lat, hisShflx_sen
#  endif
# ifdef SST_SKIN
     &      , hisSST_skin
# endif
# ifdef BIOLOGY
     &      , hisHel
#  ifdef BIO_NChlPZD
     &      , hisChC
#   ifdef OXYGEN
     &      , hisU10, hisKvO2, hisO2sat
#   endif
#  elif defined BIO_BioEBUS
     &      , hisAOU, hisWIND10
#  endif
# endif  /* BIOLOGY */
# ifdef SEDIMENT
     &      , hisSed
# endif
#endif
#ifdef BBL
     &      , hisBBL
#endif
#if defined DIAGNOSTICS_TS
     &      , nciddia, nrecdia, nrpfdia
     &      , diaTime, diaTime2, diaTstep
     &      , diaTXadv, diaTYadv, diaTVadv, diaTHmix
     &      , diaTVmix, diaTForc, diaTrate
# if defined DIAGNOSTICS_TS_MLD
     &      , diaTXadv_mld, diaTYadv_mld, diaTVadv_mld, diaTHmix_mld
     &      , diaTVmix_mld, diaTForc_mld, diaTrate_mld, diaTentr_mld
# endif
# ifdef AVERAGES
     &      , nciddia_avg, nrecdia_avg, nrpfdia_avg
     &      , diaTime_avg, diaTime2_avg, diaTstep_avg
     &      , diaTXadv_avg, diaTYadv_avg, diaTVadv_avg
     &      , diaTHmix_avg, diaTVmix_avg, diaTForc_avg
     &      , diaTrate_avg
#  if defined DIAGNOSTICS_TS_MLD
     &      , diaTXadv_mld_avg, diaTYadv_mld_avg, diaTVadv_mld_avg
     &      , diaTHmix_mld_avg, diaTVmix_mld_avg, diaTForc_mld_avg
     &      , diaTrate_mld_avg, diaTentr_mld_avg
#  endif
# endif
#endif
#ifdef DIAGNOSTICS_UV
     &      , nciddiaM, nrecdiaM, nrpfdiaM
     &      , diaTimeM, diaTime2M, diaTstepM
     &      , diaMXadv, diaMYadv, diaMVadv, diaMCor
     &      , diaMPrsgrd, diaMHmix, diaMVmix, diaMrate
# ifdef MRL_WCI
     &      , diaMvf, diaMbrk, diaMStCo
     &      , diaMVvf, diaMPrscrt, diaMsbk
     &      , diaMbwf, diaMfrc
# endif
# ifdef AVERAGES
     &      , nciddiaM_avg, nrecdiaM_avg, nrpfdiaM_avg
     &      , diaTimeM_avg, diaTime2M_avg, diaTstepM_avg
     &      , diaMXadv_avg, diaMYadv_avg, diaMVadv_avg
     &      , diaMCor_avg, diaMPrsgrd_avg, diaMHmix_avg
     &      , diaMVmix_avg, diaMrate_avg
#  ifdef MRL_WCI
     &      , diaMvf_avg, diaMbrk_avg, diaMStCo_avg
     &      , diaMVvf_avg, diaMPrscrt_avg, diaMsbk_avg
     &      , diaMbwf_avg,diaMfrc_avg
#  endif
# endif
#endif
#ifdef DIAGNOSTICS_BIO
     &      , nciddiabio, nrecdiabio, nrpfdiabio
     &      , diaTimebio, diaTime2bio, diaTstepbio, diabioFlux
     &      , diabioVSink
     &      , diabioGasExc
# ifdef AVERAGES
     &      , nciddiabio_avg, nrecdiabio_avg, nrpfdiabio_avg
     &      , diaTimebio_avg, diaTime2bio_avg, diaTstepbio_avg 
     &      , diabioFlux_avg
     &      , diabioVSink_avg
     &      , diabioGasExc_avg
# endif
#endif 
#ifdef AVERAGES
     &      , ncidavg,  nrecavg,  nrpfavg
     &      , avgTime, avgTime2, avgTstep, avgZ,    avgUb,  avgVb
     &      , avgBostr, avgWstr, avgUWstr, avgVWstr
     &      , avgShflx, avgSwflx, avgShflx_rsw
# ifdef SOLVE3D
     &      , avgU,    avgV,     avgT,     avgR
     &      , avgO,    avgW,     avgVisc,  avgDiff
     &      , avgAkv,  avgAkt,   avgAks
     &      , avgHbl,  avgHbbl
#  ifdef GLS_MIXING
     &      , avgAkk, avgAkp, avgTke, avgGls, avgLsc
#  endif
#  ifdef BIOLOGY
     &      , avgHel
#   ifdef BIO_NChlPZD
     &      , avgChC
#    ifdef OXYGEN
     &      , avgU10, avgKvO2, avgO2sat
#    endif
#   elif defined BIO_BioEBUS 
     &      , avgAOU, avgWIND10
#   endif
#  endif  /* BIOLOGY */
#  ifdef BULK_FLUX
     &      , avgShflx_rlw
     &      , avgShflx_lat, avgShflx_sen
#  endif
#  ifdef SST_SKIN
     &      , avgSST_skin
#  endif
#  ifdef SEDIMENT
     &      , avgSed
#  endif
# endif
# ifdef BBL
     &      , avgBBL
# endif
# if defined WKB_WWAVE || defined OW_COUPLING
     &      , avgWKB
# endif
# ifdef MRL_WCI
     &      , avgSUP, avgUST2D, avgVST2D
#  ifdef SOLVE3D
     &      , avgUST, avgVST, avgAkb, avgAkw
     &      , avgKVF, avgCALP, avgKAPS, avgWST
#  endif
# endif
#endif
     &      , wrthis
#ifdef AVERAGES
     &      , wrtavg
#endif
#if defined DIAGNOSTICS_TS
     &      , wrtdia3D
     &      , wrtdia2D
# ifdef AVERAGES
     &      , wrtdia3D_avg
     &      , wrtdia2D_avg
# endif
#endif
#if defined DIAGNOSTICS_UV
     &      , wrtdiaM
# ifdef AVERAGES
     &      , wrtdiaM_avg
# endif
#endif
#ifdef DIAGNOSTICS_BIO
     &      , wrtdiabioFlux
     &      , wrtdiabioVSink
     &      , wrtdiabioGasExc
# ifdef AVERAGES
     &      , wrtdiabioFlux_avg
     &      , wrtdiabioVSink_avg
     &      , wrtdiabioGasExc_avg
# endif
#endif

      character*80 date_str, title, start_date
      character*80 ininame,  grdname,  hisname
     &         ,   rstname,  frcname,  bulkname,  usrname
     &         ,   qbarname, tsrcname
#ifdef AVERAGES
     &                                ,   avgname
#endif
#ifdef DIAGNOSTICS_TS 
     &                                ,  dianame
# ifdef AVERAGES
     &                                ,  dianame_avg
# endif
#endif
#ifdef DIAGNOSTICS_UV
     &                                ,  dianameM
# ifdef AVERAGES
     &                                ,  dianameM_avg
# endif
#endif
#ifdef DIAGNOSTICS_BIO
     &                                ,  dianamebio
# ifdef AVERAGES
     &                                ,  dianamebio_avg
# endif
#endif
#if (defined TCLIMATOLOGY  && !defined ANA_TCLIMA)\
 || (defined ZCLIMATOLOGY  && !defined ANA_SSH)\
 || (defined M2CLIMATOLOGY && !defined ANA_M2CLIMA)\
 || (defined M3CLIMATOLOGY && !defined ANA_M3CLIMA)
     &                                ,   clmname
#endif
#ifdef FRC_BRY 
     &                                ,   bry_file
#endif
#if defined WKB_WWAVE && !defined ANA_BRY_WKB
     &                                ,   brywkb_file
#endif
#ifdef ASSIMILATION
     &                      ,  aparnam,   assname
#endif
#ifdef BIOLOGY
     &                                ,   bioname
#endif
#ifdef SEDIMENT
     &                                ,   sedname
#endif

#ifdef SOLVE3D
      character*75  vname(20, 500)
#else
      character*75  vname(20, 90)
#endif

      common /cncscrum/       date_str,   title,  start_date
     &         ,   ininame,  grdname, hisname
     &         ,   rstname,  frcname, bulkname,  usrname
     &         ,   qbarname, tsrcname
#ifdef AVERAGES
     &                                ,  avgname
#endif
#if defined DIAGNOSTICS_TS
     &                                ,  dianame
# ifdef AVERAGES
     &                                ,  dianame_avg
# endif
#endif
#if defined DIAGNOSTICS_UV
     &                                ,  dianameM
# ifdef AVERAGES
     &                                ,  dianameM_avg
# endif
#endif
#ifdef DIAGNOSTICS_BIO
     &                                ,  dianamebio
# ifdef AVERAGES
     &                                ,  dianamebio_avg
# endif
#endif
#if (defined TCLIMATOLOGY  && !defined ANA_TCLIMA)\
 || (defined ZCLIMATOLOGY  && !defined ANA_SSH)\
 || (defined M2CLIMATOLOGY && !defined ANA_M2CLIMA)\
 || (defined M3CLIMATOLOGY && !defined ANA_M3CLIMA)
     &                                ,   clmname
#endif
#ifdef FRC_BRY
     &                                ,   bry_file
#endif
#if defined WKB_WWAVE && !defined ANA_BRY_WKB
     &                                ,   brywkb_file
#endif
#ifdef ASSIMILATION
     &                      ,  aparnam,   assname
#endif
#ifdef SEDIMENT
     &                      ,  sedname
#endif
#ifdef BIOLOGY
     &                      ,  bioname
#endif
     &                      ,  vname

