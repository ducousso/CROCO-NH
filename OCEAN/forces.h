! $Id: forces.h 1564 2014-06-24 17:39:21Z gcambon $
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
!  This is include file "forces.h"
!--------------------------------------------------------------------
!  SURFACE MOMENTUM FLUX (WIND STRESS):
!--------------------------------------------------------------------
!  sustr |  XI- and ETA-components of kinematic surface momentum flux
!  svstr |  (wind stresses) defined at horizontal U- and V-points.
!            dimensioned as [m^2/s^2].
!
      real sustr(GLOBAL_2D_ARRAY)
      real svstr(GLOBAL_2D_ARRAY)
      common /forces_sustr/sustr /forces_svstr/svstr
#ifndef ANA_SMFLUX
!
!  tsms      Time of surface momentum stresses.
!
!  sustrg |  Two-time level gridded data for XI- and ETA-componets
!  svstrg |  of kinematic surface momentum flux (wind stess).
!
!  sustrp |  Two-time level point data for XI- and ETA-componets 
!  svstrp |  of kinematic surface momentum flux (wind stess).
!
      real sustrg(GLOBAL_2D_ARRAY,2)
      real svstrg(GLOBAL_2D_ARRAY,2)
      common /smsdat_sustrg/sustrg /smsdat_svstrg/svstrg

      real    sustrp(2), svstrp(2), sms_time(2)
      real    sms_cycle, sms_scale 
      integer itsms, sms_ncycle, sms_rec, lsusgrd  
      integer lsvsgrd,sms_tid, susid, svsid
      common /smsdat1/ sustrp, svstrp, sms_time
      common /smsdat2/ sms_cycle, sms_scale
      common /smsdat3/ itsms, sms_ncycle, sms_rec, lsusgrd
      common /smsdat4/ lsvsgrd,sms_tid, susid, svsid

#  undef SMFLUX_DATA
#endif /* !ANA_SMFLUX */
!
!  BOTTOM MOMENTUM FLUX:
!--------------------------------------------------------------------
!  bustr |  XI- and ETA-components of kinematic bottom momentum flux
!  bvstr |  (drag) defined at horizontal U- and V-points [m^2/s^2].
!
      real bustr(GLOBAL_2D_ARRAY)
      real bvstr(GLOBAL_2D_ARRAY)
      common /forces_bustr/bustr /forces_bvstr/bvstr
#ifndef ANA_BMFLUX
!
!  tbms      Time of surface momentum stresses.
!
!  bustrg |  Two-time level gridded data for XI- and ETA-componets 
!  bvstrg |  of kinematic bottom momentum flux.
!
!  bustrp |  Two-time level point data for XI- and ETA-componets   
!  bvstrp |  of kinematic bottom momentum flux.
!
      real bustrg(GLOBAL_2D_ARRAY,2)
      real bvstrg(GLOBAL_2D_ARRAY,2)
      common /bmsdat_bustrg/bustrg /bmsdat_bvstrg/bvstrg

      real bms_tintrp(2), bustrp(2),    bvstrp(2), tbms(2)
      real bmsclen, bms_tstart, bms_tend,  tsbms, sclbms
      integer itbms,      bmstid,busid, bvsid,     tbmsindx
      logical bmscycle,   bms_onerec,   lbusgrd,   lbvsgrd
      common /bmsdat1/bms_tintrp, bustrp,       bvstrp,    tbms
      common /bmsdat2/bmsclen,    bms_tstart,   bms_tend,  tsbms,   sclbms
      common /bmsdat3/itbms,      bmstid,busid, bvsid,     tbmsindx
      common /bmsdat4/bmscycle,   bms_onerec,   lbusgrd,   lbvsgrd

#  undef BMFLUX_DATA
#endif /* !ANA_BMFLUX */
#ifdef SOLVE3D
!
!  SURFACE TRACER FLUXES: 
!--------------------------------------------------------------------
!  stflx   Kinematic surface fluxes of tracer type variables at
!          horizontal RHO-points. Physical dimensions [degC m/s] -
!          temperature; [PSU m/s] - salinity.
!
      real stflx(GLOBAL_2D_ARRAY,NT)
      common /forces_stflx/stflx
# ifdef BULK_FLUX
      real shflx_rsw(GLOBAL_2D_ARRAY)
      common /frc_shflx_rsw/shflx_rsw
      real shflx_rlw(GLOBAL_2D_ARRAY)
      common /frc_shflx_rlw/shflx_rlw
      real shflx_lat(GLOBAL_2D_ARRAY)
      common /frc_shflx_lat/shflx_lat
      real shflx_sen(GLOBAL_2D_ARRAY)
      common /frc_shflx_sen/shflx_sen
# endif
# ifdef SST_SKIN
      real sst_skin(GLOBAL_2D_ARRAY)
      common /frc_sst_skin/ sst_skin
      real dT_skin(GLOBAL_2D_ARRAY)
      common /frc_dT_skin/ dT_skin
# endif
# if !defined ANA_STFLUX || !defined ANA_SSFLUX
!
!  stflxg   Two-time level surface tracer flux grided data.
!  stflxp   Two-time level surface tracer flux point  data.
!  tstflx   Time of surface tracer flux.
!
      real stflxg(GLOBAL_2D_ARRAY,2,NT)
      common /stfdat_stflxg/stflxg

      real stflxp(2,NT), stf_time(2,NT)
      real stf_cycle(NT), stf_scale(NT)
      integer itstf(NT), stf_ncycle(NT), stf_rec(NT) 
      integer lstfgrd(NT), stf_tid(NT), stf_id(NT)
      common /stfdat1/ stflxp,  stf_time, stf_cycle, stf_scale
      common /stfdat2/ itstf, stf_ncycle, stf_rec, lstfgrd
      common /stfdat3/  stf_tid, stf_id
#   undef STFLUX_DATA
# endif /* !ANA_STFLUX || !ANA_SSFLUX */
!
!  BOTTOM TRACER FLUXES:
!--------------------------------------------------------------------
!  btflx  Kinematic bottom fluxes of tracer type variables at
!         horizontal RHO-points. Physical dimensions [degC m/s] -
!         temperature; [PSU m/s] - salinity.
!
      real btflx(GLOBAL_2D_ARRAY,NT)
      common /forces_btflx/btflx
# ifndef ANA_BTFLUX
!
!  btflxg   Two-time level bottom tracer flux grided data.
!  btflxp   Two-time level bottom tracer flux point data.
!  tbtflx   Time of bottom tracer flux.
!
      real btflxg(GLOBAL_2D_ARRAY,2,NT)
      common /btfdat_btflxg/btflxg

      real sclbtf(NT), btf_tstart(NT), btf_tend(NT)
      real btfclen(NT), tsbtf(NT)
      real btf_tintrp(2,NT), btflxp(2,NT),  tbtflx(2,NT)
      integer itbtf(NT), btfid(NT), btftid(NT),tbtfindx(NT)
      logical lbtfgrd(NT), btfcycle(NT), btf_onerec(NT)
      common /btfdat1/ sclbtf, btf_tstart, btf_tend, btfclen
      common /btfdat2/ tsbtf,  btf_tintrp,   btflxp,        tbtflx
      common /btfdat3/ itbtf,  btfid,        btftid,        tbtfindx
      common /btfdat4/ lbtfgrd, btfcycle,    btf_onerec

#   undef BTFLUX_DATA
# endif /* !ANA_BTFLUX */
#ifdef QCORRECTION
!
!  HEAT FLUX CORRECTION
!--------------------------------------------------------------------
!  dqdt     Kinematic surface net heat flux sensitivity to SST [m/s].
!  sst      Current sea surface temperature [degree Celsius].
!
      real dqdt(GLOBAL_2D_ARRAY)
      real sst(GLOBAL_2D_ARRAY)
      common /forces_dqdt/dqdt /forces_sst/sst
#  ifndef ANA_SST
!
!  dqdtg |  Two-time-level grided data for net surface heat flux
!  sstg  |  sensitivity to SST grided data [Watts/m^2/Celsius] and
!              sea surface temperature [degree Celsius].
!  dqdtp |  Two-time-level point data for net surface heat flux
!  sstp  |  sensitivity to SST grided data [Watts/m^2/Celsius] and
!              sea surface temperature [degree Celsius].
!  tsst     Time of sea surface temperature data.
!
      real dqdtg(GLOBAL_2D_ARRAY,2)
      real sstg(GLOBAL_2D_ARRAY,2)
      common /sstdat_dqdtg/dqdtg /sstdat_sstg/sstg

      real    sstp(2), dqdtp(2), sst_time(2)
      real    sst_cycle, scldqdt
      integer itsst, sst_ncycle, sst_rec,  sst_tid,  sst_id
      integer dqdt_id,     lsstgrd,   sstunused
      common /sstdat1/ sstp, dqdtp, sst_time
      common /sstdat2/ sst_cycle, scldqdt
      common /sstdat3/ itsst, sst_ncycle, sst_rec, sst_tid, sst_id
      common /sstdat4/ dqdt_id, lsstgrd, sstunused

#    undef SST_DATA
#  endif /* !ANA_SST */
# endif /* QCORRECTION */
# if defined SALINITY && defined SFLX_CORR
!
!  SALT FLUX CORRECTION
!--------------------------------------------------------------------
!  sss      Current sea surface salinity [PSU].
!
      real sss(GLOBAL_2D_ARRAY)
      common /forces_sss/sss
#  if !defined QCORRECTION
      real dqdt(GLOBAL_2D_ARRAY)
      common /forces_dqdt/dqdt 
#  endif
#  ifndef ANA_SSS
!
!  dqdtg |  Two-time-level grided data for net surface heat flux
!  sssg  |  Two-time-level grided data for 
!              sea surface salinity [PSU].
!  dqdtp |  Two-time-level point data for net surface heat flux
!  sssp  |  Two-time-level point data for
!              sea surface salinity [PSU].
!  tsss     Time of sea surface salinity data.
!
      real sssg(GLOBAL_2D_ARRAY,2)
      common /sssdat_sssg/sssg

      real sssp(2),  sss_time(2)
      real sss_cycle
      integer itsss, sss_ncycle, sss_rec,  sss_tid,  sss_id
      integer lsssgrd,   sssunused
      common /sssdat1/sssp,  sss_time, sss_cycle
      common /sssdat2/itsss, sss_ncycle, sss_rec,  sss_tid, sss_id
      common /sssdat3/lsssgrd,   sssunused
#   if !defined QCORRECTION
      real dqdtg(GLOBAL_2D_ARRAY,2)
      real    dqdtp(2)
      real    scldqdt
      integer dqdt_id
      common /sstdat_dqdtg/dqdtg
      common /sssdat1/ dqdtp
      common /sstdat2/ scldqdt
      common /sstdat3/ dqdt_id
#   endif
#   undef SSS_DATA
#  endif /* !ANA_SSS */
# endif /* SALINITY && SFLX_CORR */
!
!
#ifdef BULK_FLUX
!
!  HEAT FLUX BULK FORMULATION
!--------------------------------------------------------------------
!  tair     surface air temperature at 2m [degree Celsius].
!  wsp      wind speed at 10m [degree Celsius].
!  rhum     surface air relative humidity 2m [fraction]
!  prate    surface precipitation rate [cm day-1]
!  radlw    net terrestrial longwave radiation [Watts meter-2]
!  radsw    net solar shortwave radiation [Watts meter-2]
!
      real tair(GLOBAL_2D_ARRAY)
      real rhum(GLOBAL_2D_ARRAY)
      real prate(GLOBAL_2D_ARRAY)
      real radlw(GLOBAL_2D_ARRAY)
      real radsw(GLOBAL_2D_ARRAY)
      real wspd(GLOBAL_2D_ARRAY)
# ifdef BULK_SM_UPDATE
      real uwnd(GLOBAL_2D_ARRAY)
      real vwnd(GLOBAL_2D_ARRAY)
# endif
# ifdef DIURNAL_INPUT_SRFLX
      real radswbio(GLOBAL_2D_ARRAY)
# endif

      common /bulk_tair/tair
     &       /bulk_rhum/rhum 
     &       /bulk_prate/prate
     &       /bulk_radlw/radlw 
     &       /bulk_radsw/radsw
     &       /bulk_wspd/wspd
# ifdef BULK_SM_UPDATE
     &       /bulk_uwnd/uwnd 
     &       /bulk_vwnd/vwnd
# endif
# ifdef DIURNAL_INPUT_SRFLX
     &       /bulk_radswbio/radswbio
# endif

      real tairg(GLOBAL_2D_ARRAY,2)
      real rhumg(GLOBAL_2D_ARRAY,2)
      real prateg(GLOBAL_2D_ARRAY,2)
      real radlwg(GLOBAL_2D_ARRAY,2)
      real radswg(GLOBAL_2D_ARRAY,2)
      real wspdg(GLOBAL_2D_ARRAY,2)
# ifdef BULK_SM_UPDATE
      real uwndg(GLOBAL_2D_ARRAY,2)
      real vwndg(GLOBAL_2D_ARRAY,2)
# endif
# ifdef DIURNAL_INPUT_SRFLX
      real radswbiog(GLOBAL_2D_ARRAY,2)
# endif

      common /bulkdat_tairg/tairg
     &       /bulkdat_rhumg/rhumg
     &       /bulkdat_prateg/prateg
     &       /bulkdat_radlwg/radlwg
     &       /bulkdat_radswg/radswg
     &       /bulkdat_wspdg/wspdg
# ifdef BULK_SM_UPDATE 
     &       /bulk_uwndg/uwndg
     &       /bulk_vwndg/vwndg
# endif
# ifdef DIURNAL_INPUT_SRFLX
     &       /bulkdat_radswbiog/radswbiog
# endif

      real    tairp(2),rhump(2),pratep(2),radlwp(2),radswp(2)
     &       ,wspdp(2)
# ifdef BULK_SM_UPDATE
     &       ,uwndp(2),vwndp(2)
# endif
# ifdef DIURNAL_INPUT_SRFLX
     &       ,radswbiop(2)
# endif
      real    bulk_time(2), bulk_cycle
      integer tair_id,rhum_id,prate_id,radlw_id,radsw_id
     & 	     ,ltairgrd,lrhumgrd,lprategrd,lradlwgrd,lradswgrd 
     &       ,wspd_id,lwspdgrd
# ifdef BULK_SM_UPDATE
     &       ,uwnd_id,vwnd_id,luwndgrd,lvwndgrd
# endif
# ifdef DIURNAL_INPUT_SRFLX
     &        ,radswbio_id,lradswbiogrd
# endif
      integer itbulk,bulk_ncycle,bulk_rec,bulk_tid
     &        ,bulkunused

      common /bulkdat1/
     &        tair_id,rhum_id,prate_id,radlw_id,radsw_id
     &        ,ltairgrd,lrhumgrd,lprategrd,lradlwgrd,lradswgrd
     &        ,itbulk, bulk_ncycle, bulk_rec, bulk_tid
     &        ,bulkunused
     &        ,wspd_id,lwspdgrd
# ifdef BULK_SM_UPDATE
     &       ,uwnd_id,vwnd_id,luwndgrd,lvwndgrd
# endif
# ifdef DIURNAL_INPUT_SRFLX
     &       ,radswbio_id,lradswbiogrd
# endif
      common /bulkdat2/
     &        tairp,rhump,pratep,radlwp,radswp
     &       ,bulk_time, bulk_cycle
     &       ,wspdp
# ifdef BULK_SM_UPDATE
     &       ,uwndp,vwndp
# endif
# ifdef DIURNAL_INPUT_SRFLX
     &       ,radswbiop
# endif
#endif /* BULK_FLUX */
!
!  SOLAR SHORT WAVE RADIATION FLUX.
!--------------------------------------------------------------------
!  srflx  Kinematic surface shortwave solar radiation flux
!         [degC m/s] at horizontal RHO-points
!
      real srflx(GLOBAL_2D_ARRAY)
      common /forces_srflx/srflx
# ifdef ANA_DIURNAL_SW
      real sin_phi(GLOBAL_2D_ARRAY)
      real cos_phi(GLOBAL_2D_ARRAY)
      real tan_phi(GLOBAL_2D_ARRAY)
      common /diu_srflx/ sin_phi, cos_phi, tan_phi
# endif
# ifdef DIURNAL_INPUT_SRFLX
      real srflxbio(GLOBAL_2D_ARRAY)
      common /forces_srflxbio/srflxbio
# endif
# ifndef ANA_SRFLUX
!
!  srflxg | Two-time-level grided and point data for surface 
!  srflxp |      solar shortwave radiation flux grided data.
!  tsrflx   Time of solar shortwave radiation flux.
!
      real srflxg(GLOBAL_2D_ARRAY,2)
      common /srfdat_srflxg/srflxg

      real srflxp(2),srf_time(2)
      real srf_cycle, srf_scale
      integer itsrf, srf_ncycle, srf_rec
      integer lsrfgrd, srf_tid, srf_id 
      common /srfdat1/ srflxp, srf_time, srf_cycle, srf_scale
      common /srfdat2/ itsrf, srf_ncycle, srf_rec, lsrfgrd, srf_tid, srf_id

# ifdef DIURNAL_INPUT_SRFLX
      real srflxbiog(GLOBAL_2D_ARRAY,2)
      common /srfdat_srflxbiog/srflxbiog

      real srflxbiop(2)
      integer srfbio_rec, lsrfbiogrd, srfbio_tid, srfbio_id
      common /srfbiodat/srflxbiop, lsrfbiogrd, srfbio_tid, srfbio_id
# endif /*  DIURNAL_INPUT_SRFLX   */

#   undef SRFLUX_DATA
# endif /* !ANA_SRFLUX */

#endif /* SOLVE3D */

!--------------------------------------------------------------------
!  WIND INDUCED WAVES: everything is defined at rho-point
!--------------------------------------------------------------------
! wfrq | BBL/MRL | wind-induced wave frequency [rad/s]
! uorb | BBL     | xi-component  of wave-induced bed orbital velocity [m/s]
! vorb | BBL     | eta-component of wave-induced bed orbital velocity [m/s]
! wdrx | MRL     | cosine of wave direction [non dimension]
! wdre | MRL     | sine of   wave direction [non dimension]
! whrm | MRL     | (RMS) wave height (twice the wave amplitude) [m]
! wdsp | MRL     | breaking dissipation rate (\epsilon_b term) [m3/s3]
! wdrg | MRL     | frictional dissipation rate (\epsilon_d term) [m3/s3]
! rdsp | ROLLER  | roller dissipation rate (\epsilon_r term) [m3/s3]
! wbst | MRL/BKPP| frictional dissipation stress (e_d k/sigma) [m2/s2]
!--------------------------------------------------------------------

#if defined BBL || defined MRL_WCI || defined OW_COUPLING 
      real wfrq(GLOBAL_2D_ARRAY)
      common /forces_wfrq/wfrq
#endif

#ifdef BBL
      real uorb(GLOBAL_2D_ARRAY)
      real vorb(GLOBAL_2D_ARRAY)
      common /forces_uorb/uorb /forces_vorb/vorb
#endif   /* BBL */

#if defined MRL_WCI || defined OW_COUPLING
      real whrm(GLOBAL_2D_ARRAY)
      real wdsp(GLOBAL_2D_ARRAY)
      real wdrg(GLOBAL_2D_ARRAY)
      real wbst(GLOBAL_2D_ARRAY)
      real wdrx(GLOBAL_2D_ARRAY)
      real wdre(GLOBAL_2D_ARRAY)
      common /forces_whrm/whrm /forces_wdsp/wdsp 
     &       /forces_wdrx/wdrx /forces_wdre/wdre 
     &       /forces_wdrg/wdrg /forces_wbst/wbst
# ifdef WAVE_ROLLER
      real rdsp(GLOBAL_2D_ARRAY)
      common /forces_rdsp/rdsp
# endif
!
!--------------------------------------------------------------------
!  WAVE AVEREAGED QUANTITIES AND TERMS
!--------------------------------------------------------------------
!  2D  |  brk2dx   |   xi-direciton 2D breaking dissipation (rho)
!  2D  |  brk2de   |  eta-direction 2D breaking dissipation (rho)
!  2D  |  frc2dx   |   xi-direciton 2D frictional dissipation (rho)
!  2D  |  frc2de   |  eta-direction 2D frictional dissipation (rho)
!  2D  |  ust2d    |   xi-direciton Stokes transport (u-point)
!  2D  |  vst2d    |  eta-direciton Stokes transport (v-point)
!  2D  |  sup      |  quasi-static wave set-up (rho-point)
!  2D  |  calP     |  pressure correction term (rho-point)
!  2D  |  Kapsrf   |  Bernoulli head terrm at the surface (rho-point)
!--------------------------------------------------------------------
!  3D  |  brk3dx   |   xi-direciton 3D breaking dissipation (rho)
!  3D  |  brk3de   |  eta-direction 3D breaking dissipation (rho)
!  3D  |  ust      |   xi-direciton 3D Stokes drift velocity (u-point)
!  3D  |  vst      |  eta-direciton 3D Stokes drift velocity (v-point)
!  3D  |  wst      |       vertical 3D Stokes drift velocity (rho-point)
!  3D  |  Kappa    |  3D Bernoulli head term (rho-point)
!  3D  |  kvf      |  vertical vortex force term (K term, 3D, rho-point)
!  3D  |  Akb      |  breaking-wave-induced additional diffusivity (w-point)
!  3D  |  Akw      |  wave-induced additional diffusivity (rho-point)
!  3D  |  E_pre    |  previous time-step value for Akw estimation (rho)
!  3D  |  frc3dx   |   xi-direciton 3D frictional dissipation (rho)
!  3D  |  frc3de   |  eta-direction 3D frictional dissipation (rho)
!--------------------------------------------------------------------
!
      real brk2dx(GLOBAL_2D_ARRAY)
      real brk2de(GLOBAL_2D_ARRAY)
      real ust2d(GLOBAL_2D_ARRAY)
      real vst2d(GLOBAL_2D_ARRAY)
      real frc2dx(GLOBAL_2D_ARRAY)
      real frc2de(GLOBAL_2D_ARRAY)
      real sup(GLOBAL_2D_ARRAY)
      common /forces_brk2dx/brk2dx /forces_brk2de/brk2de
      common /forces_ust2d/ust2d /forces_vst2d/vst2d
      common /forces_frc2dx/frc2dx /forces_frc2de/frc2de
      common /forces_sup/sup
# ifdef SOLVE3D
      real calP(GLOBAL_2D_ARRAY)
      real Kapsrf(GLOBAL_2D_ARRAY)
      common /forces_calP/calP /forces_Kapsrf/Kapsrf
#  ifndef WAVE_SFC_BREAK
      real brk3dx(GLOBAL_2D_ARRAY,N)
      real brk3de(GLOBAL_2D_ARRAY,N)
      common /forces_brk3dx/brk3dx /forces_brk3de/brk3de
#  endif
#  ifdef WAVE_BODY_STREAMING
      real frc3dx(GLOBAL_2D_ARRAY,N)
      real frc3de(GLOBAL_2D_ARRAY,N)
      common /forces_frc3dx/frc3dx /forces_frc3de/frc3de
#  endif
      real ust(GLOBAL_2D_ARRAY,N)
      real vst(GLOBAL_2D_ARRAY,N)
      real wst(GLOBAL_2D_ARRAY,N)
      real kvf(GLOBAL_2D_ARRAY,N)
      real Akb(GLOBAL_2D_ARRAY,0:N)
      real Akw(GLOBAL_2D_ARRAY,0:N)
      real E_pre(GLOBAL_2D_ARRAY,0:N)
      common /forces_stokes/ust, vst, wst
      common /forces_kvf/kvf /forces_Akb/Akb /forces_Akw/Akw 
      common /forces_E_pre/E_pre
# endif  /* SOLVE3D */
#endif   /* MRL_WCI */

#if defined BBL || defined MRL_WCI 
!--------------------------------------------------------------------
! Awave  | for present time   | wave amplitude [m]
! Pwave  | for present time   | wave direction [radians]
! Dwave  | for present time   | wave period [s]
!--------------------------------------------------------------------
      real Awave(GLOBAL_2D_ARRAY)
      real Dwave(GLOBAL_2D_ARRAY)
      real Pwave(GLOBAL_2D_ARRAY)
      common /bbl_Awave/Awave /bbl_Dwave/Dwave /bbl_Pwave/Pwave

# if !defined ANA_WWAVE && !defined WKB_WWAVE
!--------------------------------------------------------------------
!  Eb    |                    | breaking dissipation [m3/s3]
!  wved  |  for present time  | frictional dissipation [m3/s3]
!  wvqb  |  step              | fraction of breaking waves [ND]
!--------------------------------------------------------------------
!wwv_time|                    | time of wind-induced waves
!--------------------------------------------------------------------
!  wwag  |                    | wave amplitude [m]
!  wwdg  |                    | wave direction [radians]
!  wwpg  |                    | wave period [s]
!  wwub  |                    | orbital velocity magnitude [m/s]
!  wwfrq |  Two-time-level    | wave frequency [rad/s]
!  wwuob |  point data        | xi-orbital velocity [m/s]
!  wwvob |  for wind induced  ! eta-orbital velocity [m/s]
!  wwdrx |                    | cosine wave direction [ND]
!  wwdre |                    ! sine wave direction [ND]
!  wwhrm |                    ! (RMS) wave height [m]
!  wweb  |                    ! breaking dissipation [m3/s3]
!  wwed  |                    ! frictional dissipation [m3/s3]
!  wwqb  |                    ! fraction of breaking waves [ND]
!--------------------------------------------------------------------
!
      real wwag(GLOBAL_2D_ARRAY,2)
      real wwdg(GLOBAL_2D_ARRAY,2)
      real wwpg(GLOBAL_2D_ARRAY,2)
      common /wwf_wwag/wwag /wwf_wwdg/wwdg /wwf_wwpg/wwpg
      real wwfrq(GLOBAL_2D_ARRAY)
      common /wwf_wwfrq/wwfrq
#  ifdef BBL_OFFLINE 
#   ifdef WAVE_OFFLINE
      real wwub(GLOBAL_2D_ARRAY,2)
      common /wwf_wwub/wwub
#   endif /* WAVE_OFFLINE */
      real wwuob(GLOBAL_2D_ARRAY,2)
      real wwvob(GLOBAL_2D_ARRAY,2)
      common /wwf_wwuob/wwuob /wwf_wwvob/wwvob
#  endif /* BBL */
#  ifdef MRL_WCI 
      real wwhrm(GLOBAL_2D_ARRAY)
      real wwdrx(GLOBAL_2D_ARRAY)
      real wwdre(GLOBAL_2D_ARRAY)
      real wweb(GLOBAL_2D_ARRAY,2)
      real Eb  (GLOBAL_2D_ARRAY)
      common /wwf_wwhrm/wwhrm /wwf_wwdrx/wwdrx /wwf_wwdre/wwdre
     &       /wwf_wweb/wweb /forces_Eb/Eb
#   if defined WAVE_OFFLINE || defined OW_COUPLING
      real wved(GLOBAL_2D_ARRAY)
      real wwed(GLOBAL_2D_ARRAY,2)
      common /forces_wved/wved /wwf_wwed/wwed
#   endif /* WAVE_OFFLINE */
#   if defined WAVE_OFFLINE && defined WAVE_ROLLER
      real wvqb(GLOBAL_2D_ARRAY)
      real wwqb(GLOBAL_2D_ARRAY,2)
      common /forces_wvqb/wvqb /wwf_wwqb/wwqb
#   endif
#  endif 
      real ww_cycle,wwv_time(2),wwap(2), wwdp(2),wwpp(2),wwep(2),
     &                 wwa_scale, wwd_scale, wwp_scale,wwe_scale,
     &                 wwagrd,   wwdgrd,    wwpgrd, wwegrd
      integer ww_ncycle,  ww_rec,  itww
     &        ,ww_file_id, ww_tid,  wwa_id, wwp_id, wwd_id
#   if defined BBL && defined WAVE_OFFLINE
     &       ,wwu_id
#   endif 
#   if defined MRL_WCI && defined WAVE_OFFLINE
     &       ,wwe_id, wwq_id, wwf_id
#   endif
      common /wwdat/ ww_cycle, wwv_time
     &       ,wwap, wwdp,wwpp,wwep
     &       ,wwa_scale, wwd_scale, wwp_scale,wwe_scale
     &       ,wwagrd,   wwdgrd,    wwpgrd, wwegrd
     &       ,ww_ncycle,  ww_rec,  itww
     &       ,ww_file_id, ww_tid,  wwa_id, wwp_id, wwd_id
#   if defined BBL && defined WAVE_OFFLINE
     &       ,wwu_id
#   endif
#   if defined MRL_WCI && defined WAVE_OFFLINE
     &       ,wwe_id, wwq_id, wwf_id
#   endif
# elif defined MRL_WCI
      real Eb(GLOBAL_2D_ARRAY)
      real wved(GLOBAL_2D_ARRAY)
      common /forces_Eb/Eb /forces_wved/wved
# endif /* ANA_WWAVE && !WKB_WWAVE */
#endif /* BBL || MRL_WCI */


