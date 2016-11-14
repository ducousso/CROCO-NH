      real dt, dtfast, time, time2, time_start, tdays                       
      integer ndtfast, iic, kstp, krhs, knew, next_kstp      

      integer numthreads, ntstart,   ntimes,  ninfo, &
              nfast,  nrrec,     nrst,    nwrt
                       
#ifdef SOLVE3D
      integer iif, nstp, nrhs, nnew, nbstep3d                              
#endif
#ifdef FLOATS
      integer nfp1, nf, nfm1, nfm2, nfm3                                   
#endif
#ifdef WKB_WWAVE
      integer wstp, wnew
#endif
      logical PREDICTOR_2D_STEP
      common /time_indices/  dt,dtfast, time, time2,time_start, tdays,    &
                             ndtfast, iic, kstp, krhs, knew, next_kstp,   &
#ifdef SOLVE3D
                             iif, nstp, nrhs, nnew, nbstep3d,             &
#endif
#ifdef FLOATS
                             nfp1, nf, nfm1, nfm2, nfm3,                  &
#endif
#ifdef WKB_WWAVE
                             wstp, wnew,                                  &
#endif
                             PREDICTOR_2D_STEP 



      real time_avg, time2_avg, rho0, &
     &               rdrg, rdrg2, Cdb_min, Cdb_max, Zob, &
     &               xl, el, visc2, visc4, gamma2
#ifdef SOLVE3D
      real  theta_s,   theta_b,   Tcline,  hc
      real  sc_w(0:N), Cs_w(0:N), sc_r(N), Cs_r(N)
      real  rx0, rx1
      real  tnu2(NT),tnu4(NT)
# ifndef NONLIN_EOS
      real R0,T0,S0, Tcoef, Scoef
# endif
      real weight(6,0:NWEIGHT)

#endif
#if  defined SPONGE || \
     defined TNUDGING   || defined M2NUDGING  || \
     defined M3NUDGING  || defined ZNUDGING
      real  x_sponge,   v_sponge
#endif
#if  defined T_FRC_BRY     || defined M2_FRC_BRY    || \
     defined M3_FRC_BRY    || defined Z_FRC_BRY     || \
     defined W_FRC_BRY     ||                          \
     defined TCLIMATOLOGY  || defined M2CLIMATOLOGY || \
     defined M3CLIMATOLOGY || defined ZCLIMATOLOGY  || \
     defined WCLIMATOLOGY
       real  tauT_in, tauT_out, tauM_in, tauM_out
#endif
#ifdef AVERAGES
      integer  ntsavg,  navg 
#endif
#ifdef BODYFORCE
      integer levbfrc,   levsfrc
#endif
#ifdef FLOATS
      integer nflt, nrpfflt
#endif
#if defined DIAGNOSTICS_TS
      integer nwrtdia
# ifdef AVERAGES
      integer ntsdia_avg, nwrtdia_avg
# endif
#endif
#if defined DIAGNOSTICS_UV
      integer nwrtdiaM
# ifdef AVERAGES
      integer ntsdiaM_avg, nwrtdiaM_avg
# endif
#endif
#ifdef DIAGNOSTICS_BIO
      integer nwrtdiabio
# ifdef AVERAGES
      integer ntsdiabio_avg, nwrtdiabio_avg
# endif
#endif
#ifdef STATIONS
      integer nsta, nrpfsta
#endif

      logical ldefhis
#ifdef SOLVE3D
      logical got_tini(NT)
#endif
#ifdef SEDIMENT
      logical got_inised(3)
#endif
#ifdef BBL
      logical got_inibed(2)
#endif
#ifdef FLOATS
      logical ldefflt
#endif
#if defined DIAGNOSTICS_TS
      logical ldefdia
# ifdef AVERAGES
      logical ldefdia_avg
# endif
#endif
#if defined DIAGNOSTICS_UV
      logical ldefdiaM
# ifdef AVERAGES
      logical ldefdiaM_avg
# endif
#endif
#ifdef DIAGNOSTICS_BIO
      logical ldefdiabio
# ifdef AVERAGES
      logical ldefdiabio_avg
# endif
#endif
#ifdef STATIONS
      logical ldefsta
#endif

      common /scalars_main/ &
     &             time_avg, time2_avg,  rho0,      rdrg,    rdrg2 &
     &           , Zob,       Cdb_min,   Cdb_max   &
     &           , xl, el,    visc2,     visc4,   gamma2 &
#ifdef SOLVE3D
     &           , theta_s,   theta_b,   Tcline,  hc     &
     &           , sc_w,      Cs_w,      sc_r,    Cs_r   &
     &           , rx0,       rx1,       tnu2,    tnu4   &
# ifndef NONLIN_EOS
     &                      , R0,T0,S0,  Tcoef,   Scoef  &
# endif
     &                      , weight  &
#endif
#if  defined SPONGE || \
     defined TNUDGING   || defined M2NUDGING  || \
     defined M3NUDGING  || defined ZNUDGING
     &                      , x_sponge,   v_sponge  &
#endif
#if  defined T_FRC_BRY     || defined M2_FRC_BRY    || \
     defined M3_FRC_BRY    || defined Z_FRC_BRY     || \
     defined W_FRC_BRY     ||                          \
     defined TCLIMATOLOGY  || defined M2CLIMATOLOGY || \
     defined M3CLIMATOLOGY || defined ZCLIMATOLOGY  || \
     defined WCLIMATOLOGY
     &                      , tauT_in, tauT_out, tauM_in, tauM_out &
#endif
     &      , numthreads,     ntstart,   ntimes,  ninfo  &
     &      , nfast,  nrrec,     nrst,    nwrt  &
#ifdef AVERAGES
     &                                 , ntsavg,  navg  &
#endif
#ifdef BODYFORCE
     &                      , levbfrc,   levsfrc  &
#endif
#ifdef FLOATS
     &                      , nflt, nrpfflt  &
#endif
#ifdef STATIONS
     &                      , nsta, nrpfsta  &
#endif
#ifdef SOLVE3D
     &                      , got_tini &
#endif
#ifdef SEDIMENT
     &                      , got_inised &
#endif
#ifdef BBL
     &                      , got_inibed  &
#endif
#ifdef FLOATS
     &                      , ldefflt  &
#endif
#if defined DIAGNOSTICS_TS
     &                      , ldefdia, nwrtdia  &
# ifdef AVERAGES
     &                      , ldefdia_avg &
     &                      , nwrtdia_avg &
     &                      , ntsdia_avg  &
# endif
#endif
#if defined DIAGNOSTICS_UV
     &                      , ldefdiaM, nwrtdiaM  &
# ifdef AVERAGES
     &                      , ldefdiaM_avg  &
     &                      , nwrtdiaM_avg  &
     &                      , ntsdiaM_avg   &
# endif
#endif
#ifdef DIAGNOSTICS_BIO
     &                      , ldefdiabio, nwrtdiabio  &
# ifdef AVERAGES
     &                      , ldefdiabio_avg   &
     &                      , nwrtdiabio_avg   &
     &                      , ntsdiabio_avg    &
# endif
#endif
#ifdef STATIONS
     &                      , ldefsta 
#endif
     &                      , ldefhis  




#ifdef MPI
!
! MPI rlated variables
! === ====== =========
!
      logical EAST_INTER, WEST_INTER, NORTH_INTER, SOUTH_INTER
      integer mynode, ii,jj, p_W,p_E,p_S,p_N, p_SW,p_SE, p_NW,p_NE
      common /comm_setup/ mynode, ii,jj, p_W,p_E,p_S,p_N, p_SW,p_SE,      &
        p_NW,p_NE, EAST_INTER, WEST_INTER, NORTH_INTER, SOUTH_INTER
          
#endif


!
! Physical constants:
! ======== ==========

      real pi, deg2rad, rad2deg
      parameter (pi=3.14159265358979323846, deg2rad=pi/180.,  &
                                            rad2deg=180./pi)


      real Eradius, g, day2sec,sec2day, jul_off,                          &
           year2day,day2year                                            
      parameter (Eradius=6371315.0,  day2sec=86400.,                      &
                 sec2day=1./86400., jul_off=2440000.,                     &
                 year2day=365.25, day2year=1./365.25)                   
!
! Acceleration of gravity (nondimensional for Soliton problem)
!
#ifdef SOLITON
      parameter (g=1.)
#else
      parameter (g=9.81)
#endif

!  Specific heat [Joules/kg/degC] for seawater, it is approximately
!  4000, and varies only slightly (see Gill, 1982, Appendix 3).
!
      real Cp
      parameter (Cp=3985.0)

!

