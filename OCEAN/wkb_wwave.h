#ifdef WKB_WWAVE
!---------------------------------------------------------
! wkx, wke  xi- and eta-dir components of wavenumber vector
! wac       wave action density (m^3/s^3)
! hrm       RMS wave height (m)
! frq       wave frequency (rad/s)
! wvn       wave number(rad/m)
! wsb       breaking dissipation, epsilon_b/rho/sigma (m3/s2)
! wfc       frictional dissipation, epsilon_d/rho/sigma (m3/s2)
!
      real wkb_btg, wkb_gam, wkb_rsb, wkb_roller,
     &     wkb_amp, wkb_ang, wkb_prd, wkb_tide
      logical wkb_agrif_done 
      common /wkb_par/ wkb_btg, wkb_gam, wkb_rsb, wkb_roller, 
     &                 wkb_amp, wkb_ang, wkb_prd, wkb_tide,
     &                 wkb_agrif_done 

      real wkx(GLOBAL_2D_ARRAY,2)
      real wke(GLOBAL_2D_ARRAY,2)
      real wac(GLOBAL_2D_ARRAY,2)
      real hrm(GLOBAL_2D_ARRAY,2)
      real frq(GLOBAL_2D_ARRAY,2)
      real wcg(GLOBAL_2D_ARRAY,2)
      real wsb(GLOBAL_2D_ARRAY,2)
      real wvn(GLOBAL_2D_ARRAY,2)
      real wfc(GLOBAL_2D_ARRAY,2)
      common /wkb_wkx/wkx /wkb_wke/wke /wkb_wac/wac
     &       /wkb_hrm/hrm /wkb_frq/frq /wkb_wcg/wcg
     &       /wkb_wsb/wsb /wkb_wvn/wvn /wkb_wfc/wfc
# ifdef WAVE_ROLLER
      real war(GLOBAL_2D_ARRAY,2)
      real wcr(GLOBAL_2D_ARRAY,2)
      real wsr(GLOBAL_2D_ARRAY,2)
      common /wkb_war/war /wkb_wcr/wcr /wkb_wsr/wsr
# endif

! for diagnostics
      integer iwave, winfo
      real*QUAD  av_wac, av_wkn, thwave
      common /wkb_diag_comm/ winfo, iwave, av_wac, av_wkn, thwave

! for CEW
# if defined MRL_CEW || defined WKB_UNSTEADY
      integer wint, interp_max, wavg, cewavg, wcew
      parameter (interp_max = 5)
      parameter (wavg = 1)
      parameter (cewavg = 10)
      parameter (wcew = 3)
#  ifdef WKB_TIME_FILTER
      real uwave(GLOBAL_2D_ARRAY,5)
      real vwave(GLOBAL_2D_ARRAY,5)
      real zwave(GLOBAL_2D_ARRAY,5)
#  else
      real uwave(GLOBAL_2D_ARRAY,2)
      real vwave(GLOBAL_2D_ARRAY,2)
      real zwave(GLOBAL_2D_ARRAY,2)
#  endif
      common /wkb_cew/ wint
      common /wkb_cewt/  uwave,  vwave,  zwave
# endif
#endif /* WKB_WWAVE */
