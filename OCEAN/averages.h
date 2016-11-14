! $Id: averages.h 1458 2014-02-03 15:01:25Z gcambon $
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
/* This is include file "averages.h": time-averaged fields
 for output purposes:
*/
#ifdef AVERAGES
      real zeta_avg(GLOBAL_2D_ARRAY)
      real ubar_avg(GLOBAL_2D_ARRAY)
      real vbar_avg(GLOBAL_2D_ARRAY)
      common /avg_zeta/zeta_avg 
     &       /avg_ubar/ubar_avg
     &       /avg_vbar/vbar_avg
      real bostr_avg(GLOBAL_2D_ARRAY)
      common /avg_bostr/bostr_avg
      real wstr_avg(GLOBAL_2D_ARRAY)
      common /avg_wstr/wstr_avg
      real sustr_avg(GLOBAL_2D_ARRAY)
      common /avg_sustr/sustr_avg
      real svstr_avg(GLOBAL_2D_ARRAY)
      common /avg_svstr/svstr_avg
      real srflx_avg(GLOBAL_2D_ARRAY)
      common /avg_srflx/srflx_avg
# ifdef SOLVE3D
      real u_avg(GLOBAL_2D_ARRAY,N)
      real v_avg(GLOBAL_2D_ARRAY,N)
      real t_avg(GLOBAL_2D_ARRAY,N,NT)
      real rho_avg(GLOBAL_2D_ARRAY,N)
      real omega_avg(GLOBAL_2D_ARRAY,0:N)
      real w_avg(GLOBAL_2D_ARRAY,N)
      common /avg_u/u_avg /avg_v/v_avg /avg_t/t_avg
     &       /avg_rho/rho_avg /avg_omega/omega_avg
     &       /avg_w/w_avg
      real stflx_avg(GLOBAL_2D_ARRAY,NT)
      common /avg_stflx/stflx_avg
#  ifdef LMD_SKPP
      real hbl_avg(GLOBAL_2D_ARRAY)
      common /avg_hbl/hbl_avg
#  endif
#  ifdef LMD_BKPP
      real hbbl_avg(GLOBAL_2D_ARRAY)
      common /avg_hbbl/hbbl_avg
#  endif
#  ifdef GLS_MIXING
      real tke_avg(GLOBAL_2D_ARRAY,0:N)
      real gls_avg(GLOBAL_2D_ARRAY,0:N)
      real Lscale_avg(GLOBAL_2D_ARRAY,0:N)
      common /avg_tke/tke_avg
      common /avg_gls/gls_avg
      common /avg_Lscale/Lscale_avg
#  endif
#  ifdef BULK_FLUX
      real shflx_rsw_avg(GLOBAL_2D_ARRAY)
      real shflx_rlw_avg(GLOBAL_2D_ARRAY)
      real shflx_lat_avg(GLOBAL_2D_ARRAY)
      real shflx_sen_avg(GLOBAL_2D_ARRAY)
      common /avg_shflx_rsw/shflx_rsw_avg
      common /avg_shflx_rlw/shflx_rlw_avg
      common /avg_shflx_lat/shflx_lat_avg
      common /avg_shflx_sen/shflx_sen_avg
#  endif
#  ifdef SST_SKIN
      real sst_skin_avg(GLOBAL_2D_ARRAY)
      common /avg_sst_skin/sst_skin_avg
#  endif
#  ifdef BIOLOGY
      real hel_avg(GLOBAL_2D_ARRAY)
      common /avg_hel/hel_avg
#   ifdef BIO_NChlPZD
      real theta_avg(GLOBAL_2D_ARRAY,N)
      common /avg_theta/theta_avg
#    ifdef OXYGEN
      real u10_avg(GLOBAL_2D_ARRAY)
      real Kv_O2_avg(GLOBAL_2D_ARRAY)
      real O2satu_avg(GLOBAL_2D_ARRAY)
      common /gasexc_O2_u10_avg/ u10_avg
      common /gasexc_O2_Kv_O2_avg/ Kv_O2_avg
      common /gasexc_O2_O2satu_avg/ O2satu_avg
#    endif /* OXYGEN */
/*-----------------------------------------------------------------*/
#   elif defined BIO_BioEBUS
      real AOU_avg(GLOBAL_2D_ARRAY,N)  
      real wind10_avg(GLOBAL_2D_ARRAY)
      common /ocean_AOU_avg/AOU_avg
      common /ocean_wind10_avg/wind10_avg    
#   endif
/*-----------------------------------------------------------------*/
#  endif /* BIOLOGY */
#  ifdef VIS_COEF_3D
      real visc3d_avg(GLOBAL_2D_ARRAY,N)
      common /avg_visc3d/visc3d_avg
#  endif
#  ifdef DIF_COEF_3D
      real diff3d_avg(GLOBAL_2D_ARRAY,N)
      common /avg_diff3d/diff3d_avg
#  endif
#  ifdef AVERAGES_K
      real Akv_avg(GLOBAL_2D_ARRAY,0:N)
      real Akt_avg(GLOBAL_2D_ARRAY,0:N,2)
      common /avg_Akv/Akv_avg /avg_Akt/Akt_avg
#   ifdef GLS_MIXING
      real Akk_avg(GLOBAL_2D_ARRAY,0:N)
      real Akp_avg(GLOBAL_2D_ARRAY,0:N)
      common /avg_Akk/Akk_avg /avg_Akp/Akp_avg
#   endif
#  endif
# endif
# ifdef WKB_WWAVE
      real hrm_avg(GLOBAL_2D_ARRAY)
      real frq_avg(GLOBAL_2D_ARRAY)
      real wac_avg(GLOBAL_2D_ARRAY)
      real wkx_avg(GLOBAL_2D_ARRAY)
      real wke_avg(GLOBAL_2D_ARRAY)
      real wdsp_avg(GLOBAL_2D_ARRAY)
      real wdrg_avg(GLOBAL_2D_ARRAY)
      common /avg_hrm/hrm_avg /avg_frq/frq_avg
     &       /avg_wac/wac_avg /avg_wkx/wkx_avg
     &       /avg_wke/wke_avg /avg_wdsp/wdsp_avg
     &       /avg_wdrg/wdrg_avg
#  ifdef WAVE_ROLLER 
      real war_avg(GLOBAL_2D_ARRAY)
      real rdsp_avg(GLOBAL_2D_ARRAY)
#  endif    
# endif
# ifdef MRL_WCI
      real sup_avg(GLOBAL_2D_ARRAY)
      real ust2d_avg(GLOBAL_2D_ARRAY)
      real vst2d_avg(GLOBAL_2D_ARRAY)
      common /avg_sup/sup_avg 
     &       /avg_ust2d/ust2d_avg /avg_vst2d/vst2d_avg 
#  ifdef SOLVE3D
      real ust_avg(GLOBAL_2D_ARRAY,N)
      real vst_avg(GLOBAL_2D_ARRAY,N)
      real wst_avg(GLOBAL_2D_ARRAY,0:N)
      real akb_avg(GLOBAL_2D_ARRAY,0:N)
      real akw_avg(GLOBAL_2D_ARRAY,0:N)
      real kvf_avg(GLOBAL_2D_ARRAY,N)
      real calp_avg(GLOBAL_2D_ARRAY)
      real kaps_avg(GLOBAL_2D_ARRAY)
      common /avg_ust/ust_avg /avg_vst/vst_avg /avg_wst/wst_avg
     &       /avg_akb/akb_avg /avg_akw/akw_avg
     &       /avg_kvf/kvf_avg /avg_calp/calp_avg /avg_kaps/kaps_avg
#  endif
# endif
#endif /* AVERAGES */
