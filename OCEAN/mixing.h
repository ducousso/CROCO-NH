! $Id: mixing.h 1618 2014-12-18 14:39:51Z rblod $
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
! This is include file "mixing.h"
!  ==== == ======= ==== ==========
!
#if defined UV_VIS2 || defined SPONGE_VIS2
      real visc2_r(GLOBAL_2D_ARRAY)
      real visc2_p(GLOBAL_2D_ARRAY)
      real visc2_sponge_r(GLOBAL_2D_ARRAY)
      real visc2_sponge_p(GLOBAL_2D_ARRAY)
      common /mixing_visc2_r/visc2_r /mixing_visc2_p/visc2_p
      common /mixing_visc2_sponge_r/visc2_sponge_r 
      common /mixing_visc2_sponge_p/visc2_sponge_p
#endif
#if defined UV_VIS4 
# if !defined SPONGE_VIS2
      real visc2_sponge_r(GLOBAL_2D_ARRAY)
      real visc2_sponge_p(GLOBAL_2D_ARRAY)
      common /mixing_visc2_sponge_r/visc2_sponge_r
      common /mixing_visc2_sponge_p/visc2_sponge_p
# endif
      real visc4_sponge_r(GLOBAL_2D_ARRAY)
      real visc4_sponge_p(GLOBAL_2D_ARRAY)
      real visc4_r(GLOBAL_2D_ARRAY)
      real visc4_p(GLOBAL_2D_ARRAY)
      common /mixing_visc4_sponge_r/visc4_sponge_r
      common /mixing_visc4_sponge_p/visc4_sponge_p
      common /mixing_visc4_r/visc4_r /mixing_visc4_p/visc4_p
#endif
#if defined TS_DIF2 || defined SPONGE_DIF2
      real diff2_sponge(GLOBAL_2D_ARRAY)
      real diff2(GLOBAL_2D_ARRAY,NT)
      common /mixing_diff2_sponge/diff2_sponge
      common /mixing_diff2/diff2
#endif
#if defined TS_DIF4 
# if !defined SPONGE_DIF2
      real diff2_sponge(GLOBAL_2D_ARRAY)
      common /mixing_diff2_sponge/diff2_sponge
# endif
      real diff4_sponge(GLOBAL_2D_ARRAY)
      real diff4(GLOBAL_2D_ARRAY,NT)
      common /mixing_diff4_sponge/diff4_sponge
      common /mixing_diff4/diff4
#endif
#ifdef VIS_COEF_3D
      real visc3d_r(GLOBAL_2D_ARRAY,N)
      common /mixing_visc3d_r/visc3d_r
      real visc3d_p(GLOBAL_2D_ARRAY,N)
      common /mixing_visc3d_p/visc3d_p
#endif
#ifdef DIF_COEF_3D
      real diff3d_u(GLOBAL_2D_ARRAY,N)
      real diff3d_v(GLOBAL_2D_ARRAY,N)
      common /mixing_diff3d_u/diff3d_u 
      common /mixing_diff3d_v/diff3d_v
# ifdef TS_DIF_SMAGO
      real diff3d_r(GLOBAL_2D_ARRAY,N)
      common /mixing_diff3d_r/diff3d_r
# endif
#endif
#if defined TS_MIX_ISO || defined TS_MIX_GEO
      real dRdx(GLOBAL_2D_ARRAY,N)
      real dRde(GLOBAL_2D_ARRAY,N)
      real idRz(GLOBAL_2D_ARRAY,0:N)
      common /mixing_dRdx/dRdx
      common /mixing_dRde/dRde
      common /mixing_idRz/idRz
# ifdef TS_MIX_ISO
      real Rslope_max,Gslope_max
      parameter (Gslope_max=5., Rslope_max=0.05)
# endif
# ifdef TS_MIX_ISO_FILT
      integer ismooth
      real csmooth
      common /mixing_csmooth/ csmooth
      common /mixing_ismooth/ ismooth
# endif
#endif /*  TS_MIX_ISO || TS_MIX_GEO */

#ifdef SOLVE3D
      real Akv(GLOBAL_2D_ARRAY,0:N)
      real Akt(GLOBAL_2D_ARRAY,0:N,2)
      common /mixing_Akv/Akv /mixing_Akt/Akt
# ifdef RANDOM_WALK
      real dAktdz(GLOBAL_2D_ARRAY,0:N)
      common /mixing_dAktdz/dAktdz
# endif

# if defined ANA_VMIX || defined BVF_MIXING \
  || defined LMD_MIXING || defined LMD_SKPP || defined LMD_BKPP \
  || defined GLS_MIXING
      real bvf(GLOBAL_2D_ARRAY,0:N)
      common /mixing_bvf/ bvf
# endif

# ifdef BIOLOGY
      real hel(GLOBAL_2D_ARRAY)
      common /lmd_hel/hel
# endif

# if defined LMD_SKPP || defined LMD_BKPP
!
! Large/McWilliams/Doney oceanic planetary boundary layer variables.
! ghats       Boundary layer nonlocal transport (m/s^2).
! hbl         Depth of oceanic surface boundary layer (m).
! hbbl        Depth of oceanic bottom boundary layer (m).
! kbl         Index of first grid level below "hbl".
! ustar       Turbulent friction velocity (m/s).
!
      integer kbl(GLOBAL_2D_ARRAY)
      integer kbbl(GLOBAL_2D_ARRAY)
      real hbbl(GLOBAL_2D_ARRAY)
      common /lmd_kpp_kbl/ kbl 
      common /lmd_kpp_hbbl/ hbbl  
      common /lmd_kpp_kbbl/ kbbl 
#  ifdef LMD_SKPP2005      
      real hbls(GLOBAL_2D_ARRAY,2)
      common /lmd_kpp_hbl/ hbls
#  else           
      real hbl (GLOBAL_2D_ARRAY  )      
      common /lmd_kpp_hbl/ hbl   
#  endif
#  ifdef LMD_NONLOCAL
      real ghats(GLOBAL_2D_ARRAY,0:N)
      common /lmd_kpp_ghats/ghats
#  endif
# endif /* LMD_SKPP || LMD_BKPP */

# ifdef LMD_MIXING
      real ustar(GLOBAL_2D_ARRAY) 
      common /lmd_kpp_ustar/ustar
# endif /* LMD_MIXING */

# ifdef GLS_MIXING
      real tke(GLOBAL_2D_ARRAY,0:N,3)
      real gls(GLOBAL_2D_ARRAY,0:N,3)
      real Akk(GLOBAL_2D_ARRAY,0:N)
      real Akp(GLOBAL_2D_ARRAY,0:N)
      real Lscale(GLOBAL_2D_ARRAY,0:N)
      common /gls_tke/tke
      common /gls_gls/gls
      common /gls_Akk/Akk
      common /gls_Akp/Akp
      common /gls_Lscale/Lscale
# endif /* GLS_MIXING */

#else

# define u(i,j,k,nrhs) ubar(i,j,kstp)
# define v(i,j,k,nrhs) vbar(i,j,kstp)
# define visc3d_r(i,j,k)  visc2_r(i,j)
# define visc3d_p(i,j,k)  visc2_p(i,j)
# define exchange_p3d_tile(a,b,c,d,visc3d_p) exchange_p2d_tile(a,b,c,d,visc2_p)

#endif /* SOLVE3D */


