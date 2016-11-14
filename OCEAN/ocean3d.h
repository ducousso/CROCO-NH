! $Id: ocean3d.h 1458 2014-02-03 15:01:25Z gcambon $
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
/* This is include file "ocean3.h". 
  --------------------------------------------
*/
#ifdef SOLVE3D
      real u(GLOBAL_2D_ARRAY,N,3)
      real v(GLOBAL_2D_ARRAY,N,3)
      real t(GLOBAL_2D_ARRAY,N,3,NT)
      common /ocean_u/u /ocean_v/v /ocean_t/t

      real Hz(GLOBAL_2D_ARRAY,N)
      real Hz_bak(GLOBAL_2D_ARRAY,N)
      real z_r(GLOBAL_2D_ARRAY,N)
      real z_w(GLOBAL_2D_ARRAY,0:N)
      real Huon(GLOBAL_2D_ARRAY,N)
      real Hvom(GLOBAL_2D_ARRAY,N)
      common /grid_Hz_bak/Hz_bak /grid_zw/z_w /grid_Huon/Huon
      common /grid_Hvom/Hvom

      real We(GLOBAL_2D_ARRAY,0:N)
# ifdef VADV_ADAPT_IMP
      real Wi(GLOBAL_2D_ARRAY,0:N)
# endif
      common /grid_Hz/Hz /grid_zr/z_r /grid_We/We
# ifdef VADV_ADAPT_IMP
      common /grid_Wi/Wi
# endif  

# ifdef NBQ
      real wz(GLOBAL_2D_ARRAY,0:N,3)
      common /ocean_wz/wz
      real Hzr(GLOBAL_2D_ARRAY,N)
      common /grid_Hzr/Hzr
      real Hz_half(GLOBAL_2D_ARRAY,N)
      common /grid_Hz_half/Hz_half
      real Hz_half_bak(GLOBAL_2D_ARRAY,N)
      common /grid_Hz_half_bak/Hz_half_bak
      real dHzdt(GLOBAL_2D_ARRAY,N)
      common /grid_dHzdt/dHzdt
#  if defined NBQ_CONSOUT || defined NBQ_CONS0
      real u2(GLOBAL_2D_ARRAY,N,3)
      common /ocean_u2/u2
      real We3(GLOBAL_2D_ARRAY,0:N)
      common /grid_We3/We3
#  endif
#  if defined NBQ_CONSOUT || defined NBQ_CONS2
      real We2(GLOBAL_2D_ARRAY,0:N)
      common /grid_We2/We2
#  endif
      real h2d(GLOBAL_2D_ARRAY)
      common /h2d/ h2d
# endif

# if defined UV_VIS4 && defined UV_MIX_GEO
      real z_u(GLOBAL_2D_ARRAY,N)
      real z_v(GLOBAL_2D_ARRAY,N)
      real dz_u(GLOBAL_2D_ARRAY,N)
      real dz_v(GLOBAL_2D_ARRAY,N)
      common /grid_zu/z_u /grid_zv/z_v
      common /grid_dz_u/dz_u /grid_dz_v/dz_v
# endif

      real rho1(GLOBAL_2D_ARRAY,N)
      real rho(GLOBAL_2D_ARRAY,N)
      common /ocean_rho1/rho1 /ocean_rho/rho
# ifdef BIOLOGY
#  ifdef BIO_NChlPZD
      real theta(GLOBAL_2D_ARRAY,N)
      common /ocean_theta/theta
#  elif defined BIO_BioEBUS  
      real AOU(GLOBAL_2D_ARRAY,N)
      common /ocean_AOU/AOU
#  endif
# endif  /* BIOLOGY */
# if defined NONLIN_EOS && defined SPLIT_EOS
      real qp1(GLOBAL_2D_ARRAY,N)
      common /ocean_qp1/qp1
      real qp2
      parameter (qp2=0.0000172)
# endif
#endif  /* SOLVE3D */

