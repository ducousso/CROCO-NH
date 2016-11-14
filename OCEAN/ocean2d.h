! $Id: ocean2d.h 1587 2014-08-04 14:56:01Z marchesiello $
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
/* This is include file "ocean2d.h".
--------------------------------------------------------------------
 zeta,rheta     Free surface elevation [m] and its time tendency;
 ubar,rubar     Vertically integrated  2D velocity components in 
 vbar,rvbar     XI- and ETA-directions and their time tendencies;
*/
      real zeta(GLOBAL_2D_ARRAY,4)
      real ubar(GLOBAL_2D_ARRAY,4)
      real vbar(GLOBAL_2D_ARRAY,4)
      common /ocean_zeta/zeta
      common /ocean_ubar/ubar
      common /ocean_vbar/vbar

#if !defined SOLVE3D && defined M2_HADV_UP3
      real urhs(GLOBAL_2D_ARRAY)
      real vrhs(GLOBAL_2D_ARRAY)
      real Duon(GLOBAL_2D_ARRAY)
      real DVom(GLOBAL_2D_ARRAY)
#endif

#ifdef OXYGEN
      real u10(GLOBAL_2D_ARRAY)
      real Kv_O2(GLOBAL_2D_ARRAY)
      real O2satu(GLOBAL_2D_ARRAY)
      common /gasexc_O2_u10/ u10 
      common /gasexc_O2_Kv_O2/ Kv_O2
      common /gasexc_O2_O2satu/ O2satu
#endif /* OXYGEN */

# if defined BIOLOGY && defined BIO_BioEBUS
      real wind10(GLOBAL_2D_ARRAY)
      common /ocean_wind10/wind10      
# endif 
