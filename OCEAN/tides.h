! $Id: tides.h 1458 2014-02-03 15:01:25Z gcambon $
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
!** Include file "tides.h"
!*************************************************** Robert Hetland ***
!** Copyright (c) 2000 Rutgers/UCLA                                  **
!************************************************* Hernan G. Arango ***
!**                                                                  **
!** Tidal Components:                                                **
!**                                                                  **
!** Each of the following arrays has a dimension in tidal components **
!** classified by period:                                            **
!**                                                                  **
!**   semi-diurnal:  M2, S2, N2, K2  (12.42, 12.00, 12.66, 11.97h)   **
!**        diurnal:  K1, O1, P1, Q1  (23.93, 25.82, 24.07, 26.87h)   **
!**                                                                  **
!** and other longer periods. The order of these tidal components is **
!** irrelevant here.  The number of components to use depends on     **
!** the regional application.                                        **
!**                                                                  **
!** NTC          Number of tidal components to consider.             **
!** SSH_Tamp     Tidal elevation amplitude (m) at RHO-points.        **
!** SSH_Tphase   Tidal elevation phase (degrees/360) at RHO-points.  **
!** Tperiod      Tidal period (s).                                   **
!** UV_Tangle    Tidal current angle (radians; counterclockwise      **
!**                from EAST and rotated to curvilinear grid) at     **
!**                RHO-points.                                       **
!** UV_Tmajor    Maximum tidal current: tidal ellipse major axis     **
!**                (m/s) at RHO-points.                              **
!** UV_Tminor    Minimum tidal current: tidal ellipse minor axis     **
!**                (m/s) at RHO-points.                              **
!** UV_Tphase    Tidal current phase (degrees/360) at RHO-points.    **
!**                                                                  **
!**********************************************************************
#if defined SSH_TIDES || defined UV_TIDES
      real Tperiod(Ntides)
      common /tides_Tperiod/ Tperiod
#endif /* SSH_TIDES || UV_TIDES */

#if defined SSH_TIDES && (defined ZCLIMATOLOGY || defined Z_FRC_BRY)
      real SSH_Tamp(GLOBAL_2D_ARRAY,Ntides)
      common /tides_SSH_Tamp/ SSH_Tamp

      real SSH_Tphase(GLOBAL_2D_ARRAY,Ntides)
      common /tides_SSH_Tphase/ SSH_Tphase
#endif

#if defined UV_TIDES && (defined M2CLIMATOLOGY || defined M2_FRC_BRY)
      real UV_Tangle(GLOBAL_2D_ARRAY,Ntides)
      common /tides_UV_Tangle/ UV_Tangle

      real UV_Tmajor(GLOBAL_2D_ARRAY,Ntides)
      common /tides_UV_Tmajor/ UV_Tmajor

      real UV_Tminor(GLOBAL_2D_ARRAY,Ntides)
      common /tides_UV_Tminor/ UV_Tminor

      real UV_Tphase(GLOBAL_2D_ARRAY,Ntides)
      common /tides_UV_Tphase/ UV_Tphase
#endif

#ifdef POT_TIDES
      real POT_Tamp(GLOBAL_2D_ARRAY,Ntides)
      common /tides_POT_Tamp/ POT_Tamp

      real POT_Tphase(GLOBAL_2D_ARRAY,Ntides)
      common /tides_POT_Tphase/ POT_Tphase

      real PTide(GLOBAL_2D_ARRAY)
      common /tides_Ptide/ Ptide
#endif

