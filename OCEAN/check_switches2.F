! $Id: check_switches2.F 1458 2014-02-03 15:01:25Z gcambon $
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
#include "cppdefs.h"
      subroutine check_switches2 (ierr) 
      implicit none
      integer ierr, iwest, ieast, isouth, inorth, iAk
#include "param.h"
#include "scalars.h"
!
! Checking the number of selections of boundary conditions. 
!
      iwest=0
      ieast=0
#ifdef EW_PERIODIC
      iwest=iwest+1
      ieast=ieast+1
#endif
#ifdef OBC_WEST
      iwest=iwest+1
#endif
#ifdef OBC_EAST
      ieast=ieast+1
#endif
      if (iwest.gt.1) then
        write(stdout,'(1x,A,1x,A)')'ERROR in "cppdefs.h": more tnan',
     &        'one boundary condition is chosen on the WESTERN EGGE.'
        ierr=ierr+1
      endif
      if (ieast.gt.1) then
        write(stdout,'(1x,A,1x,A)')'ERROR in "cppdefs.h": more tnan',
     &        'one boundary condition is chosen on the EASTERN EGGE.'
        ierr=ierr+1
      endif

      isouth=0
      inorth=0
#ifdef NS_PERIODIC
      isouth=isouth+1
      inorth=inorth+1
#endif
#ifdef OBC_SOUTH
      isouth=isouth+1
#endif
#ifdef OBC_NORTH
      inorth=inorth+1
#endif
      if (isouth.gt.1) then
        write(stdout,'(1x,A,1x,A)')'ERROR in "cppdefs.h": more tnan',
     &       'one boundary condition is chosen on the SOUTHERN EGGE.'
        ierr=ierr+1
      endif 
      if (inorth.gt.1) then
        write(stdout,'(1x,A,1x,A)')'ERROR in "cppdefs.h": more tnan',
     &       'one boundary condition is chosen on the NORTHERN EGGE.'
        ierr=ierr+1
      endif
!
! Checking the number of selections vertical mixing schemes.
!
      iAk=0
#ifdef ANA_VMIX
      iAk=iAk+1 
#endif
#ifdef BVF_MIXING
      iAk=iAk+1
#endif
#ifdef LMD_MIXING
      iAk=iAk+1
#endif
#ifdef MY2_MIXING
      iAk=iAk+1
#endif
#ifdef MY25_MIXING
      iAk=iAk+1
#endif
#ifdef PP_MIXING
      iAk=iAk+1
#endif
      if (iAk.gt.1) then
        write(stdout,'(1x,A,1x,A)') 'ERROR in "cppdefs.h":',
     &       'more than one vertical mixing scheme is chosen.'
        ierr=ierr+1
      endif
!
! Stop if explicit time-step splitting on shallow water set-up.
!
#ifndef SOLVE3D
      if (ndtfast.gt.1) then
        write(stdout,'(1x,A,I3,1x,A/8x,A,6x,A)') 'ERROR: NDTFAST =',
     &         ndtfast, 'is greater than unity for a shallow water',
     &        'configuration.','Change it to unity in startup file.'
     &           
        ierr=ierr+1
      endif
#endif
      return
      end
