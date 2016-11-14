! $Id: dynparam.h 1458 2014-02-03 15:01:25Z gcambon $
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
! Dimensions of Physical Grid and array dimensions: 
! =========== == ======== ==== === ===== ============
! LLm,MMm  Number of the internal points of the PHYSICAL grid.
!          in the XI- and ETA-directions [physical side boundary
!          points and peroodic ghost points (if any) are excluded].
!
! Lm,Mm    Number of the internal points [see above] of array
!          covering a Message Passing subdomain. In the case when
!          no Message Passing partitioning is used, these two are
!          the same as LLm,MMm. 
!
      LLm=LLm0;  MMm=MMm0      ! LLm0,MMm0 defined in param.h

