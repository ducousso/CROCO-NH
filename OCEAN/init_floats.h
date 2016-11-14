! $Id: init_floats.h 1458 2014-02-03 15:01:25Z gcambon $
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
/*
** Include file "init_floats.h".
**********************************************************************
** Copyright (c) 2000 Rutgers/UCLA                                  **
**********************************************************************
**                                                                  **
** Ftitle     Floats application title.                             **
** Ft0, Fx0, ... input trajectory variables red in the floats.in file. 
** These variables are used by put_global_atts.F to write this      **
** float.in into netcdf float output file.                          ** 
**********************************************************************
*/

      real Ft0(Mfloats), Fx0(Mfloats), Fy0(Mfloats), Fz0(Mfloats),
     &        Fdt(Mfloats), Fdx(Mfloats), Fdy(Mfloats), Fdz(Mfloats)
      common /ncrealfloats/ Ft0, Fx0, Fy0, Fz0, Fdt, Fdx, Fdy, Fdz

      integer  Fcoor(Mfloats),  Ftype(Mfloats), Fcount(Mfloats), 
     &         Fgrd(Mfloats), i_floats
      common /ncintfloats/ Fcoor, Ftype, Fcount, Fgrd, i_floats

      character*80 Ftitle
      common /nccharfloats/ Ftitle
