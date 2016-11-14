! $Id: o2sato.F 1458 2014-02-03 15:01:25Z gcambon $
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
!  ---------------------------------------------------------------------
!  $Log: o2sato.F,v $
!  Revision 1.1  2001/10/19 21:22:14  frenzel
!  Routines from the OCMIP project for the ROMS carbonized ecosystem model.
!  Some of these routines were adapted for the use in a parallel code.
!
! 
!  Revision 1.1  1999/03/22 12:57:48  orr
!  Initial revision
! 
!  ---------------------------------------------------------------------
!  
      function o2sato(T,S)
!
! ********************************************************************
!                                     
! Computes the oxygen saturation concentration at 1 atm total pressure
! in mol/m^3 given the temperature (t, in deg C) and the salinity (s,
! in permil). 
!
! FROM GARCIA AND GORDON (1992), LIMNOLOGY and OCEANOGRAPHY.
! THE FORMULA USED IS FROM PAGE 1310, EQUATION (8).
!
! *** NOTE: THE "A3*TS^2" TERM (IN THE PAPER) IS INCORRECT. ***
! *** IT SHOULDN'T BE THERE.                                ***
!
! o2sato IS DEFINED BETWEEN T(freezing) <= T <= 40(deg C) AND
! 0 permil <= S <= 42 permil
! C
! CHECK VALUE:  T = 10.0 deg C, S = 35.0 permil, 
! o2sato = 0.282015 mol/m^3
!
! The unit for the return value is now [mmol/m^3], as needed by ROMS
!
! ********************************************************************
!
      implicit none
      real    o2sato

! input variables
      real   T   ! Temperature [C]
      real   S   ! Salinity [PSU]

      real A0, A1, A2, A3, A4, A5, B0, B1, B2, B3, C0
      parameter (A0 = 2.00907, A1 = 3.22014, A2 = 4.05010,
     &           A3 = 4.94457, A4 = -2.56847E-1, A5 = 3.88767,
     &           B0=-6.24523E-3, B1=-7.37614E-3, B2=-1.03410E-2,
     &           B3=-8.17083E-3, C0=-4.88682E-7)

! other variables
      real    TT      
      real    TK        ! Temperature [K]
      real    TS, TS2, TS3, TS4, TS5
      real    CO
!
      TT  = 298.15-T
      TK  = 273.15+T
      TS  = LOG(TT/TK)
      TS2 = TS**2
      TS3 = TS**3
      TS4 = TS**4
      TS5 = TS**5
      CO  = A0 + A1*TS + A2*TS2 + A3*TS3 + A4*TS4 + A5*TS5
     $     + S*(B0 + B1*TS + B2*TS2 + B3*TS3)
     $     + C0*(S*S)
      o2sato = EXP(CO)
!
!  Convert from ml/l to mmol/m^-3 for ROMS
!
      o2sato = o2sato/22.3916*1000.0
      return
      end
