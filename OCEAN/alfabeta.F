! $Id: alfabeta.F 1458 2014-02-03 15:01:25Z gcambon $
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
#if defined LMD_SKPP || defined LMD_BKPP
      subroutine alfabeta_tile (Istr,Iend,Jstr,Jend, alpha,beta)
!
!--------------------------------------------------------------------
!  This routine computes thermal expansion and saline contraction
!  coefficients as a function of potential temperature, salinity,
!  and pressure from a polynomial expression (Jackett & McDougall,
!  1992). The polynomial expression was found from fitting to 248
!  values  in the  oceanographic  ranges of  salinity,  potential
!  temperature,  and pressure.  It  assumes no pressure variation
!  along geopotential surfaces,  that is,  depth and pressure are
!  interchangeable. The coefficients are evaluated at the surface.
!
!  On Output:
!
!     alpha    Thermal expansion coefficient [kg/m^3/Celsius].
!     beta     Saline contraction coefficient [kg/m^3/PSU].
!
!  Adapted from original "rati" and "beta" routines.
!
!  Copyright (c) 1996 Rutgers University
!--------------------------------------------------------------------
!
      implicit none
# include "param.h"
      integer Istr,Iend,Jstr,Jend, i,j
      integer imin,imax,jmin,jmax
      real    alpha(PRIVATE_2D_SCRATCH_ARRAY),
     &        beta(PRIVATE_2D_SCRATCH_ARRAY)
# include "grid.h"
# include "ocean3d.h"
# include "scalars.h"
# ifdef NONLIN_EOS
      real      Q00, Q01, Q02, Q03, Q04, Q05, U00, U01, U02, U03,
     &          U04, V00, V01, V02, W00
      parameter(Q00=+999.842594 , Q01=+6.793952E-2, Q02=-9.095290E-3,
     &          Q03=+1.001685E-4, Q04=-1.120083E-6, Q05=+6.536332E-9,
     &          U00=+0.824493   , U01=-4.08990E-3 , U02=+7.64380E-5 ,
     &          U03=-8.24670E-7 , U04=+5.38750E-9 , V00=-5.72466E-3 ,
     &          V01=+1.02270E-4 , V02=-1.65460E-6 , W00=+4.8314E-4  )
      real      Tt, Ts, sqrtTs, cff
# endif
!
# include "compute_auxiliary_bounds.h"
!
!---------------------------------------------------------------
!  Compute thermal expansion and saline contraction coefficients
!  at surface 
!
!  Ts salinity [PSU].
!  Tt potential temperature [deg Celsius].
!  den(Ts,Tt,0) surface density [kg/m^3]
!  rho1(Ts,Tt,0)=den(Ts,Tt,0)-1000. , computed from Jackett & 
!                                               McDougall, 1992)
!  alpha(Ts,Tt,0)=-d(rho1(Ts,Tt,0))/d(Tt) / den(Ts,Tt,0)
!  beta(Ts,Tt,0) = d(rho1(Ts,Tt,0))/d(Ts) / den(Ts,Tt,0)
!---------------------------------------------------------------
!
# ifdef EW_PERIODIC
#  define I_EXT_RANGE Istr-1,Iend+1
# else
      if (WESTERN_EDGE) then
        imin=Istr
      else
        imin=Istr-1
      endif
      if (EASTERN_EDGE) then
        imax=Iend
      else
        imax=Iend+1
      endif
#  define I_EXT_RANGE imin,imax
# endif
# ifdef NS_PERIODIC
#  define J_EXT_RANGE Jstr-1,Jend+1
# else
      if (SOUTHERN_EDGE) then
        jmin=Jstr
      else
        jmin=Jstr-1
      endif
      if (NORTHERN_EDGE) then
        jmax=Jend
      else
        jmax=Jend+1
      endif
#  define J_EXT_RANGE jmin,jmax
# endif

      do j=J_EXT_RANGE
        do i=I_EXT_RANGE
# ifdef NONLIN_EOS 
          Tt=t(i,j,N,nstp,itemp)
#  ifdef SALINITY
          Ts=t(i,j,N,nstp,isalt)
          sqrtTs=sqrt(Ts)
#  else
          Ts=0.
          sqrtTs=0.
#  endif
          cff=1./rho0

          alpha(i,j)=-cff*( Q01+Tt*(2.*Q02+Tt*(3.*Q03+
     &                                      Tt*(4.*Q04+Tt*5.*Q05)))
     &                   +Ts*(U01+Tt*(2.*U02+Tt*(3.*U03+Tt*4.*U04))
     &                                     +sqrtTs*(V01+Tt*2.*V02))
     &                                                            )

          beta(i,j)= cff*( U00+Tt*(U01+Tt*(U02+Tt*(U03+Tt*U04)))
     &                 +1.5*(V00+Tt*(V01+Tt*V02))*sqrtTs+2.*W00*Ts
     &                                                            )
# else     
!
!   Linear Equation of state thermal expansion and saline
!   contraction coefficients:
!
          alpha(i,j)=abs(Tcoef)
#  ifdef SALINITY
          beta(i,j)=abs(Scoef)
#  else
          beta(i,j)=0.
#  endif
# endif /* NONLIN_EOS */
        enddo
      enddo

c*      do i=Istr,Iend,4
c*        write(6,15) i,alpha(i,10)*1.e7,beta(i,10)*1.e7
c*      enddo
c*  15  format(1x,'i = ',i4,' alpha = ',f6.1,1x,'  beta = ',f6.1)

#else
      subroutine alfabeta_empty
#endif /* LMD_SKPP || LMD_BKPP */
      return
      end
