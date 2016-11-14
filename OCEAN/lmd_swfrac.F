! $Id: lmd_swfrac.F 1458 2014-02-03 15:01:25Z gcambon $
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
#if defined LMD_SKPP || defined LMD_BKPP || defined GLS_MIXING

      subroutine lmd_swfrac_tile (Istr,Iend,Jstr,Jend, Zscale,Z,swdk)
!
!--------------------------------------------------------------------
!  This subroutine computes the  fraction of solar shortwave flux
!  penetrating to specified depth (times Zscale) due to exponential
!  decay in Jerlov water type.
!
!  Input:
!          Zscale   scale factor to apply to depth array
!          Z        vertical height [meters, negative] for
!                        desired solar short-wave fraction.
!  Output:
!          swdk     shortwave (radiation) fractional decay.
!
!  Reference:
!
!  Paulson, C.A., and J.J. Simpson, 1977: Irradiance meassurements
!  in the upper ocean, J. Phys. Oceanogr., 7, 952-956.
!
!  This routine was adapted from Bill Large 1995 code.
!--------------------------------------------------------------------
!
      implicit none
# include "param.h"
# include "scalars.h"
# include "lmd_kpp.h"
      integer Istr,Iend,Jstr,Jend, i,j,indx
      real Zscale, Z(PRIVATE_2D_SCRATCH_ARRAY),
     &             swdk(PRIVATE_2D_SCRATCH_ARRAY)
      integer lmd_Jwt
      real lmd_mu1(5), lmd_mu2(5), lmd_r1(5), cff1, cff2, cff3 ,cff4

      lmd_mu1(1)=0.35    !  Define reciprocal of the absorption
      lmd_mu1(2)=0.6     !  coefficient for each of two solar
      lmd_mu1(3)=1.0     !  wavelength bands as a function 
      lmd_mu1(4)=1.5     !  of water type (Ref: Paulson and
      lmd_mu1(5)=1.4     !  Simpson, 1977).

      lmd_mu2(1)=23.0
      lmd_mu2(2)=20.0
      lmd_mu2(3)=17.0
      lmd_mu2(4)=14.0
      lmd_mu2(5)=7.9

      lmd_r1(1)=0.58    !  Define fraction of the total radiance 
      lmd_r1(2)=0.62    !  for wavelength band 1 as a function of 
      lmd_r1(3)=0.67    !  Jerlov water type. The fraction for
      lmd_r1(4)=0.77    !  wavelength band 2 is lmd_r2=1-lmd_r1.
      lmd_r1(5)=0.78
                        !  Set Jerlov water type to assign
      lmd_Jwt=1         !  everywhere; an integer from 1 to 5.

!
!  On first call: initialize (for now) initialize index for Jerlov
!  water type to 1 (type I). This variable is used in the computation
!  of the fraction solar shortwave flux.
!
      if (iic.eq.ntstart) then
        do j=Jstr,Jend
          do i=Istr,Iend
            Jwtype(i,j)=lmd_Jwt
          enddo
        enddo
      endif
!
!  Use Paulson and Simpson (1977) two wavelength bands solar
!  absorption model.
!
      do j=Jstr,Jend
        do i=Istr,Iend
          indx=Jwtype(i,j)      
          cff1=Z(i,j)*Zscale/lmd_mu1(indx)
          cff2=Z(i,j)*Zscale/lmd_mu2(indx)
          if (cff1.ge.-20.) then            ! Quick fix to avoid computing 
            cff3=lmd_r1(indx) *exp(cff1)    ! exp(-15000)
          else
            cff3=0.
          endif
          if (cff2.ge.-20.) then         
            cff4=(1.-lmd_r1(indx)) *exp(cff2)
          else
            cff4=0.
          endif
          swdk(i,j)=cff3+cff4
        enddo
      enddo
      return
      end

      subroutine lmd_swfrac_ER_tile (Istr,Iend,Jstr,Jend, Zscale,Z,swdk)
!
!--------------------------------------------------------------------
!  This subroutine computes the  fraction of solar shortwave flux
!  penetrating to specified depth (times Zscale) due to exponential
!  decay in Jerlov water type.
!
!  Input:
!          Zscale   scale factor to apply to depth array
!          Z        vertical height [meters, negative] for
!                        desired solar short-wave fraction.
!  Output:
!          swdk     shortwave (radiation) fractional decay.
!
!  Reference:
!
!  Paulson, C.A., and J.J. Simpson, 1977: Irradiance meassurements
!  in the upper ocean, J. Phys. Oceanogr., 7, 952-956.
!
!  This routine was adapted from Bill Large 1995 code.
!--------------------------------------------------------------------
!
      implicit none
# include "param.h"
# include "scalars.h"
# include "lmd_kpp.h"
      integer Istr,Iend,Jstr,Jend, i,j,indx
      integer imin,imax,jmin,jmax
      real Zscale, Z(PRIVATE_2D_SCRATCH_ARRAY),
     &             swdk(PRIVATE_2D_SCRATCH_ARRAY)
      integer lmd_Jwt
      real lmd_mu1(5), lmd_mu2(5), lmd_r1(5), cff1, cff2, cff3 ,cff4

      lmd_mu1(1)=0.35    !  Define reciprocal of the absorption
      lmd_mu1(2)=0.6     !  coefficient for each of two solar
      lmd_mu1(3)=1.0     !  wavelength bands as a function 
      lmd_mu1(4)=1.5     !  of water type (Ref: Paulson and
      lmd_mu1(5)=1.4     !  Simpson, 1977).

      lmd_mu2(1)=23.0
      lmd_mu2(2)=20.0
      lmd_mu2(3)=17.0
      lmd_mu2(4)=14.0
      lmd_mu2(5)=7.9

      lmd_r1(1)=0.58    !  Define fraction of the total radiance 
      lmd_r1(2)=0.62    !  for wavelength band 1 as a function of 
      lmd_r1(3)=0.67    !  Jerlov water type. The fraction for
      lmd_r1(4)=0.77    !  wavelength band 2 is lmd_r2=1-lmd_r1.
      lmd_r1(5)=0.78
                        !  Set Jerlov water type to assign
      lmd_Jwt=1         !  everywhere; an integer from 1 to 5.

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

!
!  On first call: initialize (for now) initialize index for Jerlov
!  water type to 1 (type I). This variable is used in the computation
!  of the fraction solar shortwave flux.
!
      if (iic.eq.ntstart) then
        do j=J_EXT_RANGE
          do i=I_EXT_RANGE
            Jwtype(i,j)=lmd_Jwt
          enddo
        enddo
      endif
!
!  Use Paulson and Simpson (1977) two wavelength bands solar
!  absorption model.
!
      do j=J_EXT_RANGE
        do i=I_EXT_RANGE
          indx=Jwtype(i,j)      
          cff1=Z(i,j)*Zscale/lmd_mu1(indx)
          cff2=Z(i,j)*Zscale/lmd_mu2(indx)
          if (cff1.ge.-20.) then            ! Quick fix to avoid computing 
            cff3=lmd_r1(indx) *exp(cff1)    ! exp(-15000)
          else
            cff3=0.
          endif
          if (cff2.ge.-20.) then         
            cff4=(1.-lmd_r1(indx)) *exp(cff2)
          else
            cff4=0.
          endif
          swdk(i,j)=cff3+cff4
        enddo
      enddo
      return
      end
#else
      subroutine lmd_swfrac
      return
      end
#endif /* LMD_SKPP || LMD_BKPP || GLS_MIXING */ 


