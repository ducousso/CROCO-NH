! $Id: online_interpolate_bulk.F 1458 2014-02-03 15:01:25Z gcambon $
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
!
!  This is the "online_interpolate_bulk.F" script
!------------------------------------------------------------------------------
!  This file contains the subfunctions enabling the online extraction of the 
!  forcing from a NCEP/CFSR dataset. A spatial and time interpolation are applied 
!  to the extracted data in order to adapt these to the considered simulation 
!  domain as well as the associated MPI/OPENMP discretisation (if defined MPI/
!  OPENMP).
!------------------------------------------------------------------------------
#include "cppdefs.h"

#if defined BULK_FLUX && defined ONLINE 


!*******************************************************************************
      subroutine interpolate_bulk_online(NXref,NYref,
     &                                   lonref,latref,varref,
     &                                   blkvar_id,iblkrec)
!------------------------------------------------------------------------------
!  This subfunction complete the call to the interpolation and the recalculation 
!  of the forcing fields to generate the roms bulk forcing (tairg, rhumg, prateg,
!  radlwg, radswg, wspdg, uwndg, vwndg) on the current tile. 
!------------------------------------------------------------------------------
!  The main steps of this interpolation and recalculation are:
!  - Interpolation from the varref matrix of the dum_array matrix on the 
!  simulation domain for the blkvar_id variable.
!  - Recalculation of the roms bulk forcings (tairg, rhumg, prateg, radlwg, 
!  radswg, wspdg, uwndg, vwndg) from the interpolated dum_array fields.

!  INPUTS
!  NXref (Vertical size of the input matrix used for the interpolation)
!  NYref (Horizontal size of the input matrix used for the interpolation)
!  lonref (longitudes of the references data used for the interpolation)
!  latref (latitudes of the references data used for the interpolation)
!  varref (reference data used for the interpolation)
!  Istr, Iend, Jstr, Jend (limits index of the interpolation)
!  blkvar_id (index of the extrapolated bulk variable)
!  iblkrec (index of the leap alternation)
!
!  OUTPUTS
!  dum_array (interpolated forcing fields)
!  tairg, rhumg, prateg, radlwg, radswg, wspdg, uwndg, vwndg (recalculated roms 
!                                                             bulk forcings)
      implicit none
# include "param.h"
# include "forces.h"
# include "scalars.h"
# include "ncscrum.h"
# include "grid.h"
# include "online.h"
      integer, intent(in) :: NXref, NYref, blkvar_id, iblkrec
      real(kind=8), intent(in) :: lonref(1:NXref), latref(1:NYref)    
      real(kind=8), intent(in) :: varref(1:NXref,1:NYref)
      real blk_dum1(GLOBAL_2D_ARRAY)
      real blk_dum2(GLOBAL_2D_ARRAY)
      real Pref, ew, Qsat
      integer i,j


      Pref=1020.         ! default air pressure [mbars]

      !--------
      ! Limits:
      !--------
      ! Local loop ranges 
      ! This is not parallel with openMP

# ifdef MPI
#  define I_RANGE 0,Lmmpi+1
#  define J_RANGE 0,Mmmpi+1
# else
#  define I_RANGE 0,LLm+1
#  define J_RANGE 0,MMm+1
# endif

      !===============
      ! Interpolation:
      !===============

# ifdef CUBIC_INTERP 
#  define MYINTERP cinterp2d
# else
#  define MYINTERP linterp2d
# endif
  
      if (blkvar_id.eq.1) then
!
! 1 process Tair
! Temperature: Convert from Kelvin to Celsius
!
        call MYINTERP(1, NXref, 1, NYref,
     &                lonref, latref, varref,
     &                I_RANGE,J_RANGE,
     &                lonr(GLOBAL_2D_ARRAY), 
     &                latr(GLOBAL_2D_ARRAY),
     &                tairg(GLOBAL_2D_ARRAY,iblkrec))

        do j=J_RANGE
          do i=I_RANGE
            tairg(i,j,iblkrec)=tairg(i,j,iblkrec)-273.15
          enddo
        enddo
# ifdef MPI
	call exchange_r2d_tile(1,Lm,1,Mm,
     &                         tairg(START_2D_ARRAY,iblkrec))
# endif

      elseif  (blkvar_id.eq.2) then
!
! 2 process Rhum
! Relative humidity: Convert specific humidity to relative
!
        call MYINTERP(1, NXref, 1, NYref,
     &                lonref, latref, varref,
     &                I_RANGE,J_RANGE,
     &                lonr(GLOBAL_2D_ARRAY), 
     &                latr(GLOBAL_2D_ARRAY),
     &                rhumg(GLOBAL_2D_ARRAY,iblkrec))
        
        do j=J_RANGE
          do i=I_RANGE
            ew=6.1121*(1.0007+3.46e-6*Pref)*
     &         exp((17.502*tairg(i,j,iblkrec))/
     &             (240.97+tairg(i,j,iblkrec)))
            Qsat=0.62197*(ew/(Pref-0.378*ew))
            rhumg(i,j,iblkrec)=rhumg(i,j,iblkrec)/Qsat
          enddo
        enddo
# ifdef MPI
	call exchange_r2d_tile(1,Lm,1,Mm,
     &                         rhumg(START_2D_ARRAY,iblkrec))
# endif

      elseif (blkvar_id.eq.3) then
!
! 3 process Prate
! Precipitation rate: Convert from [kg/m^2/s] to cm/day
!
!
        call MYINTERP(1, NXref, 1, NYref,
     &                lonref, latref, varref,
     &                I_RANGE,J_RANGE,
     &                lonr(GLOBAL_2D_ARRAY), 
     &                latr(GLOBAL_2D_ARRAY),
     &                prateg(GLOBAL_2D_ARRAY,iblkrec))
        
        do j=J_RANGE
          do i=I_RANGE
            prateg(i,j,iblkrec)=prateg(i,j,iblkrec)*8640.
            if (1.e-4.gt.abs(prateg(i,j,iblkrec))) then
              prateg(i,j,iblkrec)=0.
            endif
          enddo
        enddo
      
      elseif (blkvar_id.eq.4) then
!
! 4 process radsw (warning!! first (4) downward then (5) upward)
! Net solar shortwave radiation: Downwards short wave  
!
#  ifdef ERA_ECMWF
! In the case of ERA we used the net short wave - downward short wave is not used
#  else
        call MYINTERP(1, NXref, 1, NYref,
     &                lonref, latref, varref,
     &                I_RANGE,J_RANGE,
     &                lonr(GLOBAL_2D_ARRAY), 
     &                latr(GLOBAL_2D_ARRAY),
     &                radswg(GLOBAL_2D_ARRAY,iblkrec))

#  endif      
      elseif (blkvar_id.eq.5) then
!
! 5 process radsw (warning!! first (4) downward then (5) upward)
! Net solar shortwave radiation: Downwards short wave - Upward short wave 
!
        call MYINTERP(1, NXref, 1, NYref,
     &                lonref, latref, varref,
     &                I_RANGE,J_RANGE,
     &                lonr(GLOBAL_2D_ARRAY), 
     &                latr(GLOBAL_2D_ARRAY),
     &                blk_dum1(GLOBAL_2D_ARRAY))

        do j=J_RANGE
          do i=I_RANGE
#  ifdef ERA_ECMWF
            radswg(i,j,iblkrec)=blk_dum1(i,j)
#  else
            radswg(i,j,iblkrec)=radswg(i,j,iblkrec)-blk_dum1(i,j)
#  endif      
            if (1e-10.gt.abs(radswg(i,j,iblkrec))) then
              radswg(i,j,iblkrec)=0.
            endif
          enddo
        enddo

      elseif (blkvar_id.eq.6) then
!
! 6 process radlw 
! Net longwave flux: Downward long wave
!
        call MYINTERP(1, NXref, 1, NYref,
     &                lonref, latref, varref,
     &                I_RANGE,J_RANGE,
     &                lonr(GLOBAL_2D_ARRAY), 
     &                latr(GLOBAL_2D_ARRAY),
     &                radlwg(GLOBAL_2D_ARRAY,iblkrec))


      
      elseif (blkvar_id.eq.7) then
!
! 7 upward longwave: OBSOLETE
!
      elseif (blkvar_id.eq.8) then
!
! 8 process uwnd
! U-component_of_wind
!
# ifdef BULK_SM_UPDATE
        call MYINTERP(1, NXref, 1, NYref,
     &                lonref, latref, varref,
     &                I_RANGE,J_RANGE,
     &                lonr(GLOBAL_2D_ARRAY), 
     &                latr(GLOBAL_2D_ARRAY),
     &                uwndg(GLOBAL_2D_ARRAY,iblkrec))

# else
       USE_BULK_SM_UPDATE !
# endif
      elseif (blkvar_id.eq.9) then
!
! 9 process vwnd
! V-component_of_wind
!
! + rotates the wind and put it at u- and v- points
!
# ifdef BULK_SM_UPDATE
        call MYINTERP(1, NXref, 1, NYref,
     &                lonref, latref, varref,
     &                I_RANGE,J_RANGE,
     &                lonr(GLOBAL_2D_ARRAY), 
     &                latr(GLOBAL_2D_ARRAY),
     &                vwndg(GLOBAL_2D_ARRAY,iblkrec))
     
        do j=J_RANGE
          do i=I_RANGE
#  ifndef RELATIVE_WIND
            wspdg(i,j,iblkrec)=sqrt(uwndg(i,j,iblkrec)*uwndg(i,j,iblkrec)
     &                             +vwndg(i,j,iblkrec)*vwndg(i,j,iblkrec))
#  endif
#  ifdef CURVGRID
            blk_dum1(i,j)=uwndg(i,j,iblkrec)*COS(angler(i,j))+
     &                    vwndg(i,j,iblkrec)*SIN(angler(i,j))
            blk_dum2(i,j)=vwndg(i,j,iblkrec)*COS(angler(i,j))-
     &                         uwndg(i,j,iblkrec)*SIN(angler(i,j))
#  else
            blk_dum1(i,j)=uwndg(i,j,iblkrec)
            blk_dum2(i,j)=vwndg(i,j,iblkrec)
#  endif
           enddo
        enddo
	 
#  ifdef MPI
        do j=0,Mmmpi+1
          do i=1,Lmmpi+1
#  else
        do j=0,MMm+1
          do i=1,LLm+1
#  endif
            uwndg(i,j,iblkrec)=0.5*(blk_dum1(i-1,j)+blk_dum1(i,j))
          enddo
        enddo

#  ifdef MPI
        do j=1,Mmmpi+1
          do i=0,Lmmpi+1
#  else
        do j=1,MMm+1
          do i=0,LLm+1
#  endif
            vwndg(i,j,iblkrec)=0.5*(blk_dum2(i,j-1)+blk_dum2(i,j))
          enddo
        enddo

#  ifdef MPI
	call exchange_u2d_tile(1,Lm,1,Mm,
     &                         uwndg(START_2D_ARRAY,iblkrec))

	call exchange_v2d_tile(1,Lm,1,Mm,
     &                         vwndg(START_2D_ARRAY,iblkrec))
#   ifndef RELATIVE_WIND
	call exchange_r2d_tile(1,Lm,1,Mm,
     &                         wspdg(START_2D_ARRAY,iblkrec))
#   endif
#  endif


# else
      USE_BULK_SM_UPDATE !
# endif
      endif
 
      return
      end


#endif /* BULK_FLUX && ONLINE */
