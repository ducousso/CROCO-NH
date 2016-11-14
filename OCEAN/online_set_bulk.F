! $Id: online_set_bulk.F 1458 2014-02-03 15:01:25Z gcambon $
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
!  This is the "online_set_bulk.F" script
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
      subroutine set_bulk_tile_online (Istr,Iend,Jstr,Jend)
!------------------------------------------------------------------------------
!  This subfunction enables the time interpolation of the roms bulk forcings 
!  (tair, rhum, prate, radlw, radsw, wspd, uwnd, vwnd) on the simulation domain
!  for the ongoing tile.
!------------------------------------------------------------------------------
!  The steps in this interpolation is:
!  - Determination of forcings (tair, rhum, prate, radlw, radsw, wspd, uwnd, 
!  vwnd) at the first time step of the simulation from the known (tairg, rhumg, 
!  prateg, radlwg, radswg, wspdg, uwndg, vwndg) fields.
!  - Time interpolation of the forcings (tair, rhum, prate, radlw, radsw, wspd, 
!  uwnd, vwnd) at cff from two known spatially interpolated fields (tairg,rhumg,
!  prateg, radlwg, radswg, wspdg, uwndg, vwndg) at cff1 and cff2.
!
!  In this subroutine the aformentioned steps are completed for every bulk 
!  forcing iterating on the various blkvar_id.
      implicit none
# include "param.h"
# include "forces.h"
# include "scalars.h"
# include "grid.h"
# ifdef RELATIVE_WIND
#  include "ocean3d.h"
# endif
# include "online.h"
      integer Istr,Iend,Jstr,Jend, i,j, it1,it2, blkvar_id
      real cff,cff1,cff2, cff3,cff4
# ifdef BULK_EP
      real cff5,cff6
# endif
# ifndef BULK_SM_UPDATE
      real cff7,cff8
# endif
# ifdef RELATIVE_WIND
      real cff9,cff10
# endif
# include "compute_extended_bounds.h"

      !===== == === === ======= ========= 
      ! Loop on all the forcing datasets:
      !===== == === === ======= =========  
      blkvar_id=0
  10  blkvar_id = blkvar_id+1
!
! Do not process #7 :  upward longwave is obsolete
!
        if (blkvar_id.eq.7) goto 10
#  ifdef ERA_ECMWF
!
! In the case of ERA the net short wave is used
! Net Short wave - 5 downward short wave not used
!
        if (blkvar_id.eq.4) goto 10
#  endif

      if(blkvar_id.gt.nblkvrs) return

      it1=3-itbulkO(blkvar_id)
      it2=itbulkO(blkvar_id)
      ! --------------------------
      ! Times of the interpolation
      ! --------------------------
      cff=time+0.5*dt
      cff1=bulk_timeO(it2,blkvar_id)-cff
      cff2=cff-bulk_timeO(it1,blkvar_id)


      !===== ============= ======= === ==== ====== 
      ! Time interpolation between two time steps:
      !===== ============= ======= === ==== ====== 
      ! The roms bulk forcings at cff are interpolated between two time steps 
      ! cff1 and cff2
      if (cff1.ge.0. .and. cff2.ge.0.) then
        if (ZEROTH_TILE .and. cff1.lt.dt) synchro_flag=.TRUE.
                                    !note cff order maters
        cff=srf_scale/(cff1+cff2)
        cff3=cff1*cff
        cff4=cff2*cff
# ifdef BULK_EP
        cff=stf_scale(isalt)/(cff1+cff2)
        cff5=cff1*cff
        cff6=cff2*cff
# endif
# ifdef BULK_SMFLUX
#  ifndef BULK_SM_UPDATE 
        cff=sms_scale/(cff1+cff2)
        cff7=cff1*cff
        cff8=cff2*cff
#  endif 
# endif 
        cff=1./(cff1+cff2)
        cff1=cff1*cff
        cff2=cff2*cff

        if (blkvar_id.eq.1) then
          do j=JstrR,JendR
            do i=IstrR,IendR
              tair(i,j)=cff1*tairg(i,j,it1)+cff2*tairg(i,j,it2)
            enddo
          enddo
        elseif (blkvar_id.eq.2) then
          do j=JstrR,JendR
            do i=IstrR,IendR
              rhum(i,j)=cff1*rhumg(i,j,it1)+cff2*rhumg(i,j,it2)
            enddo
          enddo
# ifdef BULK_EP
        elseif (blkvar_id.eq.3) then
          do j=JstrR,JendR
            do i=IstrR,IendR
              prate(i,j)=cff5*prateg(i,j,it1)+cff6*prateg(i,j,it2)
            enddo
          enddo
# endif
        elseif (blkvar_id.eq.5) then
          do j=JstrR,JendR
            do i=IstrR,IendR
              radsw(i,j)=cff3*radswg(i,j,it1)+cff4*radswg(i,j,it2)
              srflx(i,j)=radsw(i,j)
            enddo
          enddo
        elseif (blkvar_id.eq.6) then
          do j=JstrR,JendR
            do i=IstrR,IendR
              radlw(i,j)=cff3*radlwg(i,j,it1)+cff4*radlwg(i,j,it2)
            enddo
          enddo
        elseif (blkvar_id.eq.9) then
# ifdef RELATIVE_WIND
          do j=JstrR,JendR
#  ifdef MPI
            do i=IstrR,Lmmpi+2
#  else
            do i=IstrR,LLm+2
#  endif
              uwnd(i,j)=cff1*uwndg(i,j,it1)+cff2*uwndg(i,j,it2)
              uwnd(i,j)=uwnd(i,j)-u(i,j,N,nrhs)
            enddo
          enddo
#  ifdef MPI
          do j=JstrR,Mmmpi+2
#  else
          do j=JstrR,MMm+2
#  endif
            do i=IstrR,IendR
              vwnd(i,j)=cff1*vwndg(i,j,it1)+cff2*vwndg(i,j,it2)
              vwnd(i,j)=vwnd(i,j)-v(i,j,N,nrhs)
            enddo
          enddo
          do j=JstrR,JendR
            do i=IstrR,IendR
              cff9 =0.5*(uwnd(i,j)+uwnd(i+1,j  ))
              cff10=0.5*(vwnd(i,j)+vwnd(i  ,j+1))
              wspd(i,j)=sqrt(cff9*cff9 + cff10*cff10)
            enddo
          enddo
#  ifdef MPI
	  call exchange_r2d_tile(Istr,Iend,Jstr,Jend,
     &                           wspd(START_2D_ARRAY))
#  endif
# else
          do j=JstrR,JendR
            do i=IstrR,IendR
              wspd(i,j)=cff1*wspdg(i,j,it1)+cff2*wspdg(i,j,it2)
#  ifdef BULK_SMFLUX
#   ifdef BULK_SM_UPDATE
              uwnd(i,j)=cff1*uwndg(i,j,it1)+cff2*uwndg(i,j,it2)
              vwnd(i,j)=cff1*vwndg(i,j,it1)+cff2*vwndg(i,j,it2)
#   else 
              sustr(i,j)=cff7*sustrg(i,j,it1)+cff8*sustrg(i,j,it2)
              svstr(i,j)=cff7*svstrg(i,j,it1)+cff8*svstrg(i,j,it2)
#   endif /* BULK_SM_UPDATE */
#  endif
            enddo
          enddo
# endif
        endif

      !====== == ==== ============== 
      ! Error in time interpolation:
      !====== == ==== ============== 
      else
        if (ZEROTH_TILE) then
          write(stdout,1) 'bulk_timeO',tdays,bulk_timeO(it2,blkvar_id)*sec2day
  1       format(/,' SET_BULK - current model time exceeds ending',
     &           1x,'value for variable: ',a,/,11x,'TDAYS = ',g12.4,
     &           2x,'TEND = ',g12.4)
          may_day_flag=2
        endif
      endif

      goto 10

      return
      end

#endif /* BULK_FLUX && ONLINE */
