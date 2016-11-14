! $Id: get_wwave.F 1458 2014-02-03 15:01:25Z gcambon $
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
#if defined  WAVE_OFFLINE && ( defined BBL || defined MRL_WCI )

                              ! Read point or gridded wind wave
      subroutine get_wwave    ! height, direction, period at approp.
                              ! time from forcing NetCDF file.
      implicit none
#include "param.h" 
#include "scalars.h"
#include "ncscrum.h"
#include "forces.h"
      real cff
      integer i,ierr, lstr,lvar, lenstr, nf_fread, advance_cycle
#include "netcdf.inc"
!
! Initialization: Inquire about the contents of forcing NetCDF file:
!================  variables and dimensions. Check for consistency.
!

      if (may_day_flag.ne.0) return      !-->  EXIT
      if (itww.eq.0 .or. iic.eq.0) then
        lstr=lenstr(frcname)
        if (may_day_flag.ne.0) return    !-->  EXIT
!
! If not opened yet, open forcing NetCDF file for reading.
! Find and save IDs for relevant variables, determine whether
! wind wave data are field or scalar values. 
!
        if (ncidfrc.eq.-1) then
          ierr=nf_open(frcname(1:lstr), nf_nowrite, ncidfrc)
          if (ierr .ne. nf_noerr) goto 4                !--> ERROR 
        endif
      

        ierr=nf_inq_varid (ncidfrc, 'wwv_time', ww_tid)
        if (ierr .ne. nf_noerr) then
          write(stdout,3) 'wwv_time', frcname(1:lstr)
          goto 99                                         !--> ERROR
        endif

! Check if wave amplitude present, and if so, if field or scalar series
      
        lvar=lenstr(vname(1,indxWWA))
        ierr=nf_inq_varid (ncidfrc,vname(1,indxWWA)(1:lvar),wwa_id)
        if (ierr .eq. nf_noerr) then
          ierr=nf_inq_varndims (ncidfrc, wwa_id, i)
          if (ierr. eq. nf_noerr) then
            if (i.gt.1) then
              wwagrd=1.
            else
              wwagrd=0.
            endif
          endif
        endif
        if (ierr .ne. nf_noerr) then
          write(stdout,3) vname(1,indxWWA)(1:lvar),frcname(1:lstr)
          goto 99                                         !--> ERROR
        endif


! Check if wave direction present, and if so, if field or scalar series

        lvar=lenstr(vname(1,indxWWD))
        ierr=nf_inq_varid (ncidfrc,vname(1,indxWWD)(1:lvar),wwd_id)
        if (ierr .eq. nf_noerr) then
          ierr=nf_inq_varndims (ncidfrc, wwd_id, i)
          if (ierr. eq. nf_noerr) then
            if (i.gt.1) then
              wwdgrd=1.
            else
              wwdgrd=0.
            endif
          endif
        endif
        if (ierr .ne. nf_noerr) then
          write(stdout,3) vname(1,indxWWD)(1:lvar),frcname(1:lstr)
          goto 99                                         !--> ERROR
        endif

     
! Check if wave period present, and if so, if field or scalar series
      
      lvar=lenstr(vname(1,indxWWP))
        ierr=nf_inq_varid (ncidfrc,vname(1,indxWWP)(1:lvar),wwp_id)
        if (ierr .eq. nf_noerr) then
          ierr=nf_inq_varndims (ncidfrc, wwp_id, i)
          if (ierr. eq. nf_noerr) then
            if (i.gt.1) then
              wwpgrd=1.
            else
              wwpgrd=0.
            endif
          endif
        endif
        if (ierr .ne. nf_noerr) then
          write(stdout,3) vname(1,indxWWP)(1:lvar),frcname(1:lstr)
          goto 99                                         !--> ERROR
        endif


! Check if breaking present, and if so, if field or scalar series
# if defined WAVE_OFFLINE && defined MRL_WCI     
      lvar=lenstr(vname(1,indxWEB))
        ierr=nf_inq_varid (ncidfrc,vname(1,indxWEB)(1:lvar),wwe_id)
        if (ierr .eq. nf_noerr) then
          ierr=nf_inq_varndims (ncidfrc, wwe_id, i)
          if (ierr. eq. nf_noerr) then
            if (i.gt.1) then
              wwegrd=1.
            else
              wwegrd=0.
            endif
          endif
        endif
        if (ierr .ne. nf_noerr) then
          write(stdout,3) vname(1,indxWEB)(1:lvar),frcname(1:lstr)
          goto 99                                         !--> ERROR
        endif
# endif      
!
! Determine whether cycling is defined to reuse the input data
! and find cycling period "ww_cycle", set initial cycling
! index "ww_ncycle" and record index "ww_rec".
! Set initial value for time index "itww" and both time record
! bounds to large negative artificial values, so that it will
! trigger the logic in the reading part below.
! Also set scale factor to convert input data to model units:
! convert wave direction from degrees to radians;
! no conversion for amplitude and period.
!
         call set_cycle (ncidfrc,   ww_tid,    ntww,
     &                  ww_cycle, ww_ncycle, ww_rec)
        if (may_day_flag.ne.0) return    !-->  EXIT
      
      itww=2
      wwv_time(1)=-1.E+20
      wwv_time(2)=-1.E+20
      wwa_scale=1.
      wwd_scale=deg2rad
      wwp_scale=1.
      wwe_scale=1.

     
       endif                   !<-- itww.eq.0 .or. iic.eq.0

 
!
! Reading data from the forcing file:  Get out, if model time is
!======== ==== ==== === ======= =====  already within the interval
! set by the past and future data times. Otherwise flip the time
! index, increment record and cyclin indices and read a new portion
! of data. Repeat it until model time is between the two times from
! data.
!
  1   i=3-itww
       cff=time+0.5*dt
        if (wwv_time(i).le.cff .and. cff.lt.wwv_time(itww)) return
        ierr=advance_cycle (ww_cycle, ntww, ww_ncycle, ww_rec)
        if (ierr.ne.0) goto 7                           !--> ERROR

        ierr=nf_get_var1_FTYPE(ncidfrc, ww_tid, ww_rec, cff)
        if (ierr .ne. nf_noerr) then
          write(stdout,6) 'wwv_time', ww_rec 
          goto 99                                         !--> ERROR
        endif
        wwv_time(i)=cff*day2sec+ww_cycle*ww_ncycle
        if (wwv_time(itww).eq.-1.E+20) wwv_time(itww)=wwv_time(i)

! Read wave amplitude

        if (wwagrd.eq.1.) then
          ierr=nf_fread (wwag(START_2D_ARRAY,i), ncidfrc, wwa_id,
     &                                             ww_rec, r2dvar)
        else
          ierr=nf_get_var1_FTYPE (ncidfrc,wwa_id,ww_rec,wwap(i))
        endif

        if (ierr .ne. nf_noerr) then
          write(stdout,6) 'wind-wave amplitude', ww_rec
          goto 99                                         !--> ERROR
        endif 
      
! Read wave direction

        if (wwdgrd.eq.1.) then
          ierr=nf_fread (wwdg(START_2D_ARRAY,i), ncidfrc, wwd_id,
     &                                             ww_rec, r2dvar)
        else
          ierr=nf_get_var1_FTYPE (ncidfrc,wwd_id,ww_rec,wwdp(i))
        endif

        if (ierr .ne. nf_noerr) then
          write(stdout,6) 'wind-wave direction', ww_rec
          goto 99                                         !--> ERROR
        endif 
      
! Read wave period

        if (wwpgrd.eq.1.) then
          ierr=nf_fread (wwpg(START_2D_ARRAY,i), ncidfrc, wwp_id,
     &                                             ww_rec, r2dvar)
        else
          ierr=nf_get_var1_FTYPE (ncidfrc,wwp_id,ww_rec,wwpp(i))
        endif

        if (ierr .ne. nf_noerr) then
          write(stdout,6) 'wind-wave period', ww_rec
          goto 99                                         !--> ERROR
        endif       

#  if defined WAVE_OFFLINE && defined MRL_WCI     
! Read breaking dissipation

        if (wwegrd.eq.1.) then
          ierr=nf_fread (wweb(START_2D_ARRAY,i), ncidfrc, wwe_id,
     &                                             ww_rec, r2dvar)
        else
          ierr=nf_get_var1_FTYPE (ncidfrc,wwe_id,ww_rec,wwep(i))
        endif

        if (ierr .ne. nf_noerr) then
          write(stdout,6) 'wind-wave breaking', ww_rec
          goto 99                                         !--> ERROR
        endif       
#endif           
       itww=i
       write(stdout,'(6x,A,1x,A,1x,g12.4,1x,I4)') 'GET_WWAVE --',
     &              'Read wind wave input data for time =', cff
#ifdef MPI
     &                                                      , mynode
#endif
      if (ntww.gt.1) goto 1
      if (ntww.eq.1) return 
!
!
! Sort out error messages: The following portion of the code is
!===== === ===== ========= not accessed unless something goes wrong.
!
  3   format(/,' GET_WWAVE - unable to find forcing variable: ',a,
     &                          /,15x,'in forcing NetCDF file: ',a)
  4   write(stdout,5) frcname(1:lstr)
  5   format(/,' GET_WWAVE - unable to open forcing NetCDF file:',
     &                                                        1x,a)
      goto 99

  6   format(/,' GET_WWAVE - error while reading variable: ',a,2x,
     &                                      ' at TIME index = ',i4)
  7   write(stdout,8) ww_rec, ntww, frcname(1:lstr), tdays,
     &                                wwv_time(itww)*sec2day
  8   format(/,' GET_WWAVE - ERROR: requested time record ',I4,
     &       1x,'exeeds the last available', /,14x,'record ',I4,
     &       1x,'in forcing NetCDF file: ',a, /,14x,'TDAYS = ',
     &       g12.4,2x,'last available wwv_time = ',g12.4)

  99  may_day_flag=2
    
      return
      end
!
!=====================================================================
!
!                             SET_WWAVE
!
!=====================================================================
!
      subroutine set_wwave (tile)
      implicit none
#include "param.h"
      integer tile 
#include "compute_tile_bounds.h"
!
      call set_wwave_tile (Istr,Iend,Jstr,Jend)
      return
      end

      subroutine set_wwave_tile (Istr,Iend,Jstr,Jend)
!
!--------------------------------------------------------------------
!  Set-up wind wave amplitude, direction, period for current tile.
!--------------------------------------------------------------------
!
      implicit none

      integer Istr,Iend,Jstr,Jend, i,j,k, it1,it2
      real fac, fac1, fac2, fac3, fac4, fac5, fac6,fac7,fac8, 
     &     wwA, wwD, wwP, wwe, Dstp,cfrq,wdir
      real cff,cff1,cff2, kh,khd, eps,xdir,ydir,
     &                                           wramp,m2p,isr2 
# ifdef BBL_OFFLINE
     &                                            , ab, orb_vel
# endif
# if !defined WAVE_OFFLINE && defined MRL_WCI
     &                                          , Btg, Gtg, sbc
# endif
      parameter (eps = 1.0d-10, m2p=1.2804973111, isr2=0.70710678)

!
# include "param.h"
# include "grid.h"
# include "forces.h"
# include "scalars.h"
# include "ocean2d.h"
!
# include "compute_extended_bounds.h"
!
! Set coefficients for interpolation. Check that for the next time
! step [when time=time+dt] both weights will still be positive, and
! if not, set synchro_flag to signal that new data should be read
! from an appropriate netCDF input file (master thread only).
! After that, either load time-invariant data, or interpolate in time.
! Complain about error and signal to quit, if interpolation is
! needed, but not possible.
!
!
      it1=3-itww
      it2=itww
      fac=time+0.5*dt
      fac1=wwv_time(it2)-fac
      fac2=fac-wwv_time(it1)
!
! Load time-invariant wind-wave data.
! Time interpolation is not performed in this case.
!
      if (ww_cycle.lt.0.) then
        if (FIRST_TIME_STEP) then
        
            wwA=wwa_scale*wwap(itww)
            wwD=wwd_scale*wwdp(itww)
            wwP=wwp_scale*wwpp(itww)
# if defined WAVE_OFFLINE && defined MRL_WCI     
            wwe=wwe_scale*wwep(itww)
# endif
          
            do j=JstrR,JendR
              do i=IstrR,IendR
                Awave(i,j)=(wwagrd)*wwa_scale*wwag(i,j,itww) +
     &                     (1.-wwagrd)*wwA
     
                Dwave(i,j)=(wwdgrd)*wwd_scale*wwdg(i,j,itww) +
     &                     (1.-wwdgrd)*wwD 

                Pwave(i,j)=(wwpgrd)*wwp_scale*wwpg(i,j,itww) +
     &                     (1.-wwpgrd)*wwP
# if defined WAVE_OFFLINE && defined MRL_WCI     
                Eb(i,j)   =(wwegrd)*wwe_scale*wweb(i,j,itww) +
     &                     (1.-wwegrd)*wwe
# endif
              enddo
            enddo
        endif
!
! Time-interpolate wind wave data from gridded or point data.
! Make sure that for the next time step [when time=time+dt]
! time+dt is still between wwv_time(it1) and wwv_time(it2);
! and if not, set synchro_flag to signal that the new forcing data
! should be read from the netCDF input file (master thread only).
!
      elseif (fac1.ge.0. .and. fac2.ge.0.) then
       if (ZEROTH_TILE .and. fac1.lt.dt) synchro_flag=.TRUE.
      
        fac=wwd_scale/(fac1+fac2)
        fac3=fac*fac1
        fac4=fac*fac2
        fac=wwp_scale/(fac1+fac2)
        fac5=fac*fac1
        fac6=fac*fac2
        fac=wwe_scale/(fac1+fac2)
        fac7=fac*fac1
        fac8=fac*fac2
        fac=wwa_scale/(fac1+fac2)
        fac1=fac*fac1
        fac2=fac*fac2
      
        wwA=fac1*wwap(it1)+fac2*wwap(it2)
        wwD=fac3*wwdp(it1)+fac4*wwdp(it2)
        wwP=fac5*wwpp(it1)+fac6*wwpp(it2)
# if defined WAVE_OFFLINE && defined MRL_WCI     
        wwe=fac7*wwep(it1)+fac8*wwep(it2)
# endif        
      do j=JstrR,JendR
          do i=IstrR,IendR
            Awave(i,j)=wwagrd*(fac1*wwag(i,j,it1)+fac2*wwag(i,j,it2))
     &                +(1.-wwagrd)*wwA
     
            Dwave(i,j)=wwdgrd*(fac3*wwdg(i,j,it1)+fac4*wwdg(i,j,it2))
     &                +(1.-wwdgrd)*wwD 

            Pwave(i,j)=wwpgrd*(fac5*wwpg(i,j,it1)+fac6*wwpg(i,j,it2))
     &                +(1.-wwpgrd)*wwP

# if defined WAVE_OFFLINE && defined MRL_WCI     
            Eb(i,j)   =wwegrd*(fac7*wweb(i,j,it1)+fac8*wweb(i,j,it2))
     &                +(1.-wwegrd)*wwe
# endif        
          enddo
        enddo

!
! Compute anyway variables for wave current interactions
!
        do j=jstrR,jendR
          do i=istrR,iendR
            Dstp=max(h(i,j),eps)
            cfrq=2.0*pi/max(Pwave(i,j)*m2p,0.1D0) ! mean to peak period
            wdir=Dwave(i,j)           ! 
!           wdir=0.5*pi+Dwave(i,j,)   ! if wdir is dir waves travel TO
!           wdir=Dwave(i,j)-1.5*pi    ! wdir is dir waves travel FROM N
# ifdef CURVGRID
     &                    -angler(i,j)
# endif            
            xdir=cos(wdir)
            ydir=sin(wdir)
# if defined MRL_WCI
            wwfrq(i,j)=cfrq
#  ifdef MASKING
     &                             *rmask(i,j)
#  endif
# endif

# ifdef MRL_WCI
            wwhrm(i,j) =2.0*Awave(i,j)*isr2 ! Hsig to Hrms conversion
            wwdrx(i,j) =xdir
            wwdre(i,j) =ydir
#  ifndef WAVE_OFFLINE
            Eb(i,j) =sbc*cfrq*((0.7071*wwhrm(i,j))**7)/
     &                    max(Dstp**5,eps)   ! 0.7071 = 1/sqrt(2): Hsig -> Hrms
#  endif
#  ifdef MASKING
              wwhrm(i,j) = wwhrm(i,j)*rmask(i,j)
              wwdrx(i,j) = wwdrx(i,j)*rmask(i,j)
              wwdre(i,j) = wwdre(i,j)*rmask(i,j)
              Eb   (i,j) = Eb   (i,j)*rmask(i,j)
#   if defined WAVE_OFFLINE && defined WAVE_FRICTION
              wwed(i,j,1)  = wwed(i,j,1)*rmask(i,j)
#   endif
#   if defined WAVE_OFFLINE && defined WAVE_ROLLER
              wwqb(i,j,1)  = wwqb(i,j,1)*rmask(i,j)
#   endif
#  endif
# endif         
          enddo
        enddo

# if !defined MRL_WCI && defined BBL && defined WAVE_RAMP
      wramp =tanh(3.0*dt*sec2day*float(iic-ntstart))
# else
      wramp =1.0
# endif
        do j=jstrR,jendR
          do i=istrR,iendR
# if defined MRL_WCI
            wfrq(i,j)=wwfrq(i,j)
            whrm(i,j)=wramp*wwhrm(i,j)
            wdrx(i,j)=wwdrx(i,j)
            wdre(i,j)=wwdre(i,j)
            Eb  (i,j)=wramp*Eb(i,j)
#  if defined WAVE_OFFLINE
#   if defined  WAVE_FRICTION
            wved(i,j)=wwed(i,j,1)
#   else
            wved(i,j)=0.0
#   endif
#  endif
#  if defined WAVE_OFFLINE && defined WAVE_ROLLER
            wvqb(i,j)=wwqb(i,j,1)
#  endif
# endif
          enddo
        enddo
      endif
      
      
!
! Unable to set-up wind wave fields:
! Complain about the error and signal to quit (ONE THREAD ONLY).
!
      
      if ((ww_cycle.gt.0.) .and. (fac1.lt.0..or.fac2.lt.0.)) then
        if (ZEROTH_TILE) then
          write(stdout,1) 'wwv_time', tdays, wwv_time(it2)*sec2day  
  1       format(/,' SET_WWAVE_TILE - current model time exceeds',
     &             ' ending value for variable: ',a,/,14x,'TDAYS = ',
     &                                      g12.4,2x,'TEND = ',g12.4)
          may_day_flag=2
        endif
      endif
!      wwfrq(:,:,itww) = Pwave(:,:) 
      return
      end
#else
      subroutine get_wwave_empty
      return
      end
#endif /* BBL && !ANA_WWAVE */
