! $Id: get_stflux.F 1458 2014-02-03 15:01:25Z gcambon $
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
#if defined SOLVE3D && (!defined ANA_STFLUX || !defined ANA_SSFLUX)

                                    ! Read point or grided surface 
      subroutine get_stflux (itrc)  ! flux for tracer variables itrc
                                    ! from forcing NetCDF file.
# define STFLUX_DATA
      implicit none
# include "param.h"
# include "forces.h"
# include "scalars.h"
# include "ncscrum.h"
# include "netcdf.inc"
      real cff
      integer itrc, i,ierr, lstr,lvar,lenstr, nf_fread, advance_cycle
!
! Initialization: Inquire about the contents of forcing NetCDF file:
!================  variables and dimensions. Check for consistency.
!
      if (may_day_flag.ne.0) return      !-->  EXIT
      if (itstf(itrc).eq.0 .or. iic.eq.0) then
        lstr=lenstr(frcname)
c**        call opencdf (frcname,N)
        if (may_day_flag.ne.0) return    !-->  EXIT
!
! If not opened yet, open forcing NetCDF file for reading.
! Find and save IDs for relevant variables, determine whether
! surface flux for tracer itrc is a field or scalar value.
!
        if (ncidfrc.eq.-1) then
          ierr=nf_open (frcname(1:lstr), nf_nowrite, ncidfrc)
          if (ierr .ne. nf_noerr) goto 4
        endif

        if (itrc.eq.itemp) then
          ierr=nf_inq_varid (ncidfrc, 'shf_time', stf_tid(itrc))
          if (ierr .ne. nf_noerr) then
            write(stdout,3) 'shf_time', frcname(1:lstr)
            goto 99                                       !--> ERROR
          endif
# ifdef SALINITY
        elseif (itrc.eq.isalt) then
          ierr=nf_inq_varid (ncidfrc, 'swf_time', stf_tid(itrc))
          if (ierr .ne. nf_noerr) then
            write(stdout,3) 'swf_time', frcname(1:lstr)
            goto 99                                       !--> ERROR
          endif
# endif
        endif
        lvar=lenstr(vname(1,indxShflx+itrc-1))
        ierr=nf_inq_varid (ncidfrc, vname(1,indxShflx+itrc-1),
     &                                          stf_id(itrc))
        if (ierr .eq. nf_noerr) then
          ierr=nf_inq_varndims (ncidfrc, stf_id(itrc), i)
          if (ierr. eq. nf_noerr) then
            if (i.gt.1) then
              lstfgrd(itrc)=1
            else
              lstfgrd(itrc)=0
            endif
          endif
        endif
        if (ierr .ne. nf_noerr) then
          write(stdout,3) vname(1,indxShflx+itrc-1), frcname(1:lstr)
          goto 99                                         !--> ERROR
        endif
!
! Determine whether there is cycling to reuse the input data and
! find cycling period "stf_cycle", set initial cycling index
! "stf_ncycle" and record index "stf_rec".
! Set initial value for time index "itstf" and both time record
! bounds to large negative artificial values, so that it will
! trigger the logic in reading part below.
!
        call set_cycle (ncidfrc,  stf_tid(itrc),    ntstf(itrc),
     &           stf_cycle(itrc), stf_ncycle(itrc), stf_rec(itrc))
        if (may_day_flag.ne.0) return    !-->  EXIT

        itstf(itrc)=2
        stf_time(1,itrc)=-1.E+20
        stf_time(2,itrc)=-1.E+20
!
! Set scale factors to convert input fluxes to model units:
!
!     Heat  flux - convert from [Watts/m^2] to [Celsius m/s]
!     Fresh Water flux - convert from [cm/day] to [PSU m/s]
!
        stf_scale(itemp)=1./(rho0*Cp)
# ifdef SALINITY
        stf_scale(isalt)=0.01/86400.
# endif
      endif
!
! Reading data from the forcing file:  Get out, if model time is
!======== ==== ==== === ======= =====  already within the interval
! set by the past and future data times. Otherwise flip the time
! index, increment record and cyclin indices and read a new portion
! of data. Repeat it until model time is between the two times from
! data.
!
  1   i=3-itstf(itrc)
       cff=time+0.5*dt
        if (stf_time(i,itrc).le.cff .and.
     &      cff.lt.stf_time(itstf(itrc),itrc)) return
        ierr=advance_cycle (stf_cycle(itrc),  ntstf(itrc),
     &                      stf_ncycle(itrc), stf_rec(itrc))
        if (ierr .ne. 0) goto 7                           !--> ERROR

        ierr=nf_get_var1_FTYPE(ncidfrc, stf_tid(itrc),
     &                             stf_rec(itrc), cff)
        if (ierr .ne. nf_noerr) then
          write(stdout,6) 'stf_time', stf_rec(i)
          goto 99                                         !--> ERROR
        endif
        stf_time(i,itrc)=cff*day2sec+stf_cycle(itrc)*stf_ncycle(itrc)
        if (stf_time(itstf(itrc),itrc).eq.-1.E+20)
     &      stf_time(itstf(itrc),itrc)=stf_time(i,itrc)

        if (lstfgrd(itrc).eq.1) then
          ierr=nf_fread(stflxg(START_2D_ARRAY,i,itrc), ncidfrc,
     &                     stf_id(itrc), stf_rec(itrc), r2dvar)
        else
          ierr=nf_get_var1_FTYPE(ncidfrc, stf_id(itrc),
     &                     stf_rec(itrc), stflxp(i,itrc))
        endif
        if (ierr .ne. nf_noerr) then
          write(stdout,6) 'stflux', stf_rec 
          goto 99                                         !--> ERROR
        endif
       itstf(itrc)=i
       MPI_master_only write(stdout,'(6x,A,1x,I2,1x,A,1x,g12.4,1x,I4)')
     &            'GET_STFLUX -- Read surface flux of tracer', itrc,
     &                                             'for time =', cff
#ifdef MPI
     &                                                      , mynode
#endif

      if (ntstf(itrc).gt.1) goto 1
      if (ntstf(itrc).eq.1) return
!
! Sort out error messages: The following portion of the code is
!===== === ===== ========= not accessed unless something goes wrong.
!
  3   format(/,' GET_STFLUX - ERROR: unable to find forcing ',
     &              'variable: ',a,/,14x,'in forcing file: ',a)
  4   write(stdout,5) frcname(1:lstr)
  5   format(/,' GET_STFLUX - ERROR: unable to open file: ', a)
      goto 99
  6   format(/,' GET_STFLUX - ERROR while reading variable: ',
     &                                a,' at TIME index = ',i4)
  7   write(stdout,8) stf_rec(itrc),ntstf(itrc),frcname(1:lstr),
     &                tdays, stf_time(itstf(itrc),itrc)*sec2day
  8   format(/,' GET_STFLUX - ERROR: requested time record ',I4,
     &       1x,'exeeds the last available', /,14x,'record ',I4,
     &       1x,'in forcing file: ', a,/,14x,'TDAYS = ',
     &             g12.4,2x,'last available STF_TIME = ',g12.4)
  99  may_day_flag=2
      return
      end

      subroutine set_stflux_tile (Istr,Iend,Jstr,Jend, itrc)
!
!--------------------------------------------------------------------
! Set-up surface tracer flux for current tile.
!--------------------------------------------------------------------
!
# define STFLUX_DATA
      implicit none
      integer Istr,Iend,Jstr,Jend, itrc, i,j, it1,it2
      real cff, cff1, cff2
# include "param.h"
# include "forces.h"
# include "scalars.h"
!
# include "compute_extended_bounds.h"
!
      it1=3-itstf(itrc)
      it2=itstf(itrc)
      cff=time+0.5*dt
      cff1=stf_time(it2,itrc)-cff
      cff2=cff-stf_time(it1,itrc)
!
! Load time invariant surface tracer flux.
! Time interpolation is not performed in this case.
!
      if (stf_cycle(itrc).lt.0.) then
        if (FIRST_TIME_STEP) then
          if (lstfgrd(itrc).eq.1) then
            cff=stf_scale(itrc)
            do j=JstrR,JendR
              do i=IstrR,IendR
                stflx(i,j,itrc)=cff*stflxg(i,j,itstf(itrc),itrc)
              enddo
            enddo
          else
            cff=stf_scale(itrc)*stflxp(itstf(itrc),itrc)
            do j=JstrR,JendR
              do i=IstrR,IendR
                stflx(i,j,itrc)=cff
              enddo
            enddo
          endif
        endif
!
! Time-interpolate surface tracer flux from grided or point data.
! Check that for the next time step [when time=time+dt] time+dt is
! still between srf_tintrp(it1) and srf_tintrp(it2); and if not,
! set synchro_flag top signal that the new forcing data should be
! read from the netCDF input file (master thread only).
!
      elseif (cff1.ge.0. .and. cff2.ge.0.) then
        if (ZEROTH_TILE .and. cff1.lt.dt) synchro_flag=.TRUE.
        cff=stf_scale(itrc)/(cff1+cff2)
        cff1=cff1*cff
        cff2=cff2*cff
        if (lstfgrd(itrc).eq.1) then
          do j=JstrR,JendR
            do i=IstrR,IendR
              stflx(i,j,itrc)=cff1*stflxg(i,j,it1,itrc)
     &                       +cff2*stflxg(i,j,it2,itrc)
            enddo
          enddo
        else
          cff=cff1*stflxp(it1,itrc)+cff2*stflxp(it2,itrc)
          do j=JstrR,JendR
            do i=IstrR,IendR
              stflx(i,j,itrc)=cff
            enddo
          enddo
        endif
!
! Unable to set-up surface tracer flux:
! Complain about the error and signal to quit (ONE THREAD ONLY).
!
      else
        if (ZEROTH_TILE) then
          write(stdout,1) 'stf_time',tdays,stf_time(it2,itrc)*sec2day
  1       format(/,' SET_STFLUX_TILE - current model time exceeds',
     &          1x,'ending value for variable: ',A8,/,14x,'TDAYS = ',
     &                                    g12.4, 2x, 'TSTF = ',g12.4)
          may_day_flag=2
        endif
      endif
      return
      end
#else
      subroutine get_stflux_empty
      return
      end
#endif /* SOLVE3D  && !ANA_STFLUX || (SALINITY && !ANA_SSFLUX) */
