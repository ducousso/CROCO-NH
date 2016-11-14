! $Id: get_smflux.F 1577 2014-07-04 10:26:41Z gcambon $
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
#ifndef ANA_SMFLUX
                              ! Read point or grided surface momentum
      subroutine get_smflux   ! flux (wind stress) at the appropriate
                              ! time from forcing netCDF file. 
# define SMFLUX_DATA
      implicit none
# include "param.h"
# include "scalars.h"
# include "ncscrum.h"
# include "forces.h"
#include "netcdf.inc"
      real cff
      integer i, ierr, lstr,lvar, lenstr, nf_fread, advance_cycle
!
! Initialization: Inquire about the contents of forcing netCDF file:
!================  variables and dimensions. Check for consistency.
!
      if (may_day_flag.ne.0) return      !-->  EXIT
      if (itsms.eq.0 .or. iic.eq.0) then
        lstr=lenstr(frcname)
c**        call opencdf (frcname,N)
        if (may_day_flag.ne.0) return    !-->  EXIT
!
! If not opened yet, open forcing netCDF file for reading. Find and
! save IDs for relevant variables, determine whether momentum stress
! components exist as fields or scalars.
!
        if (ncidfrc.eq.-1) then
          ierr=nf_open (frcname(1:lstr), nf_nowrite, ncidfrc)
          if (ierr .ne. nf_noerr) goto 4                 !--> ERROR 
        endif

        ierr=nf_inq_varid (ncidfrc, 'sms_time', sms_tid)
        if (ierr .ne. nf_noerr) then
          write(stdout,3) 'sms_time', frcname(1:lstr)
          goto 99                                         !--> ERROR
        endif

        lvar=lenstr(vname(1,indxSUSTR))
        ierr=nf_inq_varid (ncidfrc,vname(1,indxSUSTR)(1:lvar),susid)
        if (ierr .eq. nf_noerr) then
          ierr=nf_inq_varndims (ncidfrc, susid, i)
          if (ierr. eq. nf_noerr) then
            if (i.gt.1) then
              lsusgrd=1
            else
              lsusgrd=0
            endif
          endif
        endif
        if (ierr .ne. nf_noerr) then
          write(stdout,3) vname(1,indxSUSTR)(1:lvar),frcname(1:lstr)
          goto 99                                         !--> ERROR
        endif

        lvar=lenstr(vname(1,indxSVSTR))
        ierr=nf_inq_varid (ncidfrc,vname(1,indxSVSTR)(1:lvar),svsid)
        if (ierr .eq. nf_noerr) then
          ierr=nf_inq_varndims (ncidfrc, svsid, i)
          if (ierr. eq. nf_noerr) then
            if (i.gt.1) then
              lsvsgrd=1
            else
              lsvsgrd=0
            endif
          endif
        endif
        if (ierr .ne. nf_noerr) then
          write(stdout,3) vname(1,indxSVSTR)(1:lvar),frcname(1:lstr)
          goto 99                                         !--> ERROR
        endif
!
! Determine whether there is cycling to reuse the input data and
! what is cycling period "sms_cycle", find initial cycling index
! "sms_ncycle" and record index "sms_rec". Set initial value for
! time index "itsms" and both time record bounds to large negative
! artificial values, so that it will trigger the logic in reading
! part below. Also set scale factor to convert input stress to
! model units: from [Newton/m^2] to [m^2/sec^2].
!
        call set_cycle (ncidfrc,   sms_tid,    ntsms,
     &                  sms_cycle, sms_ncycle, sms_rec)
        if (may_day_flag.ne.0) return    !-->  EXIT
        itsms=2
        sms_time(1)=-1.E+20
        sms_time(2)=-1.E+20
        sms_scale=1./rho0
      endif                   !<-- itsms.eq.0 .or. iic.eq.0
!
! Reading data from the forcing file:  Get out, if model time is
!======== ==== ==== === ======= =====  already within the interval
! set by the past and future data times. Otherwise flip the time
! index, increment record and cyclin indices and read a new portion
! of data. Repeat it until model time is between the two times from
! data.
!
  1   i=3-itsms
       cff=time+0.5*dt  
        if (sms_time(i).le.cff .and. cff.lt.sms_time(itsms)) return
        ierr=advance_cycle (sms_cycle, ntsms, sms_ncycle, sms_rec)
        if (ierr.ne.0) goto 7                           !--> ERROR

        ierr=nf_get_var1_FTYPE(ncidfrc, sms_tid, sms_rec, cff)
        if (ierr .ne. nf_noerr) then
          write(stdout,6) 'sms_time', sms_rec 
          goto 99                                         !--> ERROR
        endif
        sms_time(i)=cff*day2sec+sms_cycle*sms_ncycle
        if (sms_time(itsms).eq.-1.E+20) sms_time(itsms)=sms_time(i)

        if (lsusgrd.eq.1) then
          ierr=nf_fread(sustrg(START_2D_ARRAY,i), ncidfrc, susid,
     &                                              sms_rec, u2dvar)
        else
          ierr=nf_get_var1_FTYPE(ncidfrc,susid,sms_rec,sustrp(i))
        endif
        if (ierr .ne. nf_noerr) then
          write(stdout,6) 'sustr', sms_rec 
          goto 99                                         !--> ERROR
        endif

        if (lsvsgrd.eq.1) then
          ierr=nf_fread(svstrg(START_2D_ARRAY,i), ncidfrc, svsid,
     &                                              sms_rec, v2dvar)
        else
          ierr=nf_get_var1_FTYPE(ncidfrc,svsid,sms_rec,svstrp(i))
        endif
        if (ierr .ne. nf_noerr) then
          write(stdout,6) 'svstr', sms_rec 
          goto 99                                         !--> ERROR
        endif
       itsms=i
       MPI_master_only write(stdout,'(6x,A,1x,A,1x,g12.4,1x,I4)') 
     &              'GET_SMFLUX --',
     &              'Read surface momentum stresses for time =', cff
#ifdef MPI
     &                                                      , mynode
#endif
      if (ntsms.gt.1) goto 1
      if (ntsms.eq.1) return
!
! Sort out error messages: The following portion of the code is 
!===== === ===== ========= not accessed unless something goes wrong.
!
  3   format(/,' GET_SMFLUX - unable to find forcing variable: ',A,
     &                          /,14x,'in forcing netCDF file: ',A)
      goto 99
  4   write(stdout,5) frcname(1:lstr)
  5   format(/,' GET_SMFLUX - unable to open forcing netCDF file:',
     &                                                        1x,A)
  6   format(/,' GET_SMFLUX - error while reading variable: ',A,2x,
     &                                      ' at TIME index = ',i4)
  7   write(stdout,8) sms_rec, ntsms, frcname(1:lstr), tdays,
     &                                sms_time(itsms)*sec2day
  8   format(/,' GET_SMFLUX - ERROR: requested time record ',I4,
     &       1x,'exeeds the last available', /,14x,'record ',I4,
     &       1x,'in forcing netCDF file: ',a, /,14x,'TDAYS = ',
     &       g12.4,2x,'last available SMS_TIME = ',g12.4)
 99   may_day_flag=2
      return  
      end

      subroutine set_smflux_tile (Istr,Iend,Jstr,Jend)
# define SMFLUX_DATA
      implicit none
      integer Istr,Iend,Jstr,Jend, i,j, it1,it2
      real cff, cff1, cff2, ustress, vstress
# include "param.h"
# ifdef SMFLUX_CURVGRID
#  include "grid.h"
# endif
# include "forces.h"
# include "scalars.h"
!
# include "compute_extended_bounds.h"
!
      it1=3-itsms
      it2=itsms
      cff=time+0.5*dt
      cff1=sms_time(it2)-cff
      cff2=cff-sms_time(it1)
!
! Load time invariant surface stresses.
!
      if (sms_cycle.lt.0.) then
        if (FIRST_TIME_STEP) then
          if (lsusgrd.eq.1 .and. lsvsgrd.eq.1) then
            do j=JstrR,JendR
              do i=Istr,IendR
                sustr(i,j)=sms_scale*sustrg(i,j,itsms)
              enddo
            enddo
            do j=Jstr,JendR
              do i=IstrR,IendR
                svstr(i,j)=sms_scale*svstrg(i,j,itsms)
              enddo
            enddo
          else
            ustress=sms_scale*sustrp(itsms)
            vstress=sms_scale*svstrp(itsms)
            do j=JstrR,JendR
              do i=Istr,IendR
# ifdef SMFLUX_CURVGRID
                cff=0.5*(angler(i,j)+angler(i-1,j))
                sustr(i,j)=ustress*cos(cff)+vstress*sin(cff)
# else
                sustr(i,j)=ustress
# endif
              enddo
            enddo
            do j=Jstr,JendR
              do i=IstrR,IendR
# ifdef SMFLUX_CURVGRID
                cff=0.5*(angler(i,j)+angler(i,j-1))
                svstr(i,j)=-ustress*sin(cff)+vstress*cos(cff)
# else
                svstr(i,j)=vstress
# endif
              enddo
            enddo
          endif
        endif
!
! Interpolate surface momentum stresses in time.
!-----------------------------------------------
!
      elseif (cff1.ge.0. .and. cff2.ge.0.) then
        if (ZEROTH_TILE .and. cff1.lt.dt) synchro_flag=.TRUE.
        cff=sms_scale/(cff1+cff2)
        cff1=cff1*cff
        cff2=cff2*cff
!
! Process grided data. Grided stresses are assumed to be already
! rotated by the appropriate angle in curvilinear configurations.
!
        if (lsusgrd.eq.1 .and. lsvsgrd.eq.1) then
          do j=JstrR,JendR
            do i=Istr,IendR
              sustr(i,j)=cff1*sustrg(i,j,it1)+cff2*sustrg(i,j,it2)
            enddo
          enddo
          do j=Jstr,JendR
            do i=IstrR,IendR
              svstr(i,j)=cff1*svstrg(i,j,it1)+cff2*svstrg(i,j,it2)
            enddo
          enddo
!
! Process point data. If appropriate, rotate stress components.
!
        else
          ustress=cff1*sustrp(it1)+cff2*sustrp(it2)
          vstress=cff1*svstrp(it1)+cff2*svstrp(it2)
# ifdef SMFLUX_CURVGRID
          do j=JstrR,JendR
            do i=Istr,IendR
              cff=0.5*(angler(i,j)+angler(i-1,j))
              sustr(i,j)=ustress*cos(cff)+vstress*sin(cff)
            enddo
          enddo
          do j=Jstr,JendR
            do i=IstrR,IendR
              cff=0.5*(angler(i,j)+angler(i,j-1))
              svstr(i,j)=-ustress*sin(cff)+vstress*cos(cff)
            enddo
          enddo
# else
          do j=JstrR,JendR
            do i=Istr,IendR
              sustr(i,j)=ustress
            enddo
          enddo
          do j=Jstr,JendR
            do i=IstrR,IendR
              svstr(i,j)=vstress
            enddo
          enddo
# endif /* SMFLUX_CURVGRID */
        endif 
!
! Unable to set-up surface momentum stresses: Complain about the
! error and signal to quit (ONE THREAD ONLY).
!
      else
        if (ZEROTH_TILE) then
          write(stdout,1) 'sms_time', tdays, sms_time(it1)*sec2day,
     &                                       sms_time(it2)*sec2day 
  1       format(/,' SET_SMFLUX_TILE - current model time',
     &           1x,'exceeds ending value for variable: ',a,/,
     &           14x,'TDAYS = ',g12.4,2x,'SMS_TSTART = ',g12.4,2x,
     &                                        'SMS_TEND = ',g12.4)
          may_day_flag=2 
        endif
      endif
      return
      end
#else
      subroutine get_smflux_empty
      return
      end
#endif /* !ANA_SMFLUX */
