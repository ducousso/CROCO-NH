! $Id: get_sst.F 1458 2014-02-03 15:01:25Z gcambon $
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
#if defined QCORRECTION && !defined ANA_SST

      subroutine get_sst
!
!  Read in point or grided sea surface temperature surface net heat
!  flux sensitivity to sea surface temperature at the appropriate
!  time from forcing NetCDF file.
!
!  These forcing fields are used when flux correction is activated:
!
!       Q_model ~ Q + dQdSST * (T_model - SST)
!
# define SST_DATA
      implicit none
# include "param.h"
# include "forces.h"
# include "scalars.h"
# include "netcdf.inc"
# include "ncscrum.h"
      real cff
      integer i,ierr, lstr,lvar,lenstr, nf_fread, advance_cycle
!
! Initialization: Inquire about the contents of forcing NetCDF file:
!================  variables and dimensions. Check for consistency.
!
      if (may_day_flag.ne.0) return      !-->  EXIT
      if (itsst.eq.0 .or. iic.eq.0) then
        lstr=lenstr(frcname)
c*        call opencdf (frcname,N)
c*        if (may_day_flag.ne.0) return    !-->  EXIT
!
! If not opened yet, open forcing NetCDF file for reading.
! Find and save IDs for relevant variables, determine whether
! SST is a field or scalar value.
!
        if (ncidfrc.eq.-1) then
          ierr=nf_open(frcname(1:lstr), nf_nowrite, ncidfrc)
          if (ierr. ne. nf_noerr) goto 4                  !--> ERROR
        endif

        ierr=nf_inq_varid (ncidfrc, 'sst_time', sst_tid)
        if (ierr .ne. nf_noerr) then
          write(stdout,3) 'sst_time', frcname(1:lstr)
          goto 99                                         !--> ERROR
        endif

        lvar=lenstr(vname(1,indxSST))
        ierr=nf_inq_varid (ncidfrc, vname(1,indxSST)(1:lvar), sst_id)
        if (ierr .eq. nf_noerr) then
          ierr=nf_inq_varndims (ncidfrc, sst_id, i) 
          if (ierr. eq. nf_noerr) then
            if (i.gt.1) then
              lsstgrd=1
            else
              lsstgrd=0
            endif
          endif
        endif
        if (ierr .ne. nf_noerr) then
          write(stdout,3) vname(1,indxSST)(1:lvar), frcname(1:lstr)
          goto 99                                         !--> ERROR
        endif

        lvar=lenstr(vname(1,indxdQdSST))
        ierr=nf_inq_varid (ncidfrc, vname(1,indxdQdSST)(1:lvar),
     &                                                    dqdt_id)
        if (ierr .ne. nf_noerr) then
          write(stdout,3) vname(1,indxdQdSST)(1:lvar),frcname(1:lstr)
          goto 99                                         !--> ERROR
        endif
!
! Determine whether there is cycling to reuse the input data and
! find cycling period "sst_cycle", set initial cycling index
! "sst_ncycle" and record index "sst_rec".
! Set initial value for time index "itsst" and both time record
! bounds to large negative artificial values, so that it will
! trigger the logic in reading part below.
! Also set scale factor to convert input dQdSST from Watts/m2/Celsius
! to meter/second.
!
        call set_cycle (ncidfrc,   sst_tid,    ntsst,
     &                  sst_cycle, sst_ncycle, sst_rec)
        if (may_day_flag.ne.0) return    !-->  EXIT
        itsst=2
        sst_time(1)=-1.E+20
        sst_time(2)=-1.E+20
        scldqdt=1./(rho0*Cp)
      endif
!
! Reading data from the forcing file:  Get out, if model time is
!======== ==== ==== === ======= =====  already within the interval
! set by the past and future data times. Otherwise flip the time
! index, increment record and cyclin indices and read a new portion
! of data. Repeat it until model time is between the two times from
! data.
!
  1   i=3-itsst
       cff=time+0.5*dt
        if (sst_time(i).le.cff .and. cff.lt.sst_time(itsst)) return
        ierr=advance_cycle (sst_cycle, ntsst, sst_ncycle, sst_rec)
        if (ierr .ne. 0) goto 7                           !--> ERROR

        ierr=nf_get_var1_FTYPE (ncidfrc, sst_tid, sst_rec, cff)
        if (ierr .ne. nf_noerr) then
          write(stdout,6) 'sst_time', sst_rec
          goto 99                                         !--> ERROR
        endif
        sst_time(i)=cff*day2sec+sst_cycle*sst_ncycle
        if (sst_time(itsst).eq.-1.E+20) sst_time(itsst)=sst_time(i)

        if (lsstgrd.eq.1) then
          ierr=nf_fread (sstg(START_2D_ARRAY,i), ncidfrc, sst_id,
     &                                           sst_rec, r2dvar)
        else
          ierr=nf_get_var1_FTYPE (ncidfrc, sst_id, sst_rec, sstp(i))
        endif
        if (ierr .ne. nf_noerr) then
          write(stdout,6) 'SST', sst_rec
          goto 99                                         !--> ERROR
        endif     

        if (lsstgrd.eq.1) then
          ierr=nf_fread (dqdtg(START_2D_ARRAY,i), ncidfrc, dqdt_id,
     &                                            sst_rec,  r2dvar)
        else
          ierr=nf_get_var1_FTYPE (ncidfrc,dqdt_id,sst_rec,dqdtp(i))
        endif
        if (ierr .ne. nf_noerr) then
          write(stdout,6) 'dQdSST', sst_rec
          goto 99                                         !--> ERROR
        endif
       itsst=i
       MPI_master_only write(stdout,'(6x,A,1x,A,1x,g12.4,1x,I4)') 
     &              'GET_SST    --',
     &              'Read SST and dQdSST fields     for time =', cff 
#ifdef MPI
     &                                                      , mynode
#endif
      if (ntsst.gt.1) goto 1
      if (ntsst.eq.1) return
!
! Sort out error messages: The following portion of the code is
!===== === ===== ========= not accessed unless something goes wrong.
!
  3   format(/,' GET_SST - ERROR: unable to find forcing variable',
     &                   ': ',a,/,11x,'in forcing NetCDF file: ',a)
  4   write(stdout,5) frcname(1:lstr)
  5   format(/,' GET_SST - ERROR: unable to open forcing NetCDF ',
     &                                                  'file: ',a)
      goto 99
  6   format(/,' GET_SST - ERROR while reading variable: ',a,2x,
     &                                      ' at TIME index = ',i4)
  7   write(stdout,8) sst_rec, ntsst, frcname(1:lstr), tdays,
     &                                      sst_time(itsst)*sec2day
  8   format(/,' GET_SST - ERROR: requested time record ',I4,
     &       1x,'exeeds the last available',  /,  11x,'record ',I4,
     &       1x,'in forcing NetCDF file: ', a, /,   11x,'TDAYS = ',
     &                g12.4,2x,'last available SST_TIME = ',g12.4)
  99  may_day_flag=2
      return
      end



      subroutine set_sst_tile (Istr,Iend,Jstr,Jend)
!
! Set-up sea surface temperature data for current tile.
!
      implicit none
      integer Istr,Iend,Jstr,Jend, i,j, it1,it2
      real cff, cff1,cff2, cff3,cff4, val1,val2
# define SST_DATA
# include "param.h"
# include "forces.h"
# include "scalars.h"
!
# include "compute_extended_bounds.h"
!
      it1=3-itsst
      it2=itsst
      cff=time+0.5*dt
      cff1=sst_time(it2)-cff
      cff2=cff-sst_time(it1)
!
!  Load time invariant SST and dQdSST data.
!
      if (sst_cycle.lt.0.) then
        if (FIRST_TIME_STEP) then
          if (lsstgrd.eq.1) then
            do j=JstrR,JendR
              do i=IstrR,IendR
                sst(i,j)=sstg(i,j,itsst)
                dqdt(i,j)=scldqdt*dqdtg(i,j,itsst)
              enddo
            enddo
          else
            val1=sstp(itsst)
            val2=scldqdt*dqdtp(itsst)
            do j=JstrR,JendR
              do i=IstrR,IendR
                sst(i,j)=val1
                dqdt(i,j)=val2
              enddo
            enddo
          endif
        endif
!
!  Time-interpolate SST and dQdSST from grided or point data.
!  Check that for the next time step [when time=time+dt] time+dt 
!  is still between sst_time(it1) and sst_time(it2); and if not,
!  set synchro_flag top signal that the new forcing data should be
!  read from the netCDF input file (master thread only).
!
      elseif (cff1.ge.0. .and. cff2.ge.0.) then
        if (ZEROTH_TILE .and. cff1.lt.dt) synchro_flag=.TRUE.
        cff=scldqdt/(cff1+cff2)
        cff3=cff1*cff
        cff4=cff2*cff
        cff=1./(cff1+cff2)
        cff1=cff1*cff
        cff2=cff2*cff
        if (lsstgrd.eq.1) then
          do j=JstrR,JendR
            do i=IstrR,IendR
              sst(i,j)=cff1*sstg(i,j,it1)+cff2*sstg(i,j,it2)
              dqdt(i,j)=cff3*dqdtg(i,j,it1)+cff4*dqdtg(i,j,it2)
            enddo
          enddo
        else
          val1=cff1*sstp(it1)+cff2*sstp(it2)
          val2=cff3*dqdtp(it1)+cff4*dqdtp(it2)
          do j=JstrR,JendR
            do i=IstrR,IendR
              sst(i,j)=val1
              dqdt(i,j)=val2
            enddo
          enddo
        endif
!
!  Unable to set-up SST and dQdSST:
!  Complain about the error and signal to quit.      
!
      else
        if (ZEROTH_TILE) then
          write(stdout,1) 'sst_time', tdays, sst_time(it2)*sec2day
  1       format(/,' SET_SST - current model time exceeds ending',
     &           1x,'value for variable: ',a,/,11x,'TDAYS = ',g12.4,
     &           2x,'TEND = ',g12.4)
          may_day_flag=2
        endif
      endif
      return
      end
#else
      subroutine get_sst_empty
      return
      end
#endif /* QCORRECTION && !ANA_SST */
