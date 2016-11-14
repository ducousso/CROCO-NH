! $Id: get_uclima.F 1458 2014-02-03 15:01:25Z gcambon $
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
#if defined M2CLIMATOLOGY && !defined ANA_M2CLIMA || \
   (defined M3CLIMATOLOGY && !defined ANA_M3CLIMA)
!
                             ! Read tracer(s) climatology
      subroutine get_uclima  ! fields from climatology file
                             ! at appropriate time.
      implicit none
# include "param.h"
# include "scalars.h"
# include "ncscrum.h"
# include "climat.h"
      real cff
      integer i, lstr,lvar,lenstr, ierr, nf_fread, advance_cycle
# include "netcdf.inc"
!
! Initialization: Inquire about the contents of climatological file:
!================ variables and dimensions. Check for consistency.
!
      if (may_day_flag.ne.0) return      !-->  EXIT
      if (iic.eq.0 ) then
        lstr=lenstr(clmname)
!
! If not opened yet, open climatological file for reading.
! Check for availability of tracer climatology foelds in input
! netCDF file and save their IDs. Signal to terminate, if not found.
!
        if (ncidclm.eq.-1) then
          ierr=nf_open (clmname(1:lstr), nf_nowrite, ncidclm)
          if (ierr .ne. nf_noerr) goto 4                !--> ERROR
        endif

        ierr=nf_inq_varid (ncidclm, 'uclm_time', uclm_tid)
        if (ierr .ne. nf_noerr) then
          write(stdout,3) 'uclm_time', clmname(1:lstr)
          goto 99                                     !--> ERROR
        endif
# ifdef M2CLIMATOLOGY
        lvar=lenstr(vname(1,indxUb))
        ierr=nf_inq_varid (ncidclm, vname(1,indxUb)(1:lvar), ubclm_id)
        if (ierr .ne. nf_noerr) then
          write(stdout,3) vname(1,indxUb)(1:lvar), clmname(1:lstr)
          goto 99                                       !--> ERROR
        endif

        lvar=lenstr(vname(1,indxVb))
        ierr=nf_inq_varid (ncidclm, vname(1,indxVb)(1:lvar), vbclm_id)
        if (ierr .ne. nf_noerr) then
          write(stdout,3) vname(1,indxVb)(1:lvar), clmname(1:lstr)
          goto 99                                       !--> ERROR
        endif
# endif
# if defined M3CLIMATOLOGY && defined SOLVE3D
        lvar=lenstr(vname(1,indxU))
        ierr=nf_inq_varid (ncidclm, vname(1,indxU)(1:lvar), uclm_id)
        if (ierr .ne. nf_noerr) then
          write(stdout,3) vname(1,indxU)(1:lvar), clmname(1:lstr)
          goto 99                                       !--> ERROR
        endif

        lvar=lenstr(vname(1,indxV))
        ierr=nf_inq_varid (ncidclm, vname(1,indxV)(1:lvar), vclm_id)
        if (ierr .ne. nf_noerr) then
          write(stdout,3) vname(1,indxV)(1:lvar), clmname(1:lstr)
          goto 99                                       !--> ERROR
        endif
# endif
!
! Determine whether there is cycling to reuse the input data
! and find cycling period "uclm_cycle", set initial cycling
! index "uclm_ncycle" and record index "uclm_rec".
! Set initial value for time index "iuclm" and both time record
! bounds to large negative artificial values, so that it will
! trigger the logic in reading part below.
!
          call set_cycle (ncidclm, uclm_tid, ntuclm,
     &                    uclm_cycle, uclm_ncycle, uclm_rec)

          if (may_day_flag.ne.0) return    !-->  EXIT
          ituclm=2
          uclm_time(1)=-1.E+20
          uclm_time(2)=-1.E+20
      endif
!
! Read in momentum climatology: Check, if if model time is within the 
! ==== == ======== ============ bounds set by the past and future data
! times. If not, flip the time index, increment record and cycling
! indices and read a new portion of data: climatology time
! coordinate and tracer climatology field. Check and read again,
! until model time is between the two time bounds.
!
 10   i=3-ituclm
      cff=time+0.5*dt
      if (uclm_time(i).le.cff .and.
     &  cff.lt.uclm_time(ituclm)) goto 1
      ierr=advance_cycle (uclm_cycle,  ntuclm,
     &                    uclm_ncycle, uclm_rec)
      if (ierr.ne.0) then
        write(stdout,7) uclm_rec, ntuclm,
     &                  clmname(1:lstr), tdays,
     &                  uclm_time(ituclm)*sec2day
        goto 99                                        !--> ERROR
      endif

      ierr=nf_get_var1_FTYPE(ncidclm, uclm_tid,
     &                             uclm_rec, cff)
      if (ierr.ne.NF_NOERR) then
        write(stdout,6) 'Xclm_time', uclm_rec
        goto 99                                        !--> ERROR
      endif
      uclm_time(i)=cff*day2sec+uclm_cycle
     &                             *uclm_ncycle
      if (uclm_time(ituclm).eq.-1.E+20)
     &    uclm_time(ituclm)=uclm_time(i)
# ifdef M2CLIMATOLOGY
      ierr=nf_fread (ubclima(START_2D_ARRAY,i),
     &                           ncidclm, ubclm_id,
     &                           uclm_rec, u2dvar)
      if (ierr.ne.NF_NOERR) then
        lvar=lenstr(vname(1,indxUb))
        write(stdout,6) vname(1,indxUb)(1:lvar), uclm_rec
        goto 99                                       !--> ERROR
      endif
!
      ierr=nf_fread (vbclima(START_2D_ARRAY,i),
     &                           ncidclm, vbclm_id,
     &                           uclm_rec, v2dvar)
      if (ierr.ne.NF_NOERR) then
        lvar=lenstr(vname(1,indxVb))
        write(stdout,6) vname(1,indxVb)(1:lvar), uclm_rec
        goto 99                                       !--> ERROR
      endif
# endif
# if defined M3CLIMATOLOGY && defined SOLVE3D
      ierr=nf_fread (uclima(START_2D_ARRAY,1,i),
     &                           ncidclm, uclm_id,
     &                           uclm_rec, u3dvar)
      if (ierr.ne.NF_NOERR) then
        lvar=lenstr(vname(1,indxU))
        write(stdout,6) vname(1,indxU)(1:lvar), uclm_rec
        goto 99                                       !--> ERROR
      endif
!
      ierr=nf_fread (vclima(START_2D_ARRAY,1,i),
     &                           ncidclm, vclm_id,
     &                           uclm_rec, v3dvar)
      if (ierr.ne.NF_NOERR) then
        lvar=lenstr(vname(1,indxV))
        write(stdout,6) vname(1,indxV)(1:lvar), uclm_rec
        goto 99                                       !--> ERROR
      endif
# endif

      ituclm=i
      MPI_master_only write(stdout,'(6x,A,1x,g12.4,1x,I4)')
     &'GET_UCLIMA -- Read momentum climatology      for time =', cff
#ifdef MPI
     &                                                      , mynode
#endif
        if (ntuclm.gt.1) goto 10
  1    continue    
      return
!
! Sort out error messages: The following portion of the code is
!===== === ===== ========= not accessed unless something goes wrong.
!
  3   format(/,' GET_UCLIMA - unable to find climatology variable: ',
     &       a,/,15x,'in climatology NetCDF file: ',a)
  4   write(stdout,5) clmname(1:lstr)
  5   format(/,' GET_UCLIMA - unable to open climatology',
     &         1x,'NetCDF file: ',a)
      goto 99
  6   format(/,' GET_UCLIMA - ERROR while reading variable: ',a,2x,
     &       ' at TIME index = ',i4)

  7   format(/,' GET_UCLIMA - ERROR: requested time record ',I4,
     &       1x,'exeeds the last available', /,14x,'record ',I4,
     &       1x,'in climatology file: ',a, /,14x,'TDAYS = ',
     &       g12.4,2x,'last available UCLM_TIME = ',g12.4)

  99  may_day_flag=2
      return
      end

      subroutine set_uclima (tile)
      implicit none
      integer tile
# include "param.h"
#ifdef  ALLOW_SINGLE_BLOCK_MODE
C$    integer  trd, omp_get_thread_num
#endif
# include "compute_tile_bounds.h"
      call set_uclima_tile (Istr,Iend,Jstr,Jend)
      return
      end

      subroutine set_uclima_tile (Istr,Iend,Jstr,Jend)
!
!--------------------------------------------------------------------
!  Set-up momentum climatology for current tile.
!--------------------------------------------------------------------
!
      implicit none
# include "param.h"
# include "scalars.h"
# include "climat.h"
      integer Istr,Iend,Jstr,Jend, i,j,k, it1,it2
      real cff, cff1, cff2
!
# include "compute_extended_bounds.h"
!
! Set coefficients for interpolation. Check that for the next time
! step [when time=time+dt] both weights will still be positive, and
! if not, set synchro_flag to signal that new data should be read
! from an appropriate netCDF input file (master thread only).
! After that either load time-invariant data, or interpolate in time
! or complain about error and signal to quit, if interpolation is
! needed, but not possible.
!
      it1=3-ituclm
      it2=ituclm
      cff=time+0.5*dt
      cff1=uclm_time(it2)-cff
      cff2=cff-uclm_time(it1)
      if (ZEROTH_TILE .and. cff1.lt.dt) synchro_flag=.TRUE.

      if (uclm_cycle.lt.0.) then      ! Load time-invariant
        if (iic.eq.0) then            ! momentum climatology. 
# ifdef M2CLIMATOLOGY
          do j=JstrR,JendR
            do i=IstrR,IendR
              ubclm(i,j)=ubclima(i,j,ituclm)
              vbclm(i,j)=vbclima(i,j,ituclm)
            enddo
          enddo
# endif
# if defined M3CLIMATOLOGY && defined SOLVE3D
          do k=1,N  
            do j=JstrR,JendR
              do i=IstrR,IendR
                uclm(i,j,k)=uclima(i,j,k,ituclm)
                vclm(i,j,k)=vclima(i,j,k,ituclm)
              enddo
            enddo
          enddo
# endif
        endif
      elseif (cff1.ge.0. .and. cff2.ge.0.) then
        cff=1./(cff1+cff2)                     ! Interpolate tracer
        cff1=cff1*cff                          ! climatology in time. 
        cff2=cff2*cff
# ifdef M2CLIMATOLOGY
        do j=JstrR,JendR
          do i=IstrR,IendR
            ubclm(i,j)=cff1*ubclima(i,j,it1)
     &                +cff2*ubclima(i,j,it2)
            vbclm(i,j)=cff1*vbclima(i,j,it1)
     &                +cff2*vbclima(i,j,it2)
          enddo
        enddo
# endif
# if defined M3CLIMATOLOGY && defined SOLVE3D
        do k=1,N
          do j=JstrR,JendR
            do i=IstrR,IendR
              uclm(i,j,k)=cff1*uclima(i,j,k,it1)
     &                   +cff2*uclima(i,j,k,it2)
              vclm(i,j,k)=cff1*vclima(i,j,k,it1)
     &                   +cff2*vclima(i,j,k,it2)
            enddo
          enddo
        enddo
# endif
      elseif (ZEROTH_TILE) then
          write(stdout,'(/1x,2A/3(1x,A,F16.10)/)')
     &            'SET_UCLIMA_TILE - current model time is outside ',
     &            'bounds of ''uclm_time''.', 'UCLM_TSTART=',
     &             uclm_time(it1)*sec2day,     'TDAYS=',  tdays,
     &            'UCLM_TEND=',   uclm_time(it2)*sec2day
          may_day_flag=2
      endif
      return
      end
#else
      subroutine get_uclima_empty 
      return
      end
#endif /* M2CLIMATOLOGY && !ANA_M2CLIMA || (M3CLIMATOLOGY && !ANA_M3CLIMA) */
