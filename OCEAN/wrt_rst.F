! $Id: wrt_rst.F 1571 2014-07-01 12:38:05Z gcambon $
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
                              ! Write model prognostic
      subroutine wrt_rst      ! variables into restart
                              ! netCDF file.
#if defined FLOATS && defined AGRIF
        USE Agrif_Util
#endif
!     implicit none
#include "param.h"
#include "scalars.h"
#include "ncscrum.h"
#include "ocean2d.h"
#include "ocean3d.h"
#include "netcdf.inc"
#include "mpi_roms.h"
#ifdef SEDIMENT
# include "sediment.h"
#endif
#ifdef BBL
# include "bbl.h"
#endif
#if defined FLOATS 
# include "ncscrum_floats.h"
# include "floats.h"
#endif
      integer ierr, record, lstr, lvar, lenstr
     &  , start(2), count(2), ibuff(4), nf_fwrite
#ifdef SOLVE3D
     &      , itrc
#endif
#  ifdef SEDIMENT
     &      , indxWrk
#  endif
#ifdef FLOATS
      integer i, j, startTinfo(3), starttrack(4)
     &  ,countTinfo(3), counttrack(4), level
      real bufftrack(1:6,NFT+1,nfloats)
# ifdef AGRIF
      type(Agrif_pgrid), pointer :: parcours
      integer tmp(NFT+1,-1:maxgrids)
# else
      integer tmp(NFT+1,1)
# endif
#endif
#if defined MPI & !defined PARALLEL_FILES
      include 'mpif.h'
      integer status(MPI_STATUS_SIZE), blank
#endif

#if defined MPI & !defined PARALLEL_FILES
      if (mynode.gt.0) then
        call MPI_Recv (blank, 1, MPI_INTEGER, mynode-1,
     &                 1, MPI_COMM_WORLD, status, ierr)
      endif
#endif
!
! Create/open restart file; write grid arrays, if so needed.
!
      call def_rst (ncidrst, nrecrst, ierr)
      if (ierr .ne. nf_noerr) goto 99
      lstr=lenstr(rstname)
!                                            !!! WARNING: Here it is
! Set record within the file.                !!! assumed that global
!                                            !!! restart record index 
      nrecrst=max(nrecrst,1)                 !!! nrecrst is already
      if (nrpfrst.eq.0) then                 !!! advanced by main.
        record=nrecrst
      else
        record=1+mod(nrecrst-1, abs(nrpfrst))
      endif

!
! Write out evolving model variables:
! ----- --- -------- ----- ----------
!
! Time step number and record indices. 
!
      ibuff(1)=iic
      ibuff(2)=nrecrst
      ibuff(3)=nrechis
#ifdef AVERAGES
      ibuff(4)=nrecavg
#else
      ibuff(4)=0
#endif
      start(1)=1
      start(2)=record
      count(1)=4
      count(2)=1
      ierr=nf_put_vara_int (ncidrst, rstTstep, start, count, ibuff)
      if (ierr .ne. nf_noerr) then
        write(stdout,1) 'time_step', record, ierr
     &                   MYID
        goto 99                                           !--> ERROR
      endif
!
! Time.
!
      ierr=nf_put_var1_FTYPE (ncidrst, rstTime, record, time)
      if (ierr .ne. nf_noerr) then
        lvar=lenstr(vname(1,indxTime))
        write(stdout,1) vname(1,indxTime)(1:lvar), record, ierr
     &                  MYID
        goto 99                                           !--> ERROR
      endif
!
! Free-surface.
!
      ierr=nf_fwrite(zeta(START_2D_ARRAY,fast_indx_out), ncidrst,
     &                                      rstZ, record, r2dvar)
      if (ierr .ne. nf_noerr) then
        lvar=lenstr(vname(1,indxZ))
        write(stdout,1) vname(1,indxZ)(1:lvar), record, ierr
     &                  MYID
        goto 99                                           !--> ERROR
      endif
!
! 2D momentum component in XI-direction.
!
      ierr=nf_fwrite(ubar(START_2D_ARRAY,fast_indx_out), ncidrst,
     &                                     rstUb, record, u2dvar)
      if (ierr .ne. nf_noerr) then
        lvar=lenstr(vname(1,indxUb))
        write(stdout,1) vname(1,indxUb)(1:lvar), record, ierr
     &                  MYID
        goto 99                                           !--> ERROR
      endif
!
! 2D momentum component in ETA-direction.
!
      ierr=nf_fwrite(vbar(START_2D_ARRAY,fast_indx_out), ncidrst,
     &                                     rstVb, record, v2dvar)
      if (ierr .ne. nf_noerr) then
        lvar=lenstr(vname(1,indxVb))
        write(stdout,1) vname(1,indxVb)(1:lvar), record, ierr
     &                  MYID
        goto 99                                           !--> ERROR
      endif
#ifdef SOLVE3D
!
! 3D momentum component in XI-direction.
!
      ierr=nf_fwrite(u(START_2D_ARRAY,1,nstp), ncidrst, rstU,
     &                                        record, u3dvar)
      if (ierr .ne. nf_noerr) then
        lvar=lenstr(vname(1,indxU))
        write(stdout,1) vname(1,indxU)(1:lvar), record, ierr
     &                  MYID
        goto 99                                           !--> ERROR
      endif
!
! 3D momentum component in ETA-direction.
!
      ierr=nf_fwrite(v(START_2D_ARRAY,1,nstp), ncidrst, rstV,
     &                                        record, v3dvar)
      if (ierr .ne. nf_noerr) then
        lvar=lenstr(vname(1,indxV))
        write(stdout,1) vname(1,indxV)(1:lvar), record, ierr
     &                  MYID
        goto 99                                           !--> ERROR
      endif
!
! Tracer variables.
!
      do itrc=1,NT
        ierr=nf_fwrite(t(START_2D_ARRAY,1,nstp,itrc), ncidrst,
     &                             rstT(itrc), record, r3dvar)
        if (ierr .ne. nf_noerr) then
          lvar=lenstr(vname(1,indxT+itrc-1))
          write(stdout,1) vname(1,indxT+itrc-1)(1:lvar), record,
     &                    ierr MYID
          goto 99                                         !--> ERROR
        endif
      enddo
# endif


# ifdef SEDIMENT
!
! Write out sediment bed layer thickness, porosity, volume 
! fraction of size class in sediment bed (2+2*NST b3dgrd variables)
!
      ierr=nf_fwrite(bed_thick, ncidrst, rstSed(1), 
     &                                       record, b3dvar)
      if (ierr .ne. nf_noerr) then
        lvar=lenstr(vname(1,indxBTHK))
        write(stdout,1) vname(1,indxBTHK)(1:lvar), record, ierr
     &                  MYID
        goto 99                                         !--> ERROR
      endif
CR    write(*,*) 'wrt_rst:  Bed_thick' MYID

      ierr=nf_fwrite(bed_poros, ncidrst, rstSed(2), 
     &                                       record, b3dvar)
      if (ierr .ne. nf_noerr) then
        lvar=lenstr(vname(1,indxBPOR))
        write(stdout,1) vname(1,indxBPOR)(1:lvar), record, ierr
     &                  MYID
        goto 99                                         !--> ERROR
      endif
CR    write(*,*) 'wrt_rst:  Bed_poros' MYID

      do itrc=1,NST
        indxWrk=indxBFRA(1)+itrc-1
        ierr=nf_fwrite(bed_frac(START_2D_ARRAY,1,itrc), 
     &             ncidrst, rstSed(itrc+2), record, b3dvar)
        if (ierr .ne. nf_noerr) then
          lvar=lenstr(vname(1,indxWrk))
          write(stdout,1) vname(1,indxWrk)(1:lvar), record, 
     &                ierr
     &                MYID
          goto 99                                         !--> ERROR
        endif
CR      write(*,*) 'wrt_rst:  Bed_frac' MYID
      enddo

# endif /* SEDIMENT */

# ifdef BBL
      ierr=nf_fwrite(Hripple, ncidrst, rstBBL(1), record, r2dvar)
      if (ierr .ne. nf_noerr) then
        lvar=lenstr(vname(1,indxHrip))
        write(stdout,1) vname(1,indxHrip)(1:lvar), record, ierr
     &                    MYID
        goto 99                                         !--> ERROR
      endif
CR    write(*,*) 'wrt_rst:  Hripple' MYID
      ierr=nf_fwrite(Lripple, ncidrst, rstBBL(2), record, r2dvar)
      if (ierr .ne. nf_noerr) then
        lvar=lenstr(vname(1,indxLrip))
        write(stdout,1) vname(1,indxLrip)(1:lvar), record, ierr
     &                  MYID
        goto 99                                         !--> ERROR
      endif
CR    write(*,*) 'wrt_rst:  Lripple' MYID
# endif /* BBL */

#ifdef FLOATS
# ifdef AGRIF
      if (Agrif_Root()) then
# endif
! write nfloats
      ierr=nf_put_vara_int (ncidrst, rstnfloats, record, 1, nfloats)
      if (ierr .ne. nf_noerr) then
        write(stdout,1) 'nfloats', record, ierr, nf_strerror(ierr)
     &                   MYID
        goto 99                                           !--> ERROR
      endif

! write Tinfo

      startTinfo(1)=1
      startTinfo(2)=1
      startTinfo(3)=record
      countTinfo(1)=5
      countTinfo(2)=nfloats
      countTinfo(3)=1

      ierr=nf_put_vara_FTYPE (ncidrst, rstTinfo, startTinfo,
     &   countTinfo, Tinfo)
      if (ierr .ne. nf_noerr) then
        write(stdout,1) 'Tinfo', record, ierr, nf_strerror(ierr)
     &                   MYID
        goto 99                                           !--> ERROR
      endif

! write grid level

      start(1)=1
      start(2)=record
      count(1)=nfloats
      count(2)=1

      ierr=nf_put_vara_int (ncidrst, rstfltgrd, start, count, fltgrd)
      if (ierr .ne. nf_noerr) then
        write(stdout,1) 'grid level', record, ierr, nf_strerror(ierr)
     &                   MYID
        goto 99                                           !--> ERROR
      endif

! write track

      starttrack(1)=1
      starttrack(2)=1
      starttrack(3)=1
      starttrack(4)=record
      counttrack(1)=6
      counttrack(2)=NFT+1
      counttrack(3)=nfloats
      counttrack(4)=1

# ifdef AGRIF
      level=-1
      tmp(1,level)=nf
      tmp(2,level)=nfm1
      tmp(3,level)=nfm2
      tmp(4,level)=nfm3
      level=0
      tmp(1,level)=nf
      tmp(2,level)=nfm1
      tmp(3,level)=nfm2
      tmp(4,level)=nfm3
      do level=1,AGRIF_Nb_Fine_Grids()
        tmp(4,level)=floattindex(level)
        tmp(1,level)=mod(tmp(4,level)+3,NFT+1)
        tmp(2,level)=mod(tmp(4,level)+2,NFT+1)
        tmp(3,level)=mod(tmp(4,level)+1,NFT+1)
      enddo
# endif

      do i=1,NFT+1
        do j=1,nfloats
          bufftrack(1,i,j)=track(ixgrd,tmp(i,fltgrd(j)),j)
          bufftrack(2,i,j)=track(iygrd,tmp(i,fltgrd(j)),j)
          bufftrack(3,i,j)=track(izgrd,tmp(i,fltgrd(j)),j)
          bufftrack(4,i,j)=track(ixrhs,tmp(i,fltgrd(j)),j)
          bufftrack(5,i,j)=track(iyrhs,tmp(i,fltgrd(j)),j)
          bufftrack(6,i,j)=track(izrhs,tmp(i,fltgrd(j)),j)
        enddo
      enddo

      ierr=nf_put_vara_FTYPE (ncidrst, rsttrack, starttrack,
     &   counttrack, bufftrack)
      if (ierr .ne. nf_noerr) then
        write(stdout,1) 'track', record, ierr, nf_strerror(ierr)
     &                   MYID
        goto 99                                           !--> ERROR
      endif
# ifdef AGRIF
      endif       ! Agrif_Root()
# endif
#endif

  1   format(/1x, 'WRT_RST ERROR while writing variable ''', A,
     &           ''' into restart file.', /11x, 'Time record:',
     &               i6, 3x, 'netCDF error code', i4, 3x, A,i4)
      goto 100 
  99  may_day_flag=3
 100  continue

!
! Synchronize restart netCDF file to disk to allow other
! processes to access data immediately after it is written.
!
#if defined MPI & !defined PARALLEL_FILES
      ierr=nf_close (ncidrst)
      if (nrpfrst.gt.0 .and. record.ge.nrpfrst) ncidrst=-1
#else
      if (nrpfrst.gt.0 .and. record.ge.nrpfrst) then 
        ierr=nf_close (ncidrst)
        ncidrst=-1
      else
        ierr=nf_sync(ncidrst)
      endif
#endif
      if (ierr .eq. nf_noerr) then
      MPI_master_only write(stdout,'(6x,A,2(A,I4,1x),A,I3)') 
     &            'WRT_RST -- wrote ',
     &            'restart fields into time record =', record, '/',
     &             nrecrst  MYID
      else
      MPI_master_only  write(stdout,'(/1x,2A/)') 
     &             'WRT_RST ERROR: Cannot ',
     &             'synchronize/close restart netCDF file.'
        may_day_flag=3
      endif

#if defined MPI & !defined PARALLEL_FILES
      if (mynode .lt. NNODES-1) then
        call MPI_Send (blank, 1, MPI_INTEGER, mynode+1,
     &                        1, MPI_COMM_WORLD,  ierr)
      endif
#endif
      return
      end
