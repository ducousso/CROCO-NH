! $Id: wrt_floats.F 1458 2014-02-03 15:01:25Z gcambon $
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
#ifdef FLOATS
                               ! Writes requested model
      subroutine wrt_floats    ! fields at requested levels 
                               ! into history netCDF file.
# ifdef AGRIF
      USE Agrif_Util 
# endif
      implicit none
# include "param.h"
# include "scalars.h"
# include "ncscrum.h"
# include "ncscrum_floats.h"
# include "grid.h"
# include "ocean2d.h"
# include "ocean3d.h"
# include "mixing.h"
# include "mpi_roms.h"
# include "floats.h"
# include "netcdf.inc"

      integer ierr,  record, lvar, lenstr, iflt, id
     &   , nfltrelmax, nfltunrelmax, indxrel(Mfloats)
     &   , indxunrel(Mfloats),Toutint(Mfloats)
     &   , start(2), count(2), ibuff(2), nf_fwrite
      logical newf
      character*65 vinfo
      real Tout(Mfloats)

# if defined MPI & !defined PARALLEL_FILES
      include 'mpif.h'
      integer status(MPI_STATUS_SIZE), blank
# endif
# if defined MPI & !defined PARALLEL_FILES
      if (mynode.gt.0) then
        call MPI_Recv (blank, 1, MPI_INTEGER, mynode-1, 
     &                 1, MPI_COMM_WORLD, status, ierr) 
      endif
# endif
!
! Create/open history file; write grid arrays, if so needed.

      newf=.false.
      call def_floats (ncidflt, nrecflt, ierr, newf)
      if (ierr .ne. nf_noerr) goto 99
!                                       !!! WARNING: Once time  
! Set record within the file.           !!! stepping has been
!                                       !!! started, it is assumed
      if (iic.eq.0) nrecflt=nrecflt+1   !!! that the global float
      if (nrpfflt.eq.0) then            !!!  history record index 
        record=nrecflt                  !!! "nrecflt" is advanced
      else                              !!! by main.
        record=1+mod(nrecflt-1, nrpfflt)
      endif

!
! Write out evolving model variables:
! ----- --- -------- ----- ----------
!
! Save indices of released and non released floats 
!
      nfltrelmax=0
      nfltunrelmax=0
      do iflt=1,nfloats
        if (fltgrd(iflt).ne.-1) then
          nfltrelmax=nfltrelmax+1
          indxrel(nfltrelmax)=iflt
        else 
          nfltunrelmax=nfltunrelmax+1
          indxunrel(nfltunrelmax)=iflt
        endif
      enddo


! Time step number and record numbers.
!
      ibuff(1)=iic
      ibuff(2)=nrecflt
      start(1)=1
      start(2)=record
      count(1)=2
      count(2)=1

      ierr=nf_put_vara_int (ncidflt, fltTstep, start, count, ibuff)
      if (ierr .ne. nf_noerr) then
        write(stdout,1) 'time_step', record, ierr, nf_strerror(ierr)
     &                   MYID
        goto 99                                           !--> ERROR
      endif
!
! Time
!

      ierr=nf_put_var1_FTYPE (ncidflt, fltTime, record, time)
      if (ierr .ne. nf_noerr) then
        lvar=lenstr(vname(1,indxTime))
        write(stdout,1) vname(1,indxTime)(1:lvar), record, ierr,
     &                  nf_strerror(ierr) MYID
        goto 99                                           !--> ERROR
      endif

!
!    define position in nc file to write float data
      start(1)=1
      count(1)=nfloats
      start(2)=record
      count(2)=1

      if (wrtflt(indxfltGrd)) then
!
! Grid level
      do id=1,nfloats
        Toutint(id)=fltgrd(id)
      enddo

      ierr=nf_put_vara_int (ncidflt, fltGlevel, start,count, Toutint)
      if (ierr .ne. nf_noerr) then
        vinfo='grid level'
        lvar=lenstr(vinfo)
        write(stdout,1) vinfo(1:lvar), record, ierr,
     &                  nf_strerror(ierr) MYID
        goto 99                                           !--> ERROR
      endif
      endif
!
!  Fills in tmp variable with flospval values for the nonreleased floats
      do id=1,nfltunrelmax
        iflt=indxunrel(id)
        Tout(iflt)=flospval
      enddo

# ifdef SOLVE3D

       if (wrtflt(indxfltTemp)) then
! temperature at floats position

      do id=1,nfltrelmax
        iflt=indxrel(id)
        Tout(iflt)=trackaux(iftem,iflt)
      enddo

      ierr=nf_put_vara_FTYPE(ncidflt,fltTemp,start,count,
     &                         Tout)
      if (ierr .ne. nf_noerr) then
        vinfo='Temp'
        lvar=lenstr(vinfo)
        write(stdout,1) vinfo(1:lvar), record, ierr,
     &                  nf_strerror(ierr) MYID
        goto 99                                           !--> ERROR
      endif
      endif

#  ifdef SALINITY

       if (wrtflt(indxfltSalt)) then
! salinity at floats position

      do id=1,nfltrelmax
        iflt=indxrel(id)
        Tout(iflt)=trackaux(ifsal,iflt)
      enddo

      ierr=nf_put_vara_FTYPE(ncidflt,fltSal,start,count,
     &                         Tout)
      if (ierr .ne. nf_noerr) then
        vinfo='Salt'
        lvar=lenstr(vinfo)
        write(stdout,1) vinfo(1:lvar), record, ierr,
     &                  nf_strerror(ierr) MYID
        goto 99                                           !--> ERROR
      endif
      endif

#  endif

       if (wrtflt(indxfltRho)) then
! density at floats position

      do id=1,nfltrelmax
        iflt=indxrel(id)
        Tout(iflt)=trackaux(ifden,iflt)
      enddo

      ierr=nf_put_vara_FTYPE(ncidflt,fltDen,start,count,
     &                         Tout)
      if (ierr .ne. nf_noerr) then
        vinfo='Den'
        lvar=lenstr(vinfo)
        write(stdout,1) vinfo(1:lvar), record, ierr,
     &                  nf_strerror(ierr) MYID
        goto 99                                           !--> ERROR
      endif
      endif

# endif /* SOLVE3D */

# ifdef IBM
! IBM data
      do id=1,nfltrelmax
        iflt=indxrel(id)
        Tout(iflt)=ibmdata(ibmage,iflt)
      enddo

      ierr=nf_put_vara_FTYPE(ncidflt,fltAge,start,count,
     &                         Tout)
      if (ierr .ne. nf_noerr) then
        vinfo='Age'
        lvar=lenstr(vinfo)
        write(stdout,1) vinfo(1:lvar), record, ierr,
     &                  nf_strerror(ierr) MYID
        goto 99                                           !--> ERROR
      endif

      do id=1,nfltrelmax
        iflt=indxrel(id)
        Tout(iflt)=ibmdata(ibmzoe,iflt)
      enddo

      ierr=nf_put_vara_FTYPE(ncidflt,fltZoe,start,count,
     &                         Tout)
      if (ierr .ne. nf_noerr) then
        vinfo='Zoe'
        lvar=lenstr(vinfo)
        write(stdout,1) vinfo(1:lvar), record, ierr,
     &                  nf_strerror(ierr) MYID
        goto 99                                           !--> ERROR
      endif
# endif /* IBM */

      if (wrtflt(indxfltVel)) then
! write mean velocity

      do id=1,nfltrelmax
        iflt=indxrel(id)
        Tout(iflt)=trackaux(ifvel,iflt)
        trackaux(ifvel,iflt)=0. !reinitializes variables for means
      enddo

      ierr=nf_put_vara_FTYPE(ncidflt,fltVel,start,count,
     &                         Tout)
      if (ierr .ne. nf_noerr) then
        vinfo='Vel'
        lvar=lenstr(vinfo)
        write(stdout,1) vinfo(1:lvar), record, ierr,
     &                  nf_strerror(ierr) MYID
        goto 99                                           !--> ERROR
      endif
      endif

!----------------------------------------------------------------
! The following variables are to be stored with a non flospval value 
! at the first time step. Therefore, a modification to nfltrelmax 
! and indxrel is done at this place. 

       if (newf) then 
          nfltrelmax=nfloats
          do iflt=1,nfloats
              indxrel(iflt)=iflt
          enddo
       endif

# ifdef SPHERICAL
!  WRITE floats (lon,lat) locations.

      do id=1,nfltrelmax
        iflt=indxrel(id)
        Tout(iflt)=trackaux(iflat,iflt)
      enddo

      ierr=nf_put_vara_FTYPE(ncidflt,fltLat,start,count,
     &                         Tout)
      if (ierr .ne. nf_noerr) then
        vinfo='Lat'
        lvar=lenstr(vinfo)
        write(stdout,1) vinfo(1:lvar), record, ierr,
     &                  nf_strerror(ierr) MYID
        goto 99                                           !--> ERROR
      endif

      do id=1,nfltrelmax
        iflt=indxrel(id)
        Tout(iflt)=trackaux(iflon,iflt)
      enddo

      ierr=nf_put_vara_FTYPE(ncidflt,fltLon,start,count,
     &                         Tout)
      if (ierr .ne. nf_noerr) then
        vinfo='Lon'
        lvar=lenstr(vinfo)
        write(stdout,1) vinfo(1:lvar), record, ierr,
     &                  nf_strerror(ierr) MYID
        goto 99                                           !--> ERROR
      endif

# endif

      if (wrtflt(indxfltGrd)) then
!  WRITE X position in the grid
      do id=1,nfltrelmax
        iflt=indxrel(id)
        Tout(iflt)=trackaux(ixgrd,iflt)
      enddo

      ierr=nf_put_vara_FTYPE(ncidflt,fltXgrd,start,count,
     &                         Tout)
      if (ierr .ne. nf_noerr) then
        vinfo='Xgrid'
        lvar=lenstr(vinfo)
        write(stdout,1) vinfo(1:lvar), record, ierr,
     &                  nf_strerror(ierr) MYID
        goto 99                                           !--> ERROR
      endif

!  WRITE Y position in the grid
      do id=1,nfltrelmax
        iflt=indxrel(id)
        Tout(iflt)=trackaux(iygrd,iflt)
      enddo

      ierr=nf_put_vara_FTYPE(ncidflt,fltYgrd,start,count,
     &                         Tout)
      if (ierr .ne. nf_noerr) then
        vinfo='Ygrid'
        lvar=lenstr(vinfo)
        write(stdout,1) vinfo(1:lvar), record, ierr,
     &                  nf_strerror(ierr) MYID
        goto 99                                           !--> ERROR
      endif

# ifdef SOLVE3D
!  WRITE Z position in the grid
      do id=1,nfltrelmax
        iflt=indxrel(id)
        Tout(iflt)=trackaux(izgrd,iflt)
      enddo

      ierr=nf_put_vara_FTYPE(ncidflt,fltZgrd,start,count,
     &                         Tout)
      if (ierr .ne. nf_noerr) then
        vinfo='Zgrid'
        lvar=lenstr(vinfo)
        write(stdout,1) vinfo(1:lvar), record, ierr,
     &                  nf_strerror(ierr) MYID
        goto 99                                           !--> ERROR
      endif
# endif
      endif

# ifdef SOLVE3D
!
! float depth

      do id=1,nfltrelmax
        iflt=indxrel(id)
        Tout(iflt)=trackaux(ifdpt,iflt)
      enddo

      ierr=nf_put_vara_FTYPE(ncidflt,fltDepth,start,count,
     &                         Tout)
      if (ierr .ne. nf_noerr) then
        vinfo='Depth'
        lvar=lenstr(vinfo)
        write(stdout,1) vinfo(1:lvar), record, ierr,
     &                  nf_strerror(ierr) MYID
        goto 99                                           !--> ERROR
      endif
# endif

  1   format(/1x, 'WRT_FLT ERROR while writing variable ''', A,
     &        ''' into float file.'  /11x, 'Time record:', I6,
     &        3x,'netCDF error code',i4 /11x,'Cause of error: ',
     &                                            A, 3x, A, i4)
      goto 100 
  99  may_day_flag=3
 100  continue


!
! Synchronize netCDF file to disk to allow other processes
! to access data immediately after it is written.
!
# if defined MPI & !defined PARALLEL_FILES
      ierr=nf_close (ncidflt)
      if (nrpfflt.gt.0 .and. record.ge.nrpfflt) ncidflt=-1
# else
      if (nrpfflt.gt.0 .and. record.ge.nrpfflt) then
        ierr=nf_close (ncidflt)
!        write(*,*) 'FLOAT FILE IS CLOSED (XA) '
        ncidflt=-1
      else
        ierr=nf_sync(ncidflt)
      endif
# endif
      if (ierr .eq. nf_noerr) then
        write(stdout,'(6x,A,2(A,I4,1x),A,I3)') 'WRT_FLT -- wrote ',
     &            'float history fields into time record =', record, 
     &            '/' ,nrecflt  MYID
      else
        write(stdout,'(/1x,2A/)') 'WRT_FLT ERROR: Cannot ',
     &             'synchronize/close float netCDF file.'
        may_day_flag=3
      endif

# if defined MPI & !defined PARALLEL_FILES
      if (mynode .lt. NNODES-1) then
        call MPI_Send (blank, 1, MPI_INTEGER, mynode+1,
     &                        1, MPI_COMM_WORLD,  ierr)
      endif
# endif

      return
      end


#else
      subroutine wrt_floats_empty
      return
      end
#endif /* FLOATS */

