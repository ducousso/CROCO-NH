! $Id: wrt_bio_diags_avg.F 1571 2014-07-01 12:38:05Z gcambon $
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
#if defined DIAGNOSTICS_BIO &&  defined AVERAGES
! Write time-averaged diags flux fields into averages netCDF file
                                  ! Writes requested model
      subroutine wrt_bio_diags_avg    ! fields at requested levels 
                                  ! into diagnostics netCDF file.
      implicit none
      integer ierr,  record, lstr, lvar, lenstr
     &   , start(2), count(2), ibuff(4), nf_fwrite, type
#if defined MPI & !defined PARALLEL_FILES
      include 'mpif.h'
      integer status(MPI_STATUS_SIZE), blank
#endif
#include "param.h"
#include "scalars.h"
#include "ncscrum.h"
#include "forces.h"
#include "grid.h"
#include "ocean2d.h"
#include "ocean3d.h"
#include "mixing.h"
#include "diagnostics.h"
#include "mpi_roms.h"

#ifdef SOLVE3D
      integer iflux
# include "work.h"
#endif

#include "netcdf.inc"

#if defined MPI & !defined PARALLEL_FILES
      if (mynode.gt.0) then
        call MPI_Recv (blank, 1, MPI_INTEGER, mynode-1, 
     &                 1, MPI_COMM_WORLD, status, ierr) 
      endif
#endif

!
! Create/open diagnostic file; write grid arrays, if so needed.
!
      call def_bio_diags_avg (nciddiabio_avg, nrecdiabio_avg, ierr)
      if (ierr .ne. nf_noerr) goto 99
      lstr=lenstr(dianamebio_avg)

!                                            !!! WARNING: Once time  
! Set record within the file.                !!! stepping has been 
!                                            !!! started, it is assumed
      nrecdiabio_avg=max(nrecdiabio_avg,1)   !!! that global history
      if (nrpfdiabio_avg.eq.0) then          !!! record index is
        record=nrecdiabio_avg                !!! advanced by main.  
      else
        record=1+mod(nrecdiabio_avg-1, nrpfdiabio_avg)
      endif
!
! Write out evolving model variables:
! ----- --- -------- ----- ----------
!
! Time step number and record numbers.
!
      type=filetype_diabio_avg
!
      ibuff(1)=iic
      ibuff(2)=nrecrst
      ibuff(3)=nrechis
!#ifdef AVERAGES
!      ibuff(4)=nrecavg
!#else
!      ibuff(4)=0
!#endif
      ibuff(4)=nrecdiabio_avg
      start(1)=1
      start(2)=record
      count(1)=4
      count(2)=1
      ierr=nf_put_vara_int (nciddiabio_avg, diaTstepbio_avg, 
     &                                             start, count, ibuff)
      if (ierr .ne. nf_noerr) then
        write(stdout,1) 'time_step_avg', record, ierr
     &                  MYID
        goto 99                                           !--> ERROR
      endif
!
! Averaged diag Time
!
C      write(*,*) 'Write Time'
      ierr=nf_put_var1_FTYPE (nciddiabio_avg, diaTimebio_avg, record,
     &                                              timediabio_avg)
      if (ierr .ne. nf_noerr) then
        lvar=lenstr(vname(1,indxTime))
        write(stdout,1) vname(1,indxTime)(1:lvar), record, ierr
     &                  MYID
        goto 99                                           !--> ERROR
      endif
!
! Averaged diag Time2
!
C      write(*,*) 'Write Time'
      ierr=nf_put_var1_FTYPE (nciddiabio_avg, diaTime2bio_avg, record,
     &                                              timediabio_avg)
      if (ierr .ne. nf_noerr) then
        lvar=lenstr(vname(1,indxTime2))
        write(stdout,1) vname(1,indxTime2)(1:lvar), record, ierr
     &                  MYID
        goto 99                                           !--> ERROR
      endif
      
!
! Tracer diag variables.
!
!
#ifdef PISCES
#  if defined key_trc_diaadd
!     3-D terms (it is called Flux but it means nothing)
      do iflux=1,NumFluxTerms
        if (wrtdiabioFlux_avg(iflux)) then
          workr=bioFlux_avg(:,:,:,iflux)
          call fillvalue3d(workr, nciddiabio_avg, diabioFlux_avg(iflux),
     &         indxbioFlux+iflux-1,record,r3dvar,type)
        endif
      end do
!     2-D terms (it is called Vsink but it means nothing)
      do iflux=1,NumVSinkTerms
        if (wrtdiabioVSink_avg(iflux)) then
!     if (iflux.eq.11)  write(*,*) 'ZMEU WRT_AVG',bioVSink_avg(30,30,iflux)
          work2d=bioVSink_avg(:,:,iflux)
          call fillvalue2d(work2d, nciddiabio_avg, diabioVsink_avg(iflux),
     &         indxbioVSink+iflux-1,record,r2dvar,type)
        endif
      end do
#  endif
      
#else
      
!     Flux terms
      do iflux=1,NumFluxTerms
        if (wrtdiabioFlux_avg(iflux)) then
          workr=bioFlux_avg(:,:,:,iflux)
          call fillvalue3d(workr, nciddiabio_avg, diabioFlux_avg(iflux),
     &         indxbioFlux+iflux-1,record,r3dvar,type)
        endif
      end do
!     vertical sinking fluxes
      do iflux=1,NumVSinkTerms
        if (wrtdiabioVSink_avg(iflux)) then
          work=bioVsink_avg(:,:,:,iflux)
          call fillvalue3d(work, nciddiabio_avg, diabioVsink_avg(iflux),
     &         indxbioVSink+iflux-1,record,w3dvar,type)
        endif
      end do
#   if (defined BIO_NChlPZD && defined OXYGEN) || defined BIO_BioEBUS
!     gas exchange fluxes
      do iflux = 1, NumGasExcTerms
        if (wrtdiabioGasExc_avg(iflux)) then
          work2d=GasExcFlux_avg(:,:,iflux)
          call fillvalue2d(work2d,nciddiabio_avg,diabioGasExc_avg(iflux),
     &         indxGasExcFlux+iflux-1,record,r2dvar,type)
        endif
      end do
#   endif
      
#endif
 1    format(/1x,'WRT_BIO_DIAG_AVG ERROR while writing variable ''', A,
     &     ''' into diag_avg file.', /11x, 'Time record:',
     &     I6,3x,'netCDF error code',i4,3x,a,i4)
      goto 100 
 99   may_day_flag=3
 100  continue
      
!     
!     Synchronize netCDF file to disk to allow other processes
!     to access data immediately after it is written.
!     
      
#if defined MPI &  !defined PARALLEL_FILES
      ierr=nf_close (nciddiabio_avg)
      if (nrpfdiabio_avg.gt.0 .and. record.ge.nrpfdiabio_avg) 
     &     nciddiabio_avg=-1
#else
      if (nrpfdiabio_avg.gt.0 .and. record.ge.nrpfdiabio_avg) then
        ierr=nf_close (nciddiabio_avg)
        nciddiabio_avg=-1
      else
        ierr=nf_sync(nciddiabio_avg)
      endif
#endif
      if (ierr .eq. nf_noerr) then
      MPI_master_only   write(stdout,'(6x,A,2(A,I4,1x),A,I3)') 
     &       'WRT_BIO_DIAG_AVG -- wrote ',
     &       'diag_avg fields into time record =', record, '/',
     &       nrecdiabio_avg  MYID
      else
        write(stdout,'(/1x,2A/)') 'WRT_BIO_DIAG_AVG ERROR: Cannot ',
     &       'synchronize/close diag_avg netCDF file.'
        may_day_flag=3
      endif
      
#if defined MPI &               !defined PARALLEL_FILES
      if (mynode .lt. NNODES-1) then
        call MPI_Send (blank, 1, MPI_INTEGER, mynode+1,
     &       1, MPI_COMM_WORLD,  ierr)
      endif
#endif
      return
      end
      
#else
      subroutine wrt_bio_diag_avg_empty
      end
#endif /* DIAGNOSTICS_BIO && AVERAGES*/
      
