! $Id: wrt_diagsM_avg.F 1458 2014-02-03 15:01:25Z gcambon $
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
#if defined DIAGNOSTICS_UV && defined AVERAGES
!
!---------------------------------------------------------------
! Write time-averaged diags flux fields into averages netCDF file
! Writes requested model fields at requested levels into
! diagnostics netCDF file.
!---------------------------------------------------------------
!
      subroutine wrt_diagsM_avg 

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
#ifdef SEDIMENT
# include "sediment.h"
#endif
#ifdef BBL
# include "bbl.h"
#endif
#ifdef SOLVE3D
      integer tile, itrc,i,j,k,ivar
# ifdef SEDIMENT
     &      , indxWrk
# endif
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
# undef DEBUG
!
!
! Create/open diagnostic file; write grid arrays, if so needed.
!
      call def_diagsM_avg (nciddiaM_avg, nrecdiaM_avg, ierr)
      if (ierr .ne. nf_noerr) goto 99
      lstr=lenstr(dianameM_avg)
!                                          !!! WARNING: Once time  
! Set record within the file.              !!! stepping has been 
!                                          !!! started, it is assumed
      nrecdiaM_avg=max(nrecdiaM_avg,1)     !!! that global history
      if (nrpfdiaM_avg.eq.0) then          !!! record index is
        record=nrecdiaM_avg                !!! advanced by main.  
      else
        record=1+mod(nrecdiaM_avg-1, nrpfdiaM_avg)
      endif
!
! Write out evolving model variables:
! ----- --- -------- ----- ----------
!
! Time step number and record numbers.
!
      type=filetype_diaM_avg
!
      ibuff(1)=iic
      ibuff(2)=nrecrst
      ibuff(3)=nrechis
      ibuff(4)=nrecdiaM_avg
      start(1)=1
      start(2)=record
      count(1)=4
      count(2)=1
      ierr=nf_put_vara_int (nciddiaM_avg, diaTstepM_avg, 
     &                                             start, count, ibuff)
      if (ierr .ne. nf_noerr) then
        write(stdout,1) 'time_step_avg', record, ierr
     &                  MYID
        goto 99                                           !--> ERROR
      endif
!
! Averaged diag Time
!
      ierr=nf_put_var1_FTYPE (nciddiaM_avg, diaTimeM_avg, record,
     &                                              timediaM_avg)
      if (ierr .ne. nf_noerr) then
        lvar=lenstr(vname(1,indxTime))
        write(stdout,1) vname(1,indxTime)(1:lvar), record, ierr
     &                  MYID
        goto 99                                           !--> ERROR
      endif

!---------------------------------------------------------------
! Momentum diagnostic variables.
!---------------------------------------------------------------
!
!
      do itrc=1,2
        if (wrtdiaM_avg(itrc)) then
!
          if (itrc.eq.1) then
            ivar=u3dvar
          else
            ivar=v3dvar
          endif
!
! indxMXadv
!
           workr=MXadv_avg(:,:,:,itrc)
           call fillvalue3d(workr,nciddiaM_avg,diaMXadv_avg(itrc),
     &                      indxMXadv+itrc-1,
     &                      record,ivar,type)  
!
! indxMYadv
!
           workr=MYadv_avg(:,:,:,itrc)
           call fillvalue3d(workr,nciddiaM_avg,diaMYadv_avg(itrc),
     &                      indxMYadv+itrc-1,
     &                      record,ivar,type)  
!
! indxMVadv
!
           workr=MVadv_avg(:,:,:,itrc)
           call fillvalue3d(workr,nciddiaM_avg,diaMVadv_avg(itrc),
     &                      indxMVadv+itrc-1,
     &                      record,ivar,type)  
!
! indxMCor
!
           workr=MCor_avg(:,:,:,itrc)
           call fillvalue3d(workr,nciddiaM_avg,diaMCor_avg(itrc),
     &                      indxMCor+itrc-1,
     &                      record,ivar,type)  
!
! indxMPrsgrd
!
           workr=MPrsgrd_avg(:,:,:,itrc)
           call fillvalue3d(workr,nciddiaM_avg,diaMPrsgrd_avg(itrc),
     &                      indxMPrsgrd+itrc-1,
     &                      record,ivar,type)  
!
! indxMHmix
!
           workr=MHmix_avg(:,:,:,itrc)
           call fillvalue3d(workr,nciddiaM_avg,diaMHmix_avg(itrc),
     &                      indxMHmix+itrc-1,
     &                      record,ivar,type)  
!
! indxMVmix
!
           workr=MVmix_avg(:,:,:,itrc)
           call fillvalue3d(workr,nciddiaM_avg,diaMVmix_avg(itrc),
     &                      indxMVmix+itrc-1,
     &                      record,ivar,type)  
#ifdef MRL_WCI
!
! indxMvf
!
           workr=Mvf_avg(:,:,:,itrc)
           call fillvalue3d(workr,nciddiaM_avg,diaMvf_avg(itrc),
     &                      indxMvf+itrc-1,
     &                      record,ivar,type)  
!
! indxMbrk
!
           workr=Mbrk_avg(:,:,:,itrc)
           call fillvalue3d(workr,nciddiaM_avg,diaMbrk_avg(itrc),
     &                      indxMbrk+itrc-1,
     &                      record,ivar,type)  
!
! indxMStCo
!
           workr=MStCo_avg(:,:,:,itrc)
           call fillvalue3d(workr,nciddiaM_avg,diaMStCo_avg(itrc),
     &                      indxMStCo+itrc-1,
     &                      record,ivar,type)  
!
! indxMVvf
!
           workr=MVvf_avg(:,:,:,itrc)
           call fillvalue3d(workr,nciddiaM_avg,diaMVvf_avg(itrc),
     &                      indxMVvf+itrc-1,
     &                      record,ivar,type)  
!
! indxMPrscrt
!
           workr=MPrscrt_avg(:,:,:,itrc)
           call fillvalue3d(workr,nciddiaM_avg,diaMPrscrt_avg(itrc),
     &                      indxMPrscrt+itrc-1,
     &                      record,ivar,type)  
!
! indxMsbk
!
           workr=Msbk_avg(:,:,:,itrc)
           call fillvalue3d(workr,nciddiaM_avg,diaMsbk_avg(itrc),
     &                      indxMsbk+itrc-1,
     &                      record,ivar,type)  
!
! indxMbwf
!
           workr=Mbwf_avg(:,:,:,itrc)
           call fillvalue3d(workr,nciddiaM_avg,diaMbwf_avg(itrc),
     &                      indxMbwf+itrc-1,
     &                      record,ivar,type)  
!
! indxMfrc
!
           workr=Mfrc_avg(:,:,:,itrc)
           call fillvalue3d(workr,nciddiaM_avg,diaMfrc_avg(itrc),
     &                      indxMfrc+itrc-1,
     &                      record,ivar,type)  
#endif
!
! indxMrate
!
           workr=Mrate_avg(:,:,:,itrc)
           call fillvalue3d(workr,nciddiaM_avg,diaMrate_avg(itrc),
     &                      indxMrate+itrc-1,
     &                      record,ivar,type)  
!
      
        endif
      enddo
!
!--#define DEBUG
#  ifdef DEBUG
      i=20
      j=20
      k=10
      itrc=1
      write(*,*) 'Write diag UV avg at : i='
     &                   ,i,' j=',j,' k=',k,' iUV=',itrc
      write(*,*) 'MXadv_avg(i,j,k,itrc)   ',MXadv_avg(i,j,k,itrc)
      write(*,*) 'MYadv_avg(i,j,k,itrc)   ',MYadv_avg(i,j,k,itrc)
      write(*,*) 'MVadv_avg(i,j,k,itrc)   ',MVadv_avg(i,j,k,itrc)
      write(*,*) 'MCor_avg(i,j,k,itrc)    ',MCor_avg(i,j,k,itrc)
      write(*,*) 'MPrsgrd_avg(i,j,k,itrc) ',MPrsgrd_avg(i,j,k,itrc)
      write(*,*) 'MHmix_avg(i,j,k,itrc)   ',MHmix_avg(i,j,k,itrc)
      write(*,*) 'MVmix_avg(i,j,k,itrc)   ',MVmix_avg(i,j,k,itrc)
# ifdef MRL_WCI
      write(*,*) 'Mvf_avg(i,j,k,itrc)     ',Mvf_avg(i,j,k,itrc)
      write(*,*) 'Mbrk_avg(i,j,k,itrc)    ',Mbrk_avg(i,j,k,itrc)
      write(*,*) 'MStCo_avg(i,j,k,itrc)   ',MStCo_avg(i,j,k,itrc)
      write(*,*) 'MVvf_avg(i,j,k,itrc)    ',MVvf_avg(i,j,k,itrc)
      write(*,*) 'MPrscrt_avg(i,j,k,itrc) ',MPrscrt_avg(i,j,k,itrc)
      write(*,*) 'Msbk_avg(i,j,k,itrc)    ',Msbk_avg(i,j,k,itrc)
      write(*,*) 'Mbwf_avg(i,j,k,itrc)    ',Mbwf_avg(i,j,k,itrc)
      write(*,*) 'Mfrc_avg(i,j,k,itrc)    ',Mfrc_avg(i,j,k,itrc)
# endif
      write(*,*) 'Mrate_avg(i,j,k,itrc)   ',Mrate_avg(i,j,k,itrc)

      write(*,*) 'SumUVavg(i,j,k,itrc) = ',MXadv_avg(i,j,k,itrc)
     &                                  +  MYadv_avg(i,j,k,itrc)
     &                                  +  MVadv_avg(i,j,k,itrc)
     &                                  +  MCor_avg(i,j,k,itrc)
     &                                  +  MPrsgrd_avg(i,j,k,itrc)
     &                                  +  MHmix_avg(i,j,k,itrc)
     &                                  +  MVmix_avg(i,j,k,itrc)
# ifdef MRL_WCI
     &                                  +  Mvf_avg(i,j,k,itrc)
     &                                  +  Mbrk_avg(i,j,k,itrc)
     &                                  +  MStCo_avg(i,j,k,itrc)
     &                                  +  MVvf_avg(i,j,k,itrc)
     &                                  +  MPrscrt_avg(i,j,k,itrc)
     &                                  +  Msbk_avg(i,j,k,itrc)
     &                                  +  Mbwf_avg(i,j,k,itrc)
     &                                  +  Mfrc_avg(i,j,k,itrc)
# endif
     &                                  -  Mrate_avg(i,j,k,itrc)
              write(*,*) '----------------------------------'
#  endif /* DEBUG */
!--# undef DEBUG

  1   format(/1x,'WRT_DIAG_AVG ERROR while writing variable ''', A,
     &          ''' into diag_avg file.', /11x, 'Time record:',
     &                   I6,3x,'netCDF error code',i4,3x,a,i4)
      goto 100 
  99  may_day_flag=3
 100  continue

!
!---------------------------------------------------------------
! Synchronize netCDF file to disk to allow other processes
! to access data immediately after it is written.
!---------------------------------------------------------------
!
#if defined MPI & !defined PARALLEL_FILES
      ierr=nf_close (nciddiaM_avg)
      if (nrpfdiaM_avg.gt.0 .and. record.ge.nrpfdiaM_avg) nciddiaM_avg=-1
#else
      if (nrpfdiaM_avg.gt.0 .and. record.ge.nrpfdiaM_avg) then
        ierr=nf_close (nciddiaM_avg)
        nciddiaM_avg=-1
      else
        ierr=nf_sync(nciddiaM_avg)
      endif
#endif


      if (ierr .eq. nf_noerr) then
       MPI_master_only write(stdout,'(6x,A,2(A,I4,1x),A,I3)') 
     &            'WRT_DIAGM_AVG -- wrote ',
     &            'diagM_avg fields into time record =', record, '/',
     &             nrecdiaM_avg  MYID
      else
        MPI_master_only write(stdout,'(/1x,2A/)') 
     &             'WRT_DIAGM_AVG ERROR: Cannot ',
     &             'synchronize/close diag_avg netCDF file.'
        may_day_flag=3
      endif

#if defined MPI & !defined PARALLEL_FILES
      if (mynode .lt. NNODES-1) then
        call MPI_Send (blank, 1, MPI_INTEGER, mynode+1,
     &                        1, MPI_COMM_WORLD,  ierr)
      endif
#endif
#undef DEBUG
      return
      end

#else
      subroutine wrt_diagsM_avg_empty
      end
#endif /* (DIAGNOSTICS_UV) && AVERAGES */
