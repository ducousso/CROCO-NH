! $Id: nf_fread_y.F 1458 2014-02-03 15:01:25Z gcambon $
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
#ifdef WRITER
      integer function nf_fwrite_y (ncid, varid, type)
#else
      integer function nf_fread_y  (ncid, varid, type)
#include "cppdefs.h"
#endif
!
! Read/Write a floating point array from/into an input/output
! NetCDF file.
!
! Arguments: A       real array of standard horizontal dimensions
!                             which is to be read or written.
!            ncid    NetCDF ID of in the file.
!            varid   variable ID of that variable in NetCDF file.
!            record  record number. 
!            type    type of the grid (RHO-, U, V, W, PSI etc.)
!
! Because significant portion of the code calculates starting and
! stopping indices for the subarray (which are exactly the same for
! both read and write operations) the code has been unified to
! guarantee that both functions are exactly adjoint. 
!            
      implicit none
#include "param.h"
      real A(Mm+2)
      integer ncid,  type,   vert_type, imin, imax,  start,
     &        varid, ierr,  horiz_type, jmin, jmax,  count,
     &        j
#if defined MPI
# include "scalars.h"
#endif
#include "netcdf.inc"
                                      ! Decode grid type into
      vert_type=type/4                ! vertical and horizontal 
      horiz_type=type-4*vert_type     ! grid types, then calculate 
      jmin=horiz_type/2               ! starting indices indices
      imin=horiz_type-2*jmin          ! in horizontal directions. 

      start=1
#if defined AGRIF && defined AGRIF_ADAPTIVE && !defined WRITER
      if (.Not.Agrif_Root()) then
        start(1)=1+(Agrif_ix()-1)*Agrif_irhox()
        start(2)=1+(Agrif_iy()-1)*Agrif_irhoy()
      endif
#endif
     
#ifdef MPI
# ifdef PARALLEL_FILES
#  ifdef NS_PERIODIC
      jmin=1  
      jmax=Mm
#  else
      if (jj.gt.0) jmin=1
      if (jj.eq.NP_ETA-1) then
        jmax=Mmmpi+1
      else
        jmax=Mmmpi
      endif
#  endif
# else
      if (jj.gt.0) then
        start=1-jmin+jminmpi
        jmin=1
      endif
      if (jj.eq.NP_ETA-1) then
        jmax=Mmmpi+1
      else
        jmax=Mmmpi
      endif     
# endif
#else
      jmax=Mm+1
#endif
      count=jmax-jmin+1

!      write(stdout,'(1x,A,i4,1x,A,i2,1(3x,A,I2,2x,A,I3,2x,A,I3))')
!     &      'NF_READ/WRITE: mynode=',mynode,'horiz_grid',horiz_type,
!     &                         'jj=',jj, 'jmin=',jmin, 'jmax=',jmax 

      do j=1,Mm+2
         A(j) = j + start - 1 + 0.25*horiz_type
      enddo

!
! Read/Write array from the disk.
!===== ===== ===== ==== === =====
!
#ifndef WRITER
      ierr=nf_get_vara_FTYPE (ncid, varid, start, count, A(1))
      nf_fread_y=ierr
      if (ierr .ne. nf_noerr) then
        write(*,'(/1x,2A,I5,1x,A,I4/)') 'NF_FREAD_Y ERROR: ',
     &             'nf_get_vara netCDF error code =', ierr
     &              MYID
        return
      endif
#else
      ierr=nf_put_vara_FTYPE (ncid, varid, start, count, A(1))
      nf_fwrite_y=ierr
      if (ierr.ne.nf_noerr) then
        write(*,'(/1x,2A,I5,1x,A,I4/)') 'NF_FWRITE_Y ERROR: ',
     &              'nf_put_vara netCDF error code =', ierr
     &               MYID
      endif
#endif

      return
      end

#ifndef WRITER
# define WRITER
# include "nf_fread_y.F"
#endif
