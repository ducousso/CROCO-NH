! $Id: nf_fread_x.F 1458 2014-02-03 15:01:25Z gcambon $
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
      integer function nf_fwrite_x (ncid, varid, type)
#else
      integer function nf_fread_x  (ncid, varid, type)
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
!            type    type of the grid (RHO-, U, V, W, PSI etc.)
!
! Because significant portion of the code calculates starting and
! stopping indices for the subarray (which are exactly the same for
! both read and write operations) the code ha      real xi_rho_vec(1:Lm+2),
! guarantee that both functions are exactly adjoint. 
!            
      implicit none
#include "param.h"
      real A(Lm+2)
      integer ncid,  type,   vert_type, imin, imax,  start,
     &        varid, ierr,  horiz_type, jmin, jmax,  count,
     &        i 
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
        start=1+(Agrif_ix()-1)*Agrif_irhox()
      endif
#endif
     
#ifdef MPI
# ifdef PARALLEL_FILES
#  ifdef EW_PERIODIC
      imin=1 
      imax=Lm
#  else
      if (ii.gt.0) imin=1
      if (ii.eq.NP_XI-1) then
        imax=Lmmpi+1
      else
        imax=Lmmpi
      endif
#  endif
# else
      if (ii.gt.0) then
        start=1-imin+iminmpi
        imin=1
      endif
      if (ii.eq.NP_XI-1) then
        imax=Lmmpi+1
      else
        imax=Lmmpi
      endif
# endif
#else
      imax=Lm+1
#endif
      count=imax-imin+1

!      write(stdout,'(1x,A,i4,1x,A,i2,1(3x,A,I2,2x,A,I3,2x,A,I3))')
!     &      'NF_READ/WRITE: mynode=',mynode,'horiz_grid',horiz_type,
!     &                         'ii=',ii, 'imin=',imin, 'imax=',imax

      do i=1,Lm+2
         A(i) = i + start - 1 + 0.5*horiz_type
      enddo

!
! Read/Write array from the disk.
!===== ===== ===== ==== === =====
!
#ifndef WRITER
      ierr=nf_get_vara_FTYPE (ncid, varid, start, count, A)
      nf_fread_x=ierr
      if (ierr .ne. nf_noerr) then
        write(*,'(/1x,2A,I5,1x,A,I4/)') 'NF_FREAD_X ERROR: ',
     &             'nf_get_vara netCDF error code =', ierr
     &              MYID
        return
      endif
#else
      ierr=nf_put_vara_FTYPE (ncid, varid, start, count, A)
!      print *,'000000000000000'
      nf_fwrite_x=ierr
      if (ierr.ne.nf_noerr) then
        write(*,'(/1x,2A,I5,1x,A,I4/)') 'NF_FWRITE_X ERROR: ',
     &              'nf_put_vara netCDF error code =', ierr
     &               MYID
      endif
#endif

      return
      end

#ifndef WRITER
# define WRITER
# include "nf_fread_x.F"
#endif
