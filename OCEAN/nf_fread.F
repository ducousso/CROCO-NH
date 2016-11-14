! $Id: nf_fread.F 1466 2014-02-06 17:37:07Z marchesiello $
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
      integer function nf_fwrite (A, ncid, varid, record, type)
#else
      integer function nf_fread  (A, ncid, varid, record, type)
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
      real A(GLOBAL_2D_ARRAY,N+1)
      integer ncid,  type,   vert_type,  imin, imax,  start(4)
     &      , varid, record, horiz_type, jmin, jmax,  count(4) 
     &      , i,j,k, shift,  ierr
#include "buffer.h"
#if defined MPI
# include "scalars.h"
#endif
#include "netcdf.inc"
                                      ! Decode grid type into
      vert_type=type/4                ! vertical and horizontal 
      horiz_type=type-4*vert_type     ! grid types, then calculate 
      jmin=horiz_type/2               ! starting indices indices
      imin=horiz_type-2*jmin          ! in horizontal directions. 

      start(1)=1
      start(2)=1
#if defined AGRIF && defined AGRIF_ADAPTIVE && !defined WRITER
      if (.Not.Agrif_Root()) then
        start(1)=1+(Agrif_ix()-1)*Agrif_irhox()
        start(2)=1+(Agrif_iy()-1)*Agrif_irhoy()
      endif
#endif
     
#ifdef MPI
# ifdef PARALLEL_FILES
!#  ifdef EW_PERIODIC
!      imin=1 
!      imax=Lm
!#  else
      if (ii.gt.0) imin=1
      if (ii.eq.NP_XI-1) then
        imax=Lmmpi+1
      else
        imax=Lmmpi
      endif
!#  endif
!#  ifdef NS_PERIODIC
!      jmin=1  
!      jmax=Mm
!#  else
      if (jj.gt.0) jmin=1
      if (jj.eq.NP_ETA-1) then
        jmax=Mmmpi+1
      else
        jmax=Mmmpi
      endif
!#  endif
# else
      if (ii.gt.0) then
        start(1)=1-imin+iminmpi
        imin=1
      endif
      if (ii.eq.NP_XI-1) then
        imax=Lmmpi+1
      else
        imax=Lmmpi
      endif
      if (jj.gt.0) then
        start(2)=1-jmin+jminmpi
        jmin=1
      endif
      if (jj.eq.NP_ETA-1) then
        jmax=Mmmpi+1
      else
        jmax=Mmmpi
      endif     
# endif
#else
      imax=Lm+1
      jmax=Mm+1
#endif
      count(1)=imax-imin+1
      count(2)=jmax-jmin+1

c**   write(stdout,'(1x,A,i4,1x,A,i2,2(3x,A,I2,2x,A,I3,2x,A,I3))')
c**  &      'NF_READ/WRITE: mynode=',mynode,'horiz_grid',horiz_type,
c**  &                         'ii=',ii, 'imin=',imin, 'imax=',imax,
C**  &                         'jj=',jj, 'jmin=',jmin, 'jmax=',jmax 


      if (vert_type.eq.0) then    ! Sort out vertical grids.
        count(3)=1                !--> 2D variables
        start(3)=record
      elseif (vert_type.eq.1) then
        count(3)=N
        count(4)=1                !--> 3D RHO-grid
        start(3)=1
        start(4)=record
      elseif (vert_type.eq.2) then
        count(3)=N+1
        count(4)=1                !--> 3D W-grid
        start(3)=1
        start(4)=record
#ifdef SEDIMENT
      elseif (vert_type.eq.3) then
        count(3)=NLAY
        count(4)=1                !--> 3D BED-grid
        start(3)=1
        start(4)=record
#endif
      else
#ifndef WRITER
        write(*,'(/1x,2A,I3/)') 'NF_FREAD ERROR: ',  
     &                    'illegal grid type', type
        nf_fread=nf_noerr+1
#else
        write(*,'(/1x,2A,I4/)') 'NF_FWRITE ERROR: ',
     &                    'illegal grid type', type
        nf_fwrite=nf_noerr+1
#endif
        return
      endif
      
!
! Read/Write array from the disk.
!===== ===== ===== ==== === =====
!
#ifndef WRITER
      ierr=nf_get_vara_FTYPE (ncid, varid, start, count, buff(1))
      nf_fread=ierr
      if (ierr .ne. nf_noerr) then
        write(*,'(/1x,2A,I5,1x,A,I4/)') 'NF_FREAD ERROR: ',
     &             'nf_get_vara netCDF error code =', ierr
     &              MYID
        return
      endif
#endif
      do k=1,count(3)
        do j=jmin,jmax
          shift=1-imin+count(1)*(j-jmin+(k-1)*count(2))
          do i=imin,imax
#ifdef WRITER
            buff(i+shift)=A(i,j,k)
#else
            A(i,j,k)=buff(i+shift)
#endif
          enddo
        enddo
      enddo
#ifdef WRITER
      ierr=nf_put_vara_FTYPE (ncid, varid, start, count, buff(1))
      nf_fwrite=ierr
      if (ierr.ne.nf_noerr) then
        write(*,'(/1x,2A,I5,1x,A,I4/)') 'NF_FWRITE ERROR: ',
     &              'nf_put_vara netCDF error code =', ierr
     &               MYID
      endif
#endif

!
! Exchange periodic and computational margins (reader only).
!
#ifdef MPI
#define LOCALLM Lmmpi
#define LOCALMM Mmmpi
#else
#define LOCALLM Lm
#define LOCALMM Mm
#endif
#ifndef WRITER
# if defined EW_PERIODIC || defined NS_PERIODIC  || defined MPI
      if (horiz_type.eq.0 .and. vert_type.eq.0) then
        call exchange_r2d_tile (1,LOCALLM,1,LOCALMM, A)
      elseif (horiz_type.eq.1 .and. vert_type.eq.0) then
        call exchange_u2d_tile (1,LOCALLM,1,LOCALMM, A)
      elseif (horiz_type.eq.2 .and. vert_type.eq.0) then
        call exchange_v2d_tile (1,LOCALLM,1,LOCALMM, A)
      elseif (horiz_type.eq.3 .and. vert_type.eq.0) then
        call exchange_p2d_tile (1,LOCALLM,1,LOCALMM, A)
#  ifdef SOLVE3D
      elseif (horiz_type.eq.0 .and. vert_type.eq.1) then
#   ifdef THREE_GHOST_POINTS_TS
        call exchange_r3d_3pts_tile (1,LOCALLM,1,LOCALMM, A)
#   else
        call exchange_r3d_tile (1,LOCALLM,1,LOCALMM, A)
#   endif
      elseif (horiz_type.eq.1 .and. vert_type.eq.1) then
        call exchange_u3d_tile (1,LOCALLM,1,LOCALMM, A)
      elseif (horiz_type.eq.2 .and. vert_type.eq.1) then
        call exchange_v3d_tile (1,LOCALLM,1,LOCALMM, A)
      elseif (horiz_type.eq.3 .and. vert_type.eq.1) then
        call exchange_p3d_tile (1,LOCALLM,1,LOCALMM, A)

      elseif (horiz_type.eq.0 .and. vert_type.eq.2) then
        call exchange_w3d_tile (1,LOCALLM,1,LOCALMM, A)

CC      elseif (horiz_type.eq.0 .and. vert_type.eq.3) then
CC        call exchange_b3d_tile (1,LOCALLM,1,LOCALMM, A)
#  endif
      endif
# endif
#endif
      return
      end

#ifndef WRITER
# define WRITER
# include "nf_fread.F"
#endif
