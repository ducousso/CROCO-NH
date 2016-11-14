! $Id: compute_starts_counts.h 1458 2014-02-03 15:01:25Z gcambon $
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
! Auxiliary module "compute_starts_counts.h":
!-------------------------------------------------------
!Given two input variables, "type" and "record", which specify grid
!type of the field to be read/written from/to netCDF file, and record
!number (if there is an unlimited dimension for that field in the
!netCDF file), compute "start" and "count" arrays which contain
!starting indices and proper counts corresponding to the field data
!stored in the file.
!
! This module supports three different policies of reading/writing
!the data: (1) in the case of shared memory code the data is always
!written by single CPU into a single file, so that start/count are
!basically determined by the dimensions of arrays as it is written
!in the file;  (2) in the case of MPI code the data can still be
!written into a single file, or (3) multiple files containing data
!individually for each MPI-node (this option is activated by
!CPP-switch PARALLEL_FILES); In the case of MPI-single-file mode each
!MPI-node (one at-a-time) writes a rectangular block of array, so that
!"starts" depend on the position of node ii,jj on the processor grid,
!while "counts" corresponds to the size of MPI subdomains.  In the
!case of PARALLEL_FILES "starts" corresponding to horizontal
!dimensions are both equal to 1, "counts" correspond to subdomains
!with physical boundaries kept, but MPI-halos striped out.
!
! Additionally, this module computes ranges "imin,imax,jmin,jmax"
!which define starting/ending indices of the portion of model array
!to be written into the file [hence in all cases count(1)=imax-imin+1,
!and count(2)=jmax-jmin+1].  This is necessary because model arrays
!have slightly different shapes than the corresponding arrays in the
!netCDF files.  The differences are due to index shifting (netCDF
!array index must always start from 1, while Fortran does not);
!Fortran array dimension padding; and stripping periodic/computational
!margins. 

      integer imin,imax,jmin,jmax, start(4), count(4),
     &                            vert_type, horiz_type

      ierr=0            ! These are default settings. In all cases
      do i=1,4          ! start,count(1:2) correspond to XI- and
        start(i)=1      ! ETA-dimensions, while index 3 is for either
        count(i)=1      ! vertical dimension (if any) or time record
      enddo             ! (2D-fields); 4 is for time record only

      vert_type=type/4              ! Decode grid type into vertical
      horiz_type=type-4*vert_type   ! and horizontal grid types, then
      jmin=horiz_type/2             ! calculate starting indices in
      imin=horiz_type-2*jmin        ! horizontal directions.

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

      if (vert_type.eq.0) then        ! Sort out vertical grids:
        start(3)=record                  !<-- 2D field variables
      elseif (vert_type.eq.1) then
        count(3)=N                       !<-- 3D RHO-grid
        start(4)=record
      elseif (vert_type.eq.2) then
        count(3)=N+1                     !<-- 3D W-grid
        start(4)=record
      else
        ierr=ierr+1
      endif
