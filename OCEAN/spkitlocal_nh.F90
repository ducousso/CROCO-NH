#include "cppdefs.h"
#ifdef NBQ
!------------------------------------------------------------------------------
!                               NHOMS
!                Non Hydrostatic Ocean Modeling System      
!------------------------------------------------------------------------------
!
!> @note <a href="http://poc.obs-mip.fr/auclair/WOcean.fr/SNH/index_snh_home.htm"> Main web documentation </a>
!
! DESCRIPTION: 
!
!> @brief <a href="http://poc.obs-mip.fr/auclair/WOcean.fr/SNH/Restricted/NH-NBQ/Sources/Images/png/Algebrique_Gestion.png">
!! Sparskit library
!! </a> : Compressed Sparse Row format (CSR).
!> @details Additional documentation:
!! - <a href="http://www-users.cs.umn.edu/~saad/software/SPARSKIT/paper.ps">Youcef Saad</a>
!! - <a href="http://people.sc.fsu.edu/~jburkardt/f_src/sparsekit/sparsekit.html">Sparskit</a>
! REVISION HISTORY:
!
!> @authors
!> @date 2015 January
!> @todo
!
!------------------------------------------------------------------------------
subroutine amubdg ( nrow, ncol, ncolb, ja, ia, jb, ib, ndegr, nnz, iw )

!*****************************************************************************80
!
!> AMUBDG gets the number of nonzero elements in each row of A * B.
!
!> Discussion:
!!
!!   The routine also computes the total number of nonzero elements in A * B.
!!
!!   Method: A' * A = sum [over i = 1, nrow]  a(i)^T a(i)
!!   where a(i) = i-th row of  A.  We must be careful not to add  the
!!   elements already accounted for.
!
!> Modified:
!!
!!   07 January 2004
!
!> Author:
!!
!!   Youcef Saad
!
!> Parameters:
!!
!!   Input, integer NROW, the row dimension of the matrix A.
!!
!!   Input, integer NCOL, the column dimension of the matrix A,
!!   (and the row dimension of B).
!!
!!   Input, integer NCOLB, the column dimension of the matrix B.
!!
!!   Input, ja, ia= row structure of input matrix A: ja = column indices of
!!   the nonzero elements of A stored by rows.
!!   ia = pointer to beginning of each row in ja.
!!
!!   Input, jb, ib, the row structure of input matrix B: jb = column indices of
!!   the nonzero elements of A stored by rows.
!!   ib is a pointer to beginning of each row in jb.
!!
!!   Output, integer NDEGR(NROW), contains the degrees (the number of
!!   nonzeros in each row of the matrix A * B.
!!
!!   Output, integer NNZ, the number of nonzero elements found in A * B.
!!
!!  Workspace, integer IW(NCOLB).
!!
  implicit none

  integer ncol
  integer ncolb
  integer nrow

  integer ia(*)
  integer ib(*)
  integer ii
  integer iw(*)
  integer j
  integer ja(*)
  integer jb(*)
  integer jc
  integer jr
  integer k
  integer last
  integer ldg
  integer ndegr(*)
  integer nnz

  iw(1:ncolb) = 0
  ndegr(1:nrow) = 0

  do ii = 1, nrow
!
!  For each row of A.
!
    ldg = 0
!
!  End-of-linked list.
!
    last = -1

    do j = ia(ii), ia(ii+1)-1
!
!  Row number to be added.
!
        jr = ja(j)

        do k = ib(jr), ib(jr+1)-1
           jc = jb(k)
!
!  Add one element to the linked list.
!
           if ( iw(jc) == 0 ) then
              ldg = ldg + 1
              iw(jc) = last
              last = jc
           end if

         end do

    end do

    ndegr(ii) = ldg
!
!  Reset IW to zero.
!
    do k = 1, ldg
      j = iw(last)
      iw(last) = 0
      last = j
     end do

  end do

  nnz = sum ( ndegr(1:nrow) )

  return
end
subroutine amux ( n, x, y, a, ja, ia )
!subroutine amux ( n, x, y, a, ja, nl )

!*****************************************************************************80
!
!! AMUX multiplies a CSR matrix A times a vector.
!
!  Discussion:
!
!    This routine multiplies a matrix by a vector using the dot product form.
!    Matrix A is stored in compressed sparse row storage.
!
!  Modified:
!
!    07 January 2004
!
!  Author:
!
!    Youcef Saad
! 
!  Parameters:
!
!    Input, integer N, the row dimension of the matrix.
!
!    Input, real X(*), and array of length equal to the column dimension
!    of A.
!
!    Input, real A(*), integer JA(*), IA(NROW+1), the matrix in CSR
!    Compressed Sparse Row format.
!
!    Output, real Y(N), the product A * X.
!

!  use module_parallele                                           ! #MPI#

  use module_nh
  implicit none
! include 'mkl_spblas.h'
  integer n

  real ( kind = 8 ) a(*)
  integer i,j
  integer ia(*)
  integer ja(*)
  integer k
! integer nl
! character(len=1) :: TRANS='N'
  real ( kind = 8 ) t
  real ( kind = 8 ) x(*)
  real ( kind = 8 ) y(*)

  do i = 1, n
!
!  Compute the inner product of row I with vector X.
!
    t = 0.0D+00
     do k = ia(i), ia(i+1)-1
!    do k = 1 + nl*(i-1), nl*i
       t = t + a(k) * x(ja(k))
     end do
     y(i) = t
  end do

  return
end

!*****************************************************************************80
!                amux pour MOM
!*****************************************************************************80
subroutine amux_s ( n , n1, x, y, a, ja, ia )

!  use module_parallele                                           ! #MPI#

  use module_nh
  implicit none
! include 'mkl_spblas.h'
  integer n,n1

  real ( kind = 8 ) a(*)
  integer i
  integer ia(*)
  integer ja(*)
  integer k
  character(len=1) :: TRANS='N'
  real ( kind = 8 ) t
  real ( kind = 8 ) x(*)
  real ( kind = 8 ) y(*)
  double precision :: cput1, cput2

  !cput1=mpi_wtime()
  
  do i = 1, n1
    t = 0.0D+00
    do k = ia(i), ia(i+1)-1
      t = t + a(k) * x(ja(k))
    end do
    y(i) = t
  end do

  do i = n1+1, n
    y(i) = a(ia(i)) * x(ja(ia(i))) + a(ia(i)+1) * x(ja(ia(i)+1)) 
  end do


  return
  end
subroutine amub2 ( nrow, ncol, job, a, ja, ia, b, jb, ib, c, jc, ic, nzmax, &
  iw, ierr )

!*****************************************************************************
!
!! AMUB performs the matrix product C = A * B.
!
!  Discussion:
!
!    The column dimension of B is not needed.
!
!  Modified:
!
!    08 January 2004
!
!  Author:
!
!    Youcef Saad
!
!  Parameters:
!
!    Input, integer NROW, the row dimension of the matrix.
!
!    Input, integer NCOL, the column dimension of the matrix.
!
!    Input, integer JOB, job indicator.  When JOB = 0, only the structure
!    is computed, that is, the arrays JC and IC, but the real values
!    are ignored.
!
!    Input, real A(*), integer JA(*), IA(NROW+1), the matrix in CSR
!    Compressed Sparse Row format.
!
!    Input, b, jb, ib, matrix B in compressed sparse row format.
!
!    Input, integer NZMAX, the length of the arrays c and jc.
!    The routine will stop if the result matrix C  has a number
!    of elements that exceeds exceeds NZMAX.
!
! on return:
!
! c,
! jc,
! ic    = resulting matrix C in compressed sparse row sparse format.
!
! ierr      = integer. serving as error message.
!         ierr = 0 means normal return,
!         ierr > 0 means that amub stopped while computing the
!         i-th row  of C with i = ierr, because the number
!         of elements in C exceeds nzmax.
!
! work arrays:
!
!  iw      = integer work array of length equal to the number of
!         columns in A.
!
  implicit none

  integer ncol
  integer nrow
  integer nzmax

  real ( kind = 8 ) a(*)
  real ( kind = 8 ) b(*)
  real ( kind = 8 ) c(nzmax)
!  integer ia(nrow+1)
!  integer ib(ncol+1)
!  integer ic(ncol+1)
  integer ia(*)
  integer ib(*)
  integer ic(*)
  integer ierr
  integer ii
  integer iw(*)
  integer ja(*)
  integer jb(*)
  integer jc(nzmax)
  integer jcol
  integer jj
  integer job
  integer jpos
  integer k
  integer ka
  integer kb
  integer len
  real ( kind = 8 ) scal
  logical values

  values = ( job /= 0 )
  len = 0
  ic(1) = 1
  ierr = 0
!
!  Initialize IW.
!
   iw(1:ncol) = 0

   do ii = 1, nrow
!
!  Row I.
!
    do ka = ia(ii), ia(ii+1)-1

      if ( values ) then
        scal = a(ka)
      end if

      jj = ja(ka)

      do kb = ib(jj), ib(jj+1)-1

           jcol = jb(kb)
           jpos = iw(jcol)

           if ( jpos == 0 ) then
              len = len + 1
              if ( nzmax < len ) then
                 ierr = ii
                 return
              end if
              jc(len) = jcol
              iw(jcol)= len
              if ( values ) then
                c(len) = scal * b(kb)
              end if
           else
              if ( values ) then
                c(jpos) = c(jpos) + scal * b(kb)
              end if
           end if

         end do

    end do

    do k = ic(ii), len
      iw(jc(k)) = 0
    end do

    ic(ii+1) = len + 1

  end do

  return
  end

#endif

