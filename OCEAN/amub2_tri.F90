#include "cppdefs.h"
#ifdef NBQ
!
!======================================================================
!                      NBQ-Mode for NH-modeling
!                            Main Routine
!======================================================================
!
!> @note Main web documentation:
!  http://poc.obs-mip.fr/auclair/WOcean.fr/SNH/index_snh_home.htm
!
! DESCRIPTION: 
!
!> @brief SNBQ driver : Non-hydrostatic algorithm with the 
!                       Non-boussinesq solver.
!
!> @details NBQ time step. See SNBQ web pages (step3d_nbq.F90) 
! REVISION HISTORY:
!
!> @authors
!> @date 2015 October
!> @todo
!
!======================================================================
!

subroutine amub2_tri ( nrow, ncol, job, a, ja, ia, b, jb, ib)

!**********************************************************************
!
!     AMUB performs the matrix product C = A * B.
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
! ierr_a      = integer. serving as error message.
!         ierr_a = 0 means normal return,
!         ierr_a > 0 means that amub stopped while computing the
!         i-th row  of C with i = ierr_a, because the number
!         of elements in C exceeds nzmax.
!
!
!**********************************************************************
!
!      use module_principal  
!      use module_parallele
      use module_nh 
      use module_nbq
!      use module_exp 

  implicit none

# include "param_F90.h"
# include "scalars_F90.h"
# include "ocean3d.h"
# include "grid.h"
# include "nbq.h"

  integer ncol
  integer nrow
  integer nzmax

  real ( kind = 8 ) a(*)
  real ( kind = 8 ) b(*)
  integer k
  integer ia(*)
  integer ib(*)
  integer ierr_a
  integer iii
  integer ja(*)
  integer jb(*)
  integer jcol
  integer jjj
  integer job
  integer jpos
!  integer k
  integer ka
  integer kb
  real ( kind = 8 ) scal
  logical values

  pdv_nbq(1:nrow)=1.d0
  plv_nbq(1:nrow)=0.d0
  puv_nbq(1:nrow)=0.d0

   do iii = 1, nrow
    do ka = ia(iii), ia(iii+1)-1
      scal = a(ka)
      jjj = ja(ka)
      do kb = ib(jjj), ib(jjj+1)-1
         jcol = jb(kb)
         if (jcol.gt.iii) then
            puv_nbq(iii)=puv_nbq(iii)     - scal * b(kb)
         elseif (jcol.eq.iii) then 
            pdv_nbq(iii)=pdv_nbq(iii)     - scal * b(kb)
         else
            plv_nbq(iii-1)=plv_nbq(iii-1) - scal * b(kb)
         endif
      end do
    end do
  end do

  return
end

#else
      subroutine amub2_tri_empty
      end subroutine amub2_tri_empty
#endif
