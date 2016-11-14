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
!> @brief
!
!> @details 
!
!
! REVISION HISTORY:
!
!> @authors
!> @date 2015 January
!> @todo
!
!------------------------------------------------------------------------------
!! qsort  FROM >>>>
!!! Ajout des double + integer + interface + ordre croissant
!!! Attention a la mémoire....

!!!! 2014-01-27 ... Ca y est problème de mémoire.
!!!!     ==> résolu avec la version QsortCxxx.

!
!     Example belonging to "Modern Fortran in Practice" by Arjen Markus
!
!     This work is licensed under the Creative Commons Attribution 3.0 Unported License.
!     To view a copy of this license, visit http://creativecommons.org/licenses/by/3.0/
!     or send a letter to:
!     Creative Commons, 444 Castro Street, Suite 900, Mountain View, California, 94041, USA.
!
!     Compact implementation of the QuickSort algorithm
!
!     Note:
!     Because the function uses Fortran 90 features, its interface should be made
!     explicit when using it in an actual program. This is easiest via a module.
!
module module_qsort
    implicit none
    
    interface qsort
!       module  procedure qsort_doubles
!       module  procedure qsort_reals
!       module  procedure qsort_ints
       module  procedure QsortCint
       module  procedure QsortCreal
!        module  procedure QsortCdouble
    end interface

contains
recursive function qsort_reals( data ) result( sorted )
    real, dimension(:), intent(in) :: data
    real, dimension(1:size(data))  :: sorted

    if ( size(data) > 1 ) then
        sorted = &
            (/ qsort_reals( pack( data(2:), data(2:) < data(1) ) ), &
               data(1),                                             &
               qsort_reals( pack( data(2:), data(2:) >= data(1) ) ) /)
    else
        sorted = data
    endif
end function qsort_reals


recursive function qsort_doubles( data ) result( sorted )
    double precision, dimension(:), intent(in) :: data
    double precision, dimension(1:size(data))  :: sorted
    integer :: sz
    sz=size(data)

    if ( size(data) > 1 ) then
        sorted = &
            (/ qsort_doubles( pack( data(2:), data(2:) < data(1) ) ), &
               data(1),                                             &
               qsort_doubles( pack( data(2:), data(2:) >= data(1) ) ) /)
    else
        sorted = data
    endif
end function qsort_doubles


recursive function qsort_ints( data ) result( sorted )
    integer, dimension(:), intent(in) :: data
    integer, dimension(1:size(data))  :: sorted
    integer :: sz
    sz=size(data)

    if ( size(data) > 1 ) then
        sorted = &
            (/ qsort_ints( pack( data(2:sz), data(2:sz) < data(1) ) ), &
               data(1),                                             &
               qsort_ints( pack( data(2:sz), data(2:sz) >= data(1) ) ) /)
    else
        sorted = data
    endif
end function qsort_ints

!---------------------------------------------------------
!#########################################################
!####      Version 2 un peu moins de memoire ???
!---       Version integer    ----------------------------
function QsortCint(data) result( sorted )
  integer, intent(inout), dimension(:) :: data
  integer, dimension(1:size(data))  :: sorted
  sorted=data
  call QsortCi(sorted)
end function QsortCint
recursive subroutine QsortCi(A)
  integer, intent(inout), dimension(:) :: A
  integer :: iq
  if(size(A) > 1) then
     call Partitioni(A, iq)
     call QsortCi(A(:iq-1))
     call QsortCi(A(iq:))
  endif
end subroutine QsortCi
subroutine Partitioni(A, marker)
  integer, intent(in out), dimension(:) :: A
  integer, intent(out) :: marker
  integer :: i, j
  integer :: temp
  integer :: x      ! pivot point
  x = A(1)
  i= 0
  j= size(A) + 1
  do
     j = j-1
     do
        if (A(j) <= x) exit
        j = j-1
     end do
     i = i+1
     do
        if (A(i) >= x) exit
        i = i+1
     end do
     if (i < j) then
        ! exchange A(i) and A(j)
        temp = A(i)
        A(i) = A(j)
        A(j) = temp
     elseif (i == j) then
        marker = i+1
        return
     else
        marker = i
        return
     endif
  end do
end subroutine Partitioni
!---       Version real    ----------------------------
function QsortCreal(data) result( sorted )
  real, intent(inout), dimension(:) :: data
  real, dimension(1:size(data))  :: sorted
  sorted=data
  call QsortCr(sorted)
end function QsortCreal
recursive subroutine QsortCr(A)
  real, intent(inout), dimension(:) :: A
  integer :: iq
  if(size(A) > 1) then
     call Partitionr(A, iq)
     call QsortCr(A(:iq-1))
     call QsortCr(A(iq:))
  endif
end subroutine QsortCr
subroutine Partitionr(A, marker)
  real, intent(in out), dimension(:) :: A
  integer, intent(out) :: marker
  integer :: i, j
  real :: temp
  real :: x      ! pivot point
  x = A(1)
  i= 0
  j= size(A) + 1
  do
     j = j-1
     do
        if (A(j) <= x) exit
        j = j-1
     end do
     i = i+1
     do
        if (A(i) >= x) exit
        i = i+1
     end do
     if (i < j) then
        ! exchange A(i) and A(j)
        temp = A(i)
        A(i) = A(j)
        A(j) = temp
     elseif (i == j) then
        marker = i+1
        return
     else
        marker = i
        return
     endif
  end do
end subroutine Partitionr
!---       Version double    ----------------------------
function QsortCdouble(data) result( sorted )
  double precision, intent(inout), dimension(:) :: data
  double precision, dimension(1:size(data))     :: sorted
  sorted=data
  call QsortCd(sorted)
end function QsortCdouble
recursive subroutine QsortCd(A)
  double precision, intent(inout), dimension(:) :: A
  integer :: iq
  if(size(A) > 1) then
     call Partitiond(A, iq)
     call QsortCd(A(:iq-1))
     call QsortCd(A(iq:))
  endif
end subroutine QsortCd
subroutine Partitiond(A, marker)
  double precision, intent(in out), dimension(:) :: A
  integer, intent(out) :: marker
  integer :: i, j
  double precision :: temp
  double precision :: x      ! pivot point
  x = A(1)
  i= 0
  j= size(A) + 1
  do
     j = j-1
     do
        if (A(j) <= x) exit
        j = j-1
     end do
     i = i+1
     do
        if (A(i) >= x) exit
        i = i+1
     end do
     if (i < j) then
        ! exchange A(i) and A(j)
        temp = A(i)
        A(i) = A(j)
        A(j) = temp
     elseif (i == j) then
        marker = i+1
        return
     else
        marker = i
        return
     endif
  end do
end subroutine Partitiond
!#-----------------------------------------------------------------------------

subroutine getblocks(nb,tabin,nbblock,blockdeb,blocklength)
 !use module_parallele
 implicit none
 integer,intent(in) :: nb
 integer,dimension(:),intent(in) :: tabin
 integer,intent(out) :: nbblock
 integer,dimension(:),intent(out) :: blockdeb
 integer,dimension(:),intent(out) :: blocklength
 integer :: bcl, bcl2
     !!!! Get blocks (number, length)
     !call getblock(idiouestesttri(1:ii),number,length)
     
     !write(5000+par%rank,*) "dans getblocks nb=",nb,tabin(1:nb)
     
     nbblock=1
     blockdeb(1) = 1
     !blocklength(0) = 0
     blocklength(1) = 1
!     write(6000+par%rank,*) "     ",1,tabin(1)
     do bcl=2,nb
!        write(6000+par%rank,*) "     ",bcl,tabin(bcl)
        if (tabin(bcl) /= tabin(bcl-1)+1) then 
	  nbblock=nbblock+1
!          write(6000+par%rank,*) "     ...",nbblock
	  blockdeb(nbblock)=bcl 
	  blocklength(nbblock-1)=bcl-blockdeb(nbblock-1)-1
	endif	
     enddo
     blocklength(nbblock) = nb-blockdeb(nbblock)
     !write(5000+par%rank,*) "nbblock=",nbblock
!     write(200+par%rank,*) " ds getblocks nbblock=",nbblock
end subroutine getblocks

end module module_qsort

#else
      module module_qsort_empty
      end module
#endif      
