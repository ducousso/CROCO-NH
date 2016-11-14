!     AGRIF (Adaptive Grid Refinement In Fortran)
!
!     Copyright (C) 2003 Laurent Debreu (Laurent.Debreu@imag.fr)
!                        Christophe Vouland (Christophe.Vouland@imag.fr)
!
!     This program is free software; you can redistribute it and/or modify
!     it under the terms of the GNU General Public License as published by
!     the Free Software Foundation; either version 2 of the License, or
!     (at your option) any later version.
!
!     This program is distributed in the hope that it will be useful,
!     but WITHOUT ANY WARRANTY; without even the implied warranty of
!     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!     GNU General Public License for more details.
!
!     You should have received a copy of the GNU General Public License
!     along with this program; if not, write to the Free Software
!     Foundation, Inc., 59 Temple Place-  Suite 330, Boston, MA 02111-1307, USA.
!
!
!> Module Agrif_Init.
!>
!> Several operations on the variables of the current grid (creation, instanciation, ...)
!! used during the creation of the grid hierarchy and during the time integration.
!
module Agrif_Init
!
    use Agrif_Grids
    use Agrif_Link
    use Agrif_Mpp
!
    implicit none
!
contains
!
!===================================================================================================
!  subroutine Agrif_Allocation
!
!> Allocates the arrays containing the values of the variables of the current grd.
!---------------------------------------------------------------------------------------------------
subroutine Agrif_Allocation ( Agrif_Gr, procname )
!---------------------------------------------------------------------------------------------------
    type(Agrif_Grid),       pointer   :: Agrif_Gr   !< Pointer on the current grid
    procedure(alloc_proc),  optional  :: procname   !< Allocation procedure (Default: Agrif_Allocationcalls)
!
    if ( present(procname) ) then
        call procname(Agrif_Gr)
    else
        call Agrif_Allocationcalls(Agrif_Gr)
    endif
    Agrif_Gr % allocation_is_done = .true.
!
    if ( Agrif_USE_ONLY_FIXED_GRIDS == 0 ) then
!
        if ( Agrif_Probdim == 1 ) allocate( Agrif_Gr%tabpoint1D(Agrif_Gr%nb(1)+1) )
        if ( Agrif_Probdim == 2 ) allocate( Agrif_Gr%tabpoint2D(Agrif_Gr%nb(1)+1, &
                                                                Agrif_Gr%nb(2)+1) )
        if ( Agrif_Probdim == 3 ) allocate( Agrif_Gr%tabpoint3D(Agrif_Gr%nb(1)+1, &
                                                                Agrif_Gr%nb(2)+1, &
                                                                Agrif_Gr%nb(3)+1) )
    endif
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Allocation
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_Instance
!
!> Make the pointer Agrif_Types::Agrif_Curgrid point to Agrif_Gr
!---------------------------------------------------------------------------------------------------
subroutine Agrif_Instance ( Agrif_Gr )
!---------------------------------------------------------------------------------------------------
    type(Agrif_Grid), pointer :: Agrif_Gr   !< Pointer on the current grid
!
    Agrif_Curgrid => Agrif_Gr
    Agrif_tabvars => Agrif_Curgrid % tabvars
    Agrif_tabvars_c => Agrif_Curgrid % tabvars_c
    Agrif_tabvars_r => Agrif_Curgrid % tabvars_r
    Agrif_tabvars_l => Agrif_Curgrid % tabvars_l
    Agrif_tabvars_i => Agrif_Curgrid % tabvars_i
!
#if defined AGRIF_MPI
    if ( Agrif_Gr % communicator /= -1 ) then
        call Agrif_MPI_switch_comm( Agrif_Gr % communicator )
    endif
#endif
!
    call Agrif_Get_numberofcells(Agrif_Gr)
    call Agrif_InitWorkSpace()
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Instance
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_initialisations
!---------------------------------------------------------------------------------------------------
subroutine Agrif_initialisations ( Agrif_Gr )
!---------------------------------------------------------------------------------------------------
    type(Agrif_Grid), pointer :: Agrif_Gr   !< Pointer on the current grid
!
    integer                         :: i
    type(Agrif_Variable),   pointer :: var => NULL()
    type(Agrif_Variable_c), pointer :: var_c => NULL()
    type(Agrif_Variable_r), pointer :: var_r => NULL()
    type(Agrif_Variable_l), pointer :: var_l => NULL()
    type(Agrif_Variable_i), pointer :: var_i => NULL()
!
    do i = 1,Agrif_NbVariables(0)
!
        var => Agrif_Gr % tabvars(i)
        var % nbdim = 0
!
        if (allocated(var%array1)) then
            var % nbdim = 1
            var % lb(1:1) = lbound(var%array1)
            var % ub(1:1) = ubound(var%array1)
        endif
        if (allocated(var%array2)) then
            var % nbdim = 2
            var % lb(1:2) = lbound(var%array2)
            var % ub(1:2) = ubound(var%array2)
        endif
        if (allocated(var%array3)) then
            var % nbdim = 3
            var % lb(1:3) = lbound(var%array3)
            var % ub(1:3) = ubound(var%array3)
        endif
        if (allocated(var%array4)) then
            var % nbdim = 4
            var % lb(1:4) = lbound(var%array4)
            var % ub(1:4) = ubound(var%array4)
        endif
        if (allocated(var%array5)) then
            var % nbdim = 5
            var % lb(1:5) = lbound(var%array5)
            var % ub(1:5) = ubound(var%array5)
        endif
        if (allocated(var%array6)) then
            var % nbdim = 6
            var % lb(1:6) = lbound(var%array6)
            var % ub(1:6) = ubound(var%array6)
        endif
!
        if (allocated(var%darray1)) var % nbdim = 1
        if (allocated(var%darray2)) var % nbdim = 2
        if (allocated(var%darray3)) var % nbdim = 3
        if (allocated(var%darray4)) var % nbdim = 4
        if (allocated(var%darray5)) var % nbdim = 5
        if (allocated(var%darray6)) var % nbdim = 6
!
        if (allocated(var%sarray1)) var % nbdim = 1
        if (allocated(var%sarray2)) var % nbdim = 2
        if (allocated(var%sarray3)) var % nbdim = 3
        if (allocated(var%sarray4)) var % nbdim = 4
        if (allocated(var%sarray5)) var % nbdim = 5
        if (allocated(var%sarray6)) var % nbdim = 6
!
    enddo

    do i = 1,Agrif_NbVariables(1)
!
        var_c => Agrif_Gr % tabvars_c(i)
        var_c % nbdim = 0
!
        if (allocated(var_c%carray1)) var_c % nbdim = 1
        if (allocated(var_c%carray2)) var_c % nbdim = 2
!
    enddo

    do i = 1,Agrif_NbVariables(2)
!
        var_r => Agrif_Gr % tabvars_r(i)
        var_r % nbdim = 0
!
    enddo

    do i = 1,Agrif_NbVariables(3)
!
        var_l => Agrif_Gr % tabvars_l(i)
        var_l % nbdim = 0
!
        if (allocated(var_l%larray1)) var_l % nbdim = 1
        if (allocated(var_l%larray2)) var_l % nbdim = 2
        if (allocated(var_l%larray3)) var_l % nbdim = 3
        if (allocated(var_l%larray4)) var_l % nbdim = 4
        if (allocated(var_l%larray5)) var_l % nbdim = 5
        if (allocated(var_l%larray6)) var_l % nbdim = 6
!
    enddo

    do i = 1,Agrif_NbVariables(4)
!
        var_i => Agrif_Gr % tabvars_i(i)
        var_i % nbdim = 0
!
        if (allocated(var_i%iarray1)) var_i % nbdim = 1
        if (allocated(var_i%iarray2)) var_i % nbdim = 2
        if (allocated(var_i%iarray3)) var_i % nbdim = 3
        if (allocated(var_i%iarray4)) var_i % nbdim = 4
        if (allocated(var_i%iarray5)) var_i % nbdim = 5
        if (allocated(var_i%iarray6)) var_i % nbdim = 6
!
    enddo

!---------------------------------------------------------------------------------------------------
end subroutine Agrif_initialisations
!===================================================================================================
!
end module Agrif_Init
