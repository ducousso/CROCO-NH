!
! $Id: modarrays.F 662 2007-05-25 15:58:52Z opalod $
!
!     AGRIF (Adaptive Grid Refinement In Fortran)
!
!     Copyright (C) 2003 Laurent Debreu (Laurent.Debreu@imag.fr)
!                Christophe Vouland (Christophe.Vouland@imag.fr)
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
!     Foundation, Inc., 59 Temple Place- Suite 330, Boston, MA 02111-1307, USA.
!
!
!> Module Agrif_Arrays
!
module Agrif_Arrays
!
    use Agrif_Types
    use Agrif_Grids
!
    implicit none
!
#if defined AGRIF_MPI
    interface
        subroutine Agrif_InvLoc ( indloc, proc_id, dir, indglob )
            integer, intent(in)     :: indloc   !< local index
            integer, intent(in)     :: proc_id  !< rank of the proc calling this function
            integer, intent(in)     :: dir      !< direction of the index
            integer, intent(out)    :: indglob  !< global index
        end subroutine Agrif_InvLoc
    end interface
    private :: Agrif_InvLoc
#endif
!
contains
!
!===================================================================================================
! subroutine Agrif_Childbounds
!
!> Computes the global indices of the child grid
!---------------------------------------------------------------------------------------------------
subroutine Agrif_Childbounds ( nbdim,           &
                               lb_var, ub_var,  &
                               lb_tab, ub_tab,  &
                               proc_id,         &
                               coords,          &
                               lb_tab_true, ub_tab_true, memberin,  &
                               indminglob3,indmaxglob3)
!---------------------------------------------------------------------------------------------------
    integer,                   intent(in)  :: nbdim         !< Number of dimensions
    integer, dimension(nbdim), intent(in)  :: lb_var        !< Local lower boundary on the current processor
    integer, dimension(nbdim), intent(in)  :: ub_var        !< Local upper boundary on the current processor
    integer, dimension(nbdim), intent(in)  :: lb_tab        !< Global lower boundary of the variable
    integer, dimension(nbdim),OPTIONAL     :: indminglob3,indmaxglob3 !< True bounds for MPI USE
    integer, dimension(nbdim), intent(in)  :: ub_tab        !< Global upper boundary of the variable
    integer,                   intent(in)  :: proc_id       !< Current processor
    integer, dimension(nbdim), intent(in)  :: coords
    integer, dimension(nbdim), intent(out) :: lb_tab_true   !< Global value of lb_var on the current processor
    integer, dimension(nbdim), intent(out) :: ub_tab_true   !< Global value of ub_var on the current processor
    logical,                   intent(out) :: memberin
!
    integer :: i, coord_i
    integer :: lb_glob_index, ub_glob_index ! Lower and upper global indices
!
    do i = 1, nbdim
!
        coord_i = coords(i)
!
#if defined AGRIF_MPI
        call Agrif_InvLoc( lb_var(i), proc_id, coord_i, lb_glob_index )
        call Agrif_InvLoc( ub_var(i), proc_id, coord_i, ub_glob_index )
        if (present(indminglob3)) then
          indminglob3(i)=lb_glob_index
          indmaxglob3(i)=ub_glob_index
        endif
#else
        lb_glob_index = lb_var(i)
        ub_glob_index = ub_var(i)
#endif
        lb_tab_true(i) = max(lb_tab(i), lb_glob_index)
        ub_tab_true(i) = min(ub_tab(i), ub_glob_index)

    enddo
!
    memberin = .true.
    do i = 1,nbdim
        if (ub_tab_true(i) < lb_tab_true(i)) then
            memberin = .false.
            exit
        endif
    enddo
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Childbounds
!===================================================================================================
!
!===================================================================================================
subroutine Agrif_get_var_global_bounds( var, lubglob, nbdim )
!---------------------------------------------------------------------------------------------------
    type(Agrif_Variable),        intent(in)  :: var
    integer, dimension(nbdim,2), intent(out) :: lubglob
    integer,                     intent(in)  :: nbdim
!
#if defined AGRIF_MPI
    include 'mpif.h'
    integer, dimension(nbdim)   :: lb, ub
    integer, dimension(nbdim,2) :: iminmaxg
    integer :: i, code, coord_i
#endif
!
#if !defined AGRIF_MPI
    call Agrif_get_var_bounds_array(var, lubglob(:,1), lubglob(:,2), nbdim)
#else
    call Agrif_get_var_bounds_array(var, lb, ub, nbdim)

    do i = 1,nbdim
        coord_i = var % root_var % coords(i)
        call Agrif_InvLoc( lb(i), Agrif_Procrank, coord_i, iminmaxg(i,1) )
        call Agrif_InvLoc( ub(i), Agrif_Procrank, coord_i, iminmaxg(i,2) )
    enddo
!
    iminmaxg(1:nbdim,2) = - iminmaxg(1:nbdim,2)
    call MPI_ALLREDUCE(iminmaxg, lubglob, 2*nbdim, MPI_INTEGER, MPI_MIN, &
                       Agrif_mpi_comm, code)
    lubglob(1:nbdim,2)  = - lubglob(1:nbdim,2)
#endif
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_get_var_global_bounds
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_get_var_bounds
!
!> Gets the lower and the upper boundaries of a variable, for one particular direction.
!---------------------------------------------------------------------------------------------------
subroutine Agrif_get_var_bounds ( variable, lower, upper, index )
!---------------------------------------------------------------------------------------------------
    type(Agrif_Variable), intent(in)    :: variable   !< Variable for which we want to extract boundaries
    integer,              intent(out)   :: lower      !< Lower bound
    integer,              intent(out)   :: upper      !< Upper bound
    integer,              intent(in)    :: index      !< Direction for wich we want to know the boundaries
!
    lower = variable % lb(index)
    upper = variable % ub(index)
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_get_var_bounds
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_get_var_bounds_array
!
!> Gets the lower and the upper boundaries of a table.
!---------------------------------------------------------------------------------------------------
subroutine Agrif_get_var_bounds_array ( variable, lower, upper, nbdim )
!---------------------------------------------------------------------------------------------------
    type(Agrif_Variable),      intent(in)   :: variable   !< Variable for which we want to extract boundaries
    integer, dimension(nbdim), intent(out)  :: lower      !< Lower bounds array
    integer, dimension(nbdim), intent(out)  :: upper      !< Upper bounds array
    integer, intent(in)                     :: nbdim      !< Numer of dimensions of the variable
!
    lower = variable % lb(1:nbdim)
    upper = variable % ub(1:nbdim)
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_get_var_bounds_array
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_array_allocate
!
!> Allocates data array in \b variable, according to the required dimension.
!---------------------------------------------------------------------------------------------------
subroutine Agrif_array_allocate ( variable, lb, ub, nbdim )
!---------------------------------------------------------------------------------------------------
    type(Agrif_Variable),      intent(inout) :: variable    !< Variable struct for allocation
    integer, dimension(nbdim), intent(in)    :: lb          !< Lower bound
    integer, dimension(nbdim), intent(in)    :: ub          !< Upper bound
    integer,                   intent(in)    :: nbdim       !< Dimension of the array
!
    select case (nbdim)
    case (1) ; allocate(variable%array1(lb(1):ub(1)))
    case (2) ; allocate(variable%array2(lb(1):ub(1),lb(2):ub(2)))
    case (3) ; allocate(variable%array3(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3)))
    case (4) ; allocate(variable%array4(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4)))
    case (5) ; allocate(variable%array5(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5)))
    case (6) ; allocate(variable%array6(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6)))
    end select
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_array_allocate
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_array_deallocate
!
!> Dellocates data array in \b variable, according to the required dimension.
!---------------------------------------------------------------------------------------------------
subroutine Agrif_array_deallocate ( variable, nbdim )
!---------------------------------------------------------------------------------------------------
    type(Agrif_Variable), intent(inout) :: variable     !< Variable struct for deallocation
    integer,              intent(in)    :: nbdim        !< Dimension of the array
!
    select case (nbdim)
    case (1) ; deallocate(variable%array1)
    case (2) ; deallocate(variable%array2)
    case (3) ; deallocate(variable%array3)
    case (4) ; deallocate(variable%array4)
    case (5) ; deallocate(variable%array5)
    case (6) ; deallocate(variable%array6)
    end select
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_array_deallocate
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_var_set_array_tozero
!
!> Reset the value of the data array in \b variable, according to the required dimension.
!---------------------------------------------------------------------------------------------------
subroutine Agrif_var_set_array_tozero ( variable, nbdim )
!---------------------------------------------------------------------------------------------------
    type(Agrif_Variable), intent(inout) :: variable     !< Variable
    integer, intent(in)                 :: nbdim        !< Dimension of the array you want to reset
!
    select case (nbdim)
    case (1) ; call Agrif_set_array_tozero_1D(variable%array1)
    case (2) ; call Agrif_set_array_tozero_2D(variable%array2)
    case (3) ; call Agrif_set_array_tozero_3D(variable%array3)
    case (4) ; call Agrif_set_array_tozero_4D(variable%array4)
    case (5) ; call Agrif_set_array_tozero_5D(variable%array5)
    case (6) ; call Agrif_set_array_tozero_6D(variable%array6)
    end select
!---------------------------------------------------------------------------------------------------
contains
!---------------------------------------------------------------------------------------------------
    subroutine Agrif_set_array_tozero_1D ( array )
        real, dimension(:),           intent(out) :: array
        array = 0.
    end subroutine Agrif_set_array_tozero_1D
!
    subroutine Agrif_set_array_tozero_2D ( array )
        real, dimension(:,:),         intent(out) :: array
        array = 0.
    end subroutine Agrif_set_array_tozero_2D
!
    subroutine Agrif_set_array_tozero_3D ( array )
        real, dimension(:,:,:),       intent(out) :: array
        array = 0.
    end subroutine Agrif_set_array_tozero_3D
!
    subroutine Agrif_set_array_tozero_4D ( array )
        real, dimension(:,:,:,:),     intent(out) :: array
        array = 0.
    end subroutine Agrif_set_array_tozero_4D
!
    subroutine Agrif_set_array_tozero_5D ( array )
        real, dimension(:,:,:,:,:),   intent(out) :: array
        array = 0.
    end subroutine Agrif_set_array_tozero_5D
!
    subroutine Agrif_set_array_tozero_6D ( array )
        real, dimension(:,:,:,:,:,:), intent(out) :: array
        array = 0.
    end subroutine Agrif_set_array_tozero_6D
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_var_set_array_tozero
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_var_copy_array
!
!> Copy a part of data array from var2 to var1
!---------------------------------------------------------------------------------------------------
subroutine Agrif_var_copy_array ( var1, inf1, sup1, var2, inf2, sup2, nbdim )
!---------------------------------------------------------------------------------------------------
    type(Agrif_Variable),      intent(inout) :: var1    !< Modified variable
    integer, dimension(nbdim), intent(in)    :: inf1    !< Lower boundary for var1
    integer, dimension(nbdim), intent(in)    :: sup1    !< Upper boundary for var1
    type(Agrif_Variable),      intent(in)    :: var2    !< Input variable
    integer, dimension(nbdim), intent(in)    :: inf2    !< Lower boundary for var2
    integer, dimension(nbdim), intent(in)    :: sup2    !< Upper boundary for var2
    integer,                   intent(in)    :: nbdim   !< Dimension of the array
!
    select case (nbdim)
    case (1) ; var1%array1(inf1(1):sup1(1)) = var2%array1(inf2(1):sup2(1))
    case (2) ; call Agrif_copy_array_2d( var1%array2, var2%array2, &
                                         lbound(var1%array2), lbound(var2%array2), inf1, sup1, inf2, sup2 )
    case (3) ; call Agrif_copy_array_3d( var1%array3, var2%array3, &
                                         lbound(var1%array3), lbound(var2%array3), inf1, sup1, inf2, sup2 )
    case (4) ; call Agrif_copy_array_4d( var1%array4, var2%array4, &
                                         lbound(var1%array4), lbound(var2%array4), inf1, sup1, inf2, sup2 )
    case (5) ; var1%array5(inf1(1):sup1(1),   &
                           inf1(2):sup1(2),   &
                           inf1(3):sup1(3),   &
                           inf1(4):sup1(4),   &
                           inf1(5):sup1(5)) = var2%array5(inf2(1):sup2(1), &
                                                          inf2(2):sup2(2), &
                                                          inf2(3):sup2(3), &
                                                          inf2(4):sup2(4), &
                                                          inf2(5):sup2(5))
    case (6) ; var1%array6(inf1(1):sup1(1),   &
                           inf1(2):sup1(2),   &
                           inf1(3):sup1(3),   &
                           inf1(4):sup1(4),   &
                           inf1(5):sup1(5),   &
                           inf1(6):sup1(6)) = var2%array6(inf2(1):sup2(1), &
                                                          inf2(2):sup2(2), &
                                                          inf2(3):sup2(3), &
                                                          inf2(4):sup2(4), &
                                                          inf2(5):sup2(5), &
                                                          inf2(6):sup2(6))
    end select
!---------------------------------------------------------------------------------------------------
contains
!---------------------------------------------------------------------------------------------------
    subroutine Agrif_copy_array_2d ( tabout, tabin, l, m, inf1, sup1, inf2, sup2 )
        integer, dimension(2), intent(in)   :: l, m
        integer, dimension(2), intent(in)   :: inf1, sup1
        integer, dimension(2), intent(in)   :: inf2, sup2
        real, dimension(l(1):,l(2):), intent(out) :: tabout
        real, dimension(m(1):,m(2):), intent(in)  :: tabin
        tabout(inf1(1):sup1(1), &
               inf1(2):sup1(2)) = tabin(inf2(1):sup2(1), &
                                        inf2(2):sup2(2))
    end subroutine Agrif_copy_array_2d
!
    subroutine Agrif_copy_array_3d ( tabout, tabin, l, m, inf1, sup1, inf2, sup2 )
        integer, dimension(3), intent(in)   :: l, m
        integer, dimension(3), intent(in)   :: inf1, sup1
        integer, dimension(3), intent(in)   :: inf2,sup2
        real, dimension(l(1):,l(2):,l(3):), intent(out) :: tabout
        real, dimension(m(1):,m(2):,m(3):), intent(in)  :: tabin
        tabout(inf1(1):sup1(1), &
               inf1(2):sup1(2), &
               inf1(3):sup1(3)) = tabin(inf2(1):sup2(1), &
                                        inf2(2):sup2(2), &
                                        inf2(3):sup2(3))
    end subroutine Agrif_copy_array_3d
!
    subroutine Agrif_copy_array_4d ( tabout, tabin, l, m, inf1, sup1, inf2, sup2 )
        integer, dimension(4), intent(in)   :: l, m
        integer, dimension(4), intent(in)   :: inf1, sup1
        integer, dimension(4), intent(in)   :: inf2, sup2
        real, dimension(l(1):,l(2):,l(3):,l(4):), intent(out) :: tabout
        real, dimension(m(1):,m(2):,m(3):,m(4):), intent(in)  :: tabin
        tabout(inf1(1):sup1(1), &
               inf1(2):sup1(2), &
               inf1(3):sup1(3), &
               inf1(4):sup1(4)) = tabin(inf2(1):sup2(1), &
                                        inf2(2):sup2(2), &
                                        inf2(3):sup2(3), &
                                        inf2(4):sup2(4))
    end subroutine Agrif_copy_array_4d
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_var_copy_array
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_var_full_copy_array
!
!> Copy the full data array from var2 to var1
!---------------------------------------------------------------------------------------------------
subroutine Agrif_var_full_copy_array ( var1, var2, nbdim )
!---------------------------------------------------------------------------------------------------
    type(Agrif_Variable), intent(inout) :: var1    !< Modified variable
    type(Agrif_Variable), intent(in)    :: var2    !< Input variable
    integer,              intent(in)    :: nbdim   !< Dimension of the array
!
    select case (nbdim)
    case (1) ; var1 % array1 = var2 % array1
    case (2) ; var1 % array2 = var2 % array2
    case (3) ; var1 % array3 = var2 % array3
    case (4) ; var1 % array4 = var2 % array4
    case (5) ; var1 % array5 = var2 % array5
    case (6) ; var1 % array6 = var2 % array6
    end select
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_var_full_copy_array
!===================================================================================================
!
!===================================================================================================
!  subroutine GiveAgrif_SpecialValueToTab_mpi
!
!> Copy \b value in data array \b var2 where it is present in \b var1.
!---------------------------------------------------------------------------------------------------
subroutine GiveAgrif_SpecialValueToTab_mpi ( var1, var2, bounds, value, nbdim )
!---------------------------------------------------------------------------------------------------
    type(Agrif_Variable),      intent(in)    :: var1    !< Modified variable
    type(Agrif_Variable),      intent(inout) :: var2    !< Input variable
    integer, dimension(:,:,:), intent(in)    :: bounds  !< Bound for both arrays
    real,                      intent(in)    :: value   !< Special value
    integer,                   intent(in)    :: nbdim   !< Dimension of the array
!
    select case (nbdim)
    case (1)
        where (var1 % array1(bounds(1,1,2):bounds(1,2,2)) == value )
            var2 % array1(bounds(1,1,1):bounds(1,2,1)) = value
        end where
    case (2)
        where (var1 % array2(bounds(1,1,2):bounds(1,2,2), &
                             bounds(2,1,2):bounds(2,2,2)) == value)
            var2 % array2(bounds(1,1,1):bounds(1,2,1), &
                          bounds(2,1,1):bounds(2,2,1)) = value
        end where
    case (3)
        where (var1 % array3(bounds(1,1,2):bounds(1,2,2), &
                             bounds(2,1,2):bounds(2,2,2), &
                             bounds(3,1,2):bounds(3,2,2)) == value)
            var2 % array3(bounds(1,1,1):bounds(1,2,1), &
                          bounds(2,1,1):bounds(2,2,1), &
                          bounds(3,1,1):bounds(3,2,1)) = value
        end where
    case (4)
        where (var1 % array4(bounds(1,1,2):bounds(1,2,2), &
                             bounds(2,1,2):bounds(2,2,2), &
                             bounds(3,1,2):bounds(3,2,2), &
                             bounds(4,1,2):bounds(4,2,2)) == value)
            var2 % array4(bounds(1,1,1):bounds(1,2,1), &
                          bounds(2,1,1):bounds(2,2,1), &
                          bounds(3,1,1):bounds(3,2,1), &
                          bounds(4,1,1):bounds(4,2,1)) = value
        end where
    case (5)
        where (var1 % array5(bounds(1,1,2):bounds(1,2,2), &
                             bounds(2,1,2):bounds(2,2,2), &
                             bounds(3,1,2):bounds(3,2,2), &
                             bounds(4,1,2):bounds(4,2,2), &
                             bounds(5,1,2):bounds(5,2,2)) == value)
            var2 % array5(bounds(1,1,1):bounds(1,2,1), &
                          bounds(2,1,1):bounds(2,2,1), &
                          bounds(3,1,1):bounds(3,2,1), &
                          bounds(4,1,1):bounds(4,2,1), &
                          bounds(5,1,1):bounds(5,2,1)) = value
        end where
    case (6)
        where (var1 % array6(bounds(1,1,2):bounds(1,2,2), &
                             bounds(2,1,2):bounds(2,2,2), &
                             bounds(3,1,2):bounds(3,2,2), &
                             bounds(4,1,2):bounds(4,2,2), &
                             bounds(5,1,2):bounds(5,2,2), &
                             bounds(6,1,2):bounds(6,2,2)) == value)
            var2 % array6(bounds(1,1,1):bounds(1,2,1), &
                          bounds(2,1,1):bounds(2,2,1), &
                          bounds(3,1,1):bounds(3,2,1), &
                          bounds(4,1,1):bounds(4,2,1), &
                          bounds(5,1,1):bounds(5,2,1), &
                          bounds(6,1,1):bounds(6,2,1)) = value
        end where
    end select
!---------------------------------------------------------------------------------------------------
end subroutine GiveAgrif_SpecialValueToTab_mpi
!===================================================================================================
!
! no more used ???
#if 0
!===================================================================================================
!  subroutine GiveAgrif_SpecialValueToTab
!---------------------------------------------------------------------------------------------------
subroutine GiveAgrif_SpecialValueToTab ( var1, var2, &
                                         lower, upper, Value, nbdim )
!---------------------------------------------------------------------------------------------------
    TYPE(Agrif_Variable),      pointer      :: var1
    TYPE(Agrif_Variable),      pointer      :: var2
    INTEGER,                   intent(in)   :: nbdim
    INTEGER, DIMENSION(nbdim), intent(in)   :: lower, upper
    REAL,                      intent(in)   :: Value
!
    select case (nbdim)
    case (1)
        where (var1 % array1( lower(1):upper(1)) == Value)
            var2 % array1(lower(1):upper(1)) = Value
        end where
    case (2)
        where (var1 % array2( lower(1):upper(1), &
                                   lower(2):upper(2)) == Value)
            var2 % array2(lower(1):upper(1), &
                               lower(2):upper(2)) = Value
        end where
    case (3)
        where (var1 % array3( lower(1):upper(1), &
                                   lower(2):upper(2), &
                                   lower(3):upper(3)) == Value)
            var2 % array3(lower(1):upper(1), &
                               lower(2):upper(2), &
                               lower(3):upper(3)) = Value
        end where
    case (4)
        where (var1 % array4( lower(1):upper(1), &
                                   lower(2):upper(2), &
                                   lower(3):upper(3), &
                                   lower(4):upper(4)) == Value)
            var2 % array4(lower(1):upper(1), &
                               lower(2):upper(2), &
                               lower(3):upper(3), &
                               lower(4):upper(4)) = Value
        end where
    case (5)
        where (var1 % array5( lower(1):upper(1), &
                                   lower(2):upper(2), &
                                   lower(3):upper(3), &
                                   lower(4):upper(4), &
                                   lower(5):upper(5)) == Value)
            var2 % array5(lower(1):upper(1), &
                               lower(2):upper(2), &
                               lower(3):upper(3), &
                               lower(4):upper(4), &
                               lower(5):upper(5)) = Value
        end where
    case (6)
        where (var1 % array6( lower(1):upper(1), &
                                   lower(2):upper(2), &
                                   lower(3):upper(3), &
                                   lower(4):upper(4), &
                                   lower(5):upper(5), &
                                   lower(6):upper(6)) == Value)
            var2 % array6(lower(1):upper(1), &
                               lower(2):upper(2), &
                               lower(3):upper(3), &
                               lower(4):upper(4), &
                               lower(5):upper(5), &
                               lower(6):upper(6)) = Value
        end where
    end select
!---------------------------------------------------------------------------------------------------
end subroutine GiveAgrif_SpecialValueToTab
!===================================================================================================
#endif
!
#if defined AGRIF_MPI
!===================================================================================================
!  subroutine Agrif_var_replace_value
!
!> Replace \b value by \var2 content in \var1 data array.
!---------------------------------------------------------------------------------------------------
subroutine Agrif_var_replace_value ( var1, var2, lower, upper, value, nbdim )
!---------------------------------------------------------------------------------------------------
    type(Agrif_Variable),      intent(inout) :: var1    !< Modified variable
    type(Agrif_Variable),      intent(in)    :: var2    !< Input variable
    integer, dimension(nbdim), intent(in)    :: lower   !< Lower bound
    integer, dimension(nbdim), intent(in)    :: upper   !< Upper bound
    real,                      intent(in)    :: value   !< Special value
    integer,                   intent(in)    :: nbdim   !< Dimension of the array
!
    integer :: i,j,k,l,m,n
!
    select case (nbdim)
    case (1)
        do i = lower(1),upper(1)
            if (var1%array1(i) == value) then
                var1%array1(i) = var2%array1(i)
            endif
        enddo
    case (2)
        do j = lower(2),upper(2)
        do i = lower(1),upper(1)
            if (var1%array2(i,j) == value) then
                var1%array2(i,j) = var2%array2(i,j)
            endif
        enddo
        enddo
    case (3)
        do k = lower(3),upper(3)
        do j = lower(2),upper(2)
        do i = lower(1),upper(1)
            if (var1%array3(i,j,k) == value) then
                var1%array3(i,j,k) = var2%array3(i,j,k)
            endif
        enddo
        enddo
        enddo
    case (4)
        do l = lower(4),upper(4)
        do k = lower(3),upper(3)
        do j = lower(2),upper(2)
        do i = lower(1),upper(1)
            if (var1%array4(i,j,k,l) == value) then
                var1%array4(i,j,k,l) = var2%array4(i,j,k,l)
            endif
        enddo
        enddo
        enddo
        enddo
    case (5)
        do m = lower(5),upper(5)
        do l = lower(4),upper(4)
        do k = lower(3),upper(3)
        do j = lower(2),upper(2)
        do i = lower(1),upper(1)
            if (var1%array5(i,j,k,l,m) == value) then
                var1%array5(i,j,k,l,m) = var2%array5(i,j,k,l,m)
            endif
        enddo
        enddo
        enddo
        enddo
        enddo
    case (6)
        do n = lower(6),upper(6)
        do m = lower(5),upper(5)
        do l = lower(4),upper(4)
        do k = lower(3),upper(3)
        do j = lower(2),upper(2)
        do i = lower(1),upper(1)
            if (var1%array6(i,j,k,l,m,n) == value) then
                var1%array6(i,j,k,l,m,n) = var2%array6(i,j,k,l,m,n)
            endif
        enddo
        enddo
        enddo
        enddo
        enddo
        enddo
    end select
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_var_replace_value
!===================================================================================================
#endif
!
!===================================================================================================
!  subroutine PreProcessToInterpOrUpdate
!---------------------------------------------------------------------------------------------------
subroutine PreProcessToInterpOrUpdate ( parent, child,              &
                                        nb_child, ub_child,         &
                                        lb_child, lb_parent,        &
                                        s_child,  s_parent,         &
                                        ds_child, ds_parent, nbdim, interp )
!---------------------------------------------------------------------------------------------------
    type(Agrif_Variable), pointer, intent(in)   :: parent       !< Variable on the parent grid
    type(Agrif_Variable), pointer, intent(in)   :: child        !< Variable on the child grid
    integer, dimension(6), intent(out)          :: nb_child     !< Number of cells on the child grid
    integer, dimension(6), intent(out)          :: ub_child     !< Upper bound on the child grid
    integer, dimension(6), intent(out)          :: lb_child     !< Lower bound on the child grid
    integer, dimension(6), intent(out)          :: lb_parent    !< Lower bound on the parent grid
    real, dimension(6),    intent(out)          :: s_child      !< Child  grid position (s_root = 0)
    real, dimension(6),    intent(out)          :: s_parent     !< Parent grid position (s_root = 0)
    real, dimension(6),    intent(out)          :: ds_child     !< Child  grid dx (ds_root = 1)
    real, dimension(6),    intent(out)          :: ds_parent    !< Parent grid dx (ds_root = 1)
    integer,               intent(out)          :: nbdim        !< Number of dimensions
    logical,               intent(in)           :: interp       !< .true. if preprocess for interpolation, \n
                                                                !! .false. if preprocess for update
!
    type(Agrif_Variable), pointer :: root_var
    type(Agrif_Grid),     pointer :: Agrif_Child_Gr
    type(Agrif_Grid),     pointer :: Agrif_Parent_Gr
    integer :: n
!
    Agrif_Child_Gr  => Agrif_Curgrid
    Agrif_Parent_Gr => Agrif_Curgrid % parent
!
    root_var => child % root_var
!
!   Number of dimensions of the current grid
    nbdim = root_var % nbdim
!
    do n = 1,nbdim
!
!       Value of interptab(n) can be either x,y,z or N for a no space dimension
        select case(root_var % interptab(n))
!
        case('x')
!
            lb_child(n)  = root_var % point(n)
            lb_parent(n) = root_var % point(n)
            nb_child(n)  = Agrif_Child_Gr % nb(1)
            s_child(n)   = Agrif_Child_Gr  % Agrif_x(1)
            s_parent(n)  = Agrif_Parent_Gr % Agrif_x(1)
            ds_child(n)  = Agrif_Child_Gr  % Agrif_dx(1)
            ds_parent(n) = Agrif_Parent_Gr % Agrif_dx(1)
!
            if ( root_var % posvar(n) == 1 ) then
                ub_child(n) = lb_child(n) + Agrif_Child_Gr % nb(1)
            else
                ub_child(n) = lb_child(n) + Agrif_Child_Gr % nb(1) - 1
                s_child(n)  = s_child(n)  + 0.5*ds_child(n)
                s_parent(n) = s_parent(n) + 0.5*ds_parent(n)
            endif
!
        case('y')
!
            lb_child(n)  = root_var % point(n)
            lb_parent(n) = root_var % point(n)
            nb_child(n)  = Agrif_Child_Gr % nb(2)
            s_child(n)   = Agrif_Child_Gr  % Agrif_x(2)
            s_parent(n)  = Agrif_Parent_Gr % Agrif_x(2)
            ds_child(n)  = Agrif_Child_Gr  % Agrif_dx(2)
            ds_parent(n) = Agrif_Parent_Gr % Agrif_dx(2)
!
            if (root_var % posvar(n)==1) then
                ub_child(n) = lb_child(n) + Agrif_Child_Gr % nb(2)
            else
                ub_child(n) = lb_child(n) + Agrif_Child_Gr % nb(2) - 1
                s_child(n)  = s_child(n)  + 0.5*ds_child(n)
                s_parent(n) = s_parent(n) + 0.5*ds_parent(n)
            endif
!
        case('z')
!
            lb_child(n)  = root_var % point(n)
            lb_parent(n) = root_var % point(n)
            nb_child(n)  = Agrif_Child_Gr % nb(3)
            s_child(n)   = Agrif_Child_Gr  % Agrif_x(3)
            s_parent(n)  = Agrif_Parent_Gr % Agrif_x(3)
            ds_child(n)  = Agrif_Child_Gr  % Agrif_dx(3)
            ds_parent(n) = Agrif_Parent_Gr % Agrif_dx(3)
!
            if (root_var % posvar(n)==1) then
                ub_child(n) = lb_child(n) + Agrif_Child_Gr % nb(3)
            else
                ub_child(n) = lb_child(n) + Agrif_Child_Gr % nb(3) - 1
                s_child(n)  = s_child(n)  + 0.5*ds_child(n)
                s_parent(n) = s_parent(n) + 0.5*ds_parent(n)
            endif
!
        case('N') ! No space dimension
!
!       The next coefficients are calculated in order to do a simple copy of
!       values of the grid variable when the interpolation routine is
!       called for this dimension.
!
            if (interp) then
                call Agrif_get_var_bounds(parent, lb_child(n), ub_child(n), n)
                nb_child(n) = parent % ub(n) - parent % lb(n)
            else
                call Agrif_get_var_bounds(child,  lb_child(n), ub_child(n), n)
                nb_child(n) = child % ub(n) - child % lb(n)
            endif
!
!           No interpolation but only a copy of the values of the grid variable
            lb_parent(n) = lb_child(n)
            s_child(n)   = 0.
            s_parent(n)  = 0.
            ds_child(n)  = 1.
            ds_parent(n) = 1.
!
        end select
!
    enddo
!---------------------------------------------------------------------------------------------------
end subroutine PreProcessToInterpOrUpdate
!===================================================================================================
!
#if defined AGRIF_MPI
!===================================================================================================
!   subroutine Agrif_GetLocalBoundaries
!---------------------------------------------------------------------------------------------------
subroutine Agrif_GetLocalBoundaries ( tab1, tab2, coord, lb, ub, deb, fin )
!---------------------------------------------------------------------------------------------------
    integer, intent(in)     ::  tab1
    integer, intent(in)     ::  tab2
    integer, intent(in)     ::  coord
    integer, intent(in)     ::  lb, ub
    integer, intent(out)    ::  deb, fin
!
    integer :: imin, imax
    integer :: i1, i2
!
    call Agrif_InvLoc(lb, AGRIF_ProcRank, coord, imin)
    call Agrif_InvLoc(ub, AGRIF_ProcRank, coord, imax)
!
    if ( imin > tab2 ) then
        i1 = imax - imin
    else
        i1 = max(tab1 - imin,0)
    endif
!
    if (imax < tab1) then
        i2 = -(imax - imin)
    else
        i2 = min(tab2 - imax,0)
    endif
!
    deb = lb + i1
    fin = ub + i2
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_GetLocalBoundaries
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_GlobalToLocalBounds
!
!> For a global index located on the current processor, tabarray gives the corresponding local index
!---------------------------------------------------------------------------------------------------
subroutine Agrif_GlobalToLocalBounds ( locbounds, lb_var, ub_var, lb_glob, ub_glob,    &
                                       coords, nbdim, rank, member )
!---------------------------------------------------------------------------------------------------
    integer, dimension(nbdim,2,2), intent(out)   :: locbounds   !< Local values of \b lb_glob and \b ub_glob
    integer, dimension(nbdim),     intent(in)    :: lb_var      !< Local lower boundary on the current processor
    integer, dimension(nbdim),     intent(in)    :: ub_var      !< Local upper boundary on the current processor
    integer, dimension(nbdim),     intent(in)    :: lb_glob     !< Global lower boundary
    integer, dimension(nbdim),     intent(in)    :: ub_glob     !< Global upper boundary
    integer, dimension(nbdim),     intent(in)    :: coords
    integer,                       intent(in)    :: nbdim       !< Dimension of the array
    integer,                       intent(in)    :: rank        !< Rank of the processor
    logical,                       intent(out)   :: member
!
    integer     :: i, i1, k
    integer     :: nbloc(nbdim)
!
    locbounds(:,1,:) =  HUGE(1)
    locbounds(:,2,:) = -HUGE(1)
!
    nbloc = 0
!
    do i = 1,nbdim
!
     if (coords(i) == 0) then
       nbloc(i) = 1
       locbounds(i,1,1) = lb_glob(i)
       locbounds(i,2,1) = ub_glob(i)
       locbounds(i,1,2) = lb_glob(i)
       locbounds(i,2,2) = ub_glob(i)
     else
        call Agrif_InvLoc(lb_var(i), rank, coords(i), i1)
!
        do k = lb_glob(i)+lb_var(i)-i1,ub_glob(i)+lb_var(i)-i1
!
            if ( (k >= lb_var(i)) .AND. (k <= ub_var(i)) ) then
                nbloc(i) = 1
                locbounds(i,1,1) = min(locbounds(i,1,1),k-lb_var(i)+i1)
                locbounds(i,2,1) = max(locbounds(i,2,1),k-lb_var(i)+i1)

                locbounds(i,1,2) = min(locbounds(i,1,2),k)
                locbounds(i,2,2) = max(locbounds(i,2,2),k)
            endif
        enddo
     endif
    enddo

    member = ( sum(nbloc) == nbdim )
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_GlobalToLocalBounds
!===================================================================================================
#endif
!
end module Agrif_Arrays
