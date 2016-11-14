!
! $Id: modsauv.F 662 2007-05-25 15:58:52Z opalod $
!
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
!     Foundation, Inc., 59 Temple Place- Suite 330, Boston, MA 02111-1307, USA.
!
!
!> Module Agrif_Save
!!
!! Module for the initialization by copy of the grids created by clustering.
!
module Agrif_Save
!
    use Agrif_Types
    use Agrif_Link
    use Agrif_Arrays
    use Agrif_Variables
!
    implicit none
!
contains
!
!===================================================================================================
! subroutine Agrif_deallocate_Arrays
!---------------------------------------------------------------------------------------------------
subroutine Agrif_deallocate_Arrays ( var )
!---------------------------------------------------------------------------------------------------
    type(Agrif_Variable), pointer :: var
!
    if (allocated(var%array1))  deallocate(var%array1)
    if (allocated(var%array2))  deallocate(var%array2)
    if (allocated(var%array3))  deallocate(var%array3)
    if (allocated(var%array4))  deallocate(var%array4)
    if (allocated(var%array5))  deallocate(var%array5)
    if (allocated(var%array6))  deallocate(var%array6)
!
    if (allocated(var%darray1)) deallocate(var%darray1)
    if (allocated(var%darray2)) deallocate(var%darray2)
    if (allocated(var%darray3)) deallocate(var%darray3)
    if (allocated(var%darray4)) deallocate(var%darray4)
    if (allocated(var%darray5)) deallocate(var%darray5)
    if (allocated(var%darray6)) deallocate(var%darray6)
!
    if (allocated(var%sarray1)) deallocate(var%sarray1)
    if (allocated(var%sarray2)) deallocate(var%sarray2)
    if (allocated(var%sarray3)) deallocate(var%sarray3)
    if (allocated(var%sarray4)) deallocate(var%sarray4)
    if (allocated(var%sarray5)) deallocate(var%sarray5)
    if (allocated(var%sarray6)) deallocate(var%sarray6)
!
!
    if (associated(var%oldvalues2D))    deallocate(var%oldvalues2D)
    if (allocated(var%posvar))          deallocate(var%posvar)
    if (allocated(var%interptab))       deallocate(var%interptab)
    if (allocated(var%coords))          deallocate(var%coords)
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_deallocate_Arrays
!---------------------------------------------------------------------------------------------------
subroutine Agrif_deallocate_Arrays_c ( var_c )
!---------------------------------------------------------------------------------------------------
    type(Agrif_Variable_c), pointer :: var_c
!
    if (allocated(var_c%carray1)) deallocate(var_c%carray1)
    if (allocated(var_C%carray2)) deallocate(var_c%carray2)
!
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_deallocate_Arrays_c
!===================================================================================================
!---------------------------------------------------------------------------------------------------
subroutine Agrif_deallocate_Arrays_l ( var_l )
!---------------------------------------------------------------------------------------------------
    type(Agrif_Variable_l), pointer :: var_l
!
!
    if (allocated(var_l%larray1)) deallocate(var_l%larray1)
    if (allocated(var_l%larray2)) deallocate(var_l%larray2)
    if (allocated(var_l%larray3)) deallocate(var_l%larray3)
    if (allocated(var_l%larray4)) deallocate(var_l%larray4)
    if (allocated(var_l%larray5)) deallocate(var_l%larray5)
    if (allocated(var_l%larray6)) deallocate(var_l%larray6)
!
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_deallocate_Arrays_l
!---------------------------------------------------------------------------------------------------
subroutine Agrif_deallocate_Arrays_i ( var_i )
!---------------------------------------------------------------------------------------------------
    type(Agrif_Variable_i), pointer :: var_i
!
!
    if (allocated(var_i%iarray1)) deallocate(var_i%iarray1)
    if (allocated(var_i%iarray2)) deallocate(var_i%iarray2)
    if (allocated(var_i%iarray3)) deallocate(var_i%iarray3)
    if (allocated(var_i%iarray4)) deallocate(var_i%iarray4)
    if (allocated(var_i%iarray5)) deallocate(var_i%iarray5)
    if (allocated(var_i%iarray6)) deallocate(var_i%iarray6)
!
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_deallocate_Arrays_i
!
!===================================================================================================
!  subroutine Agrif_Free_data_before
!---------------------------------------------------------------------------------------------------
subroutine Agrif_Free_data_before ( Agrif_Gr )
!---------------------------------------------------------------------------------------------------
    type(Agrif_Grid), pointer   :: Agrif_Gr ! Pointer on the current grid
!
    integer :: i
    type(Agrif_Variables_List), pointer :: parcours
    type(Agrif_Variable),       pointer :: var
    type(Agrif_Variable_c),     pointer :: var_c
    type(Agrif_Variable_r),     pointer :: var_r
    type(Agrif_Variable_l),     pointer :: var_l
    type(Agrif_Variable_i),     pointer :: var_i
!
    do i = 1,Agrif_NbVariables(0)
!
        var => Agrif_Gr % tabvars(i)
!
        if ( .NOT. Agrif_Mygrid % tabvars(i) % restore ) then
            call Agrif_deallocate_Arrays(var)
        endif
!
        if (associated(var%list_interp)) then
            call Agrif_Free_list_interp(var%list_interp)
        endif
!
    enddo
    do i=1,Agrif_NbVariables(1)
        var_c => Agrif_Gr % tabvars_c(i)
        call Agrif_deallocate_Arrays_c(var_c)
    enddo
    do i=1,Agrif_NbVariables(3)
        var_l => Agrif_Gr % tabvars_l(i)
        call Agrif_deallocate_Arrays_l(var_l)
    enddo
    do i=1,Agrif_NbVariables(4)
        var_i => Agrif_Gr % tabvars_i(i)
        call Agrif_deallocate_Arrays_i(var_i)
    enddo

    parcours => Agrif_Gr % variables

    do i = 1,Agrif_Gr%NbVariables
!
        if ( .NOT. parcours%var%root_var % restore ) then
            call Agrif_deallocate_Arrays(parcours%var)
        endif
!
        if (associated(parcours%var%list_interp)) then
            call Agrif_Free_list_interp(parcours%var%list_interp)
        endif
!
        if ( .NOT. parcours%var%root_var % restore ) then
            deallocate(parcours%var)
        endif
!
        parcours => parcours % next
!
    enddo
!
    if ( Agrif_USE_ONLY_FIXED_GRIDS == 0 ) then
        if ( Agrif_Probdim == 1 ) deallocate(Agrif_Gr%tabpoint1D)
        if ( Agrif_Probdim == 2 ) deallocate(Agrif_Gr%tabpoint2D)
        if ( Agrif_Probdim == 3 ) deallocate(Agrif_Gr%tabpoint3D)
    endif
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Free_data_before
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_Free_list_interp
!---------------------------------------------------------------------------------------------------
recursive subroutine Agrif_Free_list_interp ( list_interp )
!---------------------------------------------------------------------------------------------------
      type(Agrif_List_Interp_Loc), pointer :: list_interp
!
      if (associated(list_interp%suiv)) call Agrif_Free_list_interp(list_interp%suiv)

#if defined AGRIF_MPI
       deallocate(list_interp%interp_loc%tab4t)
       deallocate(list_interp%interp_loc%memberinall)
       deallocate(list_interp%interp_loc%sendtoproc1)
       deallocate(list_interp%interp_loc%recvfromproc1)
#endif
       deallocate(list_interp%interp_loc)
       deallocate(list_interp)
       nullify(list_interp)
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Free_list_interp
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_Free_data_after
!---------------------------------------------------------------------------------------------------
subroutine Agrif_Free_data_after ( Agrif_Gr )
!---------------------------------------------------------------------------------------------------
    type(Agrif_Grid), pointer   :: Agrif_Gr  !< Pointer on the current grid
!
    integer :: i
    type(Agrif_Variables_List), pointer :: parcours, rootparcours
    type(Agrif_Variable),       pointer :: var
!
    do i = 1,Agrif_NbVariables(0)
        if ( Agrif_Mygrid % tabvars(i) % restore ) then
            var => Agrif_Gr%tabvars(i)
            call Agrif_deallocate_Arrays(var)
        endif
    enddo
!
    parcours => Agrif_Gr%variables
    rootparcours => Agrif_Mygrid%variables
!
    do i = 1,Agrif_Gr%NbVariables
        if (rootparcours%var % restore ) then
            call Agrif_deallocate_Arrays(parcours%var)
            deallocate(parcours%var)
        endif
        parcours => parcours % next
        rootparcours => rootparcours % next
    enddo
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Free_data_after
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_CopyFromOld_All
!
!> Called in Agrif_Clustering#Agrif_Init_Hierarchy.
!---------------------------------------------------------------------------------------------------
recursive subroutine Agrif_CopyFromOld_All ( g, oldchildlist )
!---------------------------------------------------------------------------------------------------
    type(Agrif_Grid), pointer, intent(inout) :: g   !< Pointer on the current grid
    type(Agrif_Grid_List),     intent(in)    :: oldchildlist
!
    type(Agrif_PGrid), pointer  :: parcours ! Pointer for the recursive procedure
    real    :: g_eps, eps, oldgrid_eps
    integer :: out
    integer :: iii
!
    out = 0
!
    parcours => oldchildlist % first
!
    do while (associated(parcours))
!
        if ((.NOT. g % fixed) .AND. (parcours % gr %oldgrid)) then
!
            g_eps = huge(1.)
            oldgrid_eps = huge(1.)
            do iii = 1,Agrif_Probdim
                g_eps = min(g_eps,g % Agrif_dx(iii))
                oldgrid_eps = min(oldgrid_eps, parcours % gr % Agrif_dx(iii))
            enddo
!
            eps = min(g_eps,oldgrid_eps)/100.
!
            do iii = 1,Agrif_Probdim
                if (g % Agrif_dx(iii) < (parcours % gr % Agrif_dx(iii) - eps)) then
                    parcours => parcours % next
                    out = 1
                    exit
                endif
            enddo
!
            if ( out == 1 ) cycle
!
            call Agrif_CopyFromOld(g,parcours%gr)
!
        endif
!
        call Agrif_CopyFromOld_All(g,parcours % gr % child_list)
!
        parcours => parcours % next
!
      enddo
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_CopyFromOld_All
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_CopyFromOld
!
!> Call to the Agrif_Copy procedure.
!---------------------------------------------------------------------------------------------------
subroutine Agrif_CopyFromOld ( new_gr, old_gr )
!---------------------------------------------------------------------------------------------------
    type(Agrif_Grid), pointer, intent(inout) :: new_gr  !< Pointer on the current grid
    type(Agrif_Grid), pointer, intent(inout) :: old_gr  !< Pointer on an old grid
!
    type(Agrif_Variable), pointer   :: new_var
    type(Agrif_Variable), pointer   :: old_var
    integer :: i
!
    do i = 1,Agrif_NbVariables(0)
        if ( Agrif_Mygrid % tabvars(i) % restore ) then
            old_var => old_gr % tabvars(i)
            new_var => new_gr % tabvars(i)
            call Agrif_Copy(new_gr, old_gr, new_var, old_var )
        endif
    enddo
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_CopyFromOld
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_CopyFromOld_AllOneVar
!
!> Called in Agrif_Clustering#Agrif_Init_Hierarchy.
!---------------------------------------------------------------------------------------------------
recursive subroutine Agrif_CopyFromOld_AllOneVar ( g, oldchildlist, indic )
!---------------------------------------------------------------------------------------------------
    type(Agrif_Grid), pointer, intent(inout) :: g            !< Pointer on the current grid
    type(Agrif_Grid_List),     intent(in)    :: oldchildlist
    integer,                   intent(in)    :: indic
!
    type(Agrif_PGrid), pointer  :: parcours ! Pointer for the recursive procedure
    real    :: g_eps,eps,oldgrid_eps
    integer :: out
    integer :: iii
!
    out = 0
!
    parcours => oldchildlist % first
!
    do while (associated(parcours))
!
        if ((.NOT. g % fixed) .AND. (parcours % gr %oldgrid)) then
!
            g_eps = huge(1.)
            oldgrid_eps = huge(1.)
            do iii = 1,Agrif_Probdim
                g_eps = min(g_eps,g % Agrif_dx(iii))
                oldgrid_eps = min(oldgrid_eps,parcours % gr % Agrif_dx(iii))
            enddo
!
            eps = min(g_eps,oldgrid_eps)/100.
!
            do iii = 1,Agrif_Probdim
                if (g % Agrif_dx(iii) < (parcours % gr % Agrif_dx(iii) - eps)) then
                    parcours => parcours % next
                    out = 1
                    exit
                endif
            enddo

            if ( out == 1 ) cycle
!
            call Agrif_CopyFromOldOneVar(g,parcours%gr,indic)
!
        endif
!
        call Agrif_CopyFromOld_AllOneVar(g,parcours%gr % child_list,indic)
!
        parcours => parcours % next
!
    enddo
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_CopyFromOld_AllOneVar
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_CopyFromOldOneVar
!
!> Call to Agrif_Copy
!---------------------------------------------------------------------------------------------------
subroutine Agrif_CopyFromOldOneVar ( new_gr, old_gr, indic )
!---------------------------------------------------------------------------------------------------
    type(Agrif_Grid), pointer, intent(inout) :: new_gr     !< Pointer on the current grid
    type(Agrif_Grid), pointer, intent(in)    :: old_gr     !< Pointer on an old grid
    integer,                   intent(in)    :: indic
!
    type(Agrif_Variable),  pointer  :: new_var, old_var
!
    new_var => Agrif_Search_Variable(new_gr, -indic)
    old_var => Agrif_Search_Variable(old_gr, -indic)
!
    call Agrif_array_allocate(new_var,new_var%lb,new_var%ub,new_var%nbdim)
    call Agrif_Copy(new_gr, old_gr, new_var,old_var)
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_CopyFromOldOneVar
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_Copy
!---------------------------------------------------------------------------------------------------
subroutine Agrif_Copy ( new_gr, old_gr, new_var, old_var )
!---------------------------------------------------------------------------------------------------
    type(Agrif_Grid),     pointer, intent(in)       :: new_gr !< Pointer on the current grid
    type(Agrif_Grid),     pointer, intent(in)       :: old_gr !< Pointer on an old grid
    type(Agrif_Variable), pointer, intent(inout)    :: new_var
    type(Agrif_Variable), pointer, intent(in)       :: old_var
!
    type(Agrif_Variable), pointer :: root ! Pointer on the variable of the root grid
    integer               :: nbdim     ! Number of dimensions of the current grid
    integer, dimension(6) :: pttabnew  ! Indexes of the first point in the domain
    integer, dimension(6) :: petabnew  ! Indexes of the first point in the domain
    integer, dimension(6) :: pttabold  ! Indexes of the first point in the domain
    integer, dimension(6) :: petabold  ! Indexes of the first point in the domain
    integer, dimension(6) :: nbtabold  ! Number of cells in each direction
    integer, dimension(6) :: nbtabnew  ! Number of cells in each direction
    real,    dimension(6) :: snew,sold
    real,    dimension(6) :: dsnew,dsold
    real    :: eps
    integer :: n
!
    root => new_var % root_var
    nbdim = root % nbdim
!
    do n = 1,nbdim
!
        select case(root % interptab(n))
!
        case('x')
!
            pttabnew(n) = root % point(n)
            pttabold(n) = root % point(n)
            snew(n)  = new_gr % Agrif_x(1)
            sold(n)  = old_gr % Agrif_x(1)
            dsnew(n) = new_gr % Agrif_dx(1)
            dsold(n) = old_gr % Agrif_dx(1)
!
            if (root % posvar(n) == 1) then
                petabnew(n) = pttabnew(n) + new_gr % nb(1)
                petabold(n) = pttabold(n) + old_gr % nb(1)
            else
                petabnew(n) = pttabnew(n) + new_gr % nb(1) - 1
                petabold(n) = pttabold(n) + old_gr % nb(1) - 1
                snew(n) = snew(n) + dsnew(n)/2.
                sold(n) = sold(n) + dsold(n)/2.
            endif
!
        case('y')
!
            pttabnew(n) = root % point(n)
            pttabold(n) = root % point(n)
            snew(n) = new_gr % Agrif_x(2)
            sold(n) = old_gr % Agrif_x(2)
            dsnew(n) = new_gr % Agrif_dx(2)
            dsold(n) = old_gr % Agrif_dx(2)
!
            if (root % posvar(n) == 1) then
                petabnew(n) = pttabnew(n) + new_gr % nb(2)
                petabold(n) = pttabold(n) + old_gr % nb(2)
            else
                petabnew(n) = pttabnew(n) + new_gr % nb(2) - 1
                petabold(n) = pttabold(n) + old_gr % nb(2) - 1
                snew(n) = snew(n) + dsnew(n)/2.
                sold(n) = sold(n) + dsold(n)/2.
            endif
!
        case('z')
!
            pttabnew(n) = root % point(n)
            pttabold(n) = root % point(n)
            snew(n)  = new_gr % Agrif_x(3)
            sold(n)  = old_gr % Agrif_x(3)
            dsnew(n) = new_gr % Agrif_dx(3)
            dsold(n) = old_gr % Agrif_dx(3)
!
            if (root % posvar(n) == 1) then
                petabnew(n) = pttabnew(n) + new_gr % nb(3)
                petabold(n) = pttabold(n) + old_gr % nb(3)
            else
                petabnew(n) = pttabnew(n) + new_gr % nb(3) - 1
                petabold(n) = pttabold(n) + old_gr % nb(3) - 1
                snew(n) = snew(n) + dsnew(n)/2.
                sold(n) = sold(n) + dsold(n)/2.
            endif
!
        case('N')
!
            call Agrif_get_var_bounds(new_var,pttabnew(n),petabnew(n),n)
!
            pttabold(n) = pttabnew(n)
            petabold(n) = petabnew(n)
            snew(n) = 0.
            sold(n) = 0.
            dsnew(n) = 1.
            dsold(n) = 1.
!
        end select
!
    enddo
!
    do n = 1,nbdim
        nbtabnew(n) = petabnew(n) - pttabnew(n)
        nbtabold(n) = petabold(n) - pttabold(n)
    enddo
!
    eps = min(minval(dsnew(1:nbdim)),minval(dsold(1:nbdim))) / 100.
!
    do n = 1,nbdim
        if (snew(n) > (sold(n)+dsold(n)*nbtabold(n)+eps)) return
        if ((snew(n)+dsnew(n)*nbtabnew(n)-eps) < sold(n)) return
    enddo
!
    call Agrif_CopynD(new_var,old_var,pttabold,petabold,pttabnew,petabnew,  &
                      sold,snew,dsold,dsnew,nbdim)
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Copy
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_CopynD
!
!> Copy from the nD variable Old_Var the nD variable New_Var
!---------------------------------------------------------------------------------------------------
subroutine Agrif_CopynD ( new_var, old_var, pttabold, petabold, pttabnew, petabnew, &
                          sold, snew, dsold, dsnew, nbdim )
!---------------------------------------------------------------------------------------------------
    type(Agrif_Variable), pointer, intent(inout) :: new_var
    type(Agrif_Variable), pointer, intent(in)    :: old_var
    integer, dimension(nbdim),     intent(in)    :: pttabnew
    integer, dimension(nbdim),     intent(in)    :: petabnew
    integer, dimension(nbdim),     intent(in)    :: pttabold
    integer, dimension(nbdim),     intent(in)    :: petabold
    real,    dimension(nbdim),     intent(in)    :: snew, sold
    real,    dimension(nbdim),     intent(in)    :: dsnew,dsold
    integer,                       intent(in)    :: nbdim
!
    integer :: i,j,k,l,m,n,i0,j0,k0,l0,m0,n0
!
    real,    dimension(nbdim) :: dim_gmin,   dim_gmax
    real,    dimension(nbdim) :: dim_newmin, dim_newmax
    real,    dimension(nbdim) :: dim_min
    integer, dimension(nbdim) :: ind_gmin,ind_newmin, ind_newmax
!
    do i = 1,nbdim
!
        dim_gmin(i) = sold(i)
        dim_gmax(i) = sold(i) + (petabold(i)-pttabold(i)) * dsold(i)
!
        dim_newmin(i) = snew(i)
        dim_newmax(i) = snew(i) + (petabnew(i)-pttabnew(i)) * dsnew(i)
!
    enddo
!
    do i = 1,nbdim
        if (dim_gmax(i) < dim_newmin(i)) return
        if (dim_gmin(i) > dim_newmax(i)) return
    enddo
!
    do i = 1,nbdim
!
        ind_newmin(i) = pttabnew(i) - floor(-(max(dim_gmin(i),dim_newmin(i))-dim_newmin(i))/dsnew(i))
        dim_min(i) = snew(i) + (ind_newmin(i)-pttabnew(i))*dsnew(i)
        ind_gmin(i)   = pttabold(i) + nint((dim_min(i)-dim_gmin(i))/dsold(i))
        ind_newmax(i) = pttabnew(i) + int((min(dim_gmax(i),dim_newmax(i))-dim_newmin(i))/dsnew(i))
!
    enddo
!
    select case (nbdim)
!
    case (1)
        i0 = ind_gmin(1)
        do i = ind_newmin(1),ind_newmax(1)
            new_var % array1(i)    = old_var % array1(i0)
            new_var % restore1D(i) = 1
            i0 = i0 + int(dsnew(1)/dsold(1))
        enddo
!
    case (2)
        i0 = ind_gmin(1) ; do i = ind_newmin(1),ind_newmax(1)
        j0 = ind_gmin(2) ; do j = ind_newmin(2),ind_newmax(2)
            new_var % array2(i,j)    = old_var % array2(i0,j0)
            new_var % restore2D(i,j) = 1
            j0 = j0 + int(dsnew(2)/dsold(2))
        enddo
        i0 = i0 + int(dsnew(1)/dsold(1))
        enddo
!
    case (3)
        i0 = ind_gmin(1) ; do i = ind_newmin(1),ind_newmax(1)
        j0 = ind_gmin(2) ; do j = ind_newmin(2),ind_newmax(2)
        k0 = ind_gmin(3) ; do k = ind_newmin(3),ind_newmax(3)
            new_var % array3(i,j,k)    =  old_var % array3(i0,j0,k0)
            new_var % restore3D(i,j,k) = 1
            k0 = k0 + int(dsnew(3)/dsold(3))
        enddo
        j0 = j0 + int(dsnew(2)/dsold(2))
        enddo
        i0 = i0 + int(dsnew(1)/dsold(1))
        enddo
!
    case (4)
        i0 = ind_gmin(1) ; do i = ind_newmin(1),ind_newmax(1)
        j0 = ind_gmin(2) ; do j = ind_newmin(2),ind_newmax(2)
        k0 = ind_gmin(3) ; do k = ind_newmin(3),ind_newmax(3)
        l0 = ind_gmin(4) ; do l = ind_newmin(4),ind_newmax(4)
            new_var % array4(i,j,k,l)    = old_var % array4(i0,j0,k0,l0)
            new_var % restore4D(i,j,k,l) = 1
            l0 = l0 + int(dsnew(4)/dsold(4))
        enddo
        k0 = k0 + int(dsnew(3)/dsold(3))
        enddo
        j0 = j0 + int(dsnew(2)/dsold(2))
        enddo
        i0 = i0 + int(dsnew(1)/dsold(1))
        enddo
!
    case (5)
        i0 = ind_gmin(1) ; do i = ind_newmin(1),ind_newmax(1)
        j0 = ind_gmin(2) ; do j = ind_newmin(2),ind_newmax(2)
        k0 = ind_gmin(3) ; do k = ind_newmin(3),ind_newmax(3)
        l0 = ind_gmin(4) ; do l = ind_newmin(4),ind_newmax(4)
        m0 = ind_gmin(5) ; do m = ind_newmin(5),ind_newmax(5)
            new_var % array5(i,j,k,l,m)    = old_var % array5(i0,j0,k0,l0,m0)
            new_var % restore5D(i,j,k,l,m) = 1
            m0 = m0 + int(dsnew(5)/dsold(5))
        enddo
        l0 = l0 + int(dsnew(4)/dsold(4))
        enddo
        k0 = k0 + int(dsnew(3)/dsold(3))
        enddo
        j0 = j0 + int(dsnew(2)/dsold(2))
        enddo
        i0 = i0 + int(dsnew(1)/dsold(1))
        enddo
!
    case (6)
        i0 = ind_gmin(1) ; do i = ind_newmin(1),ind_newmax(1)
        j0 = ind_gmin(2) ; do j = ind_newmin(2),ind_newmax(2)
        k0 = ind_gmin(3) ; do k = ind_newmin(3),ind_newmax(3)
        l0 = ind_gmin(4) ; do l = ind_newmin(4),ind_newmax(4)
        m0 = ind_gmin(5) ; do m = ind_newmin(5),ind_newmax(5)
        n0 = ind_gmin(6) ; do n = ind_newmin(6),ind_newmax(6)
            new_var % array6(i,j,k,l,m,n)    = old_var % array6(i0,j0,k0,l0,m0,n0)
            new_var % restore6D(i,j,k,l,m,n) = 1
            n0 = n0 + int(dsnew(6)/dsold(6))
        enddo
        m0 = m0 + int(dsnew(5)/dsold(5))
        enddo
        l0 = l0 + int(dsnew(4)/dsold(4))
        enddo
        k0 = k0 + int(dsnew(3)/dsold(3))
        enddo
        j0 = j0 + int(dsnew(2)/dsold(2))
        enddo
        i0 = i0 + int(dsnew(1)/dsold(1))
        enddo
!
      end select
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_CopynD
!===================================================================================================
!
end module Agrif_Save
