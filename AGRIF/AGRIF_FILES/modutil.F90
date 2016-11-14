!
! $Id: modutil.F 662 2007-05-25 15:58:52Z opalod $
!
!     Agrif (Adaptive Grid Refinement In Fortran)
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
!> Module Agrif_Util
!!
!! This module contains the two procedures called in the main program :
!! - #Agrif_Init_Grids allows the initialization of the root coarse grid
!! - #Agrif_Step allows the creation of the grid hierarchy and the management of the time integration.
!
module Agrif_Util
!
    use Agrif_Clustering
    use Agrif_BcFunction
    use Agrif_seq
!
    implicit none
!
    abstract interface
        subroutine step_proc()
        end subroutine step_proc
    end interface
!
contains
!
!===================================================================================================
!  subroutine Agrif_Step
!
!> Creates the grid hierarchy and manages the time integration procedure.
!> It is called in the main program.
!> Calls subroutines #Agrif_Regrid and #Agrif_Integrate.
!---------------------------------------------------------------------------------------------------
subroutine Agrif_Step ( procname )
!---------------------------------------------------------------------------------------------------
    procedure(step_proc)    :: procname     !< subroutine to call on each grid
    type(agrif_grid), pointer :: ref_grid
!
! Set the clustering variables
    call Agrif_clustering_def()
!
! Creation and initialization of the grid hierarchy
    if ( Agrif_USE_ONLY_FIXED_GRIDS == 1 ) then
!
        if ( (Agrif_Mygrid % ngridstep == 0) .AND. (.not. Agrif_regrid_has_been_done) ) then
            call Agrif_Regrid()
            Agrif_regrid_has_been_done = .TRUE.
        endif
!
    else
!
        if (mod(Agrif_Mygrid % ngridstep,Agrif_Regridding) == 0) then
            call Agrif_Regrid()
        endif
!
    endif
!
! Time integration of the grid hierarchy
    if (agrif_coarse) then
      ref_grid => agrif_coarsegrid
    else
      ref_grid => agrif_mygrid
    endif
    if ( Agrif_Parallel_sisters ) then
        call Agrif_Integrate_Parallel(ref_grid,procname)
    else
        call Agrif_Integrate(ref_grid,procname)
    endif
!
    if ( ref_grid%child_list%nitems > 0 ) call Agrif_Instance(ref_grid)
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Step
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_Step_Child
!
!> Apply 'procname' to each grid of the hierarchy
!---------------------------------------------------------------------------------------------------
subroutine Agrif_Step_Child ( procname )
!---------------------------------------------------------------------------------------------------
    procedure(step_proc)    :: procname     !< subroutine to call on each grid
!
    if ( Agrif_Parallel_sisters ) then
        call Agrif_Integrate_Child_Parallel(Agrif_Mygrid,procname)
    else
        call Agrif_Integrate_Child(Agrif_Mygrid,procname)
    endif
!
    if ( Agrif_Mygrid%child_list%nitems > 0 ) call Agrif_Instance(Agrif_Mygrid)
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Step_Child
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_Step_Childs
!
!> Apply 'procname' to each child grids of the current grid
!---------------------------------------------------------------------------------------------------
!     **************************************************************************
!!!   Subroutine Agrif_Step_Childs
!     **************************************************************************
!
      Subroutine Agrif_Step_Childs(procname)
!
    procedure(step_proc)    :: procname     !< subroutine to call on each grid
!     Pointer argument
      Type(Agrif_Grid),pointer   :: g        ! Pointer on the current grid
!

!
!     Local pointer
      Type(Agrif_pgrid),pointer  :: parcours ! Pointer for the recursive
                                             ! procedure
!
      g => Agrif_Curgrid
      
      parcours => g % child_list % first
!
!     Recursive procedure for the time integration of the grid hierarchy      
      Do while (associated(parcours))
!
!       Instanciation of the variables of the current grid
        Call Agrif_Instance(parcours % gr)

!     One step on the current grid

         Call procname ()
        parcours => parcours % next
      enddo
   
      If (associated(g % child_list % first)) Call Agrif_Instance (g)
      Return
      End Subroutine Agrif_Step_Childs
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_Regrid
!
!> Creates the grid hierarchy from fixed grids and adaptive mesh refinement.
!---------------------------------------------------------------------------------------------------
subroutine Agrif_Regrid ( procname )
!---------------------------------------------------------------------------------------------------
    procedure(init_proc), optional    :: procname     !< Initialisation subroutine (Default: Agrif_InitValues)
!
    type(Agrif_Rectangle), pointer     :: coarsegrid_fixed
    type(Agrif_Rectangle), pointer     :: coarsegrid_moving
    integer                            :: i, j
    integer :: nunit
    logical :: BEXIST
    TYPE(Agrif_Rectangle)            :: newrect    ! Pointer on a new grid
    integer :: is_coarse, rhox, rhoy, rhoz, rhot
!
    if ( Agrif_USE_ONLY_FIXED_GRIDS == 0 ) &
        call Agrif_detect_all(Agrif_Mygrid)     ! Detection of areas to be refined
!
    allocate(coarsegrid_fixed)
    allocate(coarsegrid_moving)
!
    if ( Agrif_USE_ONLY_FIXED_GRIDS == 0 ) &
        call Agrif_Cluster_All(Agrif_Mygrid,coarsegrid_moving) ! Clustering
!
    if ( Agrif_USE_FIXED_GRIDS == 1 .OR. Agrif_USE_ONLY_FIXED_GRIDS == 1 ) then
!
        if (Agrif_Mygrid % ngridstep == 0) then
!            
            nunit = Agrif_Get_Unit()
            open(nunit, file='AGRIF_FixedGrids.in', form='formatted', status="old", ERR=99)
            if (agrif_coarse) then ! SKIP the coarse grid declaration
                if (Agrif_Probdim == 3) then
                    read(nunit,*) is_coarse, rhox, rhoy, rhoz, rhot
                elseif (Agrif_Probdim == 2) then
                    read(nunit,*) is_coarse, rhox, rhoy, rhot
                elseif (Agrif_Probdim == 2) then
                    read(nunit,*) is_coarse, rhox, rhot
                endif
            endif
!           Creation of the grid hierarchy from the Agrif_FixedGrids.in file
            do i = 1,Agrif_Probdim
                coarsegrid_fixed % imin(i) = 1
                coarsegrid_fixed % imax(i) = Agrif_Mygrid % nb(i) + 1
            enddo
            j = 1
            call Agrif_Read_Fix_Grd(coarsegrid_fixed,j,nunit)
            close(nunit)
!
            call Agrif_gl_clear(Agrif_oldmygrid)
!
!           Creation of the grid hierarchy from coarsegrid_fixed
            call Agrif_Create_Grids(Agrif_Mygrid,coarsegrid_fixed)
            
        else
            call Agrif_gl_copy(Agrif_oldmygrid, Agrif_Mygrid % child_list)
        endif
    else
        call Agrif_gl_copy(Agrif_oldmygrid, Agrif_Mygrid % child_list)
        call Agrif_gl_clear(Agrif_Mygrid % child_list)
    endif
!
    if ( Agrif_USE_ONLY_FIXED_GRIDS == 0 ) then
!
        call Agrif_Save_All(Agrif_oldmygrid)
        call Agrif_Free_before_All(Agrif_oldmygrid)
!
!       Creation of the grid hierarchy from coarsegrid_moving
        call Agrif_Create_Grids(Agrif_Mygrid,coarsegrid_moving)
!
    endif
!
!   Initialization of the grid hierarchy by copy or interpolation
!
#if defined AGRIF_MPI
    if ( Agrif_Parallel_sisters ) then
        call Agrif_Init_Hierarchy_Parallel_1(Agrif_Mygrid)
        call Agrif_Init_Hierarchy_Parallel_2(Agrif_Mygrid,procname)
    else
        call Agrif_Init_Hierarchy(Agrif_Mygrid,procname)
    endif
#else
        call Agrif_Init_Hierarchy(Agrif_Mygrid,procname)
#endif
!
    if ( Agrif_USE_ONLY_FIXED_GRIDS == 0 ) call Agrif_Free_after_All(Agrif_oldmygrid)
!
    Agrif_regrid_has_been_done = .TRUE.
!
    call Agrif_Instance( Agrif_Mygrid )
!
    deallocate(coarsegrid_fixed)
    deallocate(coarsegrid_moving)
!
    return
!
!     Opening error
!
99  INQUIRE(FILE='AGRIF_FixedGrids.in',EXIST=BEXIST)
    if (.not. BEXIST) then
        print*,'ERROR : File AGRIF_FixedGrids.in not found.'
        STOP
    else
        print*,'Error opening file AGRIF_FixedGrids.in'
        STOP
    endif
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Regrid
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_detect_All
!
!> Detects areas to be refined.
!---------------------------------------------------------------------------------------------------
recursive subroutine Agrif_detect_all ( g )
!---------------------------------------------------------------------------------------------------
    TYPE(Agrif_Grid),  pointer  :: g        !< Pointer on the current grid
!
    Type(Agrif_PGrid), pointer  :: parcours ! Pointer for the recursive procedure
    integer, DIMENSION(3)       :: size
    integer                     :: i
    real                        :: g_eps
!
    parcours => g % child_list % first
!
!   To be positioned on the finer grids of the grid hierarchy
!
    do while (associated(parcours))
        call Agrif_detect_all(parcours % gr)
        parcours => parcours % next
    enddo
!
    g_eps = huge(1.)
    do i = 1,Agrif_Probdim
        g_eps = min(g_eps, g % Agrif_dx(i))
    enddo
!
    g_eps = g_eps / 100.
!
    if ( Agrif_Probdim == 1 ) g%tabpoint1D = 0
    if ( Agrif_Probdim == 2 ) g%tabpoint2D = 0
    if ( Agrif_Probdim == 3 ) g%tabpoint3D = 0
!
    do i = 1,Agrif_Probdim
        if ( g%Agrif_dx(i)/Agrif_coeffref(i) < (Agrif_mind(i)-g_eps) ) return
    enddo
!
    call Agrif_instance(g)
!
!   Detection (Agrif_detect is a users routine)
!
    do i = 1,Agrif_Probdim
        size(i) = g % nb(i) + 1
    enddo
!
    SELECT CASE (Agrif_Probdim)
    CASE (1)
        call Agrif_detect(g%tabpoint1D,size)
    CASE (2)
        call Agrif_detect(g%tabpoint2D,size)
    CASE (3)
        call Agrif_detect(g%tabpoint3D,size)
    END SELECT
!
!   Addition of the areas detected on the child grids
!
    parcours => g % child_list % first
!
    do while (associated(parcours))
        call Agrif_Add_detected_areas(g,parcours % gr)
        parcours => parcours % next
    enddo
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_detect_all
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_Add_detected_areas
!
!> Adds on the parent grid the areas detected on its child grids
!---------------------------------------------------------------------------------------------------
subroutine Agrif_Add_detected_areas ( parentgrid, childgrid )
!---------------------------------------------------------------------------------------------------
    Type(Agrif_Grid), pointer   :: parentgrid
    Type(Agrif_Grid), pointer   :: childgrid
!
    integer :: i,j,k
!
    do i = 1,childgrid%nb(1)+1
        if ( Agrif_Probdim == 1 ) then
            if (childgrid%tabpoint1D(i)==1) then
                parentgrid%tabpoint1D(childgrid%ix(1)+(i-1)/Agrif_Coeffref(1)) = 1
            endif
        else
            do j=1,childgrid%nb(2)+1
                if (Agrif_Probdim==2) then
                    if (childgrid%tabpoint2D(i,j)==1) then
                        parentgrid%tabpoint2D(                       &
                            childgrid%ix(1)+(i-1)/Agrif_Coeffref(1), &
                            childgrid%ix(2)+(j-1)/Agrif_Coeffref(2)) = 1
                    endif
                else
                    do k=1,childgrid%nb(3)+1
                        if (childgrid%tabpoint3D(i,j,k)==1) then
                            parentgrid%tabpoint3D(                       &
                                childgrid%ix(1)+(i-1)/Agrif_Coeffref(1), &
                                childgrid%ix(2)+(j-1)/Agrif_Coeffref(2), &
                                childgrid%ix(3)+(k-1)/Agrif_Coeffref(3)) = 1
                        endif
                    enddo
                endif
            enddo
        endif
    enddo
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Add_detected_areas
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_Free_before_All
!---------------------------------------------------------------------------------------------------
recursive subroutine Agrif_Free_before_All ( gridlist )
!---------------------------------------------------------------------------------------------------
    Type(Agrif_Grid_List), intent(inout)    :: gridlist !< Grid list
!
    Type(Agrif_PGrid), pointer   :: parcours ! Pointer for the recursive procedure
!
    parcours => gridlist % first
!
    do while (associated(parcours))
!
        if (.not. parcours%gr%fixed) then
            call Agrif_Free_data_before(parcours%gr)
            parcours % gr % oldgrid = .TRUE.
        endif
!
        call Agrif_Free_before_all (parcours % gr % child_list)
!
        parcours => parcours % next
!
    enddo
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Free_before_All
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_Save_All
!---------------------------------------------------------------------------------------------------
recursive subroutine Agrif_Save_All ( gridlist )
!---------------------------------------------------------------------------------------------------
    type(Agrif_Grid_List), intent(inout)    :: gridlist !< Grid list
!
    type(Agrif_PGrid), pointer   :: parcours ! Pointer for the recursive procedure
!
    parcours => gridlist % first
!
    do while (associated(parcours))
!
        if (.not. parcours%gr%fixed) then
            call Agrif_Instance(parcours%gr)
            call Agrif_Before_Regridding()
            parcours % gr % oldgrid = .TRUE.
        endif
!
        call Agrif_Save_All(parcours % gr % child_list)
!
        parcours => parcours % next
!
      enddo
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Save_All
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_Free_after_All
!---------------------------------------------------------------------------------------------------
recursive subroutine Agrif_Free_after_All ( gridlist )
!---------------------------------------------------------------------------------------------------
    Type(Agrif_Grid_List), intent(inout)    :: gridlist       !< Grid list to free
!
    Type(Agrif_PGrid), pointer  :: parcours ! Pointer for the recursive proced
    Type(Agrif_PGrid), pointer  :: preparcours
    Type(Agrif_PGrid), pointer  :: preparcoursini
!
    allocate(preparcours)
!
    preparcoursini => preparcours
!
    nullify(preparcours % gr)
!
    preparcours % next => gridlist % first
    parcours => gridlist % first
!
    do while (associated(parcours))
!
        if ( (.NOT. parcours%gr % fixed) .AND. (parcours%gr % oldgrid) ) then
            call Agrif_Free_data_after(parcours%gr)
        endif
!
        call Agrif_Free_after_all( parcours%gr % child_list )
!
        if (parcours % gr % oldgrid) then
            deallocate(parcours % gr)
            preparcours % next => parcours % next
            deallocate(parcours)
            parcours => preparcours % next
        else
            preparcours => preparcours % next
            parcours => parcours % next
        endif
!
    enddo
!
    deallocate(preparcoursini)
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Free_after_All
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_Integrate
!
!> Manages the time integration of the grid hierarchy.
!! Recursive subroutine and call on subroutines Agrif_Init::Agrif_Instance and #Agrif_Step
!---------------------------------------------------------------------------------------------------
recursive subroutine Agrif_Integrate ( g, procname )
!---------------------------------------------------------------------------------------------------
    type(Agrif_Grid), pointer   :: g        !< Pointer on the current grid
    procedure(step_proc)        :: procname !< Subroutine to call on each grid
!
    type(Agrif_PGrid), pointer  :: parcours ! Pointer for the recursive procedure
    integer                     :: nbt      ! Number of time steps of the current grid
    integer                     :: i, k
!
!   Instanciation of the variables of the current grid
!    if ( g % fixedrank /= 0 ) then
        call Agrif_Instance(g)
!    endif
!
!   One step on the current grid
!
    call procname ()
!
!   Number of time steps on the current grid
!
    g%ngridstep = g % ngridstep + 1
    parcours => g % child_list % first
!
!   Recursive procedure for the time integration of the grid hierarchy
    do while (associated(parcours))
!
!       Instanciation of the variables of the current grid
        call Agrif_Instance(parcours % gr)
!
!       Number of time steps
        nbt = 1
        do i = 1,Agrif_Probdim
            nbt = max(nbt, parcours % gr % timeref(i))
        enddo
!
        do k = 1,nbt
            call Agrif_Integrate(parcours % gr, procname)
        enddo
!
        parcours => parcours % next
!
    enddo
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Integrate
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_Integrate_Parallel
!
!> Manages the time integration of the grid hierarchy in parallel
!! Recursive subroutine and call on subroutines Agrif_Init::Agrif_Instance and #Agrif_Step
!---------------------------------------------------------------------------------------------------
recursive subroutine Agrif_Integrate_Parallel ( g, procname )
!---------------------------------------------------------------------------------------------------
    type(Agrif_Grid), pointer   :: g        !< Pointer on the current grid
    procedure(step_proc)        :: procname !< Subroutine to call on each grid
!
#if defined AGRIF_MPI
    type(Agrif_PGrid), pointer  :: gridp    ! Pointer for the recursive procedure
    integer                     :: nbt      ! Number of time steps of the current grid
    integer                     :: i, k, is
!
!   Instanciation of the variables of the current grid
    if ( g % fixedrank /= 0 ) then
        call Agrif_Instance(g)
    endif
!
! One step on the current grid
    call procname ()
!
! Number of time steps on the current grid
    g % ngridstep = g % ngridstep + 1
!
! Continue only if the grid has defined sequences of child integrations.
    if ( .not. associated(g % child_seq) ) return
!
    do is = 1, g % child_seq % nb_seqs
!
!     For each sequence, a given processor does integrate only on grid.
        gridp => Agrif_seq_select_child(g,is)
!
!     Instanciation of the variables of the current grid
        call Agrif_Instance(gridp % gr)
!
!     Number of time steps
        nbt = 1
        do i = 1,Agrif_Probdim
            nbt = max(nbt, gridp % gr % timeref(i))
        enddo
!
        do k = 1,nbt
            call Agrif_Integrate_Parallel(gridp % gr, procname)
        enddo
!
    enddo
#else
    call Agrif_Integrate( g, procname )
#endif
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Integrate_Parallel
!===================================================================================================
!
!===================================================================================================
!
!
!===================================================================================================
!  subroutine Agrif_Integrate_ChildGrids
!
!> Manages the time integration of the grid hierarchy.
!! Call the subroutine procname on each child grid of the current grid
!---------------------------------------------------------------------------------------------------
recursive subroutine Agrif_Integrate_ChildGrids ( procname )
!---------------------------------------------------------------------------------------------------
    procedure(step_proc)        :: procname !< Subroutine to call on each grid
!
    type(Agrif_PGrid), pointer  :: parcours ! Pointer for the recursive procedure
    integer                     :: nbt      ! Number of time steps of the current grid
    integer                     :: i, k, is
    type(Agrif_Grid)                    , pointer :: save_grid
    type(Agrif_PGrid), pointer  :: gridp    ! Pointer for the recursive procedure
    
    save_grid => Agrif_Curgrid

! Number of time steps on the current grid
    save_grid % ngridstep = save_grid % ngridstep + 1
    
#ifdef AGRIF_MPI
    if ( .not. Agrif_Parallel_sisters ) then
#endif
    parcours => save_grid % child_list % first
!
!   Recursive procedure for the time integration of the grid hierarchy
    do while (associated(parcours))
!
!       Instanciation of the variables of the current grid
        call Agrif_Instance(parcours % gr)
!
!       Number of time steps
        nbt = 1
        do i = 1,Agrif_Probdim
            nbt = max(nbt, parcours % gr % timeref(i))
        enddo
!
        do k = 1,nbt
            call procname()
        enddo
!
        parcours => parcours % next
!
    enddo

#ifdef AGRIF_MPI
    else
! Continue only if the grid has defined sequences of child integrations.
    if ( .not. associated(save_grid % child_seq) ) return
!
    do is = 1, save_grid % child_seq % nb_seqs
!
!     For each sequence, a given processor does integrate only on grid.
        gridp => Agrif_seq_select_child(save_grid,is)
!
!     Instanciation of the variables of the current grid
        call Agrif_Instance(gridp % gr)
!
!     Number of time steps
        nbt = 1
        do i = 1,Agrif_Probdim
            nbt = max(nbt, gridp % gr % timeref(i))
        enddo
!
        do k = 1,nbt
            call procname()
        enddo
!
    enddo
    endif
#endif 

    call Agrif_Instance(save_grid)
    
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Integrate_ChildGrids
!===================================================================================================
!===================================================================================================
!  subroutine Agrif_Integrate_Child
!
!> Manages the time integration of the grid hierarchy.
!! Recursive subroutine and call on subroutines Agrif_Instance & Agrif_Step.
!---------------------------------------------------------------------------------------------------
recursive subroutine Agrif_Integrate_Child ( g, procname )
!---------------------------------------------------------------------------------------------------
    type(Agrif_Grid), pointer   :: g        !< Pointer on the current grid
    procedure(step_proc)        :: procname !< Subroutine to call on each grid
!
    type(Agrif_PGrid), pointer  :: parcours ! Pointer for the recursive procedure
!
!   One step on the current grid
!
    call procname ()
!
!   Number of time steps on the current grid
!
    parcours => g % child_list % first
!
!   Recursive procedure for the time integration of the grid hierarchy
    do while (associated(parcours))
!
!       Instanciation of the variables of the current grid
        call Agrif_Instance(parcours % gr)
        call Agrif_Integrate_Child (parcours % gr, procname)
        parcours => parcours % next
!
    enddo
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Integrate_Child
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_Integrate_Child_Parallel
!
!> Manages the time integration of the grid hierarchy.
!! Recursive subroutine and call on subroutines Agrif_Instance & Agrif_Step.
!---------------------------------------------------------------------------------------------------
recursive subroutine Agrif_Integrate_Child_Parallel ( g, procname )
!---------------------------------------------------------------------------------------------------
    type(Agrif_Grid), pointer   :: g        !< Pointer on the current grid
    procedure(step_proc)        :: procname !< Subroutine to call on each grid
!
#if defined AGRIF_MPI
    type(Agrif_PGrid), pointer  :: gridp    ! Pointer for the recursive procedure
    integer                     :: is
!
! Instanciation of the variables of the current grid
    call Agrif_Instance(g)
!
! One step on the current grid
    call procname ()
!
! Continue only if the grid has defined sequences of child integrations.
    if ( .not. associated(g % child_seq) ) return
!
    do is = 1, g % child_seq % nb_seqs
!
!     For each sequence, a given processor does integrate only on grid.
        gridp => Agrif_seq_select_child(g,is)
        call Agrif_Integrate_Child_Parallel(gridp % gr, procname)
!
    enddo
!
    call Agrif_Instance(g)
#else
    call Agrif_Integrate_Child( g, procname )
#endif
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Integrate_Child_Parallel
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_Init_Grids
!
!> Initializes the root coarse grid pointed by Agrif_Mygrid. It is called in the main program.
!---------------------------------------------------------------------------------------------------
subroutine Agrif_Init_Grids ( procname1, procname2 )
!---------------------------------------------------------------------------------------------------
    procedure(typedef_proc), optional   :: procname1 !< (Default: Agrif_probdim_modtype_def)
    procedure(alloc_proc),   optional   :: procname2 !< (Default: Agrif_Allocationcalls)
!
    integer :: i, ierr_allocate, nunit
    integer :: is_coarse, rhox,rhoy,rhoz,rhot
    logical :: BEXIST
!
    if (present(procname1)) Then
        call procname1()
    else
        call Agrif_probdim_modtype_def()
    endif
!

! TEST FOR COARSE GRID (GRAND MOTHER GRID) in AGRIF_FixedGrids.in
    nunit = Agrif_Get_Unit()
    open(nunit, file='AGRIF_FixedGrids.in', form='formatted', status="old", ERR=98)
    if (Agrif_Probdim == 3) then
       read(nunit,*) is_coarse, rhox, rhoy, rhoz, rhot
    elseif (Agrif_Probdim == 2) then
       read(nunit,*) is_coarse, rhox, rhoy, rhot
    elseif (Agrif_Probdim == 2) then
       read(nunit,*) is_coarse, rhox, rhot
    endif
    if (is_coarse == -1) then
       agrif_coarse = .TRUE.
       if (Agrif_Probdim == 3) then
          coarse_spaceref(1:3)=(/rhox,rhoy,rhoz/)
       elseif (Agrif_Probdim == 2) then
          coarse_spaceref(1:2)=(/rhox,rhoy/)
       elseif (Agrif_Probdim == 2) then
          coarse_spaceref(1:1)=(/rhox/)
       endif
       coarse_timeref(1:Agrif_Probdim) = rhot
    endif
    close(nunit)
    
    Agrif_UseSpecialValue = .FALSE.
    Agrif_UseSpecialValueFineGrid = .FALSE.
    Agrif_SpecialValue = 0.
    Agrif_SpecialValueFineGrid = 0.
!
    allocate(Agrif_Mygrid)
    allocate(Agrif_OldMygrid)
!
!   Space and time refinement factors are set to 1 on the root grid
!
    do i = 1,Agrif_Probdim
        Agrif_Mygrid % spaceref(i) = coarse_spaceref(i)
        Agrif_Mygrid % timeref(i)  = coarse_timeref(i)
    enddo
!
!   Initialization of the number of time steps
    Agrif_Mygrid % ngridstep = 0
    Agrif_Mygrid % grid_id   = 0
!
!   No parent grid for the root coarse grid
    nullify(Agrif_Mygrid % parent)
!
!   Initialization of the minimum positions, global abscissa and space steps
    do i = 1, Agrif_Probdim
        Agrif_Mygrid % ix(i) = 1
        Agrif_Mygrid % Agrif_x(i)  = 0.
        Agrif_Mygrid % Agrif_dx(i) = 1./Agrif_Mygrid % spaceref(i)
        Agrif_Mygrid % Agrif_dt(i) = 1./Agrif_Mygrid % timeref(i)
!       Borders of the root coarse grid
        Agrif_Mygrid % NearRootBorder(i) = .true.
        Agrif_Mygrid % DistantRootBorder(i) = .true.
    enddo
!
!   The root coarse grid is a fixed grid
    Agrif_Mygrid % fixed = .TRUE.
!   Level of the root grid
    Agrif_Mygrid % level = 0
!   Maximum level in the hierarchy
    Agrif_MaxLevelLoc = 0
!
!   Number of the grid pointed by Agrif_Mygrid (root coarse grid)
    Agrif_Mygrid % rank = 1
!
!   Number of the root grid as a fixed grid
    Agrif_Mygrid % fixedrank = 0
!
!   Initialization of some fields of the root grid variables
    ierr_allocate = 0
    if( Agrif_NbVariables(0) > 0 ) allocate(Agrif_Mygrid % tabvars(Agrif_NbVariables(0)),stat=ierr_allocate)
    if( Agrif_NbVariables(1) > 0 ) allocate(Agrif_Mygrid % tabvars_c(Agrif_NbVariables(1)),stat=ierr_allocate)
    if( Agrif_NbVariables(2) > 0 ) allocate(Agrif_Mygrid % tabvars_r(Agrif_NbVariables(2)),stat=ierr_allocate)
    if( Agrif_NbVariables(3) > 0 ) allocate(Agrif_Mygrid % tabvars_l(Agrif_NbVariables(3)),stat=ierr_allocate)
    if( Agrif_NbVariables(4) > 0 ) allocate(Agrif_Mygrid % tabvars_i(Agrif_NbVariables(4)),stat=ierr_allocate)
    if (ierr_allocate /= 0) THEN
      STOP "*** ERROR WHEN ALLOCATING TABVARS ***"
    endif
!
!   Initialization of the other fields of the root grid variables (number of
!   cells, positions, number and type of its dimensions, ...)
    call Agrif_Instance(Agrif_Mygrid)
    call Agrif_Set_numberofcells(Agrif_Mygrid)
!
!   Allocation of the array containing the values of the grid variables
    call Agrif_Allocation(Agrif_Mygrid, procname2)
    call Agrif_initialisations(Agrif_Mygrid)
!
!   Total number of fixed grids
    Agrif_nbfixedgrids = 0
    
! If a grand mother grid is declared

    if (agrif_coarse) then
      allocate(Agrif_Coarsegrid)

      Agrif_Coarsegrid % ngridstep = 0
      Agrif_Coarsegrid % grid_id   = -9999
      
    do i = 1, Agrif_Probdim
        Agrif_Coarsegrid%spaceref(i) = coarse_spaceref(i)
        Agrif_Coarsegrid%timeref(i) = coarse_timeref(i)
        Agrif_Coarsegrid % ix(i) = 1
        Agrif_Coarsegrid % Agrif_x(i)  = 0.
        Agrif_Coarsegrid % Agrif_dx(i) = 1.
        Agrif_Coarsegrid % Agrif_dt(i) = 1.
!       Borders of the root coarse grid
        Agrif_Coarsegrid % NearRootBorder(i) = .true.
        Agrif_Coarsegrid % DistantRootBorder(i) = .true.
        Agrif_Coarsegrid % nb(i) =Agrif_mygrid%nb(i) / coarse_spaceref(i)
    enddo      

!   The root coarse grid is a fixed grid
    Agrif_Coarsegrid % fixed = .TRUE.
!   Level of the root grid
    Agrif_Coarsegrid % level = -1
    
    Agrif_Coarsegrid % grand_mother_grid = .true.

!   Number of the grid pointed by Agrif_Mygrid (root coarse grid)
    Agrif_Coarsegrid % rank = -9999
!
!   Number of the root grid as a fixed grid
    Agrif_Coarsegrid % fixedrank = -9999
    
      Agrif_Mygrid%parent => Agrif_Coarsegrid
      
! Not used but required to prevent seg fault
      Agrif_Coarsegrid%parent => Agrif_Mygrid
      
      call Agrif_Create_Var(Agrif_Coarsegrid)

! Reset to null
      Nullify(Agrif_Coarsegrid%parent)
      
      Agrif_Coarsegrid%child_list%nitems = 1
      allocate(Agrif_Coarsegrid%child_list%first)
      allocate(Agrif_Coarsegrid%child_list%last)
      Agrif_Coarsegrid%child_list%first%gr => Agrif_Mygrid
      Agrif_Coarsegrid%child_list%last%gr => Agrif_Mygrid

    endif
    
    return

98  INQUIRE(FILE='AGRIF_FixedGrids.in',EXIST=BEXIST)
    if (.not. BEXIST) then
        print*,'ERROR : File AGRIF_FixedGrids.in not found.'
        STOP
    else
        print*,'Error opening file AGRIF_FixedGrids.in'
        STOP
    endif
    
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Init_Grids
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_Deallocation
!
!> Deallocates all data arrays.
!---------------------------------------------------------------------------------------------------
subroutine Agrif_Deallocation
!---------------------------------------------------------------------------------------------------
    integer                         :: nb
    type(Agrif_Variable), pointer   :: var
    type(Agrif_Variable_c), pointer   :: var_c
    type(Agrif_Variable_l), pointer   :: var_l
    type(Agrif_Variable_i), pointer   :: var_i
!
    do nb = 1,Agrif_NbVariables(0)
!
        var => Agrif_Mygrid % tabvars(nb)
!
        if ( allocated(var % array1) ) deallocate(var % array1)
        if ( allocated(var % array2) ) deallocate(var % array2)
        if ( allocated(var % array3) ) deallocate(var % array3)
        if ( allocated(var % array4) ) deallocate(var % array4)
        if ( allocated(var % array5) ) deallocate(var % array5)
        if ( allocated(var % array6) ) deallocate(var % array6)
!
        if ( allocated(var % sarray1) ) deallocate(var % sarray1)
        if ( allocated(var % sarray2) ) deallocate(var % sarray2)
        if ( allocated(var % sarray3) ) deallocate(var % sarray3)
        if ( allocated(var % sarray4) ) deallocate(var % sarray4)
        if ( allocated(var % sarray5) ) deallocate(var % sarray5)
        if ( allocated(var % sarray6) ) deallocate(var % sarray6)
!
        if ( allocated(var % darray1) ) deallocate(var % darray1)
        if ( allocated(var % darray2) ) deallocate(var % darray2)
        if ( allocated(var % darray3) ) deallocate(var % darray3)
        if ( allocated(var % darray4) ) deallocate(var % darray4)
        if ( allocated(var % darray5) ) deallocate(var % darray5)
        if ( allocated(var % darray6) ) deallocate(var % darray6)
!
    enddo
!
    do nb = 1,Agrif_NbVariables(1)
!
        var_c => Agrif_Mygrid % tabvars_c(nb)
!
        if ( allocated(var_c % carray1) ) deallocate(var_c % carray1)
        if ( allocated(var_c % carray2) ) deallocate(var_c % carray2)
!
    enddo

    do nb = 1,Agrif_NbVariables(3)
!
        var_l => Agrif_Mygrid % tabvars_l(nb)
!
        if ( allocated(var_l % larray1) ) deallocate(var_l % larray1)
        if ( allocated(var_l % larray2) ) deallocate(var_l % larray2)
        if ( allocated(var_l % larray3) ) deallocate(var_l % larray3)
        if ( allocated(var_l % larray4) ) deallocate(var_l % larray4)
        if ( allocated(var_l % larray5) ) deallocate(var_l % larray5)
        if ( allocated(var_l % larray6) ) deallocate(var_l % larray6)
!
    enddo
!
    do nb = 1,Agrif_NbVariables(4)
!
        var_i => Agrif_Mygrid % tabvars_i(nb)
!
        if ( allocated(var_i % iarray1) ) deallocate(var_i % iarray1)
        if ( allocated(var_i % iarray2) ) deallocate(var_i % iarray2)
        if ( allocated(var_i % iarray3) ) deallocate(var_i % iarray3)
        if ( allocated(var_i % iarray4) ) deallocate(var_i % iarray4)
        if ( allocated(var_i % iarray5) ) deallocate(var_i % iarray5)
        if ( allocated(var_i % iarray6) ) deallocate(var_i % iarray6)
!
    enddo
!
    if ( allocated(Agrif_Mygrid % tabvars) )   deallocate(Agrif_Mygrid % tabvars)
    if ( allocated(Agrif_Mygrid % tabvars_c) ) deallocate(Agrif_Mygrid % tabvars_c)
    if ( allocated(Agrif_Mygrid % tabvars_r) ) deallocate(Agrif_Mygrid % tabvars_r)
    if ( allocated(Agrif_Mygrid % tabvars_l) ) deallocate(Agrif_Mygrid % tabvars_l)
    if ( allocated(Agrif_Mygrid % tabvars_i) ) deallocate(Agrif_Mygrid % tabvars_i)
    deallocate(Agrif_Mygrid)
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Deallocation
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_Step_adj
!
!> creates the grid hierarchy and manages the backward time integration procedure.
!> It is called in the main program.
!> calls subroutines #Agrif_Regrid and #Agrif_Integrate_adj.
!---------------------------------------------------------------------------------------------------
subroutine Agrif_Step_adj ( procname )
!---------------------------------------------------------------------------------------------------
    procedure(step_proc)    :: procname     !< Subroutine to call on each grid
!
!   Creation and initialization of the grid hierarchy
!
!   Set the clustering variables
    call Agrif_clustering_def()
!
    if ( Agrif_USE_ONLY_FIXED_GRIDS .EQ. 1 ) then
!
        if (Agrif_Mygrid % ngridstep == 0) then
            if (.not.Agrif_regrid_has_been_done ) then
                call Agrif_Regrid()
            endif
            call Agrif_Instance(Agrif_Mygrid)
        endif
!
    else
!
        if (mod(Agrif_Mygrid % ngridstep, Agrif_Regridding) == 0) then
            call Agrif_Regrid()
            call Agrif_Instance(Agrif_Mygrid)
        endif
!
    endif
!
!   Time integration of the grid hierarchy
!
    call Agrif_Integrate_adj (Agrif_Mygrid,procname)
!
    if ( Agrif_Mygrid % child_list % nitems > 0 ) call Agrif_Instance(Agrif_Mygrid)
!
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Step_adj
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_Integrate_adj
!
!> Manages the backward time integration of the grid hierarchy.
!! Recursive subroutine and call on subroutines Agrif_Init::Agrif_Instance and #Agrif_Step_adj
!---------------------------------------------------------------------------------------------------
recursive subroutine Agrif_Integrate_adj ( g, procname )
!---------------------------------------------------------------------------------------------------
    type(Agrif_Grid), pointer   :: g        !< Pointer on the current grid
    procedure(step_proc)        :: procname !< Subroutine to call on each grid
!
    type(Agrif_pgrid), pointer :: parcours ! pointer for the recursive procedure
    integer                    :: nbt      ! Number of time steps of the current grid
    integer                    :: k
!
!   Instanciation of the variables of the current grid
    if ( g%fixedrank /= 0 ) then
        call Agrif_Instance(g)
    endif
!
!   Number of time steps on the current grid
!
    g%ngridstep = g % ngridstep + 1
    parcours => g % child_list % first
!
!   Recursive procedure for the time integration of the grid hierarchy
    do while (associated(parcours))
!
!   Instanciation of the variables of the current grid
        call Agrif_Instance(parcours % gr)
!
!       Number of time steps
        nbt = 1
        do k = 1,Agrif_Probdim
            nbt = max(nbt, parcours % gr % timeref(k))
        enddo
!
        do k = nbt,1,-1
            call Agrif_Integrate_adj(parcours % gr, procname)
        enddo
!
        parcours => parcours % next
!
    enddo
!
    if ( g % child_list % nitems > 0 )  call Agrif_Instance(g)
!
!   One step on the current grid
    call procname ()
!
end subroutine Agrif_Integrate_adj
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_Step_Child_adj
!
!> Apply 'procname' to each grid of the hierarchy from Child to Parent
!---------------------------------------------------------------------------------------------------
subroutine Agrif_Step_Child_adj ( procname )
!---------------------------------------------------------------------------------------------------
    procedure(step_proc)        :: procname !< Subroutine to call on each grid
!
    call Agrif_Integrate_Child_adj(Agrif_Mygrid,procname)
!
    if ( Agrif_Mygrid % child_list % nitems > 0 ) call Agrif_Instance(Agrif_Mygrid)
!
end subroutine Agrif_Step_Child_adj
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_Integrate_Child_adj
!
!> Manages the backward time integration of the grid hierarchy.
!! Recursive subroutine and call on subroutines Agrif_Init::Agrif_Instance & Agrif_Step_adj.
!---------------------------------------------------------------------------------------------------
recursive subroutine Agrif_Integrate_Child_adj ( g, procname )
!---------------------------------------------------------------------------------------------------
    type(Agrif_Grid),pointer   :: g          !< Pointer on the current grid
    procedure(step_proc)       :: procname   !< Subroutine to call on each grid
!
    type(Agrif_PGrid),pointer  :: parcours   !< Pointer for the recursive procedure
!
    parcours => g % child_list % first
!
!   Recursive procedure for the time integration of the grid hierarchy
    do while (associated(parcours))
!
!       Instanciation of the variables of the current grid
        call Agrif_Instance(parcours % gr)
        call Agrif_Integrate_Child_adj(parcours % gr, procname)
!
       parcours => parcours % next
!
    enddo
    if ( g % child_list % nitems > 0 )  call Agrif_Instance(g)
!
!   One step on the current grid
    call procname()
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Integrate_Child_adj
!===================================================================================================
!
!===================================================================================================

end module Agrif_Util
