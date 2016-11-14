!
! $Id: modinterp.F 779 2007-12-22 17:04:17Z rblod $
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
!> Module to initialize a fine grid from its parent grid, by using a space interpolation
!
module Agrif_Interpolation
!
    use Agrif_InterpBasic
    use Agrif_Arrays
    use Agrif_Mask
    use Agrif_CurgridFunctions
#if defined AGRIF_MPI
    use Agrif_Mpp
#endif
!
    implicit none
!
    logical, private:: precomputedone(7) = .FALSE.
!
    private :: Agrif_Parentbounds
    private :: Agrif_Interp_1D_recursive, Agrif_Interp_2D_recursive, Agrif_Interp_3D_recursive
    private :: Agrif_Interp_4D_recursive, Agrif_Interp_5D_recursive, Agrif_Interp_6D_recursive
    private :: Agrif_InterpBase
    private :: Agrif_Find_list_interp, Agrif_AddTo_list_interp
!
contains
!
!===================================================================================================
!  subroutine Agrif_InterpVariable
!
!> Sets some arguments of subroutine Agrif_InterpnD, n being the dimension of the grid variable
!---------------------------------------------------------------------------------------------------
subroutine Agrif_InterpVariable ( parent, child, torestore, procname )
!---------------------------------------------------------------------------------------------------
    type(Agrif_Variable), pointer       :: parent       !< Variable on the parent grid
    type(Agrif_Variable), pointer       :: child        !< Variable on the child grid
    logical,              intent(in)    :: torestore    !< .false. indicates that the results of the
                                                        !! interpolation are applied on the whole current grid
    procedure()                         :: procname     !< Data recovery procedure
!---------------------------------------------------------------------------------------------------
    logical               :: memberin
    integer               :: nbdim        ! Number of dimensions of the current grid
    integer, dimension(6) :: type_interp  ! Type of interpolation (linear,spline,...)
    integer, dimension(6) :: nb_child
    integer, dimension(6) :: lb_child
    integer, dimension(6) :: ub_child
    integer, dimension(6) :: lb_parent
    real   , dimension(6) :: s_child,   s_parent
    real   , dimension(6) :: ds_child, ds_parent
    integer, dimension(child % root_var % nbdim,2,2) :: childarray
!
    nbdim       = child % root_var % nbdim
    type_interp = child % root_var % type_interp
!
    call PreProcessToInterpOrUpdate( parent,   child,       &
                                     nb_child, ub_child,    &
                                     lb_child, lb_parent,   &
                                      s_child,  s_parent,   &
                                     ds_child, ds_parent, nbdim, interp=.true.)
!
!   Call to a procedure of interpolation against the number of dimensions of the grid variable
!
    call Agrif_InterpnD(type_interp, parent, child, &
                        lb_child, ub_child,         &
                        lb_child, lb_parent,        &
                        s_child,   s_parent,        &
                        ds_child, ds_parent,        &
                        child, torestore, nbdim,    &
                        childarray, memberin,       &
                        .false., procname, 0, 0)
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_InterpVariable
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_InterpnD
!
!> Interpolates a nD grid variable from its parent grid, by using a space interpolation
!---------------------------------------------------------------------------------------------------
subroutine Agrif_InterpnD ( type_interp, parent, child, pttab, petab, pttab_Child, pttab_Parent, &
                            s_Child, s_Parent, ds_Child, ds_Parent, restore, torestore,          &
                            nbdim, childarray, memberin, in_bc, procname, nb, ndir )
!---------------------------------------------------------------------------------------------------
#if defined AGRIF_MPI
    include 'mpif.h'
#endif
!
    INTEGER, DIMENSION(6),     INTENT(in)   :: type_interp  !< Type of interpolation !    (linear,...)
    TYPE(Agrif_Variable),      pointer      :: parent       !< Variable of the parent grid
    TYPE(Agrif_Variable),      pointer      :: child        !< Variable of the child grid
    INTEGER, DIMENSION(nbdim), INTENT(in)   :: pttab        !< Index of the first point inside the domain
    INTEGER, DIMENSION(nbdim), INTENT(in)   :: petab        !< Index of the first point inside the domain
    INTEGER, DIMENSION(nbdim), INTENT(in)   :: pttab_Child  !< Index of the first point inside the domain
                                                            !<    for the child grid variable
    INTEGER, DIMENSION(nbdim), INTENT(in)   :: pttab_Parent !< Index of the first point inside the domain
                                                            !<    for the parent grid variable
    REAL,    DIMENSION(nbdim), INTENT(in)   :: s_Child,s_Parent   !< Positions of the parent and child grids
    REAL,    DIMENSION(nbdim), INTENT(in)   :: ds_Child,ds_Parent !< Space steps of the parent and child grids
    TYPE(Agrif_Variable),      pointer      :: restore            !< Indicates points where interpolation
    LOGICAL,                   INTENT(in)   :: torestore          !< Indicates if the array restore is used
    INTEGER,                   INTENT(in)   :: nbdim
    LOGICAL,                   INTENT(out)  :: memberin
    LOGICAL,                   INTENT(in)   :: in_bc              !< .true. if called from Agrif_CorrectVariable \n
                                                                  !! .false. if called from Agrif_InterpVariable
    procedure()                             :: procname           !< Data recovery procedure
    INTEGER,                   INTENT(in)   :: nb, ndir
!
    INTEGER                       :: i,j,k,l,m,n
    INTEGER, DIMENSION(nbdim)     :: pttruetab,cetruetab
    INTEGER, DIMENSION(nbdim)     :: indmin,     indmax
    INTEGER, DIMENSION(nbdim)     :: indminglob, indmaxglob
#if defined AGRIF_MPI
    INTEGER, DIMENSION(nbdim)     :: indminglob2,indmaxglob2
    INTEGER, DIMENSION(nbdim)     :: indminglob3,indmaxglob3
#endif
    LOGICAL, DIMENSION(nbdim)     :: noraftab
    REAL   , DIMENSION(nbdim)     :: s_Child_temp,s_Parent_temp
    INTEGER, DIMENSION(nbdim)     :: lowerbound, upperbound, coords
    INTEGER, DIMENSION(nbdim,2,2), INTENT(OUT) :: childarray
    INTEGER, DIMENSION(nbdim,2,2)              :: parentarray
    LOGICAL :: member
    LOGICAL :: find_list_interp
!
#if defined AGRIF_MPI
!
    INTEGER, PARAMETER          :: etiquette = 100
    INTEGER                     :: code, local_proc
    INTEGER, DIMENSION(nbdim,4)                     :: tab3
    INTEGER, DIMENSION(nbdim,4,0:Agrif_Nbprocs-1)   :: tab4
    INTEGER, DIMENSION(nbdim,0:Agrif_Nbprocs-1,8)   :: tab4t
    INTEGER,DIMENSION(nbdim,2) :: tab5
    INTEGER,DIMENSION(nbdim,2,0:Agrif_Nbprocs-1) :: tab6
    INTEGER,DIMENSION(nbdim,0:Agrif_Nbprocs-1,2) :: tab5t
    LOGICAL, DIMENSION(0:Agrif_Nbprocs-1)           :: memberinall
    LOGICAL, DIMENSION(0:Agrif_Nbprocs-1)           :: sendtoproc1, recvfromproc1
    LOGICAL, DIMENSION(1)                           :: memberin1
    LOGICAL                                         :: memberout
!
#endif
!
    type(Agrif_Variable), pointer, save :: tempC => NULL()        ! Temporary child grid variable
    type(Agrif_Variable), pointer, save :: tempP => NULL()        ! Temporary parent grid variable
    type(Agrif_Variable), pointer, save :: tempPextend => NULL()  ! Temporary parent grid variable
    type(Agrif_Variable), pointer, save :: parentvalues => NULL()
!
    coords = child % root_var % coords
!
!   Boundaries of the current grid where interpolation is done
    find_list_interp =                                              &
        Agrif_Find_list_interp(                                     &
            child % list_interp,                                    &
            pttab, petab, pttab_Child, pttab_Parent, nbdim,         &
            indmin, indmax, indminglob, indmaxglob,                 &
            pttruetab, cetruetab, memberin                          &
#if defined AGRIF_MPI
           ,indminglob2, indmaxglob2, parentarray,                  &
            member, tab4t,memberinall,  sendtoproc1, recvfromproc1  &
#endif
        )
!
    if (.not.find_list_interp) then
!
        call Agrif_get_var_bounds_array(child, lowerbound, upperbound, nbdim)
        call Agrif_Childbounds(nbdim, lowerbound, upperbound,               &
                               pttab, petab, Agrif_Procrank, coords,        &
                               pttruetab, cetruetab, memberin)
        call Agrif_Parentbounds(type_interp,nbdim,indminglob,indmaxglob,    &
                                s_Parent_temp,s_Child_temp,                 &
                                s_Child,ds_Child,                           &
                                s_Parent,ds_Parent,                         &
                                pttab,petab,                                &
                                pttab_Child,pttab_Parent,                   &
                                child%root_var % posvar, coords)
#if defined AGRIF_MPI
        if (memberin) then
            call Agrif_Parentbounds(type_interp,nbdim,indmin,indmax,        &
                                    s_Parent_temp,s_Child_temp,             &
                                    s_Child,ds_Child,                       &
                                    s_Parent,ds_Parent,                     &
                                    pttruetab,cetruetab,                    &
                                    pttab_Child,pttab_Parent,               &
                                    child%root_var % posvar, coords)
        endif

        local_proc = Agrif_Procrank
        call Agrif_get_var_bounds_array(parent,lowerbound,upperbound,nbdim)
        call Agrif_ChildGrid_to_ParentGrid()
!
        call Agrif_Childbounds(nbdim,lowerbound,upperbound,                 &
                               indminglob,indmaxglob, local_proc, coords,   &
                               indminglob2,indmaxglob2,member,              &
                               indminglob3,indmaxglob3)
!
        if (member) then
            call Agrif_GlobalToLocalBounds(parentarray,                     &
                                           lowerbound,  upperbound,         &
                                           indminglob2, indmaxglob2, coords,&
                                           nbdim, local_proc, member)
        endif

        call Agrif_ParentGrid_to_ChildGrid()
#else
        parentarray(:,1,1) = indminglob
        parentarray(:,2,1) = indmaxglob
        parentarray(:,1,2) = indminglob
        parentarray(:,2,2) = indmaxglob
        indmin = indminglob
        indmax = indmaxglob
        member = .TRUE.
#endif
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Correct for non refined directions
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        do i=1,nbdim
          if (coords(i) == 0) then
             indmin(i) = indminglob(i)
             indmax(i) = indmaxglob(i)
             pttruetab(i) = indminglob(i)
             cetruetab(i) = indmaxglob(i)
          endif
        enddo

    else

#if defined AGRIF_MPI
        s_Parent_temp = s_Parent + (indmin - pttab_Parent) * ds_Parent
        s_Child_temp  = s_Child + (pttruetab - pttab_Child) * ds_Child
#else
        parentarray(:,1,1) = indminglob
        parentarray(:,2,1) = indmaxglob
        parentarray(:,1,2) = indminglob
        parentarray(:,2,2) = indmaxglob
        indmin = indminglob
        indmax = indmaxglob
        member = .TRUE.
        s_Parent_temp = s_Parent + (indminglob - pttab_Parent) * ds_Parent
        s_Child_temp  = s_Child + (pttab - pttab_Child) * ds_Child
#endif
    endif
!
    if (member) then
        if (.not.associated(tempP)) allocate(tempP)
!
        call Agrif_array_allocate(tempP,parentarray(:,1,1),parentarray(:,2,1),nbdim)
        call Agrif_var_set_array_tozero(tempP,nbdim)

        call Agrif_ChildGrid_to_ParentGrid()
!
        select case (nbdim)
        case(1)
            call procname(tempP%array1,                         &
                      parentarray(1,1,2),parentarray(1,2,2),.TRUE.,nb,ndir)
        case(2)
            call procname(tempP%array2,                         &
                      parentarray(1,1,2),parentarray(1,2,2),    &
                      parentarray(2,1,2),parentarray(2,2,2),.TRUE.,nb,ndir)
        case(3)
            call procname(tempP%array3,                         &
                      parentarray(1,1,2),parentarray(1,2,2),    &
                      parentarray(2,1,2),parentarray(2,2,2),    &
                      parentarray(3,1,2),parentarray(3,2,2),.TRUE.,nb,ndir)
        case(4)
            call procname(tempP%array4,                         &
                      parentarray(1,1,2),parentarray(1,2,2),    &
                      parentarray(2,1,2),parentarray(2,2,2),    &
                      parentarray(3,1,2),parentarray(3,2,2),    &
                      parentarray(4,1,2),parentarray(4,2,2),.TRUE.,nb,ndir)
        case(5)
            call procname(tempP%array5,                         &
                      parentarray(1,1,2),parentarray(1,2,2),    &
                      parentarray(2,1,2),parentarray(2,2,2),    &
                      parentarray(3,1,2),parentarray(3,2,2),    &
                      parentarray(4,1,2),parentarray(4,2,2),    &
                      parentarray(5,1,2),parentarray(5,2,2),.TRUE.,nb,ndir)
        case(6)
            call procname(tempP%array6,                         &
                      parentarray(1,1,2),parentarray(1,2,2),    &
                      parentarray(2,1,2),parentarray(2,2,2),    &
                      parentarray(3,1,2),parentarray(3,2,2),    &
                      parentarray(4,1,2),parentarray(4,2,2),    &
                      parentarray(5,1,2),parentarray(5,2,2),    &
                      parentarray(6,1,2),parentarray(6,2,2),.TRUE.,nb,ndir)
        end select
!
        call Agrif_ParentGrid_to_ChildGrid()
!
    endif

#if defined AGRIF_MPI
    if (.not.find_list_interp) then
!
        tab3(:,1) = indminglob2(:)
        tab3(:,2) = indmaxglob2(:)
        tab3(:,3) = indmin(:)
        tab3(:,4) = indmax(:)
        tab5(:,1) = indminglob3(:)
        tab5(:,2) = indmaxglob3(:)
!
        call MPI_ALLGATHER(tab3,4*nbdim,MPI_INTEGER,tab4,4*nbdim,MPI_INTEGER,Agrif_mpi_comm,code)
        call MPI_ALLGATHER(tab5,2*nbdim,MPI_INTEGER,tab6,2*nbdim,MPI_INTEGER,Agrif_mpi_comm,code)
        if (.not.associated(tempPextend))   allocate(tempPextend)

        do k=0,Agrif_Nbprocs-1
            do j=1,4
                do i=1,nbdim
                    tab4t(i,k,j) = tab4(i,j,k)
                enddo
            enddo
        enddo

        do k=0,Agrif_Nbprocs-1
          do j=1,2
            do i=1,nbdim
               tab5t(i,k,j) = tab6(i,j,k)
            enddo
          enddo
        enddo
      
        memberin1(1) = memberin
        call MPI_ALLGATHER(memberin1,1,MPI_LOGICAL,memberinall,1,MPI_LOGICAL,Agrif_mpi_comm,code)

        call Get_External_Data_first(tab4t(:,:,1),tab4t(:,:,2),         &
                                     tab4t(:,:,3),tab4t(:,:,4),         &
                                     nbdim,memberinall, coords,         &
                                     sendtoproc1,recvfromproc1,         &
                                     tab4t(:,:,5),tab4t(:,:,6),         &
                                     tab4t(:,:,7),tab4t(:,:,8),         &
                                     tab5t(:,:,1),tab5t(:,:,2))
    endif

    call ExchangeSameLevel(sendtoproc1,recvfromproc1,nbdim,         &
            tab4t(:,:,3),tab4t(:,:,4),tab4t(:,:,5),tab4t(:,:,6),    &
            tab4t(:,:,7),tab4t(:,:,8),memberin,tempP,tempPextend)
#else
    tempPextend => tempP
#endif

    if (.not.find_list_interp) then
        call Agrif_Addto_list_interp(                           &
                child%list_interp,pttab,petab,                  &
                pttab_Child,pttab_Parent,indmin,indmax,         &
                indminglob,indmaxglob,                          &
                pttruetab,cetruetab,                            &
                memberin,nbdim                                  &
#if defined AGRIF_MPI
               ,indminglob2,indmaxglob2,                        &
                parentarray,                                    &
                member,                                         &
                tab4t,memberinall,sendtoproc1,recvfromproc1     &
#endif
        )
    endif
!
    if (memberin) then
!
        if (.not.associated(tempC)) allocate(tempC)
!
        call Agrif_array_allocate(tempC,pttruetab,cetruetab,nbdim)
!
!       Special values on the parent grid
        if (Agrif_UseSpecialValue) then
!
            noraftab(1:nbdim) = child % root_var % interptab(1:nbdim) == 'N'
!
            if (.not.associated(parentvalues))  allocate(parentvalues)
!
            call Agrif_array_allocate(parentvalues,indmin,indmax,nbdim)
            call Agrif_var_full_copy_array(parentvalues,tempPextend,nbdim)
!
            call Agrif_CheckMasknD(tempPextend,parentvalues,    &
                    indmin(1:nbdim),indmax(1:nbdim),            &
                    indmin(1:nbdim),indmax(1:nbdim),            &
                    noraftab(1:nbdim),nbdim)
!
            call Agrif_array_deallocate(parentvalues,nbdim)
!
        endif
!
!       Interpolation of the current grid
!
        if ( memberin ) then
            select case(nbdim)
            case(1)
                call Agrif_Interp_1D_recursive( type_interp(1),                         &
                                                tempPextend%array1,                     &
                                                tempC%array1,                           &
                                                indmin(1), indmax(1),                   &
                                                pttruetab(1),    cetruetab(1),          &
                                                s_Child_temp(1), s_Parent_temp(1),      &
                                                ds_Child(1),     ds_Parent(1) )
            case(2)
                call Agrif_Interp_2D_recursive( type_interp(1:2),                       &
                                                tempPextend % array2,                   &
                                                tempC       % array2,                   &
                                                indmin(1:2), indmax(1:2),               &
                                                pttruetab(1:2),    cetruetab(1:2),      &
                                                s_Child_temp(1:2), s_Parent_temp(1:2),  &
                                                ds_Child(1:2),    ds_Parent(1:2) )
            case(3)
                call Agrif_Interp_3D_recursive( type_interp(1:3),                       &
                                                tempPextend % array3,                   &
                                                tempC       % array3,                   &
                                                indmin(1:3), indmax(1:3),               &
                                                pttruetab(1:3),    cetruetab(1:3),      &
                                                s_Child_temp(1:3), s_Parent_temp(1:3),  &
                                                ds_Child(1:3),    ds_Parent(1:3) )
            case(4)
                call Agrif_Interp_4D_recursive( type_interp(1:4),                       &
                                                tempPextend % array4,                   &
                                                tempC       % array4,                   &
                                                indmin(1:4), indmax(1:4),               &
                                                pttruetab(1:4),    cetruetab(1:4),      &
                                                s_Child_temp(1:4), s_Parent_temp(1:4),  &
                                                ds_Child(1:4),    ds_Parent(1:4) )
            case(5)
                call Agrif_Interp_5D_recursive( type_interp(1:5),                       &
                                                tempPextend % array5,                   &
                                                tempC       % array5,                   &
                                                indmin(1:5), indmax(1:5),               &
                                                pttruetab(1:5),    cetruetab(1:5),      &
                                                s_Child_temp(1:5), s_Parent_temp(1:5),  &
                                                ds_Child(1:5),    ds_Parent(1:5) )
            case(6)
                call Agrif_Interp_6D_recursive( type_interp(1:6),                       &
                                                tempPextend % array6,                   &
                                                tempC       % array6,                   &
                                                indmin(1:6), indmax(1:6),               &
                                                pttruetab(1:6),    cetruetab(1:6),      &
                                                s_Child_temp(1:6), s_Parent_temp(1:6),  &
                                                ds_Child(1:6),    ds_Parent(1:6) )
            end select
!
            call Agrif_get_var_bounds_array(child,lowerbound,upperbound,nbdim)

#if defined AGRIF_MPI
            call Agrif_GlobalToLocalBounds(childarray, lowerbound, upperbound,  &
                                            pttruetab, cetruetab, coords,       &
                                            nbdim, Agrif_Procrank, memberout)
#else
            childarray(:,1,1) = pttruetab
            childarray(:,2,1) = cetruetab
            childarray(:,1,2) = pttruetab
            childarray(:,2,2) = cetruetab
!cccccccccccccc       memberout = .TRUE.
#endif
!
!           Special values on the child grid
            if (Agrif_UseSpecialValueFineGrid) then
                call GiveAgrif_SpecialValueToTab_mpi( child, tempC, childarray, Agrif_SpecialValueFineGrid,nbdim )
            endif
!
        endif   ! ( memberin )
!
        if (torestore) then
!
#if defined AGRIF_MPI
!
            SELECT CASE (nbdim)
            CASE (1)
                do i = pttruetab(1),cetruetab(1)
!hildarrayAModifier     if (restore%restore1D(i) == 0)      &
!hildarrayAModifier         child%array1(childarray(i,1,2)) = tempC%array1(i)
                enddo
            CASE (2)
                do i = pttruetab(1),cetruetab(1)
                do j = pttruetab(2),cetruetab(2)
!hildarrayAModifier     if (restore%restore2D(i,j) == 0)    &
!hildarrayAModifier         child%array2(childarray(i,1,2), &
!hildarrayAModifier                          childarray(j,2,2)) = tempC%array2(i,j)
                enddo
                enddo
            CASE (3)
                do i = pttruetab(1),cetruetab(1)
                do j = pttruetab(2),cetruetab(2)
                do k = pttruetab(3),cetruetab(3)
!hildarrayAModifier     if (restore%restore3D(i,j,k) == 0)  &
!hildarrayAModifier         child%array3(childarray(i,1,2), &
!hildarrayAModifier                          childarray(j,2,2), &
!hildarrayAModifier                          childarray(k,3,2)) = tempC%array3(i,j,k)
                enddo
                enddo
                enddo
            CASE (4)
                do i = pttruetab(1),cetruetab(1)
                do j = pttruetab(2),cetruetab(2)
                do k = pttruetab(3),cetruetab(3)
                do l = pttruetab(4),cetruetab(4)
!hildarrayAModifier     if (restore%restore4D(i,j,k,l) == 0)    &
!hildarrayAModifier         child%array4(childarray(i,1,2),     &
!hildarrayAModifier                          childarray(j,2,2),     &
!hildarrayAModifier                          childarray(k,3,2),     &
!hildarrayAModifier                          childarray(l,4,2)) = tempC%array4(i,j,k,l)
                enddo
                enddo
                enddo
                enddo
            CASE (5)
                do i = pttruetab(1),cetruetab(1)
                do j = pttruetab(2),cetruetab(2)
                do k = pttruetab(3),cetruetab(3)
                do l = pttruetab(4),cetruetab(4)
                do m = pttruetab(5),cetruetab(5)
!hildarrayAModifier     if (restore%restore5D(i,j,k,l,m) == 0)  &
!hildarrayAModifier         child%array5(childarray(i,1,2),     &
!hildarrayAModifier                          childarray(j,2,2),     &
!hildarrayAModifier                          childarray(k,3,2),     &
!hildarrayAModifier                          childarray(l,4,2),     &
!hildarrayAModifier                          childarray(m,5,2)) = tempC%array5(i,j,k,l,m)
                enddo
                enddo
                enddo
                enddo
                enddo
            CASE (6)
                do i = pttruetab(1),cetruetab(1)
                do j = pttruetab(2),cetruetab(2)
                do k = pttruetab(3),cetruetab(3)
                do l = pttruetab(4),cetruetab(4)
                do m = pttruetab(5),cetruetab(5)
                do n = pttruetab(6),cetruetab(6)
!hildarrayAModifier     if (restore%restore6D(i,j,k,l,m,n) == 0)    &
!hildarrayAModifier         child%array6(childarray(i,1,2),         &
!hildarrayAModifier                          childarray(j,2,2),         &
!hildarrayAModifier                          childarray(k,3,2),         &
!hildarrayAModifier                          childarray(l,4,2),         &
!hildarrayAModifier                          childarray(m,5,2),         &
!hildarrayAModifier                          childarray(n,6,2)) = tempC%array6(i,j,k,l,m,n)
                enddo
                enddo
                enddo
                enddo
                enddo
                enddo
            END SELECT
!
#else
            select case (nbdim)
            case (1)
                do i = pttruetab(1),cetruetab(1)
                    if (restore%restore1D(i) == 0)          &
                        parray1(i) = tempC % array1(i)
                enddo
            case (2)
                do j = pttruetab(2),cetruetab(2)
                do i = pttruetab(1),cetruetab(1)
                    if (restore%restore2D(i,j) == 0)        &
                        parray2(i,j) = tempC % array2(i,j)
                enddo
                enddo
            case (3)
                do k = pttruetab(3),cetruetab(3)
                do j = pttruetab(2),cetruetab(2)
                do i = pttruetab(1),cetruetab(1)
                    if (restore%restore3D(i,j,k) == 0)      &
                        parray3(i,j,k) = tempC % array3(i,j,k)
                enddo
                enddo
                enddo
            case (4)
                do l = pttruetab(4),cetruetab(4)
                do k = pttruetab(3),cetruetab(3)
                do j = pttruetab(2),cetruetab(2)
                do i = pttruetab(1),cetruetab(1)
                    if (restore%restore4D(i,j,k,l) == 0)    &
                        parray4(i,j,k,l) = tempC % array4(i,j,k,l)
                enddo
                enddo
                enddo
                enddo
            case (5)
                do m = pttruetab(5),cetruetab(5)
                do l = pttruetab(4),cetruetab(4)
                do k = pttruetab(3),cetruetab(3)
                do j = pttruetab(2),cetruetab(2)
                do i = pttruetab(1),cetruetab(1)
                    if (restore%restore5D(i,j,k,l,m) == 0)  &
                        parray5(i,j,k,l,m) = tempC % array5(i,j,k,l,m)
                enddo
                enddo
                enddo
                enddo
                enddo
            case (6)
                do n = pttruetab(6),cetruetab(6)
                do m = pttruetab(5),cetruetab(5)
                do l = pttruetab(4),cetruetab(4)
                do k = pttruetab(3),cetruetab(3)
                do j = pttruetab(2),cetruetab(2)
                do i = pttruetab(1),cetruetab(1)
                    if (restore%restore6D(i,j,k,l,m,n) == 0)    &
                        parray6(i,j,k,l,m,n) = tempC % array6(i,j,k,l,m,n)
                enddo
                enddo
                enddo
                enddo
                enddo
                enddo
            end select
!
#endif
!
        else    ! .not.to_restore
!
            if (memberin) then
    !
                if ( .not.in_bc ) then
                    select case(nbdim)
                    case(1)
                        call procname(tempC % array1(            &
                                childarray(1,1,1):childarray(1,2,1)), &
                                childarray(1,1,2),childarray(1,2,2),.FALSE.,nb,ndir)
                    case(2)
                        call procname(                                &
                                tempC % array2(            &
                                childarray(1,1,1):childarray(1,2,1),  &
                                childarray(2,1,1):childarray(2,2,1)), &
                                childarray(1,1,2),childarray(1,2,2),  &
                                childarray(2,1,2),childarray(2,2,2),.FALSE.,nb,ndir)
                    case(3)
                        call procname(                                &
                                tempC % array3(            &
                                childarray(1,1,1):childarray(1,2,1),  &
                                childarray(2,1,1):childarray(2,2,1),  &
                                childarray(3,1,1):childarray(3,2,1)), &
                                childarray(1,1,2),childarray(1,2,2),  &
                                childarray(2,1,2),childarray(2,2,2),  &
                                childarray(3,1,2),childarray(3,2,2),.FALSE.,nb,ndir)
                    case(4)
                        call procname(                                &
                                tempC % array4(            &
                                childarray(1,1,1):childarray(1,2,1),  &
                                childarray(2,1,1):childarray(2,2,1),  &
                                childarray(3,1,1):childarray(3,2,1),  &
                                childarray(4,1,1):childarray(4,2,1)), &
                                childarray(1,1,2),childarray(1,2,2),  &
                                childarray(2,1,2),childarray(2,2,2),  &
                                childarray(3,1,2),childarray(3,2,2),  &
                                childarray(4,1,2),childarray(4,2,2),.FALSE.,nb,ndir)
                    case(5)
                        call procname(                                &
                                tempC % array5(            &
                                childarray(1,1,1):childarray(1,2,1),  &
                                childarray(2,1,1):childarray(2,2,1),  &
                                childarray(3,1,1):childarray(3,2,1),  &
                                childarray(4,1,1):childarray(4,2,1),  &
                                childarray(5,1,1):childarray(5,2,1)), &
                                childarray(1,1,2),childarray(1,2,2),  &
                                childarray(2,1,2),childarray(2,2,2),  &
                                childarray(3,1,2),childarray(3,2,2),  &
                                childarray(4,1,2),childarray(4,2,2),  &
                                childarray(5,1,2),childarray(5,2,2),.FALSE.,nb,ndir)
                    case(6)
                        call procname(                                &
                                tempC % array6(            &
                                childarray(1,1,1):childarray(1,2,1),  &
                                childarray(2,1,1):childarray(2,2,1),  &
                                childarray(3,1,1):childarray(3,2,1),  &
                                childarray(4,1,1):childarray(4,2,1),  &
                                childarray(5,1,1):childarray(5,2,1),  &
                                childarray(6,1,1):childarray(6,2,1)), &
                                childarray(1,1,2),childarray(1,2,2),  &
                                childarray(2,1,2),childarray(2,2,2),  &
                                childarray(3,1,2),childarray(3,2,2),  &
                                childarray(4,1,2),childarray(4,2,2),  &
                                childarray(5,1,2),childarray(5,2,2),  &
                                childarray(6,1,2),childarray(6,2,2),.FALSE.,nb,ndir)
                    end select
                else    ! we are in_bc
                    select case (nbdim)
                    case (1)
                        parray1(childarray(1,1,2):childarray(1,2,2)) =    &
                         tempC%array1(childarray(1,1,1):childarray(1,2,1))
                    case (2)
                        parray2(childarray(1,1,2):childarray(1,2,2),      &
                                      childarray(2,1,2):childarray(2,2,2)) =    &
                         tempC%array2(childarray(1,1,1):childarray(1,2,1),      &
                                      childarray(2,1,1):childarray(2,2,1))
                    case (3)
                        parray3(childarray(1,1,2):childarray(1,2,2),      &
                                      childarray(2,1,2):childarray(2,2,2),      &
                                      childarray(3,1,2):childarray(3,2,2)) =    &
                         tempC%array3(childarray(1,1,1):childarray(1,2,1),      &
                                      childarray(2,1,1):childarray(2,2,1),      &
                                      childarray(3,1,1):childarray(3,2,1))
                    case (4)
                        parray4(childarray(1,1,2):childarray(1,2,2),      &
                                      childarray(2,1,2):childarray(2,2,2),      &
                                      childarray(3,1,2):childarray(3,2,2),      &
                                      childarray(4,1,2):childarray(4,2,2)) =    &
                         tempC%array4(childarray(1,1,1):childarray(1,2,1),      &
                                      childarray(2,1,1):childarray(2,2,1),      &
                                      childarray(3,1,1):childarray(3,2,1),      &
                                      childarray(4,1,1):childarray(4,2,1))
                    case (5)
                        parray5(childarray(1,1,2):childarray(1,2,2),      &
                                      childarray(2,1,2):childarray(2,2,2),      &
                                      childarray(3,1,2):childarray(3,2,2),      &
                                      childarray(4,1,2):childarray(4,2,2),      &
                                      childarray(5,1,2):childarray(5,2,2)) =    &
                         tempC%array5(childarray(1,1,1):childarray(1,2,1),      &
                                      childarray(2,1,1):childarray(2,2,1),      &
                                      childarray(3,1,1):childarray(3,2,1),      &
                                      childarray(4,1,1):childarray(4,2,1),      &
                                      childarray(5,1,1):childarray(5,2,1))
                    case (6)
                        parray6(childarray(1,1,2):childarray(1,2,2),      &
                                      childarray(2,1,2):childarray(2,2,2),      &
                                      childarray(3,1,2):childarray(3,2,2),      &
                                      childarray(4,1,2):childarray(4,2,2),      &
                                      childarray(5,1,2):childarray(5,2,2),      &
                                      childarray(6,1,2):childarray(6,2,2)) =    &
                         tempC%array6(childarray(1,1,1):childarray(1,2,1),      &
                                      childarray(2,1,1):childarray(2,2,1),      &
                                      childarray(3,1,1):childarray(3,2,1),      &
                                      childarray(4,1,1):childarray(4,2,1),      &
                                      childarray(5,1,1):childarray(5,2,1),      &
                                      childarray(6,1,1):childarray(6,2,1))
                    end select
                endif  ! < (.not.in_bc)
            endif  ! < memberin
!
        endif

        call Agrif_array_deallocate(tempPextend,nbdim)
        call Agrif_array_deallocate(tempC,nbdim)

    endif
!
!   Deallocations
#if defined AGRIF_MPI
    if (member) then
        call Agrif_array_deallocate(tempP,nbdim)
    endif
#endif
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_InterpnD
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_Parentbounds
!
!> Calculates the bounds of the parent grid for the interpolation of the child grid
!---------------------------------------------------------------------------------------------------
subroutine Agrif_Parentbounds ( type_interp, nbdim, indmin, indmax, &
                                s_Parent_temp, s_Child_temp,        &
                                s_Child, ds_Child,                  &
                                s_Parent,ds_Parent,                 &
                                pttruetab, cetruetab,               &
                                pttab_Child, pttab_Parent, posvar, coords )
!---------------------------------------------------------------------------------------------------
    INTEGER, DIMENSION(6),     intent(in)  :: type_interp
    INTEGER,                   intent(in)  :: nbdim
    INTEGER, DIMENSION(nbdim), intent(out) :: indmin, indmax
    REAL,    DIMENSION(nbdim), intent(out) :: s_Parent_temp, s_child_temp
    REAL,    DIMENSION(nbdim), intent(in)  :: s_Child, ds_child
    REAL,    DIMENSION(nbdim), intent(in)  :: s_Parent,ds_Parent
    INTEGER, DIMENSION(nbdim), intent(in)  :: pttruetab, cetruetab
    INTEGER, DIMENSION(nbdim), intent(in)  :: pttab_Child, pttab_Parent
    INTEGER, DIMENSION(nbdim), intent(in)  :: posvar
    INTEGER, DIMENSION(nbdim), intent(in)  :: coords
!
    INTEGER :: i
    REAL,DIMENSION(nbdim) :: dim_newmin, dim_newmax
!
    dim_newmin = s_Child + (pttruetab - pttab_Child) * ds_Child
    dim_newmax = s_Child + (cetruetab - pttab_Child) * ds_Child
!
    do i = 1,nbdim
!
        indmin(i) = pttab_Parent(i) + agrif_int((dim_newmin(i)-s_Parent(i))/ds_Parent(i))
        indmax(i) = pttab_Parent(i) + agrif_ceiling((dim_newmax(i)-s_Parent(i))/ds_Parent(i))
!
!       Necessary for the Quadratic interpolation
!
        if ( (pttruetab(i) == cetruetab(i)) .and. (posvar(i) == 1) ) then
        elseif ( coords(i) == 0 ) then  ! (interptab == 'N')
        elseif ( (type_interp(i) == Agrif_ppm)     .or.     &
                 (type_interp(i) == Agrif_eno)     .or.     &
                 (type_interp(i) == Agrif_ppm_lim) .or.     &
                 (type_interp(i) == Agrif_weno) ) then
            indmin(i) = indmin(i) - 2
            indmax(i) = indmax(i) + 2
        elseif ( (type_interp(i) /= Agrif_constant) .and.   &
                 (type_interp(i) /= Agrif_linear) ) then
            indmin(i) = indmin(i) - 1
            indmax(i) = indmax(i) + 1
        endif
!
    enddo
!
    s_Parent_temp = s_Parent + (indmin - pttab_Parent) * ds_Parent
    s_Child_temp  = s_Child + (pttruetab - pttab_Child) * ds_Child
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Parentbounds
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_Interp_1D_Recursive
!
!> Subroutine for the interpolation of a 1D grid variable.
!> It calls Agrif_InterpBase.
!---------------------------------------------------------------------------------------------------
subroutine Agrif_Interp_1D_recursive ( type_interp, tabin, tabout,  &
                                       indmin, indmax,              &
                                       pttab_child, petab_child,    &
                                       s_child,  s_parent,          &
                                       ds_child, ds_parent )
!---------------------------------------------------------------------------------------------------
    integer,            intent(in)  :: type_interp
    integer,            intent(in)  :: indmin, indmax
    integer,            intent(in)  :: pttab_child, petab_child
    real,               intent(in)  :: s_child, s_parent
    real,               intent(in)  :: ds_child, ds_parent
    real, dimension(            &
        indmin:indmax           &
    ),                  intent(in)  :: tabin
    real, dimension(            &
        pttab_child:petab_child &
    ),                  intent(out) :: tabout
!---------------------------------------------------------------------------------------------------
    call Agrif_InterpBase(type_interp,                      &
                          tabin(indmin:indmax),             &
                          tabout(pttab_child:petab_child),  &
                          indmin, indmax,                   &
                          pttab_child, petab_child,         &
                          s_parent,    s_child,             &
                         ds_parent,   ds_child)
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Interp_1D_recursive
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_Interp_2D_Recursive
!
!> Subroutine for the interpolation of a 2D grid variable.
!> It calls Agrif_Interp_1D_recursive and Agrif_InterpBase.
!---------------------------------------------------------------------------------------------------
subroutine Agrif_Interp_2D_recursive ( type_interp, tabin, tabout,  &
                                       indmin, indmax,              &
                                       pttab_child, petab_child,    &
                                       s_child,  s_parent,          &
                                       ds_child, ds_parent )
!---------------------------------------------------------------------------------------------------
    integer, dimension(2),              intent(in)  :: type_interp
    integer, dimension(2),              intent(in)  :: indmin, indmax
    integer, dimension(2),              intent(in)  :: pttab_child, petab_child
    real,    dimension(2),              intent(in)  :: s_child, s_parent
    real,    dimension(2),              intent(in)  :: ds_child, ds_parent
    real,    dimension(                 &
        indmin(1):indmax(1),            &
        indmin(2):indmax(2)),           intent(in)  :: tabin
    real,    dimension(                 &
        pttab_child(1):petab_child(1),  &
        pttab_child(2):petab_child(2)), intent(out) :: tabout
!---------------------------------------------------------------------------------------------------
    real, dimension(                    &
        pttab_child(1):petab_child(1),  &
        indmin(2):indmax(2))            :: tabtemp
    real, dimension(                    &
        pttab_child(2):petab_child(2),  &
        pttab_child(1):petab_child(1))  :: tabout_trsp
    real, dimension(                    &
        indmin(2):indmax(2),            &
        pttab_child(1):petab_child(1))  :: tabtemp_trsp
    integer                             :: i, j, coeffraf
!---------------------------------------------------------------------------------------------------
!
    coeffraf = nint ( ds_parent(1) / ds_child(1) )
!
    if ((type_interp(1) == Agrif_Linear) .and. (coeffraf /= 1)) then
!---CDIR NEXPAND
        if(.NOT. precomputedone(1))     &
            call Linear1dPrecompute2d(                  &
                    indmax(2)-indmin(2)+1,              &
                    indmax(1)-indmin(1)+1,              &
                    petab_child(1)-pttab_child(1)+1,    &
                    s_parent(1),s_child(1),ds_parent(1),ds_child(1),1)
!---CDIR NEXPAND
        call Linear1dAfterCompute(tabin,tabtemp,size(tabin),size(tabtemp),1)
!
    elseif ((type_interp(1) == Agrif_PPM) .and. (coeffraf /= 1)) then
!---CDIR NEXPAND
        if(.NOT. precomputedone(1))     &
            call PPM1dPrecompute2d(                     &
                    indmax(2)-indmin(2)+1,              &
                    indmax(1)-indmin(1)+1,              &
                    petab_child(1)-pttab_child(1)+1,        &
                    s_parent(1),s_child(1),ds_parent(1),ds_child(1),1)
!---CDIR NEXPAND
        call PPM1dAfterCompute(tabin,tabtemp,size(tabin),size(tabtemp),1)
    else
        do j = indmin(2),indmax(2)
!
!---CDIR NEXPAND
            call Agrif_Interp_1D_recursive(type_interp(1),                  &
                    tabin(indmin(1):indmax(1),j),               &
                    tabtemp(pttab_child(1):petab_child(1),j),   &
                    indmin(1),indmax(1),                    &
                    pttab_child(1),petab_child(1),          &
                    s_child(1), s_parent(1),                &
                    ds_child(1),ds_parent(1))
!
        enddo
    endif

    coeffraf = nint(ds_parent(2)/ds_child(2))
    tabtemp_trsp = TRANSPOSE(tabtemp)

    if ((type_interp(2) == Agrif_Linear) .and. (coeffraf /= 1)) then
!---CDIR NEXPAND
        if(.NOT. precomputedone(2))     &
            call Linear1dPrecompute2d(                  &
                    petab_child(1)-pttab_child(1)+1,    &
                    indmax(2)-indmin(2)+1,              &
                    petab_child(2)-pttab_child(2)+1,    &
                    s_parent(2),s_child(2),ds_parent(2),ds_child(2),2)
!---CDIR NEXPAND
        call Linear1dAfterCompute(tabtemp_trsp,tabout_trsp, &
                size(tabtemp_trsp),size(tabout_trsp),2)

    elseif ((type_interp(2) == Agrif_PPM) .and. (coeffraf /= 1)) then
!---CDIR NEXPAND
        if(.NOT. precomputedone(2))     &
            call PPM1dPrecompute2d(                     &
                    petab_child(1)-pttab_child(1)+1,    &
                    indmax(2)-indmin(2)+1,              &
                    petab_child(2)-pttab_child(2)+1,    &
                    s_parent(2),s_child(2),ds_parent(2),ds_child(2),2)
!---CDIR NEXPAND
        call PPM1dAfterCompute(tabtemp_trsp, tabout_trsp,    &
                               size(tabtemp_trsp), size(tabout_trsp), 2)
    else
        do i = pttab_child(1), petab_child(1)
!
!---CDIR NEXPAND
            call Agrif_InterpBase(type_interp(2),                                   &
                                  tabtemp_trsp(indmin(2):indmax(2), i),             &
                                  tabout_trsp(pttab_child(2):petab_child(2), i),    &
                                  indmin(2), indmax(2),                             &
                                  pttab_child(2), petab_child(2),                   &
                                  s_parent(2),    s_child(2),                       &
                                 ds_parent(2),   ds_child(2) )
        enddo
    endif
!
    tabout = TRANSPOSE(tabout_trsp)
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Interp_2D_recursive
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_Interp_3D_Recursive
!
!> Subroutine for the interpolation of a 3D grid variable.
!> It calls #Agrif_Interp_2D_recursive and #Agrif_InterpBase.
!---------------------------------------------------------------------------------------------------
subroutine Agrif_Interp_3D_recursive ( type_interp, tabin, tabout,  &
                                       indmin, indmax,              &
                                       pttab_child, petab_child,    &
                                       s_child,  s_parent,          &
                                       ds_child, ds_parent )
!---------------------------------------------------------------------------------------------------
    integer, dimension(3),              intent(in)  :: type_interp
    integer, dimension(3),              intent(in)  :: indmin, indmax
    integer, dimension(3),              intent(in)  :: pttab_child, petab_child
    real,    dimension(3),              intent(in)  :: s_child, s_parent
    real,    dimension(3),              intent(in)  :: ds_child, ds_parent
    real,    dimension(                 &
        indmin(1):indmax(1),            &
        indmin(2):indmax(2),            &
        indmin(3):indmax(3)),           intent(in)  :: tabin
    real,    dimension(                 &
        pttab_child(1):petab_child(1),  &
        pttab_child(2):petab_child(2),  &
        pttab_child(3):petab_child(3)), intent(out) :: tabout
!---------------------------------------------------------------------------------------------------
    real, dimension(                    &
        pttab_child(1):petab_child(1),  &
        pttab_child(2):petab_child(2),  &
        indmin(3):indmax(3))            :: tabtemp
    integer                             :: i, j, k, coeffraf
    integer                             :: locind_child_left, kdeb
!
    coeffraf = nint ( ds_parent(1) / ds_child(1) )
    if ( (type_interp(1) == Agrif_Linear) .and. (coeffraf/=1) ) then
        call Linear1dPrecompute2d(indmax(2)-indmin(2)+1,            &
                                  indmax(1)-indmin(1)+1,            &
                                  petab_child(1)-pttab_child(1)+1,  &
                                  s_parent(1),s_child(1),ds_parent(1),ds_child(1),1)
        precomputedone(1) = .TRUE.
    elseif ( (type_interp(1) == Agrif_PPM) .and. (coeffraf/=1) ) then
        call PPM1dPrecompute2d(indmax(2)-indmin(2)+1,           &
                               indmax(1)-indmin(1)+1,           &
                               petab_child(1)-pttab_child(1)+1, &
                               s_parent(1),s_child(1),ds_parent(1),ds_child(1),1)
        precomputedone(1) = .TRUE.
    endif

    coeffraf = nint ( ds_parent(2) / ds_child(2) )
    if ( (type_interp(2) == Agrif_Linear) .and. (coeffraf/=1) ) then
        call Linear1dPrecompute2d(petab_child(1)-pttab_child(1)+1,  &
                                  indmax(2)-indmin(2)+1,            &
                                  petab_child(2)-pttab_child(2)+1,  &
                                  s_parent(2),s_child(2),ds_parent(2),ds_child(2),2)
        precomputedone(2) = .TRUE.
    elseif ( (type_interp(2) == Agrif_PPM) .and. (coeffraf/=1) ) then
        call PPM1dPrecompute2d(petab_child(1)-pttab_child(1)+1, &
                               indmax(2)-indmin(2)+1,           &
                               petab_child(2)-pttab_child(2)+1, &
                               s_parent(2),s_child(2),ds_parent(2),ds_child(2),2)
        precomputedone(2) = .TRUE.
    endif
!
    do k = indmin(3), indmax(3)
        call Agrif_Interp_2D_recursive(type_interp(1:2),                            &
                                       tabin(indmin(1):indmax(1),                   &
                                             indmin(2):indmax(2), k),               &
                                       tabtemp(pttab_child(1):petab_child(1),       &
                                               pttab_child(2):petab_child(2), k),   &
                                       indmin(1:2), indmax(1:2),                    &
                                       pttab_child(1:2), petab_child(1:2),          &
                                       s_child(1:2),     s_parent(1:2),             &
                                       ds_child(1:2),   ds_parent(1:2) )
    enddo
!
    precomputedone(1) = .FALSE.
    precomputedone(2) = .FALSE.
    coeffraf = nint(ds_parent(3)/ds_child(3))
!
    if ( coeffraf == 1 ) then
        locind_child_left = 1 + agrif_int((s_child(3)-s_parent(3))/ds_parent(3))
        kdeb = indmin(3)+locind_child_left-2
        do k = pttab_child(3),petab_child(3)
            kdeb = kdeb + 1
            do j = pttab_child(2), petab_child(2)
            do i = pttab_child(1), petab_child(1)
                tabout(i,j,k) = tabtemp(i,j,kdeb)
            enddo
            enddo
        enddo
    else
        do j = pttab_child(2), petab_child(2)
        do i = pttab_child(1), petab_child(1)
            call Agrif_InterpBase(type_interp(3),                                   &
                                  tabtemp(i,j,indmin(3):indmax(3)),                 &
                                  tabout(i,j,pttab_child(3):petab_child(3)),        &
                                  indmin(3), indmax(3),                             &
                                  pttab_child(3), petab_child(3),                   &
                                  s_parent(3),    s_child(3),                       &
                                 ds_parent(3),   ds_child(3) )
        enddo
        enddo
    endif
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Interp_3D_recursive
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_Interp_4D_Recursive
!
!> Subroutine for the interpolation of a 4D grid variable.
!> It calls #Agrif_Interp_3D_recursive and #Agrif_InterpBase.
!---------------------------------------------------------------------------------------------------
subroutine Agrif_Interp_4D_recursive ( type_interp, tabin, tabout,  &
                                       indmin, indmax,              &
                                       pttab_child, petab_child,    &
                                       s_child,  s_parent,          &
                                       ds_child, ds_parent )
!---------------------------------------------------------------------------------------------------
    integer, dimension(4),              intent(in)  :: type_interp
    integer, dimension(4),              intent(in)  :: indmin, indmax
    integer, dimension(4),              intent(in)  :: pttab_child, petab_child
    real,    dimension(4),              intent(in)  :: s_child, s_parent
    real,    dimension(4),              intent(in)  :: ds_child, ds_parent
    real,    dimension(                 &
        indmin(1):indmax(1),            &
        indmin(2):indmax(2),            &
        indmin(3):indmax(3),            &
        indmin(4):indmax(4)),           intent(in)  :: tabin
    real,    dimension(                 &
        pttab_child(1):petab_child(1),  &
        pttab_child(2):petab_child(2),  &
        pttab_child(3):petab_child(3),  &
        pttab_child(4):petab_child(4)), intent(out) :: tabout
!---------------------------------------------------------------------------------------------------
    real, dimension(                    &
        pttab_child(1):petab_child(1),  &
        pttab_child(2):petab_child(2),  &
        pttab_child(3):petab_child(3),  &
        indmin(4):indmax(4))            :: tabtemp
    integer                             :: i, j, k, l
!
    do l = indmin(4), indmax(4)
        call Agrif_Interp_3D_recursive(type_interp(1:3),                            &
                                       tabin(indmin(1):indmax(1),                   &
                                             indmin(2):indmax(2),                   &
                                             indmin(3):indmax(3), l),               &
                                       tabtemp(pttab_child(1):petab_child(1),       &
                                               pttab_child(2):petab_child(2),       &
                                               pttab_child(3):petab_child(3), l),   &
                                       indmin(1:3), indmax(1:3),                    &
                                       pttab_child(1:3), petab_child(1:3),          &
                                       s_child(1:3),    s_parent(1:3),              &
                                       ds_child(1:3),  ds_parent(1:3) )
    enddo
!
    do k = pttab_child(3), petab_child(3)
    do j = pttab_child(2), petab_child(2)
    do i = pttab_child(1), petab_child(1)
        call Agrif_InterpBase(type_interp(4),                                       &
                              tabtemp(i,j,k,indmin(4):indmax(4)),                   &
                              tabout(i,j,k,pttab_child(4):petab_child(4)),          &
                              indmin(4), indmax(4),                                 &
                              pttab_child(4), petab_child(4),                       &
                              s_parent(4),    s_child(4),                           &
                             ds_parent(4),   ds_child(4) )
    enddo
    enddo
    enddo
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Interp_4D_recursive
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_Interp_5D_Recursive
!
!> Subroutine for the interpolation of a 5D grid variable.
!> It calls #Agrif_Interp_4D_recursive and #Agrif_InterpBase.
!---------------------------------------------------------------------------------------------------
subroutine Agrif_Interp_5D_recursive ( type_interp, tabin, tabout,  &
                                       indmin, indmax,              &
                                       pttab_child, petab_child,    &
                                       s_child,  s_parent,          &
                                       ds_child, ds_parent )
!---------------------------------------------------------------------------------------------------
    integer, dimension(5),              intent(in)  :: type_interp
    integer, dimension(5),              intent(in)  :: indmin, indmax
    integer, dimension(5),              intent(in)  :: pttab_child, petab_child
    real,    dimension(5),              intent(in)  :: s_child, s_parent
    real,    dimension(5),              intent(in)  :: ds_child, ds_parent
    real,    dimension(                 &
        indmin(1):indmax(1),            &
        indmin(2):indmax(2),            &
        indmin(3):indmax(3),            &
        indmin(4):indmax(4),            &
        indmin(5):indmax(5)),           intent(in)  :: tabin
    real,    dimension(                 &
        pttab_child(1):petab_child(1),  &
        pttab_child(2):petab_child(2),  &
        pttab_child(3):petab_child(3),  &
        pttab_child(4):petab_child(4),  &
        pttab_child(5):petab_child(5)), intent(out) :: tabout
!---------------------------------------------------------------------------------------------------
    real, dimension(                    &
        pttab_child(1):petab_child(1),  &
        pttab_child(2):petab_child(2),  &
        pttab_child(3):petab_child(3),  &
        pttab_child(4):petab_child(4),  &
        indmin(5):indmax(5))            :: tabtemp
    integer                             :: i, j, k, l, m
!
    do m = indmin(5), indmax(5)
        call Agrif_Interp_4D_recursive(type_interp(1:4),                            &
                                       tabin(indmin(1):indmax(1),                   &
                                             indmin(2):indmax(2),                   &
                                             indmin(3):indmax(3),                   &
                                             indmin(4):indmax(4),m),                &
                                       tabtemp(pttab_child(1):petab_child(1),       &
                                               pttab_child(2):petab_child(2),       &
                                               pttab_child(3):petab_child(3),       &
                                               pttab_child(4):petab_child(4), m),   &
                                       indmin(1:4),indmax(1:4),                     &
                                       pttab_child(1:4), petab_child(1:4),          &
                                       s_child(1:4),     s_parent(1:4),             &
                                       ds_child(1:4),   ds_parent(1:4) )
    enddo
!
    do l = pttab_child(4), petab_child(4)
    do k = pttab_child(3), petab_child(3)
    do j = pttab_child(2), petab_child(2)
    do i = pttab_child(1), petab_child(1)
        call Agrif_InterpBase(type_interp(5),                                       &
                              tabtemp(i,j,k,l,indmin(5):indmax(5)),                 &
                              tabout(i,j,k,l,pttab_child(5):petab_child(5)),        &
                              indmin(5), indmax(5),                                 &
                              pttab_child(5), petab_child(5),                       &
                              s_parent(5),   s_child(5),                            &
                              ds_parent(5), ds_child(5) )
    enddo
    enddo
    enddo
    enddo
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Interp_5D_recursive
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_Interp_6D_Recursive
!
!> Subroutine for the interpolation of a 6D grid variable.
!> It calls #Agrif_Interp_5D_recursive and Agrif_InterpBase.
!---------------------------------------------------------------------------------------------------
subroutine Agrif_Interp_6D_recursive ( type_interp, tabin, tabout,  &
                                       indmin, indmax,              &
                                       pttab_child, petab_child,    &
                                       s_child,  s_parent,          &
                                       ds_child, ds_parent )
!---------------------------------------------------------------------------------------------------
    integer, dimension(6),                  intent(in)  :: type_interp
    integer, dimension(6),                  intent(in)  :: indmin, indmax
    integer, dimension(6),                  intent(in)  :: pttab_child, petab_child
    real,    dimension(6),                  intent(in)  :: s_child, s_parent
    real,    dimension(6),                  intent(in)  :: ds_child, ds_parent
    real,    dimension(                 &
        indmin(1):indmax(1),            &
        indmin(2):indmax(2),            &
        indmin(3):indmax(3),            &
        indmin(4):indmax(4),            &
        indmin(5):indmax(5),            &
        indmin(6):indmax(6)),               intent(in)  :: tabin
    real,    dimension(                 &
        pttab_child(1):petab_child(1),  &
        pttab_child(2):petab_child(2),  &
        pttab_child(3):petab_child(3),  &
        pttab_child(4):petab_child(4),  &
        pttab_child(5):petab_child(5),  &
        pttab_child(6):petab_child(6)),     intent(out) :: tabout
!---------------------------------------------------------------------------------------------------
    real, dimension(                    &
        pttab_child(1):petab_child(1),  &
        pttab_child(2):petab_child(2),  &
        pttab_child(3):petab_child(3),  &
        pttab_child(4):petab_child(4),  &
        pttab_child(5):petab_child(5),  &
        indmin(6):indmax(6))            :: tabtemp
    integer                             :: i, j, k, l, m, n
!
    do n = indmin(6), indmax(6)
        call Agrif_Interp_5D_recursive(type_interp(1:5),                            &
                                       tabin(indmin(1):indmax(1),                   &
                                             indmin(2):indmax(2),                   &
                                             indmin(3):indmax(3),                   &
                                             indmin(4):indmax(4),                   &
                                             indmin(5):indmax(5), n),               &
                                        tabtemp(pttab_child(1):petab_child(1),      &
                                                pttab_child(2):petab_child(2),      &
                                                pttab_child(3):petab_child(3),      &
                                                pttab_child(4):petab_child(4),      &
                                                pttab_child(5):petab_child(5), n),  &
                                        indmin(1:5),indmax(1:5),                    &
                                        pttab_child(1:5), petab_child(1:5),         &
                                        s_child(1:5), s_parent(1:5),                &
                                        ds_child(1:5),ds_parent(1:5) )
    enddo
!
    do m = pttab_child(5), petab_child(5)
    do l = pttab_child(4), petab_child(4)
    do k = pttab_child(3), petab_child(3)
    do j = pttab_child(2), petab_child(2)
    do i = pttab_child(1), petab_child(1)
        call Agrif_InterpBase(type_interp(6),                                       &
                              tabtemp(i,j,k,l,m,indmin(6):indmax(6)),               &
                              tabout(i,j,k,l,m,pttab_child(6):petab_child(6)),      &
                              indmin(6), indmax(6),                                 &
                              pttab_child(6), petab_child(6),                       &
                              s_parent(6),   s_child(6),                            &
                              ds_parent(6), ds_child(6) )
    enddo
    enddo
    enddo
    enddo
    enddo
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Interp_6D_recursive
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_InterpBase
!
!> Calls the interpolation method chosen by the user (linear, lagrange, spline, etc.).
!---------------------------------------------------------------------------------------------------
subroutine Agrif_InterpBase ( type_interp, parenttab, childtab, indmin, indmax, &
                              pttab_child, petab_child,                         &
                              s_parent, s_child, ds_parent, ds_child )
!---------------------------------------------------------------------------------------------------
    INTEGER                                                 :: type_interp
    INTEGER                                                 :: indmin, indmax
    INTEGER                                                 :: pttab_child, petab_child
    REAL, DIMENSION(indmin:indmax),           INTENT(IN)    :: parenttab
    REAL, DIMENSION(pttab_child:petab_child), INTENT(OUT)   :: childtab
    REAL                                                    :: s_parent, s_child
    REAL                                                    :: ds_parent,ds_child
!
    if ( (indmin == indmax) .and. (pttab_child == petab_child) ) then
!
        childtab(pttab_child) = parenttab(indmin)
!
    elseif (type_interp == Agrif_LINEAR) then    !       Linear interpolation
!
        call Agrif_basicinterp_linear1D(parenttab,childtab,                   &
                indmax-indmin+1,petab_child-pttab_child+1,  &
                s_parent,s_child,ds_parent,ds_child)
!
    elseif ( type_interp == Agrif_PPM ) then     !       PPM interpolation

        call PPM1d(parenttab,childtab,                      &
                indmax-indmin+1,petab_child-pttab_child+1,  &
                s_parent,s_child,ds_parent,ds_child)
!
    elseif ( type_interp == Agrif_PPM_LIM ) then     !       PPM interpolation

        call PPM1d_lim(parenttab,childtab,                      &
                indmax-indmin+1,petab_child-pttab_child+1,  &
                s_parent,s_child,ds_parent,ds_child)
!
    elseif (type_interp == Agrif_LAGRANGE) then  !       Lagrange interpolation
!
        call lagrange1D(parenttab,childtab,                 &
                indmax-indmin+1,petab_child-pttab_child+1,  &
                s_parent,s_child,ds_parent,ds_child)
!
    elseif (type_interp == Agrif_ENO) then       !       Eno interpolation
!
        call ENO1d(parenttab,childtab,                      &
                indmax-indmin+1,petab_child-pttab_child+1,  &
                s_parent,s_child,ds_parent,ds_child)
!
    elseif (type_interp == Agrif_WENO) then      !       Weno interpolation
!
        call WENO1d(parenttab,childtab,                     &
                indmax-indmin+1,petab_child-pttab_child+1,  &
                s_parent,s_child,ds_parent,ds_child)
!
    elseif (type_interp == Agrif_LINEARCONSERV) then !   Linear conservative interpolation
!
        call Linear1dConserv(parenttab,childtab,            &
                indmax-indmin+1,petab_child-pttab_child+1,  &
                s_parent,s_child,ds_parent,ds_child)
!
    elseif (type_interp == Agrif_LINEARCONSERVLIM) then !Linear conservative interpolation
!
        call Linear1dConservLim(parenttab,childtab,         &
                indmax-indmin+1,petab_child-pttab_child+1,  &
                s_parent,s_child,ds_parent,ds_child)
!
    elseif (type_interp == Agrif_CONSTANT) then
!
        call Constant1d(parenttab,childtab,                 &
                indmax-indmin+1,petab_child-pttab_child+1,  &
                s_parent,s_child,ds_parent,ds_child)
!
    endif
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_InterpBase
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_Find_list_interp
!---------------------------------------------------------------------------------------------------
function Agrif_Find_list_interp ( list_interp, pttab, petab, pttab_Child, pttab_Parent,     &
                                    nbdim, indmin, indmax, indminglob,  indmaxglob,         &
                                    pttruetab, cetruetab, memberin                          &
#if defined AGRIF_MPI
                                   ,indminglob2, indmaxglob2, parentarray,                  &
                                    member, tab4t, memberinall, sendtoproc1, recvfromproc1  &
#endif
    ) result(find_list_interp)
!---------------------------------------------------------------------------------------------------
    type(Agrif_List_Interp_Loc),   pointer     :: list_interp
    integer,                       intent(in)  :: nbdim
    integer, dimension(nbdim),     intent(in)  :: pttab, petab, pttab_Child, pttab_Parent
    integer, dimension(nbdim),     intent(out) :: indmin, indmax
    integer, dimension(nbdim),     intent(out) :: indminglob, indmaxglob
    integer, dimension(nbdim),     intent(out) :: pttruetab, cetruetab
    logical,                       intent(out) :: memberin
#if defined AGRIF_MPI
    integer, dimension(nbdim),     intent(out) :: indminglob2, indmaxglob2
    integer, dimension(nbdim,2,2), intent(out) :: parentarray
    logical,                       intent(out) :: member
    integer, dimension(nbdim,0:Agrif_Nbprocs-1,8), intent(out) :: tab4t
    logical, dimension(0:Agrif_Nbprocs-1),         intent(out) :: memberinall
    logical, dimension(0:Agrif_Nbprocs-1),         intent(out) :: sendtoproc1, recvfromproc1
#endif
    logical :: find_list_interp
!
    integer :: i
    type(Agrif_List_Interp_Loc), pointer :: parcours
    type(Agrif_Interp_Loc),      pointer :: pil

    find_list_interp = .false.

    if ( .not. associated(list_interp) )    return

    parcours => list_interp
    find_loop : do while ( associated(parcours) )

        pil => parcours % interp_loc

        do i = 1,nbdim
            if ( (pttab(i) /= pil % pttab(i)) .or. &
                 (petab(i) /= pil % petab(i)) .or. &
                 (pttab_child(i)  /= pil % pttab_child(i)) .or. &
                 (pttab_parent(i) /= pil % pttab_parent(i)) ) then
                parcours => parcours % suiv
                cycle find_loop
            endif
        enddo

        indmin = pil % indmin(1:nbdim)
        indmax = pil % indmax(1:nbdim)

        pttruetab = pil % pttruetab(1:nbdim)
        cetruetab = pil % cetruetab(1:nbdim)

#if !defined AGRIF_MPI
        indminglob  = pil % indminglob(1:nbdim)
        indmaxglob  = pil % indmaxglob(1:nbdim)
#else
        indminglob  = pil % indminglob2(1:nbdim)
        indmaxglob  = pil % indmaxglob2(1:nbdim)
        indminglob2 = pil % indminglob2(1:nbdim)
        indmaxglob2 = pil % indmaxglob2(1:nbdim)
        parentarray = pil % parentarray(1:nbdim,:,:)
        member      = pil % member
        tab4t         = pil % tab4t(1:nbdim, 0:Agrif_Nbprocs-1, 1:8)
        memberinall   = pil % memberinall(0:Agrif_Nbprocs-1)
        sendtoproc1   = pil % sendtoproc1(0:Agrif_Nbprocs-1)
        recvfromproc1 = pil % recvfromproc1(0:Agrif_Nbprocs-1)
#endif
        memberin = pil % memberin
        find_list_interp = .true.
        exit find_loop
    enddo find_loop
!---------------------------------------------------------------------------------------------------
end function Agrif_Find_list_interp
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_AddTo_list_interp
!---------------------------------------------------------------------------------------------------
subroutine Agrif_AddTo_list_interp ( list_interp, pttab, petab, pttab_Child, pttab_Parent,  &
                                     indmin, indmax, indminglob, indmaxglob,                &
                                     pttruetab, cetruetab,                                  &
                                     memberin, nbdim                                        &
#if defined AGRIF_MPI
                                    ,indminglob2, indmaxglob2,                              &
                                     parentarray,                                           &
                                     member,                                                &
                                     tab4t, memberinall, sendtoproc1, recvfromproc1         &
#endif
    )
!---------------------------------------------------------------------------------------------------
    type(Agrif_List_Interp_Loc), pointer    :: list_interp
    integer                                 :: nbdim
    integer, dimension(nbdim)               :: pttab, petab, pttab_Child, pttab_Parent
    integer, dimension(nbdim)               :: indmin,indmax
    integer, dimension(nbdim)               :: indminglob, indmaxglob
    integer, dimension(nbdim)               :: pttruetab, cetruetab
    logical                                 :: memberin
#if defined AGRIF_MPI
    integer, dimension(nbdim,2,2)           :: parentarray
    logical                                 :: member
    integer, dimension(nbdim)                       :: indminglob2,indmaxglob2
    integer, dimension(nbdim,0:Agrif_Nbprocs-1,8)   :: tab4t
    logical, dimension(0:Agrif_Nbprocs-1)           :: memberinall
    logical, dimension(0:Agrif_Nbprocs-1)           :: sendtoproc1
    logical, dimension(0:Agrif_Nbprocs-1)           :: recvfromproc1
#endif
!
    type(Agrif_List_Interp_Loc), pointer    :: parcours
    type(Agrif_Interp_Loc),      pointer    :: pil
!
    allocate(parcours)
    allocate(parcours % interp_loc)

    pil => parcours % interp_loc

    pil % pttab(1:nbdim) = pttab(1:nbdim)
    pil % petab(1:nbdim) = petab(1:nbdim)
    pil % pttab_child(1:nbdim) = pttab_child(1:nbdim)
    pil % pttab_parent(1:nbdim) = pttab_parent(1:nbdim)

    pil % indmin(1:nbdim) = indmin(1:nbdim)
    pil % indmax(1:nbdim) = indmax(1:nbdim)

    pil % memberin = memberin
#if !defined AGRIF_MPI
    pil % indminglob(1:nbdim) = indminglob(1:nbdim)
    pil % indmaxglob(1:nbdim) = indmaxglob(1:nbdim)
#else
    pil % indminglob2(1:nbdim) = indminglob2(1:nbdim)
    pil % indmaxglob2(1:nbdim) = indmaxglob2(1:nbdim)
    pil % parentarray(1:nbdim,:,:) = parentarray(1:nbdim,:,:)
    pil % member = member
    allocate(pil % tab4t(nbdim, 0:Agrif_Nbprocs-1, 8))
    allocate(pil % memberinall(0:Agrif_Nbprocs-1))
    allocate(pil % sendtoproc1(0:Agrif_Nbprocs-1))
    allocate(pil % recvfromproc1(0:Agrif_Nbprocs-1))
    pil % tab4t         = tab4t
    pil % memberinall   = memberinall
    pil % sendtoproc1   = sendtoproc1
    pil % recvfromproc1 = recvfromproc1
#endif

    pil % pttruetab(1:nbdim) = pttruetab(1:nbdim)
    pil % cetruetab(1:nbdim) = cetruetab(1:nbdim)

    parcours % suiv => list_interp
    list_interp => parcours
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Addto_list_interp
!===================================================================================================
!
end module Agrif_Interpolation
