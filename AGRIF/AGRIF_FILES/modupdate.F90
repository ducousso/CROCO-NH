!
! $Id: modupdate.F 779 2007-12-22 17:04:17Z rblod $
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
!> Module Agrif_Update
!>
!> This module  contains procedures to update a parent grid from its child grids.
!
module Agrif_Update
!
    use Agrif_UpdateBasic
    use Agrif_Arrays
    use Agrif_CurgridFunctions
    use Agrif_Mask
#if defined AGRIF_MPI
    use Agrif_Mpp
#endif
!
    implicit none
!
    logical, private :: precomputedone(7) = .FALSE.
!
contains
!
!===================================================================================================
!  subroutine Agrif_UpdateVariable
!
!> subroutine to set arguments for Agrif_UpdatenD
!---------------------------------------------------------------------------------------------------
subroutine Agrif_UpdateVariable ( parent, child, updateinf, updatesup, procname )
!---------------------------------------------------------------------------------------------------
    type(Agrif_Variable),  pointer      :: parent       !< Variable on the parent grid
    type(Agrif_Variable),  pointer      :: child        !< Variable on the child grid
    integer, dimension(6), intent(in)   :: updateinf    !< First positions where interpolations are calculated
    integer, dimension(6), intent(in)   :: updatesup    !< Last  positions where interpolations are calculated
    procedure()                         :: procname     !< Data recovery procedure
!---------------------------------------------------------------------------------------------------
    integer, dimension(6) :: nb_child           ! Number of cells on the child grid
    integer, dimension(6) :: lb_child
    integer, dimension(6) :: ub_child
    integer, dimension(6) :: lb_parent
    real   , dimension(6) ::  s_child           ! Child  grid position (s_root = 0)
    real   , dimension(6) ::  s_parent          ! Parent grid position (s_root = 0)
    real   , dimension(6) :: ds_child           ! Child  grid dx (ds_root = 1)
    real   , dimension(6) :: ds_parent          ! Parent grid dx (ds_root = 1)
    logical, dimension(6) :: do_update          ! Indicates if we perform update for each dimension
    integer, dimension(6) :: posvar             ! Position of the variable on the cell (1 or 2)
    integer, dimension(6) :: oldparentlbound, oldparentubound
    integer               :: n, nbdim
    logical               :: wholeupdate
    type(Agrif_Variable), pointer :: root   ! Variable on the root grid
!
    root => child % root_var
    nbdim = root % nbdim
!
    call PreProcessToInterpOrUpdate( parent,   child,       &
                                     nb_child, ub_child,    &
                                     lb_child, lb_parent,   &
                                      s_child,  s_parent,   &
                                     ds_child, ds_parent, nbdim, interp=.false. )
!
    do_update(:) = .true.
    posvar(1:nbdim) = root % posvar(1:nbdim)
!
    do n = 1,nbdim
!
        if ( root % interptab(n) == 'N' ) then
            posvar(n) = 1
            do_update(n) = .false.
            oldparentlbound(n) = parent % lb(n)
            oldparentubound(n) = parent % ub(n)
            parent % lb(n) = child % lb(n)
            parent % ub(n) = child % ub(n)
        end if
!
    enddo

    wholeupdate = .FALSE.
!
    do n = 1,nbdim
        if ( do_update(n) ) then
            if ( (updateinf(n) > updatesup(n)) .OR. &
                ((updateinf(n) == -99) .AND. (updatesup(n) == -99)) &
               ) then
                wholeupdate = .TRUE.
            endif
        endif
    enddo
!
    IF (wholeupdate) THEN
        call Agrif_UpdateWhole(parent, child,               &
                updateinf(1:nbdim), updatesup(1:nbdim),     &
                lb_child(1:nbdim), lb_parent(1:nbdim),      &
                nb_child(1:nbdim), posvar(1:nbdim),         &
                do_update(1:nbdim),                         &
                s_child(1:nbdim),   s_parent(1:nbdim),      &
                ds_child(1:nbdim), ds_parent(1:nbdim), nbdim, procname)
    ELSE
        call Agrif_UpdateBcnD(parent, child,                &
                updateinf(1:nbdim), updatesup(1:nbdim),     &
                lb_child(1:nbdim), lb_parent(1:nbdim),      &
                nb_child(1:nbdim), posvar(1:nbdim),         &
                do_update(1:nbdim),                         &
                s_child(1:nbdim),   s_parent(1:nbdim),      &
                ds_child(1:nbdim), ds_parent(1:nbdim), nbdim, procname)
    ENDIF
!
    do n = 1,nbdim
!
        if ( root % interptab(n) == 'N' ) then  ! No space DIMENSION
            parent % lb(n) = oldparentlbound(n)
            parent % ub(n) = oldparentubound(n)
        end if
!
    enddo
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_UpdateVariable
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_UpdateWhole
!---------------------------------------------------------------------------------------------------
subroutine Agrif_UpdateWhole ( parent, child, uinf, usup,       &
                               lb_child, lb_parent,             &
                               nb_child, posvar,                &
                               do_update,                       &
                               s_child,   s_parent,             &
                               ds_child, ds_parent, nbdim, procname )
!---------------------------------------------------------------------------------------------------
#if defined AGRIF_MPI
    include 'mpif.h'
#endif
!
    type(Agrif_Variable),      pointer    :: parent         !< Variable on the parent grid
    type(Agrif_Variable),      pointer    :: child          !< Variable on the child grid
    integer, dimension(nbdim), intent(in) :: uinf           !< First positions where interpolations are calculated
    integer, dimension(nbdim), intent(in) :: usup           !< Last  positions where interpolations are calculated
    integer,                   intent(in) :: nbdim          !< Number of dimensions of the grid variable
    integer, dimension(nbdim), intent(in) :: lb_child       !< Index of the first point inside the domain for the parent grid variable
    integer, dimension(nbdim), intent(in) :: lb_parent      !< Index of the first point inside the domain for the child grid variable
    integer, dimension(nbdim), intent(in) :: nb_child       !< Number of cells of the child grid
    integer, dimension(nbdim), intent(in) :: posvar         !< Position of the variable on the cell (1 or 2)
    logical, dimension(nbdim), intent(in) :: do_update      !< Indicates if we update for each dimension
    real,    dimension(nbdim), intent(in) :: s_child        !< Positions of the child grid
    real,    dimension(nbdim), intent(in) :: s_parent       !< Positions of the parent grid
    real,    dimension(nbdim), intent(in) :: ds_child       !< Space steps of the child grid
    real,    dimension(nbdim), intent(in) :: ds_parent      !< Space steps of the parent grid
    procedure()                           :: procname       !< Data recovery procedure
!
    integer, dimension(nbdim)     :: type_update ! Type of update (copy or average)
    integer, dimension(nbdim,2)   :: lubglob
    integer, dimension(nbdim,2,2) :: indtab      ! limits of the child grid that will be used in the update scheme
    integer, dimension(nbdim,2,2) :: indtruetab  ! grid variable where boundary conditions are
    integer :: coeffraf, i
    integer :: uinfloc, usuploc
!
    type_update = child % root_var % type_update(1:nbdim)
!
    do i = 1, nbdim
!
        if ( do_update(i) ) then
!
            coeffraf = nint(ds_parent(i)/ds_child(i))
            uinfloc = 0
            usuploc = nb_child(i)/coeffraf - 1

            IF (posvar(i) == 1) THEN
                usuploc = usuploc - 1
            ENDIF

            IF (uinf(i) > usup(i)) THEN
                uinfloc = uinf(i)
                usuploc = usuploc - uinf(i)
            ENDIF

            indtab(i,1,1) = lb_child(i) + (uinfloc + 1) * coeffraf
            indtab(i,1,2) = lb_child(i) + (usuploc + 1) * coeffraf

            IF ( posvar(i) == 1 ) THEN
                IF ( type_update(i) == Agrif_Update_Full_Weighting ) THEN
                    indtab(i,1,1) = indtab(i,1,1) - (coeffraf - 1)
                    indtab(i,1,2) = indtab(i,1,2) + (coeffraf - 1)
                ELSE IF ( type_update(i) /= Agrif_Update_Copy ) THEN
                    indtab(i,1,1) = indtab(i,1,1) - coeffraf / 2
                    indtab(i,1,2) = indtab(i,1,2) + coeffraf / 2
                ENDIF
            ELSE
                indtab(i,1,1) = indtab(i,1,1) - coeffraf
                indtab(i,1,2) = indtab(i,1,2) - 1
    ! at this point, indices are OK for an average
                IF ( type_update(i) == Agrif_Update_Full_Weighting ) THEN
                    indtab(i,1,1) = indtab(i,1,1) - coeffraf / 2
                    indtab(i,1,2) = indtab(i,1,2) + coeffraf / 2
                ENDIF
            ENDIF
!
        else    ! IF ( .not.do_update(i) ) THEN
!
            if ( posvar(i) == 1 ) then
                indtab(i,1,1) = lb_child(i)
                indtab(i,1,2) = lb_child(i) + nb_child(i)
            else
                indtab(i,1,1) = lb_child(i)
                indtab(i,1,2) = lb_child(i) + nb_child(i) - 1
            endif
!
        endif
    enddo

! lubglob contains the global lbound and ubound of the child array
! lubglob(:,1) : global lbound for each dimension
! lubglob(:,2) : global lbound for each dimension
!
    call Agrif_get_var_global_bounds(child, lubglob, nbdim)
!
    indtruetab(1:nbdim,1,1) = max(indtab(1:nbdim,1,1), lubglob(1:nbdim,1))
    indtruetab(1:nbdim,1,2) = min(indtab(1:nbdim,1,2), lubglob(1:nbdim,2))
!
    call Agrif_UpdatenD(type_update, parent, child,                         &
                        indtruetab(1:nbdim,1,1), indtruetab(1:nbdim,1,2),   &
                        lb_child(1:nbdim), lb_parent(1:nbdim),              &
                        s_child(1:nbdim), s_parent(1:nbdim),                &
                        ds_child(1:nbdim), ds_parent(1:nbdim),              &
#if defined AGRIF_MPI
                        posvar, do_update,                                  &
#endif
                        nbdim, procname)
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_UpdateWhole
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_UpdateBcnd
!---------------------------------------------------------------------------------------------------
subroutine Agrif_UpdateBcnd ( parent, child, uinf, usup,        &
                              lb_child, lb_parent,              &
                              nb_child, posvar,                 &
                              do_update,                        &
                              s_child,  s_parent,               &
                              ds_child, ds_parent, nbdim, procname )
!---------------------------------------------------------------------------------------------------
#if defined AGRIF_MPI
    include 'mpif.h'
#endif
!
    type(Agrif_Variable), pointer           :: parent       !< Variable on the parent grid
    type(Agrif_Variable), pointer           :: child        !< Variable on the child grid
    integer, dimension(nbdim), intent(in)   :: uinf         !< First positions where interpolations are calculated
    integer, dimension(nbdim), intent(in)   :: usup         !< Last  positions where interpolations are calculated
    integer                                 :: nbdim        !< Number of dimensions of the grid variable
    integer, dimension(nbdim), intent(in)   :: lb_child     !< Index of the first point inside the domain for
                                                            !!   the parent grid variable
    integer, dimension(nbdim), intent(in)   :: lb_parent    !< Index of the first point inside the domain for
                                                            !!   the child grid variable
    integer, dimension(nbdim), intent(in)   :: nb_child     !< Number of cells of the child grid
    integer, dimension(nbdim), intent(in)   :: posvar       !< Position of the variable on the cell (1 or 2)
    logical, dimension(nbdim), intent(in)   :: do_update    !< Indicates if we update for each dimension
    real,    dimension(nbdim), intent(in)   :: s_child      !< Positions of the child grid
    real,    dimension(nbdim), intent(in)   :: s_parent     !< Positions of the parent grid
    real,    dimension(nbdim), intent(in)   :: ds_child     !< Space steps of the child grid
    real,    dimension(nbdim), intent(in)   :: ds_parent    !< Space steps of the parent grid
    procedure()                             :: procname     !< Data recovery procedure
!
    integer,dimension(nbdim)     :: type_update ! Type of update (copy or average)
    integer,dimension(nbdim,2)   :: lubglob
    integer                      :: i
    integer,dimension(nbdim,2,2) :: indtab         ! Arrays indicating the limits of the child
    integer,dimension(nbdim,2,2) :: indtruetab     ! grid variable where boundary conditions are
    integer,dimension(nbdim,2,2,nbdim)   :: ptres  ! calculated
    integer                      :: nb, ndir
    integer :: coeffraf
!
    type_update = child % root_var % type_update(1:nbdim)
!
    DO i = 1, nbdim
        coeffraf = nint(ds_parent(i)/ds_child(i))
        indtab(i,1,1) = lb_child(i) + (uinf(i) + 1) * coeffraf
        indtab(i,1,2) = lb_child(i) + (usup(i) + 1) * coeffraf

        indtab(i,2,1) = lb_child(i) + nb_child(i) - (usup(i)+1) *  coeffraf
        indtab(i,2,2) = lb_child(i) + nb_child(i) - (uinf(i)+1) *  coeffraf

        IF (posvar(i) == 1) THEN
            IF (type_update(i) == Agrif_Update_Full_Weighting) THEN
                indtab(i,:,1) = indtab(i,:,1) - (coeffraf - 1)
                indtab(i,:,2) = indtab(i,:,2) + (coeffraf - 1)
            ELSE IF (type_update(i) /= Agrif_Update_Copy) THEN
                indtab(i,:,1) = indtab(i,:,1) - coeffraf / 2
                indtab(i,:,2) = indtab(i,:,2) + coeffraf / 2
            ENDIF
        ELSE
            indtab(i,1,1) = indtab(i,1,1) - coeffraf
            indtab(i,1,2) = indtab(i,1,2) - 1
            indtab(i,2,2) = indtab(i,2,2) + coeffraf - 1
            IF (type_update(i) == Agrif_Update_Full_Weighting) THEN
                indtab(i,1,1) = indtab(i,1,1) - coeffraf/2
                indtab(i,1,2) = indtab(i,1,2) + coeffraf/2
                indtab(i,2,1) = indtab(i,2,1) - coeffraf/2
                indtab(i,2,2) = indtab(i,2,2) + coeffraf/2
            ENDIF
        ENDIF
    ENDDO
!
    call Agrif_get_var_global_bounds(child,lubglob,nbdim)
!
    indtruetab(1:nbdim,1,1) = max(indtab(1:nbdim,1,1),lubglob(1:nbdim,1))
    indtruetab(1:nbdim,1,2) = max(indtab(1:nbdim,1,2),lubglob(1:nbdim,1))
    indtruetab(1:nbdim,2,1) = min(indtab(1:nbdim,2,1),lubglob(1:nbdim,2))
    indtruetab(1:nbdim,2,2) = min(indtab(1:nbdim,2,2),lubglob(1:nbdim,2))
!
    do nb = 1,nbdim
        if ( do_update(nb) ) then
            do ndir = 1,2
                ptres(nb,1,ndir,nb) = indtruetab(nb,ndir,1)
                ptres(nb,2,ndir,nb) = indtruetab(nb,ndir,2)
                do i = 1,nbdim
                    if ( i /= nb ) then
                        if ( do_update(i) ) then
                            ptres(i,1,ndir,nb) = indtruetab(i,1,1)
                            ptres(i,2,ndir,nb) = indtruetab(i,2,2)
                        else
                            if (posvar(i) == 1) then
                                ptres(i,1,ndir,nb) = lb_child(i)
                                ptres(i,2,ndir,nb) = lb_child(i) + nb_child(i)
                            else
                                ptres(i,1,ndir,nb) = lb_child(i)
                                ptres(i,2,ndir,nb) = lb_child(i) + nb_child(i) - 1
                            endif
                        endif
                    endif
                enddo
            enddo
        endif
    enddo
!
    do nb = 1,nbdim
        if ( do_update(nb) ) then
            do ndir = 1,2
                call Agrif_UpdatenD(type_update, parent, child,             &
                        ptres(1:nbdim,1,ndir,nb),ptres(1:nbdim,2,ndir,nb),  &
                        lb_child(1:nbdim),lb_parent(1:nbdim),               &
                        s_child(1:nbdim),s_parent(1:nbdim),                 &
                        ds_child(1:nbdim),ds_parent(1:nbdim),               &
#if defined AGRIF_MPI
                        posvar,do_update,                                   &
#endif
                        nbdim,procname,nb,ndir)
            enddo
        endif
    enddo
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_UpdateBcnd
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_UpdatenD
!
!> updates a 2D grid variable on the parent grid of the current grid
!---------------------------------------------------------------------------------------------------
subroutine Agrif_UpdatenD ( type_update, parent, child,     &
                            pttab, petab,                   &
                            lb_child, lb_parent,            &
                            s_child,  s_parent,             &
                            ds_child, ds_parent,            &
#if defined AGRIF_MPI
                            posvar, do_update,              &
#endif
                            nbdim, procname, nb, ndir )
!---------------------------------------------------------------------------------------------------
#if defined AGRIF_MPI
    include 'mpif.h'
#endif
!
    type(Agrif_Variable), pointer           :: parent       !< Variable of the parent grid
    type(Agrif_Variable), pointer           :: child        !< Variable of the child grid
    integer,                   intent(in)   :: nbdim
    integer, dimension(nbdim), intent(in)   :: type_update  !< Type of update (copy or average)
    integer, dimension(nbdim), intent(in)   :: pttab        !< Index of the first point inside the domain
    integer, dimension(nbdim), intent(in)   :: petab        !< Index of the first point inside the domain
    integer, dimension(nbdim), intent(in)   :: lb_child  !< Index of the first point inside the domain for the child
                                                            !!    grid variable
    integer, dimension(nbdim), intent(in)   :: lb_parent !< Index of the first point inside the domain for the parent
                                                            !!    grid variable
    real,    dimension(nbdim), intent(in)   :: s_child      !< Positions of the child grid
    real,    dimension(nbdim), intent(in)   :: s_parent     !< Positions of the parent grid
    real,    dimension(nbdim), intent(in)   :: ds_child     !< Space steps of the child grid
    real,    dimension(nbdim), intent(in)   :: ds_parent    !< Space steps of the parent grid
    procedure()                             :: procname     !< Data recovery procedure
    integer, optional,         intent(in)   :: nb, ndir
!---------------------------------------------------------------------------------------------------
    integer, dimension(nbdim)       :: pttruetab, cetruetab
#if defined AGRIF_MPI
    integer, dimension(nbdim)       :: posvar  !< Position of the variable on the cell (1 or 2)
    logical, dimension(nbdim)       :: do_update
#endif
    integer, dimension(nbdim)       :: coords
    integer, dimension(nbdim)       :: indmin, indmax
    integer, dimension(nbdim)       :: indminglob, indmaxglob
    real   , dimension(nbdim)       :: s_Child_temp, s_Parent_temp
    integer, dimension(nbdim)       :: lowerbound,upperbound
    integer, dimension(nbdim)       :: pttruetabwhole, cetruetabwhole
    integer, dimension(nbdim,2,2)   :: childarray
    integer, dimension(nbdim,2,2)   :: parentarray
    integer,dimension(nbdim)     :: type_update_temp
    logical :: memberin, member
    integer :: nbin, ndirin
!
#if defined AGRIF_MPI
!
    integer,dimension(nbdim)    :: indminglob2,indmaxglob2
    logical, dimension(0:Agrif_Nbprocs-1) :: sendtoproc1,recvfromproc1
    logical, dimension(0:Agrif_Nbprocs-1) :: sendtoproc2,recvfromproc2
    integer                               :: code, local_proc
    integer                               :: i,j,k
    integer, dimension(nbdim,4)           :: tab3
    integer, dimension(nbdim,4,0:Agrif_Nbprocs-1) :: tab4
    integer, dimension(nbdim,0:Agrif_Nbprocs-1,8) :: tab4t
    integer, dimension(nbdim,0:Agrif_Nbprocs-1,8) :: tab5t
    logical :: find_list_update
    logical, dimension(0:Agrif_Nbprocs-1) :: memberinall, memberinall2
    logical, dimension(1) :: memberin1
!
#endif
!
    type(Agrif_Variable), pointer, save :: tempC => NULL()       ! Temporary child grid variable
    type(Agrif_Variable), pointer, save :: tempP => NULL()       ! Temporary parent grid variable
    type(Agrif_Variable), pointer, save :: tempCextend => NULL() ! Temporary child
    type(Agrif_Variable), pointer, save :: tempPextend => NULL() ! Temporary parent
    type(Agrif_Variable), pointer :: tempP_indic, tempP_average
    type(Agrif_Variable), pointer :: tempC_indic
    logical :: compute_average
    real :: coeff_multi
    integer :: nb_dimensions
!
!   Get local lower and upper bound of the child variable
    call Agrif_get_var_bounds_array(child, lowerbound, upperbound, nbdim)

! here pttab and petab corresponds to the (global) indices of the points needed in the update
! pttruetab and cetruetab contains only indices that are present on the local processor
!
    coords = child % root_var % coords
!
    call Agrif_Childbounds( nbdim, lowerbound, upperbound, pttab, petab, Agrif_Procrank,    &
                            coords, pttruetab, cetruetab, memberin )
    call Agrif_Prtbounds( nbdim, indminglob, indmaxglob, s_Parent_temp, s_Child_temp,       &
                         s_child, ds_child, s_parent, ds_parent,                            &
                         pttab, petab, lb_child, lb_parent                                  &
#if defined AGRIF_MPI
                       , posvar, type_update, do_update, pttruetabwhole, cetruetabwhole     &
#endif
            )

#if defined AGRIF_MPI
!
    IF (memberin) THEN
        call Agrif_GlobalToLocalBounds(childarray,lowerbound,upperbound,    &
                                       pttruetab,cetruetab, coords,         &
                                       nbdim, Agrif_Procrank, member)
    ENDIF

    call Agrif_Prtbounds(nbdim, indmin, indmax,                     &
                         s_Parent_temp, s_Child_temp,               &
                         s_child, ds_child, s_parent, ds_parent,    &
                         pttruetab, cetruetab, lb_child, lb_parent, &
                         posvar, type_update, do_update,            &
                         pttruetabwhole, cetruetabwhole)
!
#else
    indmin = indminglob
    indmax = indmaxglob
    pttruetabwhole = pttruetab
    cetruetabwhole = cetruetab
    childarray(:,1,2) = pttruetab
    childarray(:,2,2) = cetruetab
#endif

    IF (.not.present(nb)) THEN
        nbin=0
        ndirin=0
    ELSE
        nbin = nb
        ndirin = ndir
    ENDIF

    IF (memberin) THEN
!
        IF ( .not.associated(tempC) )  allocate(tempC)
!
        call Agrif_array_allocate(tempC,pttruetab,cetruetab,nbdim)
        call Agrif_var_set_array_tozero(tempC,nbdim)

        SELECT CASE (nbdim)
        CASE(1)
            CALL procname(tempC%array1,                 &
                      childarray(1,1,2),childarray(1,2,2),.TRUE.,nbin,ndirin)
        CASE(2)
            CALL procname(tempC%array2,                 &
                      childarray(1,1,2),childarray(1,2,2),  &
                      childarray(2,1,2),childarray(2,2,2),.TRUE.,nbin,ndirin)
        CASE(3)
            CALL procname(tempC%array3,                 &
                      childarray(1,1,2),childarray(1,2,2),  &
                      childarray(2,1,2),childarray(2,2,2),  &
                      childarray(3,1,2),childarray(3,2,2),.TRUE.,nbin,ndirin)
        CASE(4)
            CALL procname(tempC%array4,                 &
                      childarray(1,1,2),childarray(1,2,2),  &
                      childarray(2,1,2),childarray(2,2,2),  &
                      childarray(3,1,2),childarray(3,2,2),  &
                      childarray(4,1,2),childarray(4,2,2),.TRUE.,nbin,ndirin)
        CASE(5)
            CALL procname(tempC%array5,                 &
                      childarray(1,1,2),childarray(1,2,2),  &
                      childarray(2,1,2),childarray(2,2,2),  &
                      childarray(3,1,2),childarray(3,2,2),  &
                      childarray(4,1,2),childarray(4,2,2),  &
                      childarray(5,1,2),childarray(5,2,2),.TRUE.,nbin,ndirin)
        CASE(6)
            CALL procname(tempC%array6,                 &
                      childarray(1,1,2),childarray(1,2,2),  &
                      childarray(2,1,2),childarray(2,2,2),  &
                      childarray(3,1,2),childarray(3,2,2),  &
                      childarray(4,1,2),childarray(4,2,2),  &
                      childarray(5,1,2),childarray(5,2,2),  &
                      childarray(6,1,2),childarray(6,2,2),.TRUE.,nbin,ndirin)
        END SELECT
!
    ENDIF
!
#if defined AGRIF_MPI
!
!     tab2 contains the necessary limits of the parent grid for each processor

    if (Associated(child%list_update)) then
        call Agrif_Find_list_update(child%list_update,pttab,petab,                      &
                                    lb_child,lb_parent,nbdim,                         &
                                    find_list_update,tab4t,tab5t,memberinall,memberinall2,  &
                                    sendtoproc1,recvfromproc1,sendtoproc2,recvfromproc2)
    else
        find_list_update = .FALSE.
    endif

    if (.not.find_list_update) then
        tab3(:,1) = pttruetab(:)
        tab3(:,2) = cetruetab(:)
        tab3(:,3) = pttruetabwhole(:)
        tab3(:,4) = cetruetabwhole(:)
!
        call MPI_ALLGATHER(tab3,4*nbdim,MPI_INTEGER,tab4,4*nbdim,MPI_INTEGER,Agrif_mpi_comm,code)

        if ( .not.associated(tempCextend) ) allocate(tempCextend)
        do k=0,Agrif_Nbprocs-1
            do j=1,4
                do i=1,nbdim
                    tab4t(i,k,j) = tab4(i,j,k)
                enddo
            enddo
        enddo

        memberin1(1) = memberin
        call MPI_ALLGATHER(memberin1,1,MPI_LOGICAL,memberinall,1,MPI_LOGICAL,Agrif_mpi_comm,code)

        call Get_External_Data_first(tab4t(:,:,1),tab4t(:,:,2),tab4t(:,:,3),tab4t(:,:,4),   &
                                     nbdim, memberinall, coords,                            &
                                     sendtoproc1,recvfromproc1,                             &
                                     tab4t(:,:,5),tab4t(:,:,6),tab4t(:,:,7),tab4t(:,:,8),   &
                                     tab4t(:,:,1),tab4t(:,:,2))
    endif

    call ExchangeSameLevel(sendtoproc1,recvfromproc1,nbdim,         &
            tab4t(:,:,3),tab4t(:,:,4),tab4t(:,:,5),tab4t(:,:,6),    &
            tab4t(:,:,7),tab4t(:,:,8),memberin,tempC,tempCextend)

#else
    tempCextend => tempC
#endif
!
!     Update of the parent grid (tempP) from the child grid (tempC)
!
    IF (memberin) THEN
!
        IF ( .not.associated(tempP) ) allocate(tempP)
!
        call Agrif_array_allocate(tempP,indmin,indmax,nbdim)
!
        if ( nbdim == 1 ) then
            tempP % array1 = 0.
            call Agrif_Update_1D_Recursive( type_update(1),  &
                                            tempP%array1,   &
                                            tempCextend%array1, &
                                            indmin(1), indmax(1),   &
                                            pttruetabwhole(1), cetruetabwhole(1),   &
                                            s_Child_temp(1), s_Parent_temp(1),      &
                                            ds_child(1), ds_parent(1) )
                                            
            IF (Agrif_UseSpecialValueInUpdate) THEN
            allocate(tempC_indic)
            allocate(tempP_indic)
            call Agrif_array_allocate(tempC_indic,lbound(tempCextend%array1),ubound(tempCextend%array1),nbdim)
            call Agrif_array_allocate(tempP_indic,lbound(tempP%array1),ubound(tempP%array1),nbdim)

            compute_average = .FALSE.
            type_update_temp(1:nbdim) = type_update(1:nbdim)
            IF (ANY(type_update(1:nbdim) == Agrif_Update_Full_Weighting)) THEN
              compute_average = .TRUE.
              allocate(tempP_average)
              call Agrif_array_allocate(tempP_average,lbound(tempP%array1),ubound(tempP%array1),nbdim)
              WHERE (type_update(1:nbdim) == Agrif_Update_Full_Weighting)
                type_update_temp(1:nbdim) = Agrif_Update_Average
              END WHERE
              call Agrif_Update_1D_Recursive( type_update_temp(1),   &
                                            tempP_average%array1,       &
                                            tempCextend%array1, &
                                            indmin(1), indmax(1),   &
                                            pttruetabwhole(1), cetruetabwhole(1),   &
                                            s_Child_temp(1), s_Parent_temp(1),      &
                                            ds_child(1), ds_parent(1) )
              coeff_multi = 1.
              do nb_dimensions=1,nbdim
                coeff_multi = coeff_multi * nint(ds_parent(nb_dimensions)/ds_child(nb_dimensions))
              enddo
            ENDIF
            
            WHERE (tempCextend%array1 == Agrif_SpecialValueFineGrid)
              tempC_indic%array1 = 0.
            ELSEWHERE
              tempC_indic%array1 = 1.
            END WHERE
            
            Agrif_UseSpecialValueInUpdate = .FALSE.
            Agrif_Update_Weights = .TRUE.
 
             call Agrif_Update_1D_Recursive( type_update_temp(1),   &
                                            tempP_indic%array1,       &
                                            tempC_indic%array1, &
                                            indmin(1), indmax(1),   &
                                            pttruetabwhole(1), cetruetabwhole(1),   &
                                            s_Child_temp(1), s_Parent_temp(1),      &
                                            ds_child(1), ds_parent(1) )

           Agrif_UseSpecialValueInUpdate = .TRUE.
           Agrif_Update_Weights = .FALSE.

           IF (compute_average) THEN
               WHERE (tempP_indic%array1 == 0.)
                  tempP%array1 = Agrif_SpecialValueFineGrid
               ELSEWHERE ((tempP_indic%array1 == coeff_multi).AND.(tempP%array1 /= Agrif_SpecialValueFineGrid))
                  tempP%array1 = tempP%array1 /tempP_indic%array1
               ELSEWHERE
                  tempP%array1 = tempP_average%array1 /tempP_indic%array1
               END WHERE

           ELSE
               WHERE (tempP_indic%array1 == 0.)
                  tempP%array1 = Agrif_SpecialValueFineGrid
               ELSEWHERE
                  tempP%array1 = tempP%array1 /tempP_indic%array1
               END WHERE
            ENDIF
           
            deallocate(tempP_indic%array1)
            deallocate(tempC_indic%array1)
            deallocate(tempC_indic)
            deallocate(tempP_indic)
            IF (compute_average) THEN
              deallocate(tempP_average%array1)
              deallocate(tempP_average)
            ENDIF
            ENDIF
            
        endif
        if ( nbdim == 2 ) then
            call Agrif_Update_2D_Recursive( type_update(1:2),   &
                                            tempP%array2,       &
                                            tempCextend%array2, &
                                            indmin(1:2), indmax(1:2),   &
                                            pttruetabwhole(1:2), cetruetabwhole(1:2),   &
                                            s_Child_temp(1:2), s_Parent_temp(1:2),      &
                                            ds_child(1:2), ds_parent(1:2) )

            IF (Agrif_UseSpecialValueInUpdate) THEN
            allocate(tempC_indic)
            allocate(tempP_indic)
            call Agrif_array_allocate(tempC_indic,lbound(tempCextend%array2),ubound(tempCextend%array2),nbdim)
            call Agrif_array_allocate(tempP_indic,lbound(tempP%array2),ubound(tempP%array2),nbdim)
 
            compute_average = .FALSE.
            type_update_temp(1:nbdim) = type_update(1:nbdim)
            IF (ANY(type_update == Agrif_Update_Full_Weighting)) THEN
              compute_average = .TRUE.
              allocate(tempP_average)
              call Agrif_array_allocate(tempP_average,lbound(tempP%array2),ubound(tempP%array2),nbdim)
              WHERE (type_update(1:nbdim) == Agrif_Update_Full_Weighting)
                type_update_temp(1:nbdim) = Agrif_Update_Average
              END WHERE
              call Agrif_Update_2D_Recursive( type_update_temp(1:2),   &
                                            tempP_average%array2,       &
                                            tempCextend%array2, &
                                            indmin(1:2), indmax(1:2),   &
                                            pttruetabwhole(1:2), cetruetabwhole(1:2),   &
                                            s_Child_temp(1:2), s_Parent_temp(1:2),      &
                                            ds_child(1:2), ds_parent(1:2) )
              coeff_multi = 1.
              do nb_dimensions=1,nbdim
                coeff_multi = coeff_multi * nint(ds_parent(nb_dimensions)/ds_child(nb_dimensions))
              enddo
            ENDIF
            
            WHERE (tempCextend%array2 == Agrif_SpecialValueFineGrid)
              tempC_indic%array2 = 0.
            ELSEWHERE
              tempC_indic%array2 = 1.
            END WHERE
            
            Agrif_UseSpecialValueInUpdate = .FALSE.
            Agrif_Update_Weights = .TRUE.
            
            call Agrif_Update_2D_Recursive( type_update_temp(1:2),   &
                                            tempP_indic%array2,       &
                                            tempC_indic%array2, &
                                            indmin(1:2), indmax(1:2),   &
                                            pttruetabwhole(1:2), cetruetabwhole(1:2),   &
                                            s_Child_temp(1:2), s_Parent_temp(1:2),      &
                                            ds_child(1:2), ds_parent(1:2) )

           Agrif_UseSpecialValueInUpdate = .TRUE.
           Agrif_Update_Weights = .FALSE.

           IF (compute_average) THEN
               WHERE (tempP_indic%array2 == 0.)
                  tempP%array2 = Agrif_SpecialValueFineGrid
               ELSEWHERE ((tempP_indic%array2 == coeff_multi).AND.(tempP%array2 /= Agrif_SpecialValueFineGrid))
                  tempP%array2 = tempP%array2 /tempP_indic%array2
               ELSEWHERE
                  tempP%array2 = tempP_average%array2 /tempP_indic%array2
               END WHERE

           ELSE
               WHERE (tempP_indic%array2 == 0.)
                  tempP%array2 = Agrif_SpecialValueFineGrid
               ELSEWHERE
                  tempP%array2 = tempP%array2 /tempP_indic%array2
               END WHERE
            ENDIF
           
            deallocate(tempP_indic%array2)
            deallocate(tempC_indic%array2)
            deallocate(tempC_indic)
            deallocate(tempP_indic)
            IF (compute_average) THEN
              deallocate(tempP_average%array2)
              deallocate(tempP_average)
            ENDIF
            ENDIF
            
        endif
        if ( nbdim == 3 ) then
            call Agrif_Update_3D_Recursive( type_update(1:3),   &
                                            tempP%array3,       &
                                            tempCextend%array3, &
                                            indmin(1:3), indmax(1:3),   &
                                            pttruetabwhole(1:3), cetruetabwhole(1:3),   &
                                            s_Child_temp(1:3), s_Parent_temp(1:3),      &
                                            ds_child(1:3), ds_parent(1:3) )
                                            
            IF (Agrif_UseSpecialValueInUpdate) THEN
            allocate(tempC_indic)
            allocate(tempP_indic)
            call Agrif_array_allocate(tempC_indic,lbound(tempCextend%array3),ubound(tempCextend%array3),nbdim)
            call Agrif_array_allocate(tempP_indic,lbound(tempP%array3),ubound(tempP%array3),nbdim)

            compute_average = .FALSE.
            type_update_temp(1:nbdim) = type_update(1:nbdim)
            IF (ANY(type_update == Agrif_Update_Full_Weighting)) THEN
              compute_average = .TRUE.
              allocate(tempP_average)
              call Agrif_array_allocate(tempP_average,lbound(tempP%array3),ubound(tempP%array3),nbdim)
              WHERE (type_update(1:nbdim) == Agrif_Update_Full_Weighting)
                type_update_temp(1:nbdim) = Agrif_Update_Average
              END WHERE
              call Agrif_Update_3D_Recursive( type_update_temp(1:3),   &
                                            tempP_average%array3,       &
                                            tempCextend%array3, &
                                            indmin(1:3), indmax(1:3),   &
                                            pttruetabwhole(1:3), cetruetabwhole(1:3),   &
                                            s_Child_temp(1:3), s_Parent_temp(1:3),      &
                                            ds_child(1:3), ds_parent(1:3) )
              coeff_multi = 1.
              do nb_dimensions=1,nbdim
                coeff_multi = coeff_multi * nint(ds_parent(nb_dimensions)/ds_child(nb_dimensions))
              enddo
            ENDIF
            
            WHERE (tempCextend%array3 == Agrif_SpecialValueFineGrid)
              tempC_indic%array3 = 0.
            ELSEWHERE
              tempC_indic%array3 = 1.
            END WHERE
            
            Agrif_UseSpecialValueInUpdate = .FALSE.
            Agrif_Update_Weights = .TRUE.
 
             call Agrif_Update_3D_Recursive( type_update_temp(1:3),   &
                                            tempP_indic%array3,       &
                                            tempC_indic%array3, &
                                            indmin(1:3), indmax(1:3),   &
                                            pttruetabwhole(1:3), cetruetabwhole(1:3),   &
                                            s_Child_temp(1:3), s_Parent_temp(1:3),      &
                                            ds_child(1:3), ds_parent(1:3) )

           Agrif_UseSpecialValueInUpdate = .TRUE.
           Agrif_Update_Weights = .FALSE.

           IF (compute_average) THEN
               WHERE (tempP_indic%array3 == 0.)
                  tempP%array3 = Agrif_SpecialValueFineGrid
               ELSEWHERE ((tempP_indic%array3 == coeff_multi).AND.(tempP%array3 /= Agrif_SpecialValueFineGrid))
                  tempP%array3 = tempP%array3 /tempP_indic%array3
               ELSEWHERE
                  tempP%array3 = tempP_average%array3 /tempP_indic%array3
               END WHERE

           ELSE
               WHERE (tempP_indic%array3 == 0.)
                  tempP%array3 = Agrif_SpecialValueFineGrid
               ELSEWHERE
                  tempP%array3 = tempP%array3 /tempP_indic%array3
               END WHERE
            ENDIF
           
            deallocate(tempP_indic%array3)
            deallocate(tempC_indic%array3)
            deallocate(tempC_indic)
            deallocate(tempP_indic)
            IF (compute_average) THEN
              deallocate(tempP_average%array3)
              deallocate(tempP_average)
            ENDIF
            ENDIF
            
        endif
        if ( nbdim == 4 ) then
            call Agrif_Update_4D_Recursive( type_update(1:4),   &
                                            tempP%array4,       &
                                            tempCextend%array4, &
                                            indmin(1:4), indmax(1:4),   &
                                            pttruetabwhole(1:4), cetruetabwhole(1:4),   &
                                            s_Child_temp(1:4), s_Parent_temp(1:4),      &
                                            ds_child(1:4), ds_parent(1:4) )
                                            
            IF (Agrif_UseSpecialValueInUpdate) THEN
            
            allocate(tempC_indic)
            allocate(tempP_indic)
            call Agrif_array_allocate(tempC_indic,lbound(tempCextend%array4),ubound(tempCextend%array4),nbdim)
            call Agrif_array_allocate(tempP_indic,lbound(tempP%array4),ubound(tempP%array4),nbdim)
           
            compute_average = .FALSE.
            type_update_temp(1:nbdim) = type_update(1:nbdim)
            IF (ANY(type_update == Agrif_Update_Full_Weighting)) THEN
              compute_average = .TRUE.
              allocate(tempP_average)
              call Agrif_array_allocate(tempP_average,lbound(tempP%array4),ubound(tempP%array4),nbdim)
              WHERE (type_update(1:nbdim) == Agrif_Update_Full_Weighting)
                type_update_temp(1:nbdim) = Agrif_Update_Average
              END WHERE
              call Agrif_Update_4D_Recursive( type_update_temp(1:4),   &
                                            tempP_average%array4,       &
                                            tempCextend%array4, &
                                            indmin(1:4), indmax(1:4),   &
                                            pttruetabwhole(1:4), cetruetabwhole(1:4),   &
                                            s_Child_temp(1:4), s_Parent_temp(1:4),      &
                                            ds_child(1:4), ds_parent(1:4) )
              coeff_multi = 1.
              do nb_dimensions=1,nbdim
                coeff_multi = coeff_multi * nint(ds_parent(nb_dimensions)/ds_child(nb_dimensions))
              enddo
            ENDIF
            
            WHERE (tempCextend%array4 == Agrif_SpecialValueFineGrid)
              tempC_indic%array4 = 0.
            ELSEWHERE
              tempC_indic%array4 = 1.
            END WHERE
            
            Agrif_UseSpecialValueInUpdate = .FALSE.
            Agrif_Update_Weights = .TRUE.
 
             call Agrif_Update_4D_Recursive( type_update_temp(1:4),   &
                                            tempP_indic%array4,       &
                                            tempC_indic%array4, &
                                            indmin(1:4), indmax(1:4),   &
                                            pttruetabwhole(1:4), cetruetabwhole(1:4),   &
                                            s_Child_temp(1:4), s_Parent_temp(1:4),      &
                                            ds_child(1:4), ds_parent(1:4) )

           Agrif_UseSpecialValueInUpdate = .TRUE.
           Agrif_Update_Weights = .FALSE.
           
           IF (compute_average) THEN
               WHERE (tempP_indic%array4 == 0.)
                  tempP%array4 = Agrif_SpecialValueFineGrid
               ELSEWHERE ((tempP_indic%array4 == coeff_multi).AND.(tempP%array4 /= Agrif_SpecialValueFineGrid))
                  tempP%array4 = tempP%array4 /tempP_indic%array4
               ELSEWHERE
                  tempP%array4 = tempP_average%array4 /tempP_indic%array4
               END WHERE

           ELSE
               WHERE (tempP_indic%array4 == 0.)
                  tempP%array4 = Agrif_SpecialValueFineGrid
               ELSEWHERE
                  tempP%array4 = tempP%array4 /tempP_indic%array4
               END WHERE
            ENDIF
            deallocate(tempP_indic%array4)
            deallocate(tempC_indic%array4)
            deallocate(tempC_indic)
            deallocate(tempP_indic)
            IF (compute_average) THEN
              deallocate(tempP_average%array4)
              deallocate(tempP_average)
            ENDIF
            ENDIF
            
        endif
        if ( nbdim == 5 ) then
            call Agrif_Update_5D_Recursive( type_update(1:5),   &
                                            tempP%array5,       &
                                            tempCextend%array5, &
                                            indmin(1:5), indmax(1:5),   &
                                            pttruetabwhole(1:5), cetruetabwhole(1:5),   &
                                            s_Child_temp(1:5), s_Parent_temp(1:5),      &
                                            ds_child(1:5), ds_parent(1:5) )
                                            
            IF (Agrif_UseSpecialValueInUpdate) THEN
            allocate(tempC_indic)
            allocate(tempP_indic)
            call Agrif_array_allocate(tempC_indic,lbound(tempCextend%array5),ubound(tempCextend%array5),nbdim)
            call Agrif_array_allocate(tempP_indic,lbound(tempP%array5),ubound(tempP%array5),nbdim)

            compute_average = .FALSE.
            type_update_temp(1:nbdim) = type_update(1:nbdim)
            IF (ANY(type_update == Agrif_Update_Full_Weighting)) THEN
              compute_average = .TRUE.
              allocate(tempP_average)
              call Agrif_array_allocate(tempP_average,lbound(tempP%array5),ubound(tempP%array5),nbdim)
              WHERE (type_update(1:nbdim) == Agrif_Update_Full_Weighting)
                type_update_temp(1:nbdim) = Agrif_Update_Average
              END WHERE
              call Agrif_Update_5D_Recursive( type_update_temp(1:5),   &
                                            tempP_average%array5,       &
                                            tempCextend%array5, &
                                            indmin(1:5), indmax(1:5),   &
                                            pttruetabwhole(1:5), cetruetabwhole(1:5),   &
                                            s_Child_temp(1:5), s_Parent_temp(1:5),      &
                                            ds_child(1:5), ds_parent(1:5) )
              coeff_multi = 1.
              do nb_dimensions=1,nbdim
                coeff_multi = coeff_multi * nint(ds_parent(nb_dimensions)/ds_child(nb_dimensions))
              enddo
            ENDIF
            
            WHERE (tempCextend%array5 == Agrif_SpecialValueFineGrid)
              tempC_indic%array5 = 0.
            ELSEWHERE
              tempC_indic%array5 = 1.
            END WHERE
            
            Agrif_UseSpecialValueInUpdate = .FALSE.
            Agrif_Update_Weights = .TRUE.
 
             call Agrif_Update_5D_Recursive( type_update_temp(1:5),   &
                                            tempP_indic%array5,       &
                                            tempC_indic%array5, &
                                            indmin(1:5), indmax(1:5),   &
                                            pttruetabwhole(1:5), cetruetabwhole(1:5),   &
                                            s_Child_temp(1:5), s_Parent_temp(1:5),      &
                                            ds_child(1:5), ds_parent(1:5) )

           Agrif_UseSpecialValueInUpdate = .TRUE.
           Agrif_Update_Weights = .FALSE.

           IF (compute_average) THEN
               WHERE (tempP_indic%array5 == 0.)
                  tempP%array5 = Agrif_SpecialValueFineGrid
               ELSEWHERE ((tempP_indic%array5 == coeff_multi).AND.(tempP%array5 /= Agrif_SpecialValueFineGrid))
                  tempP%array5 = tempP%array5 /tempP_indic%array5
               ELSEWHERE
                  tempP%array5 = tempP_average%array5 /tempP_indic%array5
               END WHERE

           ELSE
               WHERE (tempP_indic%array5 == 0.)
                  tempP%array5 = Agrif_SpecialValueFineGrid
               ELSEWHERE
                  tempP%array5 = tempP%array5 /tempP_indic%array5
               END WHERE
            ENDIF
           
            deallocate(tempP_indic%array5)
            deallocate(tempC_indic%array5)
            deallocate(tempC_indic)
            deallocate(tempP_indic)
            IF (compute_average) THEN
              deallocate(tempP_average%array5)
              deallocate(tempP_average)
            ENDIF
            ENDIF
            
        endif
        if ( nbdim == 6 ) then
            call Agrif_Update_6D_Recursive( type_update(1:6),   &
                                            tempP%array6,       &
                                            tempCextend%array6, &
                                            indmin(1:6), indmax(1:6),   &
                                            pttruetabwhole(1:6), cetruetabwhole(1:6),   &
                                            s_Child_temp(1:6), s_Parent_temp(1:6),      &
                                            ds_child(1:6), ds_parent(1:6) )
            IF (Agrif_UseSpecialValueInUpdate) THEN
            allocate(tempC_indic)
            allocate(tempP_indic)
            call Agrif_array_allocate(tempC_indic,lbound(tempCextend%array6),ubound(tempCextend%array6),nbdim)
            call Agrif_array_allocate(tempP_indic,lbound(tempP%array6),ubound(tempP%array6),nbdim)

            compute_average = .FALSE.
            type_update_temp(1:nbdim) = type_update(1:nbdim)
            IF (ANY(type_update == Agrif_Update_Full_Weighting)) THEN
              compute_average = .TRUE.
              allocate(tempP_average)
              call Agrif_array_allocate(tempP_average,lbound(tempP%array6),ubound(tempP%array6),nbdim)
              type_update_temp(1:nbdim) = type_update
              WHERE (type_update(1:nbdim) == Agrif_Update_Full_Weighting)
                type_update_temp(1:nbdim) = Agrif_Update_Average
              END WHERE
              call Agrif_Update_6D_Recursive( type_update_temp(1:6),   &
                                            tempP_average%array6,       &
                                            tempCextend%array6, &
                                            indmin(1:6), indmax(1:6),   &
                                            pttruetabwhole(1:6), cetruetabwhole(1:6),   &
                                            s_Child_temp(1:6), s_Parent_temp(1:6),      &
                                            ds_child(1:6), ds_parent(1:6) )
              coeff_multi = 1.
              do nb_dimensions=1,nbdim
                coeff_multi = coeff_multi * nint(ds_parent(nb_dimensions)/ds_child(nb_dimensions))
              enddo
            ENDIF

           IF (compute_average) THEN
               WHERE (tempP_indic%array6 == 0.)
                  tempP%array6 = Agrif_SpecialValueFineGrid
               ELSEWHERE ((tempP_indic%array6 == coeff_multi).AND.(tempP%array6 /= Agrif_SpecialValueFineGrid))
                  tempP%array6 = tempP%array6 /tempP_indic%array6
               ELSEWHERE
                  tempP%array6 = tempP_average%array6 /tempP_indic%array6
               END WHERE

           ELSE
               WHERE (tempP_indic%array6 == 0.)
                  tempP%array6 = Agrif_SpecialValueFineGrid
               ELSEWHERE
                  tempP%array6 = tempP%array6 /tempP_indic%array6
               END WHERE
            ENDIF
            
            Agrif_UseSpecialValueInUpdate = .FALSE.
            Agrif_Update_Weights = .TRUE.
 
             call Agrif_Update_6D_Recursive( type_update_temp(1:6),   &
                                            tempP_indic%array6,       &
                                            tempC_indic%array6, &
                                            indmin(1:6), indmax(1:6),   &
                                            pttruetabwhole(1:6), cetruetabwhole(1:6),   &
                                            s_Child_temp(1:6), s_Parent_temp(1:6),      &
                                            ds_child(1:6), ds_parent(1:6) )

           Agrif_UseSpecialValueInUpdate = .TRUE.
           Agrif_Update_Weights = .FALSE.
           
            WHERE (tempP_indic%array6 == 0.)
              tempP%array6 = Agrif_SpecialValueFineGrid
            ELSEWHERE
              tempP%array6 = tempP%array6 /tempP_indic%array6
            END WHERE
           
            deallocate(tempP_indic%array6)
            deallocate(tempC_indic%array6)
            deallocate(tempC_indic)
            deallocate(tempP_indic)
            IF (compute_average) THEN
              deallocate(tempP_average%array6)
              deallocate(tempP_average)
            ENDIF
            ENDIF
        endif
!
        call Agrif_array_deallocate(tempCextend,nbdim)
!
    ENDIF

#if defined AGRIF_MPI
    local_proc = Agrif_Procrank
    call Agrif_get_var_bounds_array(parent,lowerbound,upperbound,nbdim)
    call Agrif_ChildGrid_to_ParentGrid()
    call Agrif_Childbounds(nbdim, lowerbound, upperbound,                   &
                           indminglob,  indmaxglob,  local_proc, coords,    &
                           indminglob2, indmaxglob2, member)
!
    IF (member) THEN
        call Agrif_GlobalToLocalBounds(parentarray, lowerbound, upperbound, &
                                       indminglob2, indmaxglob2, coords,    &
                                       nbdim, local_proc, member)
    ENDIF

    call Agrif_ParentGrid_to_ChildGrid()

    if (.not.find_list_update) then
        tab3(:,1) = indmin(:)
        tab3(:,2) = indmax(:)
        tab3(:,3) = indminglob2(:)
        tab3(:,4) = indmaxglob2(:)
!
        call MPI_ALLGATHER(tab3,4*nbdim,MPI_INTEGER,tab4,4*nbdim,MPI_INTEGER,Agrif_mpi_comm,code)

        IF ( .not.associated(tempPextend) ) allocate(tempPextend)
        DO k=0,Agrif_Nbprocs-1
            do j=1,4
                do i=1,nbdim
                    tab5t(i,k,j) = tab4(i,j,k)
                enddo
            enddo
        enddo

        memberin1(1) = member
        call MPI_ALLGATHER(memberin1,1,MPI_LOGICAL,memberinall2,1,MPI_LOGICAL,Agrif_mpi_comm,code)
        call Get_External_Data_first(tab5t(:,:,1),tab5t(:,:,2),tab5t(:,:,3),tab5t(:,:,4),   &
                                     nbdim, memberinall2, coords,                           &
                                     sendtoproc2, recvfromproc2,                            &
                                     tab5t(:,:,5),tab5t(:,:,6),tab5t(:,:,7),tab5t(:,:,8),   &
                                     tab5t(:,:,1),tab5t(:,:,2))

        call Agrif_Addto_list_update(child%list_update,pttab,petab,lb_child,lb_parent,      &
                                     nbdim,tab4t,tab5t,memberinall,memberinall2,            &
                                     sendtoproc1,recvfromproc1,sendtoproc2,recvfromproc2)

    endif

    call ExchangeSameLevel(sendtoproc2,recvfromproc2,nbdim,                     &
                            tab5t(:,:,3),tab5t(:,:,4),tab5t(:,:,5),tab5t(:,:,6),&
                            tab5t(:,:,7),tab5t(:,:,8),member,tempP,tempPextend)
#else
    tempPextend => tempP
    parentarray(:,1,1) = indmin
    parentarray(:,2,1) = indmax
    parentarray(:,1,2) = indmin
    parentarray(:,2,2) = indmax
    member = .TRUE.
#endif
!
!   Special values on the child grid
    if ( Agrif_UseSpecialValueFineGrid ) then
!
!cc         noraftab(1:nbdim) =
!cc     &    child % root_var % interptab(1:nbdim) == 'N'
!
#if defined AGRIF_MPI
!
!          allocate(childvalues% var)
!
!          Call Agrif_array_allocate(childvalues%var,
!     &                      pttruetab,cetruetab,nbdim)
!          Call Agrif_var_full_copy_array(childvalues% var,
!     &                                tempC,
!     &                                nbdim)
!          Call Agrif_CheckMasknD(tempC,childvalues,
!     &                           pttruetab(1:nbdim),cetruetab(1:nbdim),
!     &                           pttruetab(1:nbdim),cetruetab(1:nbdim),
!     &                           noraftab(1:nbdim),nbdim)
!          Call Agrif_array_deallocate(childvalues% var,nbdim)
!         Deallocate(childvalues % var)
!
#else
!
!          Call Agrif_get_var_bounds_array(child,
!     &                              lowerbound,upperbound,nbdim)
!          Call Agrif_CheckMasknD(tempC,child,
!     &                           pttruetab(1:nbdim),cetruetab(1:nbdim),
!     &                           lowerbound,
!     &                           upperbound,
!     &                           noraftab(1:nbdim),nbdim)
!
#endif
!
    endif
!
!   Special values on the parent grid
    if (Agrif_UseSpecialValue) then
!
#if defined AGRIF_MPI
!
!          Call GiveAgrif_SpecialValueToTab_mpi(parent,tempP,
!     &                 parentarray,
!     &                 Agrif_SpecialValue,nbdim)
!
!
#else
!
!          Call GiveAgrif_SpecialValueToTab(parent,tempP,
!     &                  indmin,indmax,
!     &                  Agrif_SpecialValue,nbdim)
!
#endif
!
    endif
!
    IF (member) THEN

        call Agrif_ChildGrid_to_ParentGrid()
!
        SELECT CASE(nbdim)
        CASE(1)
            call procname( tempPextend % array1(            &
                    parentarray(1,1,1):parentarray(1,2,1)), &
                    parentarray(1,1,2),parentarray(1,2,2),.FALSE.,nbin,ndirin)
        CASE(2)
            call procname( tempPextend % array2(            &
                    parentarray(1,1,1):parentarray(1,2,1),  &
                    parentarray(2,1,1):parentarray(2,2,1)), &
                    parentarray(1,1,2),parentarray(1,2,2),  &
                    parentarray(2,1,2),parentarray(2,2,2),.FALSE.,nbin,ndirin)
        CASE(3)
            call procname( tempPextend % array3(            &
                    parentarray(1,1,1):parentarray(1,2,1),  &
                    parentarray(2,1,1):parentarray(2,2,1),  &
                    parentarray(3,1,1):parentarray(3,2,1)), &
                    parentarray(1,1,2),parentarray(1,2,2),  &
                    parentarray(2,1,2),parentarray(2,2,2),  &
                    parentarray(3,1,2),parentarray(3,2,2),.FALSE.,nbin,ndirin)
        CASE(4)
            call procname( tempPextend % array4(            &
                    parentarray(1,1,1):parentarray(1,2,1),  &
                    parentarray(2,1,1):parentarray(2,2,1),  &
                    parentarray(3,1,1):parentarray(3,2,1),  &
                    parentarray(4,1,1):parentarray(4,2,1)), &
                    parentarray(1,1,2),parentarray(1,2,2),  &
                    parentarray(2,1,2),parentarray(2,2,2),  &
                    parentarray(3,1,2),parentarray(3,2,2),  &
                    parentarray(4,1,2),parentarray(4,2,2),.FALSE.,nbin,ndirin)
        CASE(5)
            call procname( tempPextend % array5(            &
                    parentarray(1,1,1):parentarray(1,2,1),  &
                    parentarray(2,1,1):parentarray(2,2,1),  &
                    parentarray(3,1,1):parentarray(3,2,1),  &
                    parentarray(4,1,1):parentarray(4,2,1),  &
                    parentarray(5,1,1):parentarray(5,2,1)), &
                    parentarray(1,1,2),parentarray(1,2,2),  &
                    parentarray(2,1,2),parentarray(2,2,2),  &
                    parentarray(3,1,2),parentarray(3,2,2),  &
                    parentarray(4,1,2),parentarray(4,2,2),  &
                    parentarray(5,1,2),parentarray(5,2,2),.FALSE.,nbin,ndirin)
        CASE(6)
            call procname( tempPextend % array6(            &
                    parentarray(1,1,1):parentarray(1,2,1),  &
                    parentarray(2,1,1):parentarray(2,2,1),  &
                    parentarray(3,1,1):parentarray(3,2,1),  &
                    parentarray(4,1,1):parentarray(4,2,1),  &
                    parentarray(5,1,1):parentarray(5,2,1),  &
                    parentarray(6,1,1):parentarray(6,2,1)), &
                    parentarray(1,1,2),parentarray(1,2,2),  &
                    parentarray(2,1,2),parentarray(2,2,2),  &
                    parentarray(3,1,2),parentarray(3,2,2),  &
                    parentarray(4,1,2),parentarray(4,2,2),  &
                    parentarray(5,1,2),parentarray(5,2,2),  &
                    parentarray(6,1,2),parentarray(6,2,2),.FALSE.,nbin,ndirin)
        END SELECT
!
        call Agrif_ParentGrid_to_ChildGrid()
!
        call Agrif_array_deallocate(tempPextend,nbdim)
!
    ENDIF
!
#if defined AGRIF_MPI
    IF (memberin) THEN
        call Agrif_array_deallocate(tempP,nbdim)
        call Agrif_array_deallocate(tempC,nbdim)
    ENDIF
#endif
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_UpdatenD
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_Prtbounds
!
!> calculates the bounds of the parent grid to be updated by the child grid
!---------------------------------------------------------------------------------------------------
subroutine Agrif_Prtbounds ( nbdim, indmin, indmax, s_Parent_temp, s_Child_temp,        &
                             s_child, ds_child, s_parent, ds_parent,                    &
                             pttruetab, cetruetab, lb_child, lb_parent            &
#if defined AGRIF_MPI
                            ,posvar, type_update, do_update,                &
                             pttruetabwhole, cetruetabwhole                             &
#endif
                )
!---------------------------------------------------------------------------------------------------
    integer,                   intent(in)   :: nbdim
    integer, dimension(nbdim), intent(out)  :: indmin, indmax
    real,    dimension(nbdim), intent(out)  :: s_Parent_temp, s_Child_temp
    real,    dimension(nbdim), intent(in)   :: s_child,  ds_child
    real,    dimension(nbdim), intent(in)   :: s_parent, ds_parent
    integer, dimension(nbdim), intent(in)   :: pttruetab, cetruetab
    integer, dimension(nbdim), intent(in)   :: lb_child, lb_parent
#if defined AGRIF_MPI
    integer, dimension(nbdim), intent(in)   :: posvar   !< Position of the variable on the cell (1 or 2)
    integer, dimension(nbdim), intent(in)   :: type_update
    logical, dimension(nbdim), intent(in)   :: do_update
    integer,dimension(nbdim), intent(out)   :: pttruetabwhole, cetruetabwhole
#endif
!
    real,dimension(nbdim) :: dim_newmin,dim_newmax
    integer :: i
#if defined AGRIF_MPI
    real    :: positionmin, positionmax
    integer :: imin, imax
    integer :: coeffraf
#endif
!
    do i = 1,nbdim
!
        dim_newmin(i) = s_child(i) + (pttruetab(i) - lb_child(i)) * ds_child(i)
        dim_newmax(i) = s_child(i) + (cetruetab(i) - lb_child(i)) * ds_child(i)
!
        indmin(i) = lb_parent(i) + agrif_ceiling((dim_newmin(i)-s_parent(i))/ds_parent(i))
        indmax(i) = lb_parent(i) + agrif_int(    (dim_newmax(i)-s_parent(i))/ds_parent(i))
!
#if defined AGRIF_MPI
        positionmin = s_parent(i) + (indmin(i)-lb_parent(i))*ds_parent(i)
        IF ( do_update(i) ) THEN
            IF (posvar(i) == 1) THEN
                IF      (type_update(i) == Agrif_Update_Average) THEN
                    positionmin = positionmin - ds_parent(i)/2.
                ELSE IF (type_update(i) == Agrif_Update_Full_Weighting) THEN
                    positionmin = positionmin - (ds_parent(i)-ds_child(i))
                ENDIF
            ELSE
                IF (type_update(i) /= Agrif_Update_Full_Weighting) THEN
                    positionmin = positionmin - ds_parent(i)/2.
                ELSE
                    coeffraf = nint(ds_parent(i)/ds_child(i))
                    if (mod(coeffraf,2) == 1) then
                        positionmin = positionmin - (ds_parent(i)-ds_child(i))
                    else
                        positionmin = positionmin - (ds_parent(i)-ds_child(i))-ds_child(i)/2.
                    endif
                ENDIF
            ENDIF
        ENDIF
!
        imin = lb_child(i) + agrif_ceiling((positionmin-s_child(i))/ds_child(i))
        positionmin = s_child(i)  + (imin - lb_child(i)) * ds_child(i)
        positionmax = s_parent(i) + (indmax(i)-lb_parent(i))*ds_parent(i)
        pttruetabwhole(i) = imin

        IF ( do_update(i) ) THEN
            IF (posvar(i) == 1) THEN
                IF      (type_update(i) == Agrif_Update_Average) THEN
                    positionmax = positionmax  + ds_parent(i)/2.
                ELSE IF (type_update(i) == Agrif_Update_Full_Weighting) THEN
                    positionmax = positionmax  + (ds_parent(i)-ds_child(i))
                ENDIF
            ELSE
                IF (type_update(i) /= Agrif_Update_Full_Weighting) THEN
                    positionmax = positionmax  + ds_parent(i)/2.
                ELSE
                    coeffraf = nint(ds_parent(i)/ds_child(i))
                    if (mod(coeffraf,2) == 1) then
                        positionmax = positionmax + (ds_parent(i)-ds_child(i))
                    else
                        positionmax = positionmax + (ds_parent(i)-ds_child(i)) + ds_child(i)/2.
                    endif
                ENDIF
            ENDIF
        ENDIF

        imax = lb_child(i) +agrif_int((positionmax-s_child(i))/ds_child(i))
        positionmax = s_child(i) + (imax - lb_child(i)) * ds_child(i)
        cetruetabwhole(i) = imax
#endif
!
        s_Parent_temp(i) = s_parent(i) + (indmin(i) - lb_parent(i)) * ds_parent(i)
        s_Child_temp(i)  = dim_newmin(i)

#if defined AGRIF_MPI
        s_Child_temp(i) = positionmin
#endif
!
    enddo
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Prtbounds
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_Update_1D_Recursive
!
!> Updates a 1D grid variable on the parent grid
!---------------------------------------------------------------------------------------------------
subroutine Agrif_Update_1D_Recursive ( type_update,         &
                                       tempP, tempC,        &
                                       indmin, indmax,      &
                                       lb_child, ub_child,  &
                                       s_child,  s_parent,  &
                                       ds_child, ds_parent )
!---------------------------------------------------------------------------------------------------
    integer,                            intent(in)  :: type_update            !< Type of update (copy or average)
    integer,                            intent(in)  :: indmin, indmax
    integer,                            intent(in)  :: lb_child, ub_child
    real,                               intent(in)  ::  s_child,  s_parent
    real,                               intent(in)  :: ds_child, ds_parent
    real, dimension(indmin:indmax),     intent(out) :: tempP
    real, dimension(lb_child:ub_child), intent(in)  :: tempC
!---------------------------------------------------------------------------------------------------
    call Agrif_UpdateBase(type_update,              &
                          tempP(indmin:indmax),     &
                          tempC(lb_child:ub_child), &
                          indmin, indmax,           &
                          lb_child, ub_child,       &
                          s_parent, s_child,        &
                          ds_parent, ds_child)
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Update_1D_Recursive
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_Update_2D_Recursive
!
!> updates a 2D grid variable on the parent grid.
!! Calls #Agrif_Update_1D_Recursive and #Agrif_UpdateBase
!---------------------------------------------------------------------------------------------------
subroutine Agrif_Update_2D_Recursive ( type_update,         &
                                       tempP, tempC,        &
                                       indmin, indmax,      &
                                       lb_child, ub_child,  &
                                        s_child,  s_parent, &
                                       ds_child, ds_parent )
!---------------------------------------------------------------------------------------------------
    integer, dimension(2),          intent(in)  :: type_update            !< Type of update (copy or average)
    integer, dimension(2),          intent(in)  :: indmin, indmax
    integer, dimension(2),          intent(in)  :: lb_child, ub_child
    real,    dimension(2),          intent(in)  ::  s_child,  s_parent
    real,    dimension(2),          intent(in)  :: ds_child, ds_parent
    real,    dimension(          &
        indmin(1):indmax(1),     &
        indmin(2):indmax(2)),       intent(out) :: tempP
    real,    dimension(:,:),        intent(in)  :: tempC
!---------------------------------------------------------------------------------------------------
    real, dimension(indmin(1):indmax(1), lb_child(2):ub_child(2)) :: tabtemp
    real, dimension(indmin(2):indmax(2), indmin(1):indmax(1))     :: tempP_trsp
    real, dimension(lb_child(2):ub_child(2), indmin(1):indmax(1)) :: tabtemp_trsp
    integer :: i, j
    integer :: coeffraf
!
    tabtemp = 0.
    coeffraf = nint ( ds_parent(1) / ds_child(1) )
!
    IF((type_update(1) == Agrif_Update_Average) .AND. (coeffraf /= 1 )) THEN
!---CDIR NEXPAND
        if ( .NOT. precomputedone(1) ) then
            call Average1dPrecompute( ub_child(2)-lb_child(2)+1,    &
                                      indmax(1)-indmin(1)+1,              &
                                      ub_child(1)-lb_child(1)+1,    &
                                      s_parent(1),s_child(1),ds_parent(1),ds_child(1),1)
!            precomputedone(1) = .TRUE.
        endif
!---CDIR NEXPAND
        call Average1dAfterCompute( tabtemp, tempC, size(tabtemp), size(tempC), &
                    s_parent(1),s_child(1),ds_parent(1),ds_child(1),1)
!
    ELSE IF ((type_update(1) == Agrif_Update_Copy) .AND. (coeffraf /= 1 ))THEN
!---CDIR NEXPAND
         if ( .NOT. precomputedone(1) ) then
            call Agrif_basicupdate_copy1d_before( ub_child(2)-lb_child(2)+1, &
                                   indmax(1)-indmin(1)+1,           &
                                   ub_child(1)-lb_child(1)+1, &
                                   s_parent(1),s_child(1),ds_parent(1),ds_child(1),1)
!            precomputedone(1) = .TRUE.
        endif
!---CDIR NEXPAND
        call Agrif_basicupdate_copy1d_after(tabtemp,tempC,size(tabtemp),size(tempC),1)
!
    ELSE
        do j = lb_child(2),ub_child(2)
!
!---CDIR NEXPAND
            call Agrif_Update_1D_Recursive( type_update(1),             &
                                            tabtemp(:,j),               &
                                            tempC(:,j-lb_child(2)+1),   &
                                            indmin(1), indmax(1),       &
                                            lb_child(1),ub_child(1),    &
                                            s_child(1),  s_parent(1),   &
                                            ds_child(1),ds_parent(1))
        enddo
    ENDIF
!
    tabtemp_trsp = TRANSPOSE(tabtemp)
    coeffraf = nint(ds_parent(2)/ds_child(2))
!
    tempP_trsp = 0.
!
    IF((type_update(2) == Agrif_Update_Average) .AND. (coeffraf /= 1 )) THEN
!---CDIR NEXPAND
        if ( .NOT. precomputedone(2) ) then
            call Average1dPrecompute( indmax(1)-indmin(1)+1,          &
                                      indmax(2)-indmin(2)+1,          &
                                      ub_child(2)-lb_child(2)+1,&
                                      s_parent(2),s_child(2),ds_parent(2),ds_child(2),2)
!            precomputedone(2) = .TRUE.
        endif
!---CDIR NEXPAND
        call Average1dAfterCompute( tempP_trsp, tabtemp_trsp, size(tempP_trsp), size(tabtemp_trsp),&
                s_parent(2),s_child(2),ds_parent(2),ds_child(2),2)
!
    ELSE IF ((type_update(2) == Agrif_Update_Copy) .AND. (coeffraf /= 1 )) THEN
!---CDIR NEXPAND
        if ( .NOT. precomputedone(2) ) then
            call Agrif_basicupdate_copy1d_before( indmax(1)-indmin(1)+1,             &
                                   indmax(2)-indmin(2)+1,             &
                                   ub_child(2)-lb_child(2)+1,   &
                                   s_parent(2),s_child(2),ds_parent(2),ds_child(2),2)
!            precomputedone(2) = .TRUE.
        endif
!---CDIR NEXPAND
        call Agrif_basicupdate_copy1d_after( tempP_trsp, tabtemp_trsp, size(tempP_trsp), size(tabtemp_trsp),2)
!
    ELSE
        do i = indmin(1),indmax(1)
!
!---CDIR NEXPAND
            call Agrif_UpdateBase(type_update(2),                                       &
                                  tempP_trsp(indmin(2):indmax(2),i),            &
                                  tabtemp_trsp(lb_child(2):ub_child(2),i),&
                                  indmin(2),indmax(2),                          &
                                  lb_child(2),ub_child(2),                &
                                  s_parent(2),s_child(2),                       &
                                  ds_parent(2),ds_child(2))
!
        enddo
    ENDIF
!
    tempP = TRANSPOSE(tempP_trsp)
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Update_2D_Recursive
!===================================================================================================
!
subroutine Agrif_Update_2D_Recursive_ok ( type_update, &
                                        tempP, tempC, &
                                        indmin, indmax,   &
                                       lb_child, ub_child,                    &
                                       s_child, s_parent, ds_child, ds_parent )
!---------------------------------------------------------------------------------------------------
    INTEGER, DIMENSION(2), intent(in)   :: type_update            !< Type of update (copy or average)
    INTEGER, DIMENSION(2), intent(in)   :: indmin, indmax
    INTEGER, DIMENSION(2), intent(in)   :: lb_child, ub_child
    REAL,    DIMENSION(2), intent(in)   :: s_child,  s_parent
    REAL,    DIMENSION(2), intent(in)   :: ds_child, ds_parent
    REAL,    DIMENSION(                 &
                indmin(1):indmax(1),    &
                indmin(2):indmax(2)),           intent(out) :: tempP
    REAL, DIMENSION(                            &
                lb_child(1):ub_child(1),  &
                lb_child(2):ub_child(2)), intent(in)  :: tempC
!
    REAL, DIMENSION(indmin(1):indmax(1), lb_child(2):ub_child(2)) :: tabtemp
    INTEGER :: i
!
    do i = lb_child(2),ub_child(2)
        call Agrif_Update_1D_Recursive(type_update(1),                              &
                                       tabtemp(:, i),          &
                                       tempC(:,i),  &
                                       indmin(1),indmax(1),                 &
                                       lb_child(1),ub_child(1),       &
                                       s_child(1), s_parent(1),             &
                                      ds_child(1),ds_parent(1))
    enddo
!
    tempP = 0.
!
    do i = indmin(1),indmax(1)
        call Agrif_UpdateBase(type_update(2),                                       &
                              tempP(i,:),             &
                              tabtemp(i,:), &
                              indmin(2),indmax(2),                          &
                              lb_child(2),ub_child(2),                &
                              s_parent(2),s_child(2),                       &
                             ds_parent(2),ds_child(2))
    enddo
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Update_2D_Recursive_ok
!===================================================================================================

!
!===================================================================================================
!  subroutine Agrif_Update_3D_Recursive
!
!> Updates a 3D grid variable on the parent grid.
!! Calls #Agrif_Update_2D_Recursive and #Agrif_UpdateBase.
!---------------------------------------------------------------------------------------------------
subroutine Agrif_Update_3D_Recursive ( type_update,         &
                                       tempP, tempC,        &
                                       indmin, indmax,      &
                                       lb_child, ub_child,  &
                                        s_child,  s_parent, &
                                       ds_child, ds_parent )
!---------------------------------------------------------------------------------------------------
    integer, dimension(3),          intent(in)  :: type_update            !< Type of update (copy or average)
    integer, dimension(3),          intent(in)  :: indmin, indmax
    integer, dimension(3),          intent(in)  :: lb_child, ub_child
    real,    dimension(3),          intent(in)  ::  s_child,  s_parent
    real,    dimension(3),          intent(in)  :: ds_child, ds_parent
    real,    dimension(          &
        indmin(1):indmax(1),     &
        indmin(2):indmax(2),     &
        indmin(3):indmax(3)),       intent(out) :: tempP
    real, dimension(             &
        lb_child(1):ub_child(1), &
        lb_child(2):ub_child(2), &
        lb_child(3):ub_child(3)),   intent(in)  :: tempC
!---------------------------------------------------------------------------------------------------
    real, dimension(            &
        indmin(1):indmax(1),    &
        indmin(2):indmax(2),    &
        lb_child(3):ub_child(3))    :: tabtemp
    integer :: i,j,k
    integer :: coeffraf,locind_child_left
    integer :: kuinf
!
    coeffraf = nint ( ds_parent(1) / ds_child(1) )
!
    if ((type_update(1) == Agrif_Update_Average) .AND. (coeffraf /= 1 )) then
!---CDIR NEXPAND
        call Average1dPrecompute(ub_child(2)-lb_child(2)+1,&
                                 indmax(1)-indmin(1)+1,&
                                 ub_child(1)-lb_child(1)+1,&
                                 s_parent(1),s_child(1),ds_parent(1),ds_child(1),1)
        precomputedone(1) = .TRUE.
    else if ((type_update(1) == Agrif_Update_Copy) .AND. (coeffraf /= 1 )) then
!---CDIR NEXPAND
         call Agrif_basicupdate_copy1d_before(ub_child(2)-lb_child(2)+1, &
                               indmax(1)-indmin(1)+1,           &
                               ub_child(1)-lb_child(1)+1, &
                               s_parent(1),s_child(1),ds_parent(1),ds_child(1),1)
        precomputedone(1) = .TRUE.
    endif
!
    coeffraf = nint ( ds_parent(2) / ds_child(2) )
!
    if ((type_update(2) == Agrif_Update_Average) .AND. (coeffraf /= 1 )) then
!---CDIR NEXPAND
        call Average1dPrecompute(indmax(1)-indmin(1)+1,&
                                 indmax(2)-indmin(2)+1,&
                                 ub_child(2)-lb_child(2)+1,&
                                 s_parent(2),s_child(2),ds_parent(2),ds_child(2),2)
        precomputedone(2) = .TRUE.
    else if ((type_update(2) == Agrif_Update_Copy) .AND. (coeffraf /= 1 )) then
!---CDIR NEXPAND
        call Agrif_basicupdate_copy1d_before( indmax(1)-indmin(1)+1,           &
                               indmax(2)-indmin(2)+1,           &
                               ub_child(2)-lb_child(2)+1, &
                               s_parent(2),s_child(2),ds_parent(2),ds_child(2),2)
        precomputedone(2) = .TRUE.
    endif
!
    do k = lb_child(3),ub_child(3)
        call Agrif_Update_2D_Recursive( type_update(1:2),tabtemp(:,:,k),tempC(:,:,k), &
                                        indmin(1:2),indmax(1:2),                &
                                        lb_child(1:2),ub_child(1:2),      &
                                        s_child(1:2),s_parent(1:2),             &
                                        ds_child(1:2),ds_parent(1:2) )
    enddo
!
    precomputedone(1) = .FALSE.
    precomputedone(2) = .FALSE.
!
    coeffraf = nint ( ds_parent(3) / ds_child(3) )
    locind_child_left = 1 + agrif_int((s_parent(3)-s_child(3))/ds_child(3))
!
    if (coeffraf == 1) then
        kuinf = lb_child(3)+locind_child_left-2
        do k=indmin(3),indmax(3)
            kuinf = kuinf + 1
            do j = indmin(2),indmax(2)
            do i = indmin(1),indmax(1)
                tempP(i,j,k) = tabtemp(i,j,kuinf)
            enddo
            enddo
        enddo
    else
        tempP = 0.
        do j = indmin(2),indmax(2)
        do i = indmin(1),indmax(1)
            call Agrif_UpdateBase(type_update(3),tempP(i,j,:),tabtemp(i,j,:),   &
                                  indmin(3),indmax(3),                  &
                                  lb_child(3),ub_child(3),        &
                                  s_parent(3),s_child(3),               &
                                  ds_parent(3),ds_child(3))
!
        enddo
        enddo
    endif
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Update_3D_Recursive
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_Update_4D_Recursive
!
!> Updates a 4D grid variable on the parent grid.
!! Calls #Agrif_Update_3D_Recursive and #Agrif_UpdateBase.
!---------------------------------------------------------------------------------------------------
subroutine Agrif_Update_4D_Recursive ( type_update,         &
                                       tempP, tempC,        &
                                       indmin, indmax,      &
                                       lb_child, ub_child,  &
                                        s_child,  s_parent, &
                                       ds_child, ds_parent )
!---------------------------------------------------------------------------------------------------
    integer, dimension(4),          intent(in)  :: type_update            !< Type of update (copy or average)
    integer, dimension(4),          intent(in)  :: indmin, indmax
    integer, dimension(4),          intent(in)  :: lb_child, ub_child
    real,    dimension(4),          intent(in)  ::  s_child,  s_parent
    real,    dimension(4),          intent(in)  :: ds_child, ds_parent
    real,    dimension(          &
        indmin(1):indmax(1),     &
        indmin(2):indmax(2),     &
        indmin(3):indmax(3),     &
        indmin(4):indmax(4)),       intent(out) :: tempP
    real, dimension(             &
        lb_child(1):ub_child(1), &
        lb_child(2):ub_child(2), &
        lb_child(3):ub_child(3), &
        lb_child(4):ub_child(4)),   intent(in)  :: tempC
!---------------------------------------------------------------------------------------------------
    real, dimension(:,:,:,:), allocatable       :: tabtemp
    integer :: i,j,k,l
!
    allocate(tabtemp(indmin(1):indmax(1), &
                     indmin(2):indmax(2), &
                     indmin(3):indmax(3), &
                     lb_child(4):ub_child(4)))
!
    do l = lb_child(4), ub_child(4)
        call Agrif_Update_3D_Recursive(type_update(1:3),                    &
                                       tabtemp(indmin(1):indmax(1),         &
                                               indmin(2):indmax(2),         &
                                               indmin(3):indmax(3), l),     &
                                       tempC(lb_child(1):ub_child(1),       &
                                             lb_child(2):ub_child(2),       &
                                             lb_child(3):ub_child(3), l),   &
                                       indmin(1:3), indmax(1:3),            &
                                       lb_child(1:3), ub_child(1:3),        &
                                        s_child(1:3),  s_parent(1:3),       &
                                       ds_child(1:3), ds_parent(1:3))
    enddo
!
    tempP = 0.
!
    do k = indmin(3), indmax(3)
    do j = indmin(2), indmax(2)
    do i = indmin(1), indmax(1)
        call Agrif_UpdateBase(type_update(4),                               &
                              tempP(i,j,k,indmin(4):indmax(4)),             &
                              tabtemp(i,j,k,lb_child(4):ub_child(4)),       &
                              indmin(4), indmax(4),                         &
                              lb_child(4), ub_child(4),                     &
                               s_parent(4), s_child(4),                     &
                              ds_parent(4),ds_child(4) )
    enddo
    enddo
    enddo
!
    deallocate(tabtemp)
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Update_4D_Recursive
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_Update_5D_Recursive
!
!> Updates a 5D grid variable on the parent grid.
!! Calls #Agrif_Update_4D_Recursive and #Agrif_UpdateBase.
!---------------------------------------------------------------------------------------------------
subroutine Agrif_Update_5D_Recursive ( type_update,         &
                                       tempP, tempC,        &
                                       indmin, indmax,      &
                                       lb_child, ub_child,  &
                                        s_child,  s_parent, &
                                       ds_child, ds_parent )
!---------------------------------------------------------------------------------------------------
    integer, dimension(5),          intent(in)  :: type_update            !< Type of update (copy or average)
    integer, dimension(5),          intent(in)  :: indmin, indmax
    integer, dimension(5),          intent(in)  :: lb_child, ub_child
    real,    dimension(5),          intent(in)  ::  s_child,  s_parent
    real,    dimension(5),          intent(in)  :: ds_child, ds_parent
    real,    dimension(          &
        indmin(1):indmax(1),     &
        indmin(2):indmax(2),     &
        indmin(3):indmax(3),     &
        indmin(4):indmax(4),     &
        indmin(5):indmax(5)),       intent(out) :: tempP
    real, dimension(             &
        lb_child(1):ub_child(1), &
        lb_child(2):ub_child(2), &
        lb_child(3):ub_child(3), &
        lb_child(4):ub_child(4), &
        lb_child(5):ub_child(5)),   intent(in)  :: tempC
!---------------------------------------------------------------------------------------------------
    real, dimension(:,:,:,:,:), allocatable     :: tabtemp
    integer :: i,j,k,l,m
!
    allocate(tabtemp(indmin(1):indmax(1), &
                     indmin(2):indmax(2), &
                     indmin(3):indmax(3), &
                     indmin(4):indmax(4), &
                     lb_child(5):ub_child(5)))
!
    do m = lb_child(5), ub_child(5)
        call Agrif_Update_4D_Recursive(type_update(1:4),                    &
                                       tabtemp(indmin(1):indmax(1),         &
                                               indmin(2):indmax(2),         &
                                               indmin(3):indmax(3),         &
                                               indmin(4):indmax(4), m),     &
                                       tempC(lb_child(1):ub_child(1),       &
                                             lb_child(2):ub_child(2),       &
                                             lb_child(3):ub_child(3),       &
                                             lb_child(4):ub_child(4), m),   &
                                       indmin(1:4),indmax(1:4),             &
                                       lb_child(1:4), ub_child(1:4),        &
                                        s_child(1:4), s_parent(1:4),        &
                                       ds_child(1:4), ds_parent(1:4))
    enddo
!
    tempP = 0.
!
    do l = indmin(4), indmax(4)
    do k = indmin(3), indmax(3)
    do j = indmin(2), indmax(2)
    do i = indmin(1), indmax(1)
        call Agrif_UpdateBase( type_update(5),                              &
                               tempP(i,j,k,l,indmin(5):indmax(5)),          &
                               tabtemp(i,j,k,l,lb_child(5):ub_child(5)),    &
                               indmin(5), indmax(5),                        &
                               lb_child(5), ub_child(5),                    &
                                s_parent(5), s_child(5),                    &
                               ds_parent(5),ds_child(5) )
    enddo
    enddo
    enddo
    enddo
!
    deallocate(tabtemp)
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Update_5D_Recursive
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_Update_6D_Recursive
!
!> Updates a 6D grid variable on the parent grid.
!! Calls #Agrif_Update_5D_Recursive and #Agrif_UpdateBase.
!---------------------------------------------------------------------------------------------------
subroutine Agrif_Update_6D_Recursive ( type_update,         &
                                       tempP, tempC,        &
                                       indmin, indmax,      &
                                       lb_child, ub_child,  &
                                        s_child,  s_parent, &
                                       ds_child, ds_parent )
!---------------------------------------------------------------------------------------------------
    integer, dimension(6),          intent(in)  :: type_update            !< Type of update (copy or average)
    integer, dimension(6),          intent(in)  :: indmin, indmax
    integer, dimension(6),          intent(in)  :: lb_child, ub_child
    real,    dimension(6),          intent(in)  ::  s_child,  s_parent
    real,    dimension(6),          intent(in)  :: ds_child, ds_parent
    real,    dimension(          &
        indmin(1):indmax(1),     &
        indmin(2):indmax(2),     &
        indmin(3):indmax(3),     &
        indmin(4):indmax(4),     &
        indmin(5):indmax(5),     &
        indmin(6):indmax(6)),       intent(out) :: tempP
    real, dimension(             &
        lb_child(1):ub_child(1), &
        lb_child(2):ub_child(2), &
        lb_child(3):ub_child(3), &
        lb_child(4):ub_child(4), &
        lb_child(5):ub_child(5), &
        lb_child(6):ub_child(6)),   intent(in)  :: tempC
!---------------------------------------------------------------------------------------------------
    real, dimension(:,:,:,:,:,:), allocatable   :: tabtemp
    integer :: i,j,k,l,m,n
!
    allocate(tabtemp(indmin(1):indmax(1), &
                     indmin(2):indmax(2), &
                     indmin(3):indmax(3), &
                     indmin(4):indmax(4), &
                     indmin(5):indmax(5), &
                     lb_child(6):ub_child(6)))
!
    do n = lb_child(6),ub_child(6)
        call Agrif_Update_5D_Recursive(type_update(1:5),                    &
                                       tabtemp(indmin(1):indmax(1),         &
                                               indmin(2):indmax(2),         &
                                               indmin(3):indmax(3),         &
                                               indmin(4):indmax(4),         &
                                               indmin(5):indmax(5), n),     &
                                       tempC(lb_child(1):ub_child(1),       &
                                             lb_child(2):ub_child(2),       &
                                             lb_child(3):ub_child(3),       &
                                             lb_child(4):ub_child(4),       &
                                             lb_child(5):ub_child(5), n),   &
                                       indmin(1:5), indmax(1:5),            &
                                       lb_child(1:5),ub_child(1:5),         &
                                       s_child(1:5), s_parent(1:5),         &
                                      ds_child(1:5),ds_parent(1:5))
    enddo
!
    tempP = 0.
!
    do m = indmin(5), indmax(5)
    do l = indmin(4), indmax(4)
    do k = indmin(3), indmax(3)
    do j = indmin(2), indmax(2)
    do i = indmin(1), indmax(1)
        call Agrif_UpdateBase( type_update(6),                              &
                               tempP(i,j,k,l,m,indmin(6):indmax(6)),        &
                               tabtemp(i,j,k,l,m,lb_child(6):ub_child(6)),  &
                               indmin(6), indmax(6),                        &
                               lb_child(6),  ub_child(6),                   &
                                s_parent(6),  s_child(6),                   &
                               ds_parent(6), ds_child(6) )
    enddo
    enddo
    enddo
    enddo
    enddo
!
    deallocate(tabtemp)
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Update_6D_Recursive
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_UpdateBase
!
!> Calls the updating method chosen by the user (copy, average or full-weighting).
!---------------------------------------------------------------------------------------------------
subroutine Agrif_UpdateBase ( type_update,              &
                              parent_tab, child_tab,    &
                              indmin, indmax,           &
                              lb_child, ub_child,       &
                              s_parent, s_child,        &
                              ds_parent, ds_child )
!---------------------------------------------------------------------------------------------------
    integer,                            intent(in) :: type_update
    integer,                            intent(in) :: indmin, indmax
    integer,                            intent(in) :: lb_child, ub_child
    real, dimension(indmin:indmax),     intent(out):: parent_tab
    real, dimension(lb_child:ub_child), intent(in) :: child_tab
    real,                               intent(in) :: s_parent,  s_child
    real,                               intent(in) :: ds_parent, ds_child
!---------------------------------------------------------------------------------------------------
    integer :: np       ! Length of parent array
    integer :: nc       ! Length of child  array
!
    np = indmax - indmin + 1
    nc = ub_child - lb_child + 1
!
    if     ( type_update == Agrif_Update_Copy ) then
!
        call Agrif_basicupdate_copy1d(              &
                    parent_tab, child_tab,          &
                    np,         nc,                 &
                    s_parent,  s_child,             &
                    ds_parent, ds_child )
!
    elseif ( type_update == Agrif_Update_Average ) then
!
        call Agrif_basicupdate_average1d(           &
                    parent_tab, child_tab,          &
                    np,         nc,                 &
                     s_parent,  s_child,            &
                    ds_parent, ds_child )
!
    elseif ( type_update == Agrif_Update_Full_Weighting ) then
!
        call Agrif_basicupdate_full_weighting1D(    &
                    parent_tab, child_tab,          &
                    np,         nc,                 &
                    s_parent,  s_child,             &
                    ds_parent, ds_child )
!
    endif
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_UpdateBase
!===================================================================================================
!
#if defined AGRIF_MPI
!===================================================================================================
!  subroutine Agrif_Find_list_update
!---------------------------------------------------------------------------------------------------
subroutine Agrif_Find_list_update ( list_update, pttab, petab, lb_child, lb_parent, nbdim, &
                                    find_list_update, tab4t, tab5t, memberinall, memberinall2,   &
                                    sendtoproc1, recvfromproc1, sendtoproc2, recvfromproc2 )
!---------------------------------------------------------------------------------------------------
    TYPE(Agrif_List_Interp_Loc),                   pointer     :: list_update
    INTEGER,                                       intent(in)  :: nbdim
    INTEGER, DIMENSION(nbdim),                     intent(in)  :: pttab, petab
    INTEGER, DIMENSION(nbdim),                     intent(in)  :: lb_child, lb_parent
    LOGICAL,                                       intent(out) :: find_list_update
    INTEGER, DIMENSION(nbdim,0:Agrif_Nbprocs-1,8), intent(out) :: tab4t
    INTEGER, DIMENSION(nbdim,0:Agrif_Nbprocs-1,8), intent(out) :: tab5t
    LOGICAL, DIMENSION(0:Agrif_Nbprocs-1), intent(out) :: memberinall,memberinall2
    LOGICAL, DIMENSION(0:Agrif_Nbprocs-1), intent(out) :: sendtoproc1,recvfromproc1
    LOGICAL, DIMENSION(0:Agrif_Nbprocs-1), intent(out) :: sendtoproc2,recvfromproc2
!
    Type(Agrif_List_Interp_Loc), Pointer :: parcours
    INTEGER :: i
!
    find_list_update = .FALSE.
!
    parcours => list_update

    Find_loop :  do while ( associated(parcours) )
        do i = 1,nbdim
            IF ((pttab(i) /= parcours%interp_loc%pttab(i)) .OR. &
                (petab(i) /= parcours%interp_loc%petab(i)) .OR. &
                (lb_child(i)  /= parcours%interp_loc%pttab_child(i)) .OR. &
                (lb_parent(i) /= parcours%interp_loc%pttab_parent(i))) THEN
                parcours => parcours%suiv
                cycle Find_loop
            ENDIF
        enddo
!
        tab4t = parcours%interp_loc%tab4t(1:nbdim,0:Agrif_Nbprocs-1,1:8)
        tab5t = parcours%interp_loc%tab5t(1:nbdim,0:Agrif_Nbprocs-1,1:8)
        memberinall  =  parcours%interp_loc%memberinall(0:Agrif_Nbprocs-1)
        memberinall2 =  parcours%interp_loc%memberinall2(0:Agrif_Nbprocs-1)
        sendtoproc1 =   parcours%interp_loc%sendtoproc1(0:Agrif_Nbprocs-1)
        sendtoproc2 =   parcours%interp_loc%sendtoproc2(0:Agrif_Nbprocs-1)
        recvfromproc1 = parcours%interp_loc%recvfromproc1(0:Agrif_Nbprocs-1)
        recvfromproc2 = parcours%interp_loc%recvfromproc2(0:Agrif_Nbprocs-1)
!
        find_list_update = .TRUE.
        exit Find_loop
!
    enddo Find_loop
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Find_list_update
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_AddTo_list_update
!---------------------------------------------------------------------------------------------------
subroutine Agrif_AddTo_list_update ( list_update, pttab, petab, lb_child, lb_parent,  &
                                     nbdim, tab4t, tab5t, memberinall, memberinall2,        &
                                     sendtoproc1, recvfromproc1, sendtoproc2, recvfromproc2 )
!---------------------------------------------------------------------------------------------------
    TYPE(Agrif_List_Interp_Loc), pointer :: list_update
    INTEGER,                                        intent(in) :: nbdim
    INTEGER, DIMENSION(nbdim),                      intent(in) :: pttab, petab
    INTEGER, DIMENSION(nbdim),                      intent(in) :: lb_child, lb_parent
    INTEGER, DIMENSION(nbdim,0:Agrif_Nbprocs-1,8),  intent(in) :: tab4t
    INTEGER, DIMENSION(nbdim,0:Agrif_Nbprocs-1,8),  intent(in) :: tab5t
    LOGICAL, DIMENSION(0:Agrif_Nbprocs-1),          intent(in) :: memberinall, memberinall2
    LOGICAL, DIMENSION(0:Agrif_Nbprocs-1),          intent(in) :: sendtoproc1, recvfromproc1
    LOGICAL, DIMENSION(0:Agrif_Nbprocs-1),          intent(in) :: sendtoproc2, recvfromproc2
!
    Type(Agrif_List_Interp_Loc), pointer :: parcours
!
    allocate(parcours)
    allocate(parcours%interp_loc)

    parcours%interp_loc%pttab(1:nbdim) = pttab(1:nbdim)
    parcours%interp_loc%petab(1:nbdim) = petab(1:nbdim)
    parcours%interp_loc%pttab_child(1:nbdim)  = lb_child(1:nbdim)
    parcours%interp_loc%pttab_parent(1:nbdim) = lb_parent(1:nbdim)

    allocate(parcours%interp_loc%tab4t(nbdim,0:Agrif_Nbprocs-1,8))
    allocate(parcours%interp_loc%tab5t(nbdim,0:Agrif_Nbprocs-1,8))

    allocate(parcours%interp_loc%memberinall (0:Agrif_Nbprocs-1))
    allocate(parcours%interp_loc%memberinall2(0:Agrif_Nbprocs-1))

    allocate(parcours%interp_loc%recvfromproc1(0:Agrif_Nbprocs-1))
    allocate(parcours%interp_loc%recvfromproc2(0:Agrif_Nbprocs-1))
    allocate(parcours%interp_loc%sendtoproc1(0:Agrif_Nbprocs-1))
    allocate(parcours%interp_loc%sendtoproc2(0:Agrif_Nbprocs-1))

    parcours%interp_loc%tab4t = tab4t
    parcours%interp_loc%tab5t = tab5t
    parcours%interp_loc%memberinall   = memberinall
    parcours%interp_loc%memberinall2  = memberinall2
    parcours%interp_loc%sendtoproc1   = sendtoproc1
    parcours%interp_loc%sendtoproc2   = sendtoproc2
    parcours%interp_loc%recvfromproc1 = recvfromproc1
    parcours%interp_loc%recvfromproc2 = recvfromproc2

    parcours%suiv => list_update
    list_update => parcours
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Addto_list_update
!===================================================================================================
#endif
!
end module Agrif_Update
