!
! $Id: modbcfunction.F 779 2007-12-22 17:04:17Z rblod $
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
!     Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
!
!> Module Agrif_BcFunction.
!
module Agrif_BcFunction
!
!     Modules used:
!
    use Agrif_Boundary
    use Agrif_Update
    use Agrif_Save
!
    implicit none
!
    interface Agrif_Set_Parent
        module procedure Agrif_Set_Parent_int,      &
                         Agrif_Set_Parent_real4,    &
                         Agrif_Set_Parent_real8
    end interface
!
    interface Agrif_Save_Forrestore
        module procedure Agrif_Save_Forrestore0d,   &
                         Agrif_Save_Forrestore2d,   &
                         Agrif_Save_Forrestore3d,   &
                         Agrif_Save_Forrestore4d
    end interface
!
contains
!
!===================================================================================================
!  subroutine Agrif_Set_parent_int
!
!> To set the TYPE of the variable
!---------------------------------------------------------------------------------------------------
subroutine Agrif_Set_parent_int(integer_variable,value)
!---------------------------------------------------------------------------------------------------
    integer, intent(in)     :: integer_variable !< indice of the variable in tabvars
    integer, intent(in)     :: value        !< input value
!
    
integer :: i
logical :: i_found

i_found = .FALSE.

do i=1,Agrif_NbVariables(4)
  if (LOC(integer_variable) == LOC(agrif_curgrid%tabvars_i(i)%iarray0)) then
     agrif_curgrid%tabvars_i(i)%parent_var%iarray0 = value
     i_found = .TRUE.
     EXIT
  endif
enddo

if (.NOT.i_found) STOP 'Agrif_Set_Integer : Variable not found'

!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Set_parent_int
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_Set_parent_real4
!---------------------------------------------------------------------------------------------------
!> To set the parent value of a real variable
!---------------------------------------------------------------------------------------------------
subroutine Agrif_Set_parent_real4 ( real_variable, value )
!---------------------------------------------------------------------------------------------------
    real(kind=4), intent(in)     :: real_variable !< input variable
    real(kind=4),intent(in) :: value        !< input value for the parent grid

integer :: i
logical :: i_found

i_found = .FALSE.

do i=1,Agrif_NbVariables(2)
  if (LOC(real_variable) == LOC(agrif_curgrid%tabvars_r(i)%array0)) then
     agrif_curgrid%tabvars_r(i)%parent_var%array0 = value
     agrif_curgrid%tabvars_r(i)%parent_var%sarray0 = value
     i_found = .TRUE.
     EXIT
  endif
enddo

IF (.NOT.i_found) THEN
do i=1,Agrif_NbVariables(2)
  if (LOC(real_variable) == LOC(agrif_curgrid%tabvars_r(i)%sarray0)) then
     agrif_curgrid%tabvars_r(i)%parent_var%array0 = value
     agrif_curgrid%tabvars_r(i)%parent_var%sarray0 = value
     i_found = .TRUE.
     EXIT
  endif
enddo
ENDIF

if (.NOT.i_found) STOP 'Agrif_Set_parent_real4 : Variable not found'

!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Set_parent_real4
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_Set_parent_real8
!---------------------------------------------------------------------------------------------------
!> To set the parent value of a real variable
!---------------------------------------------------------------------------------------------------
subroutine Agrif_Set_parent_real8 ( real_variable, value )
!---------------------------------------------------------------------------------------------------
    real(kind=8), intent(in)     :: real_variable !< input variable
    real(kind=8),intent(in) :: value        !< input value for the parent grid

integer :: i
logical :: i_found

i_found = .FALSE.

do i=1,Agrif_NbVariables(2)
  if (LOC(real_variable) == LOC(agrif_curgrid%tabvars_r(i)%array0)) then
     agrif_curgrid%tabvars_r(i)%parent_var%darray0 = value
     agrif_curgrid%tabvars_r(i)%parent_var%array0 = value
     i_found = .TRUE.
     EXIT
  endif
enddo

IF (.NOT.i_found) THEN
do i=1,Agrif_NbVariables(2)
  if (LOC(real_variable) == LOC(agrif_curgrid%tabvars_r(i)%darray0)) then
     agrif_curgrid%tabvars_r(i)%parent_var%darray0 = value
     agrif_curgrid%tabvars_r(i)%parent_var%array0 = value
     i_found = .TRUE.
     EXIT
  endif
enddo
ENDIF

if (.NOT.i_found) STOP 'Agrif_Set_parent_real8 : Variable not found'

!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Set_parent_real8
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_Set_bc
!---------------------------------------------------------------------------------------------------
subroutine Agrif_Set_bc ( tabvarsindic, bcinfsup, Interpolationshouldbemade )
!---------------------------------------------------------------------------------------------------
    integer,               intent(in)   :: tabvarsindic !< indice of the variable in tabvars
    integer, dimension(2), intent(in)   :: bcinfsup     !< bcinfsup
    logical, optional,     intent(in)   :: Interpolationshouldbemade !< interpolation should be made
!
    integer                         :: indic ! indice of the variable in tabvars
    type(Agrif_Variable),  pointer  :: var
!
    var => Agrif_Search_Variable(Agrif_Curgrid,tabvarsindic)
    if (.not.associated(var)) return ! Grand mother grid case
!
    if ( Agrif_Curgrid % fixedrank /= 0 ) then
        if ( .not.associated(var % oldvalues2D) ) then
            allocate(var % oldvalues2D(2,1))
            var % interpIndex = -1
            var % oldvalues2D = 0.
        endif
        if ( present(Interpolationshouldbemade) ) then
            var % Interpolationshouldbemade = Interpolationshouldbemade
        endif
    endif
!
    var % bcinf = bcinfsup(1)
    var % bcsup = bcinfsup(2)
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Set_bc
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_Set_interp
!---------------------------------------------------------------------------------------------------
subroutine Agrif_Set_interp ( tabvarsindic, interp, interp1, interp2, interp3 , interp4)
!---------------------------------------------------------------------------------------------------
    integer,           intent(in)   :: tabvarsindic !< indice of the variable in tabvars
    integer, optional, intent(in)   :: interp, interp1, interp2, interp3, interp4
!
    integer                         :: indic ! indice of the variable in tabvars
    type(Agrif_Variable), pointer   :: var
!
    var => Agrif_Search_Variable(Agrif_Curgrid,tabvarsindic)
    if (.not.associated(var)) return ! Grand mother grid case
!
    var % type_interp = Agrif_Constant
!
    if (present(interp))    var % type_interp    = interp
    if (present(interp1))   var % type_interp(1) = interp1
    if (present(interp2))   var % type_interp(2) = interp2
    if (present(interp3))   var % type_interp(3) = interp3
    if (present(interp4))   var % type_interp(4) = interp4
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Set_interp
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_Set_bcinterp
!---------------------------------------------------------------------------------------------------
subroutine Agrif_Set_bcinterp ( tabvarsindic, interp,   interp1,  interp2,  interp3, interp4, &
                                              interp11, interp12, interp21, interp22 )
!---------------------------------------------------------------------------------------------------
    INTEGER,           intent(in)   :: tabvarsindic !< indice of the variable in tabvars
    INTEGER, OPTIONAL, intent(in)   :: interp,   interp1,  interp2,  interp3, interp4
    INTEGER, OPTIONAL, intent(in)   :: interp11, interp12, interp21, interp22
!
    INTEGER                         :: indic ! indice of the variable in tabvars
    TYPE(Agrif_Variable), pointer   :: var
!
    var => Agrif_Search_Variable(Agrif_Curgrid,tabvarsindic)
!
    var % type_interp_bc = Agrif_Constant
!
    if (present(interp))    var % type_interp_bc      = interp
    if (present(interp1))   var % type_interp_bc(:,1) = interp1
    if (present(interp11))  var % type_interp_bc(1,1) = interp11
    if (present(interp12))  var % type_interp_bc(1,2) = interp12
    if (present(interp2))   var % type_interp_bc(:,2) = interp2
    if (present(interp21))  var % type_interp_bc(2,1) = interp21
    if (present(interp22))  var % type_interp_bc(2,2) = interp22
    if (present(interp3))   var % type_interp_bc(:,3) = interp3
    if (present(interp4))   var % type_interp_bc(:,4) = interp4
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Set_bcinterp
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_Set_UpdateType
!---------------------------------------------------------------------------------------------------
subroutine Agrif_Set_UpdateType ( tabvarsindic, update,  update1, update2, &
                                                update3, update4, update5 )
!---------------------------------------------------------------------------------------------------
    INTEGER,           intent(in) :: tabvarsindic     !< indice of the variable in tabvars
    INTEGER, OPTIONAL, intent(in) :: update, update1, update2, update3, update4, update5
!
    INTEGER                         :: indic ! indice of the variable in tabvars
    type(Agrif_Variable),  pointer  :: root_var
!

        root_var => Agrif_Search_Variable(Agrif_Mygrid,tabvarsindic)

!
    root_var % type_update = Agrif_Update_Copy
    if (present(update))    root_var % type_update    = update
    if (present(update1))   root_var % type_update(1) = update1
    if (present(update2))   root_var % type_update(2) = update2
    if (present(update3))   root_var % type_update(3) = update3
    if (present(update4))   root_var % type_update(4) = update4
    if (present(update5))   root_var % type_update(5) = update5
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Set_UpdateType
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_Set_restore
!---------------------------------------------------------------------------------------------------
subroutine Agrif_Set_restore ( tabvarsindic )
!---------------------------------------------------------------------------------------------------
    INTEGER, intent(in) :: tabvarsindic     !< indice of the variable in tabvars
!
    INTEGER :: indic  !  indice of the variable in tabvars
!
print *,'CURRENTLY BROKEN'
STOP

    indic = Agrif_Curgrid%tabvars_i(tabvarsindic)%iarray0
!
    Agrif_Mygrid%tabvars(indic) % restore = .TRUE.
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Set_restore
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_Init_variable
!---------------------------------------------------------------------------------------------------
subroutine Agrif_Init_variable ( tabvarsindic, procname )
!---------------------------------------------------------------------------------------------------
    INTEGER, intent(in)  :: tabvarsindic     !< indice of the variable in tabvars
    procedure()          :: procname         !< Data recovery procedure
!
    if ( Agrif_Curgrid%level <= 0 ) return
!
    call Agrif_Interp_variable(tabvarsindic, procname)
    call Agrif_Bc_variable(tabvarsindic, procname, 1.)
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Init_variable
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_Bc_variable
!---------------------------------------------------------------------------------------------------
subroutine Agrif_Bc_variable ( tabvarsindic, procname, calledweight )
!---------------------------------------------------------------------------------------------------
    integer,        intent(in) :: tabvarsindic     !< indice of the variable in tabvars
    procedure()                :: procname
    real, optional, intent(in) :: calledweight
!
    real    :: weight
    logical :: pweight
    integer :: indic
    integer :: nbdim
    type(Agrif_Variable), pointer :: root_var
    type(Agrif_Variable), pointer :: parent_var
    type(Agrif_Variable), pointer :: child_var
    type(Agrif_Variable), pointer :: child_tmp      ! Temporary variable on the child grid
    integer :: i
    integer,dimension(7) :: lb, ub
!
    if ( Agrif_Curgrid%level <= 0 ) return
!
!
    if ( present(calledweight) ) then
        weight  = calledweight
        pweight = .true.
    else
        weight  = 0.
        pweight = .false.
    endif
!
        child_var  => Agrif_Search_Variable(Agrif_Curgrid,tabvarsindic)
        parent_var => child_var % parent_var
        root_var   => child_var % root_var
!
    nbdim = root_var % nbdim
!
    do i=1,nbdim
      if (root_var%coords(i) == 0) then
        lb(i) = parent_var%lb(i)
        ub(i) = parent_var%ub(i)
      else
        lb(i) = child_var%lb(i)
        ub(i) = child_var%ub(i)
      endif
    enddo

    select case( nbdim )
    case(1)
        allocate(parray1(lb(1):ub(1)))
    case(2)
        allocate(parray2(lb(1):ub(1), &
                         lb(2):ub(2) ))
    case(3)
        allocate(parray3(lb(1):ub(1), &
                         lb(2):ub(2), &
                         lb(3):ub(3) ))
    case(4)
        allocate(parray4(lb(1):ub(1), &
                         lb(2):ub(2), &
                         lb(3):ub(3), &
                         lb(4):ub(4) ))
    case(5)
        allocate(parray5(lb(1):ub(1), &
                         lb(2):ub(2), &
                         lb(3):ub(3), &
                         lb(4):ub(4), &
                         lb(5):ub(5) ))
    case(6)
        allocate(parray6(lb(1):ub(1), &
                         lb(2):ub(2), &
                         lb(3):ub(3), &
                         lb(4):ub(4), &
                         lb(5):ub(5), &
                         lb(6):ub(6) ))
    end select
!
!   Create temporary child variable
    allocate(child_tmp)
!
    child_tmp % root_var => root_var
    child_tmp % oldvalues2D => child_var % oldvalues2D
!
!   Index indicating if a space interpolation is necessary
    child_tmp % interpIndex =  child_var % interpIndex
    child_tmp % list_interp => child_var % list_interp
    child_tmp % Interpolationshouldbemade = child_var % Interpolationshouldbemade
!
    child_tmp % point = child_var % point
    child_tmp % lb = child_var % lb
    child_tmp % ub = child_var % ub
!
    child_tmp % bcinf = child_var % bcinf
    child_tmp % bcsup = child_var % bcsup
!
    child_tmp % childarray = child_var % childarray
    child_tmp % memberin   = child_var % memberin
!
    call Agrif_CorrectVariable(parent_var, child_tmp, pweight, weight, procname)
!
    child_var % childarray = child_tmp % childarray
    child_var % memberin   = child_tmp % memberin
!
    child_var % oldvalues2D => child_tmp % oldvalues2D
    child_var % list_interp => child_tmp % list_interp
!
    child_var % interpIndex = child_tmp % interpIndex
!
    deallocate(child_tmp)
!
    select case( nbdim )
        case(1); deallocate(parray1)
        case(2); deallocate(parray2)
        case(3); deallocate(parray3)
        case(4); deallocate(parray4)
        case(5); deallocate(parray5)
        case(6); deallocate(parray6)
    end select
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Bc_variable
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_Interp_variable
!---------------------------------------------------------------------------------------------------
subroutine Agrif_Interp_variable ( tabvarsindic, procname )
!---------------------------------------------------------------------------------------------------
    integer,     intent(in)     :: tabvarsindic     !< indice of the variable in tabvars
    procedure()                 :: procname         !< Data recovery procedure
!
    integer :: nbdim
    integer :: indic  ! indice of the variable in tabvars
    logical :: torestore
    type(Agrif_Variable), pointer   :: root_var
    type(Agrif_Variable), pointer   :: parent_var       ! Variable on the parent grid
    type(Agrif_Variable), pointer   :: child_var        ! Variable on the parent grid
    type(Agrif_Variable), pointer   :: child_tmp        ! Temporary variable on the child grid
!
    if ( Agrif_Curgrid%level <= 0 ) return
!

        child_var  => Agrif_Search_Variable(Agrif_Curgrid,tabvarsindic)
        parent_var => child_var % parent_var
        root_var   => child_var % root_var

!
    nbdim     = root_var % nbdim
    torestore = root_var % restore
!
    allocate(child_tmp)
!
    child_tmp % root_var => root_var
    child_tmp % nbdim = root_var % nbdim
    child_tmp % point = child_var % point
    child_tmp % lb = child_var % lb
    child_tmp % ub = child_var % ub
    child_tmp % interpIndex =  child_var % interpIndex
    child_tmp % list_interp => child_var % list_interp
    child_tmp % Interpolationshouldbemade = child_var % Interpolationshouldbemade
!
    if ( torestore ) then
        select case( nbdim )
        case(1)
            parray1 = child_var % array1
            child_tmp % restore1D => child_var % restore1D
        case(2)
            parray2 = child_var % array2
            child_tmp % restore2D => child_var % restore2D
        case(3)
            parray3 = child_var % array3
            child_tmp % restore3D => child_var % restore3D
        case(4)
            parray4 = child_var % array4
            child_tmp % restore4D => child_var % restore4D
        case(5)
            parray5 = child_var % array5
            child_tmp % restore5D => child_var % restore5D
        case(6)
            parray6 = child_var % array6
            child_tmp % restore6D => child_var % restore6D
        end select
    endif
!
    call Agrif_InterpVariable(parent_var, child_tmp, torestore, procname)
!
    child_var % list_interp => child_tmp % list_interp
!
    deallocate(child_tmp)
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Interp_variable
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_Update_Variable
!---------------------------------------------------------------------------------------------------
subroutine Agrif_Update_Variable ( tabvarsindic, procname, &
                                   locupdate, locupdate1, locupdate2, locupdate3, locupdate4 )
!---------------------------------------------------------------------------------------------------
    integer,               intent(in)           :: tabvarsindic     !< Indice of the variable in tabvars
    procedure()                                 :: procname         !< Data recovery procedure
    integer, dimension(2), intent(in), optional :: locupdate
    integer, dimension(2), intent(in), optional :: locupdate1
    integer, dimension(2), intent(in), optional :: locupdate2
    integer, dimension(2), intent(in), optional :: locupdate3
    integer, dimension(2), intent(in), optional :: locupdate4
!---------------------------------------------------------------------------------------------------
    integer :: indic
    integer :: nbdim
    integer, dimension(6)           :: updateinf    ! First positions where interpolations are calculated
    integer, dimension(6)           :: updatesup    ! Last  positions where interpolations are calculated
    type(Agrif_Variable), pointer   :: root_var
    type(Agrif_Variable), pointer   :: parent_var
    type(Agrif_Variable), pointer   :: child_var
!
    if ( Agrif_Root() .AND. (.not.agrif_coarse) ) return
    if (agrif_curgrid%grand_mother_grid) return
!

        child_var  => Agrif_Search_Variable(Agrif_Curgrid, tabvarsindic)
        parent_var => child_var % parent_var

        if (.not.associated(parent_var)) then
          ! can occur during the first update of Agrif_Coarsegrid (if any)
          parent_var => Agrif_Search_Variable(Agrif_Curgrid % parent, tabvarsindic)
          child_var % parent_var => parent_var
        endif

        root_var   => child_var % root_var

!
    nbdim = root_var % nbdim
!
    updateinf = -99
    updatesup = -99
!
    if ( present(locupdate) ) then
        updateinf(1:nbdim) = locupdate(1)
        updatesup(1:nbdim) = locupdate(2)
    endif
!
    if ( present(locupdate1) ) then
        updateinf(1) = locupdate1(1)
        updatesup(1) = locupdate1(2)
    endif
!
    if ( present(locupdate2) ) then
        updateinf(2) = locupdate2(1)
        updatesup(2) = locupdate2(2)
    endif

    if ( present(locupdate3) ) then
        updateinf(3) = locupdate3(1)
        updatesup(3) = locupdate3(2)
    endif

    if ( present(locupdate4) ) then
        updateinf(4) = locupdate4(1)
        updatesup(4) = locupdate4(2)
    endif
!
    call Agrif_UpdateVariable( parent_var, child_var, updateinf, updatesup, procname )
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Update_Variable
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_Save_ForRestore0D
!---------------------------------------------------------------------------------------------------
subroutine Agrif_Save_ForRestore0D ( tabvarsindic0, tabvarsindic )
!---------------------------------------------------------------------------------------------------
    integer, intent(in) :: tabvarsindic0, tabvarsindic
!
    type(Agrif_Variable), pointer   :: root_var, save_var
    integer :: nbdim
!
print *,'CURRENTLY BROKEN'
STOP
    root_var => Agrif_Mygrid % tabvars(tabvarsindic0)
    save_var => Agrif_Curgrid % tabvars(tabvarsindic0)
    nbdim =  root_var % nbdim
!
    select case(nbdim)
        case(2); call Agrif_Save_ForRestore2D(save_var % array2, tabvarsindic)
        case(3); call Agrif_Save_ForRestore3D(save_var % array3, tabvarsindic)
        case(4); call Agrif_Save_ForRestore4D(save_var % array4, tabvarsindic)
    end select
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Save_ForRestore0D
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_Save_ForRestore2D
!---------------------------------------------------------------------------------------------------
subroutine Agrif_Save_ForRestore2D ( q, tabvarsindic )
!---------------------------------------------------------------------------------------------------
    real, dimension(:,:), intent(in)        :: q
    integer,              intent(in)        :: tabvarsindic
!
    type(Agrif_Variable),  pointer  :: root_var, save_var
    integer                         :: indic
!
print *,'CURRENTLY BROKEN'
STOP
    indic = tabvarsindic
    if (tabvarsindic >= 0) then
        if (Agrif_Curgrid%tabvars_i(tabvarsindic)%nbdim == 0) then
            indic = Agrif_Curgrid%tabvars_i(tabvarsindic)%iarray0
        endif
    endif
!
    if (indic <= 0) then
        save_var => Agrif_Search_Variable(Agrif_Curgrid,-indic)
        root_var => Agrif_Search_Variable(Agrif_Mygrid,-indic)
    else
        save_var => Agrif_Curgrid % tabvars(indic)
        root_var => Agrif_Mygrid % tabvars(indic)
    endif
!
    if ( .not.allocated(save_var%array2) ) then
        allocate(save_var%array2(save_var%lb(1):save_var%ub(1),  &
                                 save_var%lb(2):save_var%ub(2)))
    endif
!
    save_var % array2  = q
    root_var % restore = .true.
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Save_ForRestore2D
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_Save_ForRestore3D
!---------------------------------------------------------------------------------------------------
subroutine Agrif_Save_ForRestore3D ( q, tabvarsindic )
!---------------------------------------------------------------------------------------------------
    real, dimension(:,:,:), intent(in)      :: q
    integer,                intent(in)      :: tabvarsindic
!
    type(Agrif_Variable),  pointer  :: root_var, save_var
    integer                         :: indic
!
print *,'CURRENTLY BROKEN'
STOP

    indic = tabvarsindic
    if (tabvarsindic >= 0) then
        if (Agrif_Curgrid%tabvars_i(tabvarsindic)%nbdim == 0) then
            indic = Agrif_Curgrid%tabvars_i(tabvarsindic)%iarray0
        endif
    endif
!
    if (indic <= 0) then
        save_var => Agrif_Search_Variable(Agrif_Curgrid,-indic)
        root_var => Agrif_Search_Variable(Agrif_Mygrid,-indic)
    else
        save_var => Agrif_Curgrid % tabvars(indic)
        root_var => Agrif_Mygrid % tabvars(indic)
    endif
!
    if ( .not.allocated(save_var%array3) ) then
        allocate(save_var%array3(save_var%lb(1):save_var%ub(1), &
                                 save_var%lb(2):save_var%ub(2), &
                                 save_var%lb(3):save_var%ub(3)))
    endif
!
    save_var % array3  = q
    root_var % restore = .true.
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Save_ForRestore3D
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_Save_ForRestore4D
!---------------------------------------------------------------------------------------------------
subroutine Agrif_Save_ForRestore4D ( q, tabvarsindic )
!---------------------------------------------------------------------------------------------------
    real, dimension(:,:,:,:), intent(in)    :: q
    integer,                  intent(in)    :: tabvarsindic
!
    type(Agrif_Variable),  pointer  :: root_var, save_var
    integer                         :: indic
!
print *,'CURRENTLY BROKEN'
STOP
    indic = tabvarsindic
    if (tabvarsindic >= 0) then
        if (Agrif_Curgrid%tabvars_i(tabvarsindic)%nbdim == 0) then
            indic = Agrif_Curgrid%tabvars_i(tabvarsindic)%iarray0
        endif
    endif
!
    if (indic <= 0) then
        save_var => Agrif_Search_Variable(Agrif_Curgrid,-indic)
        root_var => Agrif_Search_Variable(Agrif_Mygrid,-indic)
    else
        save_var => Agrif_Curgrid % tabvars(indic)
        root_var => Agrif_Mygrid % tabvars(indic)
    endif
!
    if (.not.allocated(save_var%array4)) then
        allocate(save_var%array4(save_var%lb(1):save_var%ub(1),&
                                 save_var%lb(2):save_var%ub(2),&
                                 save_var%lb(3):save_var%ub(3),&
                                 save_var%lb(4):save_var%ub(4)))
    endif
!
    save_var % array4  = q
    root_var % restore = .true.
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Save_ForRestore4D
!===================================================================================================
!
end module Agrif_BcFunction
