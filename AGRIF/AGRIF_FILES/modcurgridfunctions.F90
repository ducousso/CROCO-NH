!
! $Id: modcurgridfunctions.F 774 2007-12-18 16:45:53Z rblod $
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
!     Foundation, Inc., 59 Temple Place-  Suite 330, Boston, MA 02111-1307, USA.
!
!> Module to define some procedures concerning the current grid
!
module Agrif_CurgridFunctions
!
    use Agrif_Init
!
    implicit none
!

    interface Agrif_Parent
        module procedure Agrif_Parent_Real_4,   &
                         Agrif_Parent_Real_8,   &
                         Agrif_Parent_Integer, &
                         Agrif_Parent_Character, &
                         Agrif_Parent_Logical
    end interface
    
contains
!
!===================================================================================================
!  function Agrif_rel_dt
!
!> Returns the time step of the current grid, relatively to the root grid (for which dt=1.).
!---------------------------------------------------------------------------------------------------
function Agrif_rel_dt ( ) result( rel_dt )
!---------------------------------------------------------------------------------------------------
    integer :: i
    real    :: rel_dt
!
    rel_dt = 1.
!
    do i = 1,Agrif_Probdim
        rel_dt = min(rel_dt, Agrif_Curgrid % Agrif_dt(i))
    enddo
!---------------------------------------------------------------------------------------------------
end function Agrif_rel_dt
!===================================================================================================
!
!===================================================================================================
!  function Agrif_rel_idt
!
!> Returns the time refinement factor of the current grid, relatively to the root grid (for which idt=1).
!---------------------------------------------------------------------------------------------------
function Agrif_rel_idt ( ) result( rel_idt )
!---------------------------------------------------------------------------------------------------
    integer :: rel_idt
!
    rel_idt = nint(1./Agrif_rel_dt())
!---------------------------------------------------------------------------------------------------
end function Agrif_rel_idt
!===================================================================================================
!
!===================================================================================================
!  function Agrif_IRhot
!
!> Returns the time refinement factor of the current grid.
!---------------------------------------------------------------------------------------------------
function Agrif_IRhot ( ) result( irhot )
!---------------------------------------------------------------------------------------------------
    integer :: i, irhot
!
    irhot = 1
!
    do i = 1,Agrif_Probdim
        irhot = max(irhot, Agrif_Curgrid % timeref(i))
    enddo
!---------------------------------------------------------------------------------------------------
end function Agrif_IRhot
!===================================================================================================
!
!===================================================================================================
!  function Agrif_Rhot
!
!> Returns the time refinement factor of the current grid.
!---------------------------------------------------------------------------------------------------
function Agrif_Rhot ( ) result( rhot )
!---------------------------------------------------------------------------------------------------
    real    :: rhot
!
    rhot = float(Agrif_IRhot())
!---------------------------------------------------------------------------------------------------
end function Agrif_Rhot
!===================================================================================================
!
!===================================================================================================
!  function Agrif_Parent_IRhot
!
!> Returns the time refinement factor of the parent of the current grid.
!---------------------------------------------------------------------------------------------------
function Agrif_Parent_IRhot ( ) result( irhot )
!---------------------------------------------------------------------------------------------------
    integer :: i, irhot
!
    irhot = 1
!
    do i = 1,Agrif_Probdim
        irhot = max(irhot, Agrif_Curgrid % parent % timeref(i))
    enddo
!---------------------------------------------------------------------------------------------------
end function Agrif_Parent_IRhot
!===================================================================================================
!
!===================================================================================================
!  function Agrif_Parent_Rhot
!
!> Returns the time refinement factor of the parent of the current grid.
!---------------------------------------------------------------------------------------------------
function Agrif_Parent_Rhot ( ) result( rhot )
!---------------------------------------------------------------------------------------------------
    real :: rhot
!
    rhot = float(Agrif_Parent_IRhot())
!---------------------------------------------------------------------------------------------------
end function Agrif_Parent_Rhot
!===================================================================================================
!
!===================================================================================================
!  function Agrif_Nbstepint
!
!> function for the calculation of the coefficients used for the time interpolation
!! (module #Agrif_Boundary).
!---------------------------------------------------------------------------------------------------
function Agrif_Nbstepint ( )
!---------------------------------------------------------------------------------------------------
    integer :: Agrif_nbstepint ! result
!
    Agrif_nbstepint = mod(Agrif_Curgrid % ngridstep, Agrif_iRhot())
!---------------------------------------------------------------------------------------------------
end function Agrif_Nbstepint
!===================================================================================================
!
!===================================================================================================
!  function Agrif_Parent_Nbstepint
!
!> function for the calculation of the coefficients used for the time interpolation
!! (module #Agrif_Boundary).
!---------------------------------------------------------------------------------------------------
function Agrif_Parent_Nbstepint ( )
!---------------------------------------------------------------------------------------------------
    integer :: Agrif_Parent_Nbstepint ! result
!
    Agrif_Parent_Nbstepint = mod(Agrif_Curgrid % parent % ngridstep, int(Agrif_Parent_Rhot()))
!---------------------------------------------------------------------------------------------------
end function Agrif_Parent_Nbstepint
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_InterpNearBorderX
!
!> Allows to interpolate (in the x direction) on a near border of the current grid if this one
!! has a common border with the root coarse grid.
!---------------------------------------------------------------------------------------------------
subroutine Agrif_InterpNearBorderX ( )
!---------------------------------------------------------------------------------------------------
    Agrif_Curgrid % NearRootBorder(1) = .FALSE.
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_InterpNearBorderX
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_InterpDistantBorderX
!
!> Allows to interpolate (in the x direction) on a distant border of the current grid if this one
!! has a common border with the root coarse grid.
!---------------------------------------------------------------------------------------------------
subroutine Agrif_InterpDistantBorderX ( )
!---------------------------------------------------------------------------------------------------
    Agrif_Curgrid % DistantRootBorder(1) = .FALSE.
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_InterpDistantBorderX
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_InterpNearBorderY
!
!> Allows to interpolate (in the y direction) on a near border of the current grid if this one
!! has a common border with the root coarse grid.
!---------------------------------------------------------------------------------------------------
subroutine Agrif_InterpNearBorderY ( )
!---------------------------------------------------------------------------------------------------
    Agrif_Curgrid % NearRootBorder(2) = .FALSE.
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_InterpNearBorderY
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_InterpDistantBorderY
!
!> Allows to interpolate (in the y direction) on a distant border of the current grid if this one
!! has a common border with the root coarse grid.
!---------------------------------------------------------------------------------------------------
subroutine Agrif_InterpDistantBorderY ( )
!---------------------------------------------------------------------------------------------------
    Agrif_Curgrid % DistantRootBorder(2) = .FALSE.
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_InterpDistantBorderY
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_InterpNearBorderZ
!
!> Allows to interpolate (in the z direction) on a near border of the current grid if this one
!! has a common border with the root coarse grid.
!---------------------------------------------------------------------------------------------------
subroutine Agrif_InterpNearBorderZ ( )
!---------------------------------------------------------------------------------------------------
    Agrif_Curgrid % NearRootBorder(3) = .FALSE.
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_InterpNearBorderZ
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_InterpDistantBorderZ
!
!> Allows to interpolate (in the z direction) on a distant border of the current grid if this one
!! has a common border with the root coarse grid.
!---------------------------------------------------------------------------------------------------
subroutine Agrif_InterpDistantBorderZ()
!---------------------------------------------------------------------------------------------------
    Agrif_Curgrid % DistantRootBorder(3) = .FALSE.
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_InterpDistantBorderZ
!===================================================================================================
!
!===================================================================================================
!  function Agrif_Parent_Nb_Step
!
!> Returns the number of time steps of the parent of the current grid.
!---------------------------------------------------------------------------------------------------
function Agrif_Parent_Nb_Step ( )
!---------------------------------------------------------------------------------------------------
    integer :: Agrif_Parent_Nb_Step ! Result
!
    if (Agrif_Root()) then
        Agrif_Parent_Nb_Step = -1
    else
        Agrif_Parent_Nb_Step = Agrif_Curgrid % parent % ngridstep
    endif
!---------------------------------------------------------------------------------------------------
end function Agrif_Parent_Nb_Step
!===================================================================================================
!
!===================================================================================================
!  function Agrif_Root
!
!> Indicates if the current grid is or not the root grid.
!---------------------------------------------------------------------------------------------------
function Agrif_Root ( )
!---------------------------------------------------------------------------------------------------
    logical :: Agrif_Root ! Result
!
    Agrif_Root = (Agrif_Curgrid % fixedrank == 0)
!---------------------------------------------------------------------------------------------------
end function Agrif_Root
!===================================================================================================
!
!===================================================================================================
!  function Agrif_GrandMother
!
!> Indicates if the current grid is or not the root grid.
!---------------------------------------------------------------------------------------------------
function Agrif_GrandMother ( )
!---------------------------------------------------------------------------------------------------
    logical :: Agrif_GrandMother ! Result
!
    Agrif_GrandMother = Agrif_Curgrid % grand_mother_grid
!---------------------------------------------------------------------------------------------------
end function Agrif_GrandMother
!===================================================================================================
!
!===================================================================================================
!  function Agrif_Parent_Root
!
!> Indicates if the parent of the current grid is or not the root grid.
!---------------------------------------------------------------------------------------------------
function Agrif_Parent_Root ( )
!---------------------------------------------------------------------------------------------------
    logical :: Agrif_Parent_Root ! Result
!
    Agrif_Parent_Root = (Agrif_Curgrid % parent % fixedrank == 0)
!---------------------------------------------------------------------------------------------------
end function Agrif_Parent_Root
!===================================================================================================
!
!===================================================================================================
!  function Agrif_Fixed
!
!> Returns the number of the current grid.
!---------------------------------------------------------------------------------------------------
function Agrif_Fixed ( )
!---------------------------------------------------------------------------------------------------
    integer :: Agrif_Fixed   ! Result
!
    if (Agrif_Curgrid % fixed) then
        Agrif_Fixed = Agrif_Curgrid % fixedrank
    else
        Agrif_Fixed = -1
    endif
!---------------------------------------------------------------------------------------------------
end function Agrif_Fixed
!===================================================================================================
!
!===================================================================================================
!  function Agrif_Parent_Fixed
!
!> Returns the number of the parent of the current grid.
!---------------------------------------------------------------------------------------------------
function Agrif_Parent_Fixed ( )
!---------------------------------------------------------------------------------------------------
    integer :: Agrif_Parent_Fixed   ! Result
!
    if (Agrif_Curgrid % parent % fixed) then
        Agrif_Parent_Fixed = Agrif_Curgrid % parent % fixedrank
    else
        Agrif_Parent_Fixed = 0
    endif
!---------------------------------------------------------------------------------------------------
end function Agrif_Parent_Fixed
!===================================================================================================
!
!===================================================================================================
!  function Agrif_Is_Fixed
!
!> Returns .TRUE. if the current grid is fixed.
!---------------------------------------------------------------------------------------------------
function Agrif_Is_Fixed ( )
!---------------------------------------------------------------------------------------------------
    logical :: Agrif_Is_Fixed   ! Result
!
    Agrif_Is_Fixed = Agrif_Curgrid % fixed
!---------------------------------------------------------------------------------------------------
end function Agrif_Is_Fixed
!===================================================================================================
!
!===================================================================================================
!  function Agrif_Parent_Is_Fixed
!
!> Returns .TRUE. if the parent of the current grid is fixed.
!---------------------------------------------------------------------------------------------------
function Agrif_Parent_Is_Fixed ( )
!---------------------------------------------------------------------------------------------------
    logical :: Agrif_Parent_Is_Fixed   ! Result
!
    Agrif_Parent_Is_Fixed = Agrif_Curgrid % parent % fixed
!---------------------------------------------------------------------------------------------------
end function Agrif_Parent_Is_Fixed
!===================================================================================================
!
!===================================================================================================
!  function Agrif_CFixed
!
!> Returns the number of the current grid.
!---------------------------------------------------------------------------------------------------
function Agrif_CFixed ( )
!---------------------------------------------------------------------------------------------------
    character(3) :: Agrif_CFixed   ! Result
!
    character(3) :: cfixed
    integer      :: fixed
!
    fixed = Agrif_Fixed()
!
    if (fixed /= -1) then
!
        if (fixed <= 9) then
            write(cfixed,'(i1)') fixed
        else
            write(cfixed,'(i2)') fixed
        endif
!
        Agrif_CFixed = cfixed
        
        if (associated(agrif_curgrid,agrif_coarsegrid)) then
          Agrif_CFixed = 'gm'
        endif
!
    else
        print*,'Call to Agrif_CFixed() on a moving grid'
        stop
    endif
!---------------------------------------------------------------------------------------------------
end function Agrif_CFixed
!===================================================================================================
!
!===================================================================================================
!  function Agrid_Parent_CFixed
!
!> Returns the number of the parent of the current grid.
!---------------------------------------------------------------------------------------------------
function Agrid_Parent_CFixed ( )
!---------------------------------------------------------------------------------------------------
    character(3) :: Agrid_Parent_CFixed   ! Result
!
    character(3) :: cfixed
    integer      :: fixed
!
    fixed = Agrif_Parent_Fixed()
!
    if(fixed /= -1) then
!
        if (fixed <= 9) then
            write(cfixed,'(i1)')fixed
        else
            write(cfixed,'(i2)')fixed
        endif
!
        Agrid_Parent_CFixed=cfixed
!
    else
        print*,'Illegal call to Agrid_Parent_CFixed()'
        stop
    endif
!---------------------------------------------------------------------------------------------------
end function Agrid_Parent_CFixed
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_ChildGrid_to_ParentGrid
!
!> Make the pointer #Agrif_Curgrid point on the parent grid of the current grid.
!---------------------------------------------------------------------------------------------------
subroutine Agrif_ChildGrid_to_ParentGrid ( )
!---------------------------------------------------------------------------------------------------
    Agrif_Curgrid % parent % save_grid => Agrif_Curgrid
    call Agrif_Instance(Agrif_Curgrid%parent)
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_ChildGrid_to_ParentGrid
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_ParentGrid_to_ChildGrid
!
!> Make the pointer #Agrif_Curgrid point on the child grid after having called the
!! #Agrif_ChildGrid_to_ParentGrid subroutine.
!---------------------------------------------------------------------------------------------------
subroutine Agrif_ParentGrid_to_ChildGrid ( )
!---------------------------------------------------------------------------------------------------
    call Agrif_Instance(Agrif_Curgrid%save_grid)
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_ParentGrid_to_ChildGrid
!===================================================================================================
!
!===================================================================================================
!  function Agrif_Get_Unit
!
!> Returns a unit not connected to any file.
!---------------------------------------------------------------------------------------------------
function Agrif_Get_Unit ( )
!---------------------------------------------------------------------------------------------------
    integer :: Agrif_Get_Unit  ! Result
!
    integer :: n
    logical :: op
!
    integer :: nunit
    integer :: iii, out, iiimax
    logical :: bexist
    integer,dimension(1:1000) :: forbiddenunit
!
!   Load forbidden Unit if the file Agrif_forbidenUnit exist
!
    INQUIRE(file='Agrif_forbiddenUnit.txt', exist=bexist)
!
    if (.not. bexist) then
!       File Agrif_forbiddenUnit.txt not found
    else
        nunit = 777
        OPEN(nunit,file='Agrif_forbiddenUnit.txt', form='formatted', status="old")
        iii = 1
        do while ( .TRUE. )
            READ(nunit,*, end=99) forbiddenunit(iii)
            iii = iii + 1
        enddo
   99   continue
        iiimax = iii
        close(nunit)
    endif
!
    do n = 7,1000
!
        INQUIRE(Unit=n,Opened=op)
!
        out = 0
        if ( bexist .AND. (.NOT.op) ) then
            do iii = 1,iiimax
                if ( n == forbiddenunit(iii) ) out = 1
            enddo
        endif
!
        if ( (.NOT.op) .AND. (out == 0) ) exit
!
    enddo
!
    Agrif_Get_Unit = n
!---------------------------------------------------------------------------------------------------
end function Agrif_Get_Unit
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_Set_Extra_Boundary_Cells
!---------------------------------------------------------------------------------------------------
subroutine Agrif_Set_Extra_Boundary_Cells ( nb_extra_cells )
!---------------------------------------------------------------------------------------------------
    integer, intent(in) :: nb_extra_cells
!
    Agrif_Extra_Boundary_Cells = nb_extra_cells
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Set_Extra_Boundary_Cells
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_Set_Efficiency
!---------------------------------------------------------------------------------------------------
subroutine Agrif_Set_Efficiency ( eff )
!---------------------------------------------------------------------------------------------------
    real, intent(in) :: eff
!
    if ( (eff < 0.) .OR. (eff > 1) ) then
        write(*,*) 'Error Efficiency should be between 0 and 1'
        stop
    else
        Agrif_Efficiency = eff
    endif
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Set_Efficiency
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_Set_Regridding
!---------------------------------------------------------------------------------------------------
subroutine Agrif_Set_Regridding ( regfreq )
!---------------------------------------------------------------------------------------------------
    integer, intent(in) :: regfreq
!
    if (regfreq < 0) then
        write(*,*) 'Regridding frequency should be positive'
        stop
    else
        Agrif_Regridding = regfreq
    endif
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Set_Regridding
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_Set_coeffref_x
!---------------------------------------------------------------------------------------------------
subroutine Agrif_Set_coeffref_x ( coeffref )
!---------------------------------------------------------------------------------------------------
    integer, intent(in) :: coeffref

      if (coeffref < 0) then
         write(*,*) 'Coefficient of raffinement should be positive'
         stop
      else
         Agrif_coeffref(1) = coeffref
      endif
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Set_coeffref_x
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_Set_coeffref_y
!---------------------------------------------------------------------------------------------------
subroutine Agrif_Set_coeffref_y ( coeffref )
!---------------------------------------------------------------------------------------------------
    integer, intent(in) :: coeffref

    if (coeffref < 0) then
        write(*,*) 'Coefficient of raffinement should be positive'
        stop
    else
        Agrif_coeffref(2) = coeffref
    endif
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Set_coeffref_y
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_Set_coeffref_z
!---------------------------------------------------------------------------------------------------
subroutine Agrif_Set_coeffref_z ( coeffref )
!---------------------------------------------------------------------------------------------------
    integer, intent(in) :: coeffref
!
    if (coeffref < 0) then
        write(*,*) 'Coefficient of raffinement should be positive'
        stop
    else
        Agrif_coeffref(3) = coeffref
    endif
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Set_coeffref_z
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_Set_coeffreft_x
!---------------------------------------------------------------------------------------------------
subroutine Agrif_Set_coeffreft_x ( coeffref )
!---------------------------------------------------------------------------------------------------
    integer, intent(in) :: coeffref

    if (coeffref < 0) then
        write(*,*) 'Coefficient of time raffinement should be positive'
        stop
    else
        Agrif_coeffreft(1) = coeffref
    endif
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Set_coeffreft_x
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_Set_coeffreft_y
!---------------------------------------------------------------------------------------------------
subroutine Agrif_Set_coeffreft_y ( coeffref )
!---------------------------------------------------------------------------------------------------
    integer, intent(in) :: coeffref
!
    if (coeffref < 0) then
        write(*,*) 'Coefficient of time raffinement should be positive'
        stop
    else
        Agrif_coeffreft(2) = coeffref
    endif
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Set_coeffreft_y
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_Set_coeffreft_z
!---------------------------------------------------------------------------------------------------
subroutine Agrif_Set_coeffreft_z ( coeffref )
!---------------------------------------------------------------------------------------------------
    integer, intent(in) :: coeffref

    if (coeffref < 0) then
        write(*,*)'Coefficient of time raffinement should be positive'
        stop
    else
        Agrif_coeffreft(3) = coeffref
    endif
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Set_coeffreft_z
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_Set_Minwidth
!---------------------------------------------------------------------------------------------------
subroutine Agrif_Set_Minwidth ( coefminwidth )
!---------------------------------------------------------------------------------------------------
    integer, intent(in) :: coefminwidth
!
    if (coefminwidth < 0) then
        write(*,*)'Coefficient of Minwidth should be positive'
        stop
    else
        Agrif_Minwidth = coefminwidth
    endif
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Set_Minwidth
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_Set_Rafmax
!---------------------------------------------------------------------------------------------------
subroutine Agrif_Set_Rafmax ( coefrafmax )
!---------------------------------------------------------------------------------------------------
    integer, intent(in) :: coefrafmax
!
    integer :: i
    real    :: res
!
    if (coefrafmax < 0) then
        write(*,*)'Coefficient of  should be positive'
        stop
    else
        res = 1.
        do i = 1,coefrafmax-1
            res = res * FLOAT(Agrif_coeffref(1))
        enddo
        if ( res == 0 ) res = 1
        Agrif_Mind(1) = 1. / res
!
        res = 1.
        do i = 1,coefrafmax-1
            res = res * FLOAT(Agrif_coeffref(2))
        enddo
        if ( res == 0 ) res = 1
        Agrif_Mind(2) = 1. / res
!
        res = 1.
        do i = 1,coefrafmax-1
            res = res * FLOAT(Agrif_coeffref(3))
        enddo
        if ( res == 0 ) res = 1
        Agrif_Mind(3) = 1. / res
!
      endif
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Set_Rafmax
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_Set_MaskMaxSearch
!---------------------------------------------------------------------------------------------------
subroutine Agrif_Set_MaskMaxSearch ( mymaxsearch )
!---------------------------------------------------------------------------------------------------
    integer, intent(in) :: mymaxsearch
!
    MaxSearch = mymaxsearch
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Set_MaskMaxSearch
!===================================================================================================
!
!===================================================================================================
!  function Agrif_Level
!---------------------------------------------------------------------------------------------------
function Agrif_Level ( )
!---------------------------------------------------------------------------------------------------
    integer :: Agrif_Level  ! Result
!
    Agrif_Level = Agrif_Curgrid % level
!---------------------------------------------------------------------------------------------------
end function Agrif_Level
!===================================================================================================
!
!===================================================================================================
!  function Agrif_MaxLevel
!---------------------------------------------------------------------------------------------------
function Agrif_MaxLevel ( )
!---------------------------------------------------------------------------------------------------
    integer :: Agrif_MaxLevel  ! Result
!
    Agrif_MaxLevel = Agrif_MaxLevelLoc
!---------------------------------------------------------------------------------------------------
end function Agrif_MaxLevel
!===================================================================================================
!
!===================================================================================================
!  function Agrif_GridAllocation_is_done
!---------------------------------------------------------------------------------------------------
function Agrif_GridAllocation_is_done ( ) result(isdone)
!---------------------------------------------------------------------------------------------------
    logical :: isdone
!
    isdone = Agrif_Curgrid % allocation_is_done
!---------------------------------------------------------------------------------------------------
end function Agrif_GridAllocation_is_done
!===================================================================================================
!

function Agrif_Parent_Real_4(real_variable) result(real_variable_parent)
real(KIND=4) :: real_variable
real(KIND=4) :: real_variable_parent

integer :: i
logical :: i_found

i_found = .FALSE.

do i=1,Agrif_NbVariables(2)
  if (LOC(real_variable) == LOC(agrif_curgrid%tabvars_r(i)%array0)) then
     real_variable_parent = agrif_curgrid%tabvars_r(i)%parent_var%array0
     i_found = .TRUE.
     EXIT
  endif
enddo

IF (.NOT.i_found) THEN
do i=1,Agrif_NbVariables(2)
  if (LOC(real_variable) == LOC(agrif_curgrid%tabvars_r(i)%sarray0)) then
     real_variable_parent = agrif_curgrid%tabvars_r(i)%parent_var%sarray0
     i_found = .TRUE.
     EXIT
  endif
enddo
ENDIF

if (.NOT.i_found) STOP 'Agrif_Parent_Real_4 : Variable not found'

end function Agrif_Parent_Real_4

function Agrif_Parent_Real_8(real_variable) result(real_variable_parent)
real(KIND=8) :: real_variable
real(KIND=8) :: real_variable_parent

integer :: i
logical :: i_found

i_found = .FALSE.

do i=1,Agrif_NbVariables(2)
  if (LOC(real_variable) == LOC(agrif_curgrid%tabvars_r(i)%array0)) then
     real_variable_parent = agrif_curgrid%tabvars_r(i)%parent_var%array0
     i_found = .TRUE.
     EXIT
  endif
enddo

IF (.NOT.i_found) THEN
do i=1,Agrif_NbVariables(2)
  if (LOC(real_variable) == LOC(agrif_curgrid%tabvars_r(i)%darray0)) then
     real_variable_parent = agrif_curgrid%tabvars_r(i)%parent_var%darray0
     i_found = .TRUE.
     EXIT
  endif
enddo
ENDIF

if (.NOT.i_found) STOP 'Agrif_Parent_Real_8 : Variable not found'

end function Agrif_Parent_Real_8

function Agrif_Parent_Integer(integer_variable) result(integer_variable_parent)
integer :: integer_variable
integer :: integer_variable_parent

integer :: i
logical :: i_found

i_found = .FALSE.

do i=1,Agrif_NbVariables(4)
  if (LOC(integer_variable) == LOC(agrif_curgrid%tabvars_i(i)%iarray0)) then
     integer_variable_parent = agrif_curgrid%tabvars_i(i)%parent_var%iarray0
     i_found = .TRUE.
     EXIT
  endif
enddo

if (.NOT.i_found) STOP 'Agrif_Parent : Variable not found'

end function Agrif_Parent_Integer

function Agrif_Parent_Character(character_variable) result(character_variable_parent)
character(*) :: character_variable
character(len(character_variable)) :: character_variable_parent

integer :: i
logical :: i_found

i_found = .FALSE.

do i=1,Agrif_NbVariables(1)
  if (LOC(character_variable) == LOC(agrif_curgrid%tabvars_c(i)%carray0)) then
     character_variable_parent = agrif_curgrid%tabvars_c(i)%parent_var%carray0
     i_found = .TRUE.
     EXIT
  endif
enddo

if (.NOT.i_found) STOP 'Agrif_Parent : Variable not found'

end function Agrif_Parent_Character

function Agrif_Parent_Logical(logical_variable) result(logical_variable_parent)
logical :: logical_variable
logical :: logical_variable_parent

integer :: i
logical :: i_found

i_found = .FALSE.

do i=1,Agrif_NbVariables(3)
  if (LOC(logical_variable) == LOC(agrif_curgrid%tabvars_l(i)%larray0)) then
     logical_variable_parent = agrif_curgrid%tabvars_l(i)%parent_var%larray0
     i_found = .TRUE.
     EXIT
  endif
enddo

if (.NOT.i_found) STOP 'Agrif_Parent : Variable not found'

end function Agrif_Parent_Logical

function Agrif_Irhox() result(i_val)
integer :: i_val
i_val = agrif_curgrid%spaceref(1)
end function Agrif_Irhox

function Agrif_Irhoy() result(i_val)
integer :: i_val
i_val = agrif_curgrid%spaceref(2)
end function Agrif_Irhoy

function Agrif_Irhoz() result(i_val)
integer :: i_val
i_val = agrif_curgrid%spaceref(3)
end function Agrif_Irhoz

function Agrif_NearCommonBorderX() result(l_val)
logical :: l_val
l_val = agrif_curgrid%nearRootBorder(1)
end function Agrif_NearCommonBorderX

function Agrif_NearCommonBorderY() result(l_val)
logical :: l_val
l_val = agrif_curgrid%nearRootBorder(2)
end function Agrif_NearCommonBorderY

function Agrif_NearCommonBorderZ() result(l_val)
logical :: l_val
l_val = agrif_curgrid%nearRootBorder(3)
end function Agrif_NearCommonBorderZ

function Agrif_DistantCommonBorderX() result(l_val)
logical :: l_val
l_val = agrif_curgrid%DistantRootBorder(1)
end function Agrif_DistantCommonBorderX

function Agrif_DistantCommonBorderY() result(l_val)
logical :: l_val
l_val = agrif_curgrid%DistantRootBorder(2)
end function Agrif_DistantCommonBorderY

function Agrif_DistantCommonBorderZ() result(l_val)
logical :: l_val
l_val = agrif_curgrid%DistantRootBorder(3)
end function Agrif_DistantCommonBorderZ

function Agrif_Ix() result(i_val)
integer :: i_val
i_val = agrif_curgrid%ix(1)
end function Agrif_Ix

function Agrif_Iy() result(i_val)
integer :: i_val
i_val = agrif_curgrid%ix(2)
end function Agrif_Iy

function Agrif_Iz() result(i_val)
integer :: i_val
i_val = agrif_curgrid%ix(3)
end function Agrif_Iz

function Agrif_Get_grid_id() result(i_val)
integer :: i_val
i_val = agrif_curgrid % grid_id
end function Agrif_Get_grid_id

function Agrif_Get_parent_id() result(i_val)
integer :: i_val
i_val = agrif_curgrid % parent % grid_id
end function Agrif_Get_parent_id

function Agrif_rhox() result(r_val)
real :: r_val
r_val = real(agrif_curgrid%spaceref(1))
end function Agrif_rhox

function Agrif_rhoy() result(r_val)
real :: r_val
r_val = real(agrif_curgrid%spaceref(2))
end function Agrif_rhoy

function Agrif_rhoz() result(r_val)
real :: r_val
r_val = real(agrif_curgrid%spaceref(3))
end function Agrif_rhoz

function Agrif_Nb_Step() result(i_val)
integer :: i_val
i_val = agrif_curgrid%ngridstep
end function Agrif_Nb_Step

function Agrif_Nb_Fine_Grids() result(i_val)
integer :: i_val
i_val = Agrif_nbfixedgrids
end function Agrif_Nb_Fine_Grids

end module Agrif_CurgridFunctions
