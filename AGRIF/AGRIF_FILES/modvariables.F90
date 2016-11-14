module Agrif_Variables
!
    use Agrif_CurgridFunctions
!
    implicit none
!
contains
!
!===================================================================================================
!  subroutine Agrif_Declare_Variable
!
!> Declare a new variable profile
!---------------------------------------------------------------------------------------------------
subroutine Agrif_Declare_Variable ( posvar, firstpoint, raf, lb, ub, varid, torestore )
!---------------------------------------------------------------------------------------------------
    integer,      dimension(:), intent(in)  :: posvar     !< position of the variable on the cell
                                                          !! (1 for the border of the edge, 2 for the center)
    integer,      dimension(:), intent(in)  :: firstpoint !< index of the first point in the real domain
    character(1), dimension(:), intent(in)  :: raf        !< Array indicating the type of dimension (space or not)
                                                          !!   for each of them
    integer,      dimension(:), intent(in)  :: lb         !< Lower bounds of the array
    integer,      dimension(:), intent(in)  :: ub         !< Upper bounds of the array
    integer,                    intent(out) :: varid      !< Id number of the newly created variable
    logical,      optional,     intent(in)  :: torestore  !< Indicates if the array restore is used
!---------------------------------------------------------------------------------------------------
    type(Agrif_Variables_List), pointer :: new_varlist
    type(Agrif_Variable),       pointer :: var
    integer                             :: nbdim, i
    logical                             :: restore

    restore = .FALSE.
    if ( Agrif_Mygrid % ngridstep /= 0 ) then
        if (present(torestore)) restore = torestore
    endif
!
    nbdim = SIZE(posvar)
!
    allocate(new_varlist)
    allocate(new_varlist % var)

    var => new_varlist % var

    allocate(var % posvar(nbdim))
    allocate(var % interptab(nbdim))
    allocate(var % coords(nbdim))
!
    var % nbdim          = nbdim
    var % interptab      = raf(1:nbdim)
    var % posvar         = posvar(1:nbdim)
    var % point(1:nbdim) = firstpoint(1:nbdim)
    var % restore = restore
!
    do i = 1,nbdim
        select case( raf(i) )
            case('x')    ; var % coords(i) = 1
            case('y')    ; var % coords(i) = 2
            case('z')    ; var % coords(i) = 3
            case('N')    ; var % coords(i) = 0
            case default ; var % coords(i) = 0
        end select
    enddo
!
    var % lb(1:nbdim) = lb(1:nbdim)
    var % ub(1:nbdim) = ub(1:nbdim)

    if ( restore ) then
        select case(nbdim)
        case(1)
            allocate(var % Restore1D(lb(1):ub(1)))
            var % Restore1D = 0
        case(2)
            allocate(var % Restore2D(lb(1):ub(1),   &
                                     lb(2):ub(2)))
            var % Restore2D = 0
        case(3)
            allocate(var % Restore3D(lb(1):ub(1),   &
                                     lb(2):ub(2),   &
                                     lb(3):ub(3)))
            var % Restore3D = 0
        case(4)
            allocate(var % Restore4D(lb(1):ub(1),   &
                                     lb(2):ub(2),   &
                                     lb(3):ub(3),   &
                                     lb(4):ub(4)))
            var % Restore4D = 0
        case(5)
            allocate(var % Restore5D(lb(1):ub(1),   &
                                     lb(2):ub(2),   &
                                     lb(3):ub(3),   &
                                     lb(4):ub(4),   &
                                     lb(5):ub(5)))
            var % Restore5D = 0
        end select
    endif

    new_varlist % next => Agrif_Curgrid % variables

    Agrif_Curgrid % variables => new_varlist
    Agrif_Curgrid % Nbvariables = Agrif_Curgrid % Nbvariables + 1

    varid = Agrif_Curgrid % Nbvariables

    var % parent_var => Agrif_Search_Variable(Agrif_Curgrid % parent, Agrif_Curgrid % nbvariables)
    var % root_var   => Agrif_Search_Variable(Agrif_Mygrid, Agrif_Curgrid % nbvariables)
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Declare_Variable
!===================================================================================================
!
!===================================================================================================
!  function Agrif_Search_Variable
!
!> Returns a pointer to the variable varid for the grid grid.
!---------------------------------------------------------------------------------------------------
function Agrif_Search_Variable ( grid, varid ) result(outvar)
!---------------------------------------------------------------------------------------------------
    type(Agrif_Grid), pointer       :: grid     !< Pointer on the current grid.
    integer,          intent(in)    :: varid    !< ID number of the variable we are looking for.
!
    type(Agrif_Variable),       pointer :: outvar
    type(Agrif_Variables_List), pointer :: parcours
    integer :: nb, varidinv
!
    if ( .not.associated(grid) ) then
        outvar => NULL()
        return
    endif
!
    parcours => grid % variables

    if (.not. associated(parcours)) then   ! can occur on the grand mother grid
           outvar => NULL()                ! during the first call by agrif_mygrid
           return
    endif

    varidinv = 1 + grid % nbvariables - varid

    do nb = 1,varidinv-1
        parcours => parcours % next
    enddo

    outvar => parcours % var
!---------------------------------------------------------------------------------------------------
end function Agrif_Search_variable
!===================================================================================================
!
end module Agrif_Variables
