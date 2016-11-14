module Agrif_Grids

    use Agrif_Types
!
    implicit none
!
!===================================================================================================
type Agrif_Grid_List
!---------------------------------------------------------------------------------------------------
!<  List of grids.
!
    integer                     :: nitems = 0      !< Number of elements in the list
    type(Agrif_PGrid), pointer  :: first => NULL() !< Pointer to the first grid in the list
    type(Agrif_PGrid), pointer  :: last  => NULL() !< Pointer to the last  grid inserted in the list
!---------------------------------------------------------------------------------------------------
end type Agrif_Grid_List
!===================================================================================================
!
!===================================================================================================
type Agrif_PGrid
!---------------------------------------------------------------------------------------------------
!<  Data type to go over the grid hierarchy (used for the creation of this grid hierarchy
!<  and during the time integration).
!
    type(Agrif_Grid) , pointer :: gr   => NULL()  !< Pointer to the actual grid data structure
    type(Agrif_PGrid), pointer :: next => NULL()  !< Next grid in the list
!
!---------------------------------------------------------------------------------------------------
end type Agrif_PGrid
!===================================================================================================
!
!===================================================================================================
type Agrif_Grid
!---------------------------------------------------------------------------------------------------
!<  Data type to define a grid (position, space and time refinement factors).
!
    type(Agrif_Grid)                    , pointer :: parent      => NULL() !< pointer on the parent grid
    type(Agrif_Grid)                    , pointer :: save_grid   => NULL() !< pointer on the save grid
    type(Agrif_Grid_List)                         :: child_list            !< List of child grids
    type(Agrif_Variable),   dimension(:), allocatable :: tabvars    !< List of grid variables
    type(Agrif_Variable_c), dimension(:), allocatable :: tabvars_c  !< List of character grid variables
    type(Agrif_Variable_r), dimension(:), allocatable :: tabvars_r  !< List of real      grid variables
    type(Agrif_Variable_l), dimension(:), allocatable :: tabvars_l  !< List of logical   grid variables
    type(Agrif_Variable_i), dimension(:), allocatable :: tabvars_i  !< List of integer   grid variables
!
    real   , dimension(3)              :: Agrif_x   !< global x, y and z position
    real   , dimension(3)              :: Agrif_dx  !< global space step in the x, y and z direction
    real   , dimension(3)              :: Agrif_dt  !< global time  step in the x, y and z direction
    integer, dimension(3)              :: nb        !< number of cells in the x, y and z direction
    integer, dimension(3)              :: ix        !< minimal position in the x, y and z direction
    integer, dimension(3)              :: spaceref  !< space refinement factor in the x, y and z direction
    integer, dimension(3)              :: timeref   !< Time refinement factor in the x, y and z direction
    integer                            :: ngridstep !< number of time steps
    integer                            :: rank
    integer                            :: grid_id   !< moving grid id
    integer                            :: fixedrank !< number of the grid
    logical                            :: fixed     !< fixed or moving grid ?
    logical                            :: oldgrid
!> \name Logicals indicating if the current grid has a common border with the root coarse grid
!> @{
    logical, dimension(3)              :: NearRootBorder
    logical, dimension(3)              :: DistantRootBorder
!> @}
!> \name Arrays for adaptive grid refinement
!> @{
    integer, dimension(:)    ,   allocatable :: tabpoint1D
    integer, dimension(:,:)  ,   allocatable :: tabpoint2D
    integer, dimension(:,:,:),   allocatable :: tabpoint3D
!> @}
!> \name Members for parallel integration
!> @{
    type(Agrif_Rectangle),  pointer    :: rect_in_parent => NULL()
    type(Agrif_Grid_List)              :: neigh_list                !< List of neighboring grids (ie. connected through a common proc)
    type(Agrif_Proc_List)              :: proc_def_list             !< List of procs that will integrate this grid
    type(Agrif_Proc_List)              :: proc_def_in_parent_list   !< List of procs that will integrate this grid (defined as in the parent grid)
    type(Agrif_Proc_List)              :: required_proc_list        !< List of procs that are required for this grid
    type(Agrif_Sequence_List), pointer :: child_seq => NULL()       !< Sequence for childs
    integer                            :: seq_num = 0
    integer                            :: size = 0
    integer                            :: dsat = 0
#if defined AGRIF_MPI
    integer                            :: communicator = -1
#endif
!> @}
    type(Agrif_Variables_List), pointer :: variables => NULL()
    integer                             :: NbVariables = 0
    integer                             :: level    !< level of the grid in the hierarchy
    logical                             :: allocation_is_done = .false.
    logical                             :: grand_mother_grid = .false.
!---------------------------------------------------------------------------------------------------
end type Agrif_Grid
!===================================================================================================
!
!> this pointer always points on the root grid of the grid hierarchy
type(Agrif_Grid) , pointer :: Agrif_Mygrid => NULL()

!> this pointer always points on the grand mother grid of the grid hierarchy (if any)
type(Agrif_Grid) , pointer :: Agrif_Coarsegrid => NULL()

!> Grid list used in the \link Agrif_Util::Agrif_Regrid() Agrif_regrid \endlink subroutine.
!> It contains  the safeguard of the grid hierarchy.
type(Agrif_Grid_List), pointer :: Agrif_oldmygrid => NULL()

!> Pointer to the current grid (the link is done by using the Agrif_Instance procedure (\see module Agrif_Init))
type(Agrif_Grid) , pointer :: Agrif_Curgrid => NULL()
!
!===================================================================================================
type Agrif_Sequence
!---------------------------------------------------------------------------------------------------
    type(Agrif_Grid_List) :: gridlist
    type(Agrif_Proc_List) :: proclist
!---------------------------------------------------------------------------------------------------
end type Agrif_Sequence
!===================================================================================================
!
!===================================================================================================
type Agrif_Sequence_List
!---------------------------------------------------------------------------------------------------
    integer :: nb_seqs
    type(Agrif_Sequence), dimension(:), allocatable :: sequences
!---------------------------------------------------------------------------------------------------
end type Agrif_Sequence_List
!===================================================================================================
!
interface
    function compare_grids ( grid1, grid2 ) result( res )
        import Agrif_Grid
        type(Agrif_Grid), intent(in)    :: grid1
        type(Agrif_Grid), intent(in)    :: grid2
        integer                         :: res  !< Result of the comparison :
                                                !!  - res  < 0   if grid1 <  grid2
                                                !!  - res == 0   if grid1 == grid2
                                                !!  - res  > 0   if grid1 >  grid2
    end function compare_grids
end interface
!
contains
!
!===================================================================================================
subroutine Agrif_gl_print ( gridlist )
!
!< DEBUG : a virer à terme.
!---------------------------------------------------------------------------------------------------
    type(Agrif_Grid_List), intent(in)   :: gridlist
!
    type(Agrif_PGrid), pointer  :: gridp
    type(Agrif_Grid),  pointer  :: grid
!
    gridp => gridlist % first
    do while ( associated(gridp) )
        grid => gridp % gr
        write(*,'("G",i0,", ")', advance='no') grid % fixedrank
        gridp => gridp % next
    enddo
    write(*,*)
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_gl_print
!===================================================================================================
!
!===================================================================================================
subroutine Agrif_gl_print_debug ( gridlist )
!
!< DEBUG : a virer à terme.
!---------------------------------------------------------------------------------------------------
    type(Agrif_Grid_List), intent(in)   :: gridlist
!
    type(Agrif_PGrid), pointer  :: gridp
    type(Agrif_Grid),  pointer  :: grid
!
    write(*,'(" (nitems=",i2,"), (id,neighs,color,dsat,size) = ")', advance='no') gridlist % nitems
    gridp => gridlist % first
    do while ( associated(gridp) )
        grid => gridp % gr
        write(*,'("(G",i0,4(",",i0),"), ")', advance='no') grid % fixedrank, &
            grid % neigh_list % nitems, grid % seq_num, grid % dsat, grid % size
        gridp => gridp % next
    enddo
    write(*,*)
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_gl_print_debug
!===================================================================================================
!
!===================================================================================================
subroutine Agrif_gl_append ( gridlist, grid )
!---------------------------------------------------------------------------------------------------
    type(Agrif_Grid_List),     intent(inout)  :: gridlist
    type(Agrif_Grid), pointer, intent(in)     :: grid
!
    type(Agrif_PGrid), pointer   :: new_gp
!
    allocate( new_gp )
!
    new_gp % gr   => grid
    new_gp % next => NULL()
!
    if ( associated(gridlist % last) ) then
    ! the list is not empty, append the new pointer at the end
        gridlist % last % next => new_gp
    else
    ! the list is empty, the new pointer is the first one
        gridlist % first => new_gp
    endif
    ! anyway, for next time 'grid' will be the last one.
    gridlist % last => new_gp
    gridlist % nitems = gridlist % nitems + 1
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_gl_append
!===================================================================================================
!
!===================================================================================================
function Agrif_gl_popfirst ( gridlist ) result ( grid )
!
!<  Removes the first item of the list and returns it.
!---------------------------------------------------------------------------------------------------
    type(Agrif_Grid_List), intent(inout)    :: gridlist
!
    type(Agrif_PGrid), pointer :: grid_p
    type(Agrif_Grid),  pointer :: grid
!
    grid_p => gridlist % first
!
    if ( .not. associated( grid_p ) ) then
        grid => NULL()
        return
    endif
!
    grid              => grid_p % gr
    gridlist % first  => grid_p % next
    gridlist % nitems =  gridlist % nitems - 1
    if ( .not. associated(gridlist % first) ) then
       nullify(gridlist % last)
    endif
    deallocate(grid_p)
!---------------------------------------------------------------------------------------------------
end function Agrif_gl_popfirst
!===================================================================================================
!
!===================================================================================================
subroutine Agrif_gl_copy ( new_gl, model )
!---------------------------------------------------------------------------------------------------
    type(Agrif_Grid_List), intent(out)  :: new_gl
    type(Agrif_Grid_List), intent(in)   :: model
!
    type(Agrif_PGrid), pointer :: gp
!
    call Agrif_gl_clear(new_gl)
    gp => model % first
!
    do while( associated(gp) )
        call Agrif_gl_append( new_gl, gp % gr )
        gp => gp % next
    enddo
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_gl_copy
!===================================================================================================
!
!===================================================================================================
function Agrif_gl_build_from_gp ( gridp ) result ( gridlist )
!---------------------------------------------------------------------------------------------------
    type(Agrif_PGrid), pointer, intent(in)   :: gridp
!
    type(Agrif_Grid_List), pointer  :: gridlist
    type(Agrif_PGrid),     pointer  :: gp
!
    allocate(gridlist)
!
    gp => gridp
!
    do while ( associated( gp ) )
        call Agrif_gl_append( gridlist, gp % gr )
        gp => gp % next
    enddo
!---------------------------------------------------------------------------------------------------
end function Agrif_gl_build_from_gp
!===================================================================================================
!
!===================================================================================================
subroutine Agrif_gp_delete ( gridp )
!---------------------------------------------------------------------------------------------------
    type(Agrif_PGrid), pointer, intent(inout)  :: gridp
!
    type(Agrif_PGrid), pointer   :: gp, gpd
!
    if ( .not. associated( gridp ) ) return
!
    gp => gridp
!
    do while( associated(gp) )
        gpd => gp
        gp  => gp % next
        deallocate(gpd)
    enddo
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_gp_delete
!===================================================================================================
!
!===================================================================================================
subroutine Agrif_gl_clear ( gridlist )
!---------------------------------------------------------------------------------------------------
    type(Agrif_Grid_List), intent(inout)  :: gridlist
!
    call Agrif_gp_delete(gridlist % first)
    gridlist % first => NULL()
    gridlist % last  => NULL()
    gridlist % nitems = 0
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_gl_clear
!===================================================================================================
!
!===================================================================================================
subroutine Agrif_gl_delete ( gridlist )
!---------------------------------------------------------------------------------------------------
    type(Agrif_Grid_List), pointer, intent(inout)  :: gridlist
!
    if ( .not. associated( gridlist ) ) return
!
    call Agrif_gp_delete(gridlist % first)
    deallocate( gridlist )
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_gl_delete
!===================================================================================================
!
!===================================================================================================
recursive function Agrif_gl_merge_sort ( gridlist, compare_func, compare_func_opt ) result( gl_sorted )
!---------------------------------------------------------------------------------------------------
    type(Agrif_Grid_List), intent(in)    :: gridlist
    procedure(compare_grids)             :: compare_func
    procedure(compare_grids), optional   :: compare_func_opt
!
    type(Agrif_Grid_List), pointer  :: gl_sorted
    type(Agrif_Grid_List), pointer  :: gl_left,  gl_sorted_left
    type(Agrif_Grid_List), pointer  :: gl_right, gl_sorted_right
    type(Agrif_PGrid),     pointer  :: grid_p
    integer                         :: n, middle
!
! if list size is 1, consider it sorted and return it
    if  ( (gridlist % nitems <= 1) ) then
        gl_sorted => Agrif_gl_build_from_gp(gridlist % first)
        return
    endif
!
! else split the list into two sublists
    n = 1
    middle    =  gridlist % nitems / 2
    grid_p    => gridlist % first
!
    allocate( gl_left, gl_right )
!
    do while ( associated(grid_p) )
        if ( n <= middle ) then
            call Agrif_gl_append(gl_left,  grid_p % gr)
        else
            call Agrif_gl_append(gl_right, grid_p % gr)
        endif
        grid_p => grid_p % next
        n = n+1
    enddo
!
! recursively call Agrif_gl_merge_sort() to further split each sublist until sublist size is 1
    gl_sorted_left  => Agrif_gl_merge_sort(gl_left,  compare_func, compare_func_opt)
    gl_sorted_right => Agrif_gl_merge_sort(gl_right, compare_func, compare_func_opt)
!
! merge the sublists returned from prior calls to gl_merge_sort() and return the resulting merged sublist
    gl_sorted => Agrif_gl_merge(gl_sorted_left, gl_sorted_right, compare_func, compare_func_opt)
!
    call Agrif_gl_delete( gl_left )
    call Agrif_gl_delete( gl_right )
    call Agrif_gl_delete( gl_sorted_left )
    call Agrif_gl_delete( gl_sorted_right )
!---------------------------------------------------------------------------------------------------
end function Agrif_gl_merge_sort
!===================================================================================================
!
!===================================================================================================
function Agrif_gl_merge ( gl_left, gl_right, compare_func, compare_func_opt ) result( gl_merged )
!---------------------------------------------------------------------------------------------------
    type(Agrif_Grid_List), intent(inout)    :: gl_left
    type(Agrif_Grid_List), intent(inout)    :: gl_right
    procedure(compare_grids)                :: compare_func
    procedure(compare_grids), optional      :: compare_func_opt
!
    type(Agrif_Grid_List), pointer  :: gl_merged
    type(Agrif_Grid),      pointer  :: poped_grid
    integer                         :: comp_value
!
    allocate( gl_merged )
!
    do while ( gl_left % nitems > 0 .or. gl_right % nitems > 0 )
!
        if ( gl_left % nitems > 0 .and. gl_right % nitems > 0 ) then
!
!         Let.s compare both items with the first compare function
            comp_value = compare_func( gl_left % first % gr, gl_right % first % gr )
!
            if     ( comp_value < 0 ) then ; poped_grid => Agrif_gl_popfirst(gl_left)
            elseif ( comp_value > 0 ) then ; poped_grid => Agrif_gl_popfirst(gl_right)
            else ! ( comp_value == 0 )
!
!             Both items are equal, let.s use the second criterion if the optional
!             compare function is present.
                if ( present(compare_func_opt) ) then
!
                    comp_value = compare_func_opt( gl_left % first % gr, gl_right % first % gr )
!
                    if ( comp_value <= 0 ) then ; poped_grid => Agrif_gl_popfirst(gl_left)
                    else                        ; poped_grid => Agrif_gl_popfirst(gl_right)
                    endif
                else
!                 If the second criterion is not present, let.s just pick the left item
                    poped_grid => Agrif_gl_popfirst(gl_left)
                endif
            endif
!
!     If one of the lists is empty, we just have to pick in the other one.
        elseif ( gl_left  % nitems > 0 ) then ; poped_grid => Agrif_gl_popfirst(gl_left)
        elseif ( gl_right % nitems > 0 ) then ; poped_grid => Agrif_gl_popfirst(gl_right)
        endif
!
        call Agrif_gl_append( gl_merged, poped_grid )
!
    enddo
!---------------------------------------------------------------------------------------------------
end function Agrif_gl_merge
!===================================================================================================
!
!===================================================================================================
function compare_grid_degrees ( grid1, grid2 ) result( res )
!---------------------------------------------------------------------------------------------------
    type(Agrif_Grid), intent(in)    :: grid1
    type(Agrif_Grid), intent(in)    :: grid2
!
    integer     :: res
!
    res = grid2 % neigh_list % nitems - grid1 % neigh_list % nitems
!---------------------------------------------------------------------------------------------------
end function compare_grid_degrees
!===================================================================================================
!
!===================================================================================================
function compare_colors ( grid1, grid2 ) result( res )
!---------------------------------------------------------------------------------------------------
    type(Agrif_Grid), intent(in)    :: grid1
    type(Agrif_Grid), intent(in)    :: grid2
!
    integer     :: res
!
    res = grid1 % seq_num - grid2 % seq_num
!---------------------------------------------------------------------------------------------------
end function compare_colors
!===================================================================================================
!
!===================================================================================================
function compare_dsat_values ( grid1, grid2 ) result( res )
!---------------------------------------------------------------------------------------------------
    type(Agrif_Grid), intent(in)    :: grid1
    type(Agrif_Grid), intent(in)    :: grid2
!
    integer     :: res
!
    res = grid2 % dsat - grid1 % dsat
!---------------------------------------------------------------------------------------------------
end function compare_dsat_values
!===================================================================================================
!
!===================================================================================================
function compare_size_values ( grid1, grid2 ) result( res )
!---------------------------------------------------------------------------------------------------
    type(Agrif_Grid), intent(in)    :: grid1
    type(Agrif_Grid), intent(in)    :: grid2
!
    integer     :: res
!
    res = grid2 % size - grid1 % size
!---------------------------------------------------------------------------------------------------
end function compare_size_values
!===================================================================================================
!
end module Agrif_Grids
