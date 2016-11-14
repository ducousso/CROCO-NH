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
!     Foundation, Inc., 59 Temple Place-Suite 330, Boston, MA 02111-1307, USA.
!
!
!
!
!> Definition of data types used in AGRIF, of several variables and parameters
!
module Agrif_Types
!
use Agrif_Procs
!
implicit none
!
integer, parameter :: Agrif_MaxRaff = 7       !< Maximum refinement ratio
integer, parameter :: Agrif_NbMaxGrids = 10   !< Maximum number of grids of the hierarchy
!
!===================================================================================================
type Agrif_LRectangle
!---------------------------------------------------------------------------------------------------
!<  Data type allowing a grid to reach a grid on the same level or its child grids
!
    type(Agrif_Rectangle) , pointer :: r    => NULL()   !< to reach a child grid
    type(Agrif_LRectangle), pointer :: next => NULL()   !< to reach a grid on the same level
!
!---------------------------------------------------------------------------------------------------
end type Agrif_LRectangle
!===================================================================================================
!
!===================================================================================================
type Agrif_Rectangle
!---------------------------------------------------------------------------------------------------
!<  Data type to define several characteristics of a grid (number, position, time and space
!<  refinement factors, etc).
!
    integer                         :: number       !< Number of the grid
    integer, dimension(3)           :: imin         !< Minimal position in the x,y and z direction
    integer, dimension(3)           :: imax         !< Maximal position in the x,y and z direction
    integer, dimension(3)           :: spaceref     !< Space refinement factor in the x,y and z direction
    integer, dimension(3)           :: timeref      !< Time refinement factor in the x,y and z direction
    type(Agrif_LRectangle), pointer :: childgrids => NULL()   !< Pointer to reach a grid on the same level or a child grid
!
!---------------------------------------------------------------------------------------------------
end type Agrif_Rectangle
!===================================================================================================
!
!===================================================================================================
type Agrif_Variable
!---------------------------------------------------------------------------------------------------
!<  Data type to characterize a grid variable.
!
    type(Agrif_Variable), pointer  :: root_var   => NULL()  !< pointer on the variable of the root grid
    type(Agrif_Variable), pointer  :: parent_var => NULL()  !< pointer on the parent variable
!
    integer,   dimension(6)              :: point           !< index of the first point in the
                                                            !<    real domain (x,y and z direction)
    integer,   dimension(:), allocatable :: posvar          !< position of the variable on the cell
                                                            !<   (1 for the boarder of the edge, 2 for the center)
    integer                            :: interpIndex = -1  !< Indication for the space interpolation (module Agrif_Boundary)
    integer                            :: nbdim = 0         !< number of dimensions of the grid variable
    character(1), dimension(:), allocatable :: interptab    !< Array indicating the type of dimension (space or not)
                                                            !!   for each of them
    integer,   dimension(:), allocatable :: coords          !< Array indicating the coordinate for each dimension
                                                            !!   of the array that is refined :
                                                            !!  'x' -> 1 ; 'y' -> 2 ; 'z' -> 3 ; 'N' -> 0

!> @}
!> \name Arrays containing the values of the grid variables (real)
!> @{
    real,    dimension(:)          , allocatable :: array1
    real,    dimension(:,:)        , allocatable :: array2
    real,    dimension(:,:,:)      , allocatable :: array3
    real,    dimension(:,:,:,:)    , allocatable :: array4
    real,    dimension(:,:,:,:,:)  , allocatable :: array5
    real,    dimension(:,:,:,:,:,:), allocatable :: array6
!> @}
!> \name Arrays containing the values of the grid variables (real*8)
!> @{
    real(8), dimension(:)          , allocatable :: darray1
    real(8), dimension(:,:)        , allocatable :: darray2
    real(8), dimension(:,:,:)      , allocatable :: darray3
    real(8), dimension(:,:,:,:)    , allocatable :: darray4
    real(8), dimension(:,:,:,:,:)  , allocatable :: darray5
    real(8), dimension(:,:,:,:,:,:), allocatable :: darray6
!> @}
!> \name Arrays containing the values of the grid variables (real*4)
!> @{
    real(4), dimension(:)          , allocatable :: sarray1
    real(4), dimension(:,:)        , allocatable :: sarray2
    real(4), dimension(:,:,:)      , allocatable :: sarray3
    real(4), dimension(:,:,:,:)    , allocatable :: sarray4
    real(4), dimension(:,:,:,:,:)  , allocatable :: sarray5
    real(4), dimension(:,:,:,:,:,:), allocatable :: sarray6
!> @}
!> \name Arrays containing the values of the grid variables (real)
!> @{
    real,    dimension(:)          , pointer :: parray1
    real,    dimension(:,:)        , pointer :: parray2
    real,    dimension(:,:,:)      , pointer :: parray3
    real,    dimension(:,:,:,:)    , pointer :: parray4
    real,    dimension(:,:,:,:,:)  , pointer :: parray5
    real,    dimension(:,:,:,:,:,:), pointer :: parray6
!> @}
!> \name Arrays containing the values of the grid variables (real*8)
!> @{
    real(8), dimension(:)          , pointer :: pdarray1
    real(8), dimension(:,:)        , pointer :: pdarray2
    real(8), dimension(:,:,:)      , pointer :: pdarray3
    real(8), dimension(:,:,:,:)    , pointer :: pdarray4
    real(8), dimension(:,:,:,:,:)  , pointer :: pdarray5
    real(8), dimension(:,:,:,:,:,:), pointer :: pdarray6
!> @}
!> \name Arrays containing the values of the grid variables (real*4)
!> @{
    real(4), dimension(:)          , pointer :: psarray1
    real(4), dimension(:,:)        , pointer :: psarray2
    real(4), dimension(:,:,:)      , pointer :: psarray3
    real(4), dimension(:,:,:,:)    , pointer :: psarray4
    real(4), dimension(:,:,:,:,:)  , pointer :: psarray5
    real(4), dimension(:,:,:,:,:,:), pointer :: psarray6
!> @}
!> \name Arrays used to restore the values
!> @{
    integer, dimension(:)          , pointer :: restore1D => NULL()
    integer, dimension(:,:)        , pointer :: restore2D => NULL()
    integer, dimension(:,:,:)      , pointer :: restore3D => NULL()
    integer, dimension(:,:,:,:)    , pointer :: restore4D => NULL()
    integer, dimension(:,:,:,:,:)  , pointer :: restore5D => NULL()
    integer, dimension(:,:,:,:,:,:), pointer :: restore6D => NULL()
!> @}

    real, dimension(:,:), pointer :: oldvalues2D => NULL() !< Array used for the time interpolation

    logical :: restore = .FALSE. !< =1 if the variable should be restored
    logical :: Interpolationshouldbemade = .FALSE. !< TRUE if the interpolation should be made in any case
    integer                 :: bcinf !< option bc
    integer                 :: bcsup !< option bc
    integer, dimension(6)   :: type_interp    !< option interp
    integer, dimension(6,6) :: type_interp_bc !< option bcinterp
    integer, dimension(6)   :: type_update    !< option update

    integer, dimension(6)   :: lb
    integer, dimension(6)   :: ub

    logical,dimension(6,2) :: memberin
    integer,dimension(6,2,2,6,2) :: childarray

    type(Agrif_List_Interp_Loc), pointer :: list_interp => NULL()
    type(Agrif_List_Interp_Loc), pointer :: list_update => NULL()
!---------------------------------------------------------------------------------------------------
end type Agrif_Variable
!===================================================================================================
!
!===================================================================================================
type Agrif_Variable_c
!---------------------------------------------------------------------------------------------------
!<  Data type to characterize a grid variable.
!
    type(Agrif_Variable_c), pointer  :: root_var   => NULL()      !< pointer on the variable of the root grid
    type(Agrif_Variable_c), pointer  :: parent_var => NULL()      !< pointer on the parent variable
!
    integer                          :: nbdim = 0                 !< number of dimensions of the grid variable
!
!> \name Arrays containing the values of the grid variables (character)
!> @{
    character(4000)                             :: carray0
    character(400), dimension(:)  , allocatable :: carray1
    character(400), dimension(:,:), allocatable :: carray2
!> @}
!---------------------------------------------------------------------------------------------------
end type Agrif_Variable_c
!===================================================================================================
!
!===================================================================================================
type Agrif_Variable_r
!---------------------------------------------------------------------------------------------------
!<  Data type to characterize a grid variable.
!
    type(Agrif_Variable_r), pointer  :: root_var   => NULL()      !< pointer on the variable of the root grid
    type(Agrif_Variable_r), pointer  :: parent_var => NULL()      !< pointer on the parent variable
!
    integer                          :: nbdim = 0                 !< number of dimensions of the grid variable
!
!> \name Arrays containing the values of the grid variables (real)
!> @{
    real                                         :: array0
    real,    dimension(:)          , allocatable :: array1
    real,    dimension(:,:)        , allocatable :: array2
    real,    dimension(:,:,:)      , allocatable :: array3
    real,    dimension(:,:,:,:)    , allocatable :: array4
    real,    dimension(:,:,:,:,:)  , allocatable :: array5
    real,    dimension(:,:,:,:,:,:), allocatable :: array6
!> @}
!> \name Arrays containing the values of the grid variables (real*8)
!> @{
    real(8)                                      :: darray0
    real(8), dimension(:)          , allocatable :: darray1
    real(8), dimension(:,:)        , allocatable :: darray2
    real(8), dimension(:,:,:)      , allocatable :: darray3
    real(8), dimension(:,:,:,:)    , allocatable :: darray4
    real(8), dimension(:,:,:,:,:)  , allocatable :: darray5
    real(8), dimension(:,:,:,:,:,:), allocatable :: darray6
!> @}
!> \name Arrays containing the values of the grid variables (real*4)
!> @{
    real(4)                                      :: sarray0
    real(4), dimension(:)          , allocatable :: sarray1
    real(4), dimension(:,:)        , allocatable :: sarray2
    real(4), dimension(:,:,:)      , allocatable :: sarray3
    real(4), dimension(:,:,:,:)    , allocatable :: sarray4
    real(4), dimension(:,:,:,:,:)  , allocatable :: sarray5
    real(4), dimension(:,:,:,:,:,:), allocatable :: sarray6
!> @}
!---------------------------------------------------------------------------------------------------
end type Agrif_Variable_r
!===================================================================================================
!===================================================================================================
!
!===================================================================================================
type Agrif_Variable_l
!---------------------------------------------------------------------------------------------------
!<  Data type to characterize a grid variable.
!
    type(Agrif_Variable_l), pointer  :: root_var   => NULL()      !< pointer on the variable of the root grid
    type(Agrif_Variable_l), pointer  :: parent_var => NULL()      !< pointer on the parent variable
!
    integer                          :: nbdim = 0                 !< number of dimensions of the grid variable
!
!> \name Arrays containing the values of the grid variables (logical)
!> @{
    logical                                      :: larray0 = .FALSE.
    logical, dimension(:)          , allocatable :: larray1
    logical, dimension(:,:)        , allocatable :: larray2
    logical, dimension(:,:,:)      , allocatable :: larray3
    logical, dimension(:,:,:,:)    , allocatable :: larray4
    logical, dimension(:,:,:,:,:)  , allocatable :: larray5
    logical, dimension(:,:,:,:,:,:), allocatable :: larray6
!> @}
!---------------------------------------------------------------------------------------------------
end type Agrif_Variable_l
!===================================================================================================
!
!===================================================================================================
type Agrif_Variable_i
!---------------------------------------------------------------------------------------------------
!<  Data type to characterize a grid variable.
!
    type(Agrif_Variable_i), pointer  :: root_var   => NULL()      !< pointer on the variable of the root grid
    type(Agrif_Variable_i), pointer  :: parent_var => NULL()      !< pointer on the parent variable
!
    integer                          :: nbdim = 0             !< number of dimensions of the grid variable
!
!> \name Arrays containing the values of the grid variables (integer)
!> @{
    integer                                      :: iarray0 = 0
    integer, dimension(:)          , allocatable :: iarray1
    integer, dimension(:,:)        , allocatable :: iarray2
    integer, dimension(:,:,:)      , allocatable :: iarray3
    integer, dimension(:,:,:,:)    , allocatable :: iarray4
    integer, dimension(:,:,:,:,:)  , allocatable :: iarray5
    integer, dimension(:,:,:,:,:,:), allocatable :: iarray6
!> @}
!---------------------------------------------------------------------------------------------------
end type Agrif_Variable_i
!===================================================================================================
!
!===================================================================================================
type Agrif_Interp_Loc
!---------------------------------------------------------------------------------------------------
    integer,dimension(6)              :: pttab, petab, pttab_Child, pttab_Parent = -99
    integer,dimension(6)              :: indmin, indmax
    integer,dimension(6)              :: pttruetab,cetruetab
    logical :: member, memberin
#if !defined AGRIF_MPI
    integer,dimension(6)              :: indminglob,indmaxglob
#else
    integer,dimension(6)              :: indminglob2,indmaxglob2
    integer,dimension(6,2,2)          :: parentarray
    integer,dimension(:,:,:), pointer :: tab4t          => NULL()
    integer,dimension(:,:,:), pointer :: tab5t          => NULL()
    logical, dimension(:),    pointer :: memberinall    => NULL()
    logical, dimension(:),    pointer :: memberinall2   => NULL()
    logical, dimension(:),    pointer :: sendtoproc1    => NULL()
    logical, dimension(:),    pointer :: sendtoproc2    => NULL()
    logical, dimension(:),    pointer :: recvfromproc1  => NULL()
    logical, dimension(:),    pointer :: recvfromproc2  => NULL()
#endif
!---------------------------------------------------------------------------------------------------
end type Agrif_Interp_Loc
!===================================================================================================

!===================================================================================================
type Agrif_List_Interp_Loc
!---------------------------------------------------------------------------------------------------
    type(Agrif_Interp_Loc),      pointer :: interp_loc => NULL()
    type(Agrif_List_Interp_Loc), pointer :: suiv       => NULL()
!---------------------------------------------------------------------------------------------------
end type Agrif_List_Interp_Loc
!===================================================================================================

!===================================================================================================
type Agrif_Variables_List
!---------------------------------------------------------------------------------------------------
    type(Agrif_Variable),       pointer :: var  => NULL()
    type(Agrif_Variables_List), pointer :: next => NULL()
!---------------------------------------------------------------------------------------------------
end type Agrif_Variables_List
!===================================================================================================
!
!===================================================================================================
!> Different parameters
!
    type(Agrif_Variable),   dimension(:), pointer :: Agrif_tabvars => NULL()
    type(Agrif_Variable_c), dimension(:), pointer :: Agrif_tabvars_c => NULL()
    type(Agrif_Variable_r), dimension(:), pointer :: Agrif_tabvars_r => NULL()
    type(Agrif_Variable_l), dimension(:), pointer :: Agrif_tabvars_l => NULL()
    type(Agrif_Variable_i), dimension(:), pointer :: Agrif_tabvars_i => NULL()
!
    integer               :: Agrif_Probdim          !< Problem dimension
    integer,dimension(0:4):: Agrif_NbVariables      !< Number of variables
    integer               :: Agrif_nbfixedgrids     !< Number of fixed grids in the grid hierarchy
    integer, dimension(3) :: Agrif_coeffref         !< Space refinement factor
    integer, dimension(3) :: Agrif_coeffreft        !< Time refinement factor
    logical               :: Agrif_UseSpecialValue          !< T if use special values on the parent grid
    logical               :: Agrif_UseSpecialValueInUpdate  !< T if use special values on the parent grid
    logical               :: Agrif_Update_Weights = .FALSE.
    logical               :: Agrif_UseSpecialValueFineGrid  !< T if use special values on the current grid
    real                  :: Agrif_SpecialValue             !< Special value on the parent grid
    real                  :: Agrif_SpecialValueFineGrid     !< Special value on the current grid
!>
!> \name Clustering parameters
!> @{
    integer               :: Agrif_Regridding = 10
    integer               :: Agrif_Minwidth
    real                  :: Agrif_Efficiency = 0.7
    integer               :: MaxSearch = 5
    real, dimension(3)    :: Agrif_mind
!> @}
!> \name parameters for the interpolation of the child grids
!> @{
    integer, parameter    :: Agrif_linear = 1           !< linear interpolation
    integer, parameter    :: Agrif_lagrange = 2         !< lagrange interpolation
    integer, parameter    :: Agrif_eno = 3              !< spline interpolation
    integer, parameter    :: Agrif_user_interp = 4      !< user defined interpolation
    integer, parameter    :: Agrif_constant = 5         !< constant interpolation
    integer, parameter    :: Agrif_linearconserv = 6    !< linear conservative interpolation
    integer, parameter    :: Agrif_linearconservlim = 7 !< linear conservative interpolation
    integer, parameter    :: Agrif_ppm = 8              !< PPM interpolation
    integer, parameter    :: Agrif_weno = 9             !< WENO5 interpolation
    integer, parameter    :: Agrif_ppm_lim = 10         !< PPM interpolation with monotonicity
!> @}
!> \name parameters for the update of the parent grids
!> @{
    integer, parameter    :: Agrif_Update_Copy = 1              !< copy
    integer, parameter    :: Agrif_Update_Average = 2           !< average
    integer, parameter    :: Agrif_Update_Full_Weighting = 3    !< full-weighting
!> @}
!> \name Raffinement grid switches
!> @{
    integer               :: Agrif_USE_ONLY_FIXED_GRIDS   !< = 1 if fixed grid mode
    integer               :: Agrif_USE_FIXED_GRIDS        !< = 1 if AMR mode + fixed grid else only AMR mode
!> @}
    integer               :: Agrif_Maxlevelloc
!
#if defined AGRIF_MPI
    integer :: Agrif_Nbprocs  !< Number of processors
    integer :: Agrif_ProcRank !< Rank of the current processor
    integer :: Agrif_Group    !< Group associated to Agrif_mpi_comm
    integer :: Agrif_mpi_comm
#else
    integer :: Agrif_ProcRank = 0
#endif
!
    integer :: Agrif_Extra_Boundary_Cells = 3       !< When computing integration sequences, the grid rects
                                                    !! are expanded to this number of cells.
    logical :: Agrif_Parallel_sisters = .FALSE.     !< When TRUE, try to compute sister grids (which have the same parent)
                                                    !! in parallel rather than sequentially.
    logical :: agrif_regrid_has_been_done = .FALSE. !< switch to skip Agrif_Regrid call
!
    real, dimension(:)          , allocatable :: parray1
    real, dimension(:,:)        , allocatable :: parray2
    real, dimension(:,:,:)      , allocatable :: parray3
    real, dimension(:,:,:,:)    , allocatable :: parray4
    real, dimension(:,:,:,:,:)  , allocatable :: parray5
    real, dimension(:,:,:,:,:,:), allocatable :: parray6
!
    logical :: agrif_debug = .false.    ! may be activaded in users subroutine for debugging purposes

! If a grand mother grid is present
    logical :: agrif_coarse = .false.
    integer, dimension(3) :: coarse_spaceref = (/1,1,1/)
    integer, dimension(3) :: coarse_timeref  = (/1,1,1/)
!
contains
!
!===================================================================================================
!  function Agrif_Ceiling
!---------------------------------------------------------------------------------------------------
integer function Agrif_Ceiling ( x )
!---------------------------------------------------------------------------------------------------
    real,   intent(in) :: x
!
    integer   :: i
!
    i = FLOOR(x)
!
    if( ABS(x - i) <= 0.0001 )then
        Agrif_Ceiling = i
    else
        Agrif_Ceiling = i+1
    endif
!---------------------------------------------------------------------------------------------------
end function Agrif_Ceiling
!===================================================================================================
!
!===================================================================================================
!  function Agrif_Int
!---------------------------------------------------------------------------------------------------
    integer function Agrif_Int(x)
!---------------------------------------------------------------------------------------------------
    real,   intent(in) :: x
!
    integer :: i
!
    i = FLOOR(x) + 1
!
    if( ABS(x - i) <= 0.0001 )then
        Agrif_Int = i
    else
        Agrif_Int = i-1
    endif
!---------------------------------------------------------------------------------------------------
end function Agrif_Int
!===================================================================================================
!
end module Agrif_Types
