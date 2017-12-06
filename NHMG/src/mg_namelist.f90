module mg_namelist

  use mg_cst
  use mg_tictoc

  implicit none

  ! smallest dimension ever for the global domain
  integer(kind=ip) :: nsmall      =   8

  integer(kind=ip) :: ns_coarsest =  40
  integer(kind=ip) :: ns_pre      =   3
  integer(kind=ip) :: ns_post     =   2

  real(kind=rp)    :: solver_prec    = 1.d-6   !- solver precision 
  integer(kind=ip) :: solver_maxiter = 50      !- maximum of solver iterations

  logical          :: autotune    =.false.     !- tuning test after a number of time steps (by default 100)
  integer(kind=ip) :: autotune_ts = 100        !- tuning test time step (if autotune=.true.)

  character(len=16) :: cmatrix='real'          !- 'real' or 'simple'

  logical           :: red_black = .true.      !- .false. or .true.
  character(len=16) :: relax_method ='RB'      !- 'Gauss-Seidel', 'GS', 
  !                                            !- 'Red-Black'   , 'RB',
  !                                            !- 'Four-Color'  , 'FC'

  character(len=16) :: interp_type='linear'    !- 'nearest'  or 'linear'

  character(len=16) :: restrict_type='avg'     !- 'avg'  or 'linear'

  logical           :: aggressive = .false.    !- .false. or .true.

  logical           :: netcdf_output = .false. !- .false. or .true.

  logical           :: check_output  = .false. !- .false. or .true.

  logical           :: surface_neumann  = .true.

  logical           :: east_west_perio = .false.
  logical           :: north_south_perio = .false.

  character(len=16) :: bench =''               !- 'seamount'

  namelist/nhparam/    &
       solver_prec   , &
       solver_maxiter, &
       nsmall        , &
       ns_coarsest   , &
       ns_pre        , &
       ns_post       , &
       autotune      , &
       autotune_ts   , &
       cmatrix       , &
       relax_method  , &
       interp_type   , &
       restrict_type , &
       netcdf_output , &
       check_output ,  &
       surface_neumann, &
       east_west_perio, &
       north_south_perio, &
       aggressive   

contains

  !--------------------------------------------------------------------
  subroutine read_nhnamelist(filename, verbose, vbrank)

    character(len=*), optional, intent(in) :: filename
    logical         , optional, intent(in) :: verbose
    integer(kind=4) , optional, intent(in) :: vbrank

    character(len=64) :: fn_nml
    logical           :: vb
    integer(kind=ip)  :: lun_nml = 4
    integer(kind=4)   :: rank

    logical :: exist=.false.

    !- Namelist file name, by default 'nhmg_namelist'
    if (present(filename)) then
       fn_nml = filename
    else
       fn_nml = 'nhmg_namelist'
    endif

    !- Check if a namelist file exist
    inquire(file=fn_nml, exist=exist)

    !- Read namelist file if it is present, else use default values
    if (exist) then

       if (vbrank == 0) then
          write(*,*)"  Opening and Reading namelist file:", TRIM(fn_nml)
       endif

       open(unit=lun_nml, File=fn_nml, ACTION='READ')

       rewind(unit=lun_nml)
       read(unit=lun_nml, nml=nhparam)

    endif

    !- Print parameters or not !
    if (present(verbose)) then
       vb = verbose
    else
       vb = .true.
    endif

    if ((trim(interp_type)=='linear') .and. (trim(restrict_type)=='linear')) then
       if (rank == 0) write(*,*) "linear interp + linear restrict is not permitted"
       stop
    endif

    if (vb) then

       if (present(vbrank)) then
          rank = vbrank
       else
          rank = 0
       endif

       if (rank == 0) then
          write(*,*)'  Non hydrostatic parameters:'
          write(*,*)'  - solver_prec   : ', solver_prec
          write(*,*)'  - solver_maxiter: ', solver_maxiter
          write(*,*)'  - nsmall        : ', nsmall 
          write(*,*)'  - ns_coarsest   : ', ns_coarsest
          write(*,*)'  - ns_pre        : ', ns_pre
          write(*,*)'  - ns_post       : ', ns_post
          write(*,*)'  - autotune      : ', autotune
          write(*,*)'  - autotune_ts   : ', autotune_ts
          write(*,*)'  - cmatrix       : ', trim(cmatrix)
          write(*,*)'  - relax_method  : ', trim(relax_method)
          write(*,*)'  - interp_type   : ', trim(interp_type)
          write(*,*)'  - restrict_type : ', trim(restrict_type)
          write(*,*)'  - aggressive    : ', aggressive
          write(*,*)'  - netcdf_output : ', netcdf_output
          write(*,*)'  - check_output : ', check_output
          write(*,*)'  - surface_neumann : ', surface_neumann
          write(*,*)'  '
       endif
    endif

  end subroutine read_nhnamelist

end module mg_namelist
