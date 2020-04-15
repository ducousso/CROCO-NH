module mg_namelist

  use mg_cst
  use mg_tictoc

  implicit none

  integer(kind=ip) :: nsmall      =   2        ! smallest dimension for the global domain
  integer(kind=ip) :: ngather     =   16        ! smallest dimension ever for a subdomain; triggers a gather

  integer(kind=ip) :: ns_coarsest =  40        ! Number of relax sweeps for the coarsest grid level
  integer(kind=ip) :: ns_pre      =   3        ! Number of relax sweeps before coarsening  (going down)
  integer(kind=ip) :: ns_post     =   2        ! Number of relax sweeps after interpolation(going up)

  real(kind=rp)    :: solver_prec    = 1.d-6   !- solver precision 
  integer(kind=ip) :: solver_maxiter = 10      !- maximum of solver iterations

  logical          :: autotune    =.false.     !- tuning test after a number of time steps (by default 100)
  integer(kind=ip) :: autotune_ts = 100        !- tuning test time step (if autotune=.true.)

  character(len=16) :: relax_method ='RB'      !- 'Gauss-Seidel', 'GS', 
  !                                            !- 'Red-Black'   , 'RB',
  !                                            !- 'Four-Color'  , 'FC'

  character(len=1) :: solver_cycle = 'F'       !- 'F' or 'f' or a letter not referenced -> Fcycle Multi-Grid
  !                                            !- 'V' or 'v'-> Vcycle Multi-Grid

  ! nskip and order are linked, order is taken into account when nskip is > 1
  integer(kind=ip)  :: nskip  = 1              !- Solve for pressure every nskip iterations, nskip=1 by default
  integer(kind=ip)  :: order  = 0              !- for pressure interpolation in time: default 0=nearest
  !                                            !- choices => 0=nearest, 1=linear, 2=quadratic, 3=cubic


  logical           :: netcdf_output = .false. !- .false. or .true.

  integer(kind=ip)  :: output_freq = 100000000 ! Number of iterations between output of statistics

  logical           :: surface_neumann  = .true.

  logical           :: east_west_perio = .false.
  logical           :: north_south_perio = .false.

  namelist/nhparam/    &
       solver_prec   , &
       solver_maxiter, &
       solver_cycle  , &
       nskip         , &
       order         , &
       nsmall        , &
       ngather       , &
       ns_coarsest   , &
       ns_pre        , &
       ns_post       , &
       autotune      , &
       autotune_ts   , &
       relax_method  , &
       netcdf_output , &
       output_freq   , &
       surface_neumann, &
       east_west_perio, &
       north_south_perio

  integer(kind=ip) :: tstart  = 1
  ! if nskip > 1: a counter to store in 4D array the pressure correction grid(1)%phis
  integer(kind=ip) :: corder  = 1 
  ! if nskip > 1: a vector with indice order to use of pressure correction extrapolation
  integer(kind=4), dimension(:), allocatable :: extrap_pt 
  ! if nskip > 1: coefs for pressure extrapolation 
  real(kind=rp), dimension(:,:), allocatable :: extrap_coef 

contains

  !--------------------------------------------------------------------
  subroutine read_nhnamelist(filename, verbose, vbrank)

    character(len=*), optional, intent(in) :: filename
    logical         , optional, intent(in) :: verbose
    integer(kind=ip) , optional, intent(in) :: vbrank

    character(len=64) :: fn_nml
    logical           :: vb
    integer(kind=ip)  :: lun_nml = 4
    integer(kind=ip)   :: rank

    integer(kind=ip) :: ii, jj, kk, mm

    real(kind=rp) :: cc
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

       if (trim(solver_cycle) == 'f') solver_cycle='F'
       if (trim(solver_cycle) == 'v') solver_cycle='V'

       if ((nskip > 1).and.((order > 3).or.(order <0))) then
          write(*,*)"  ERROR in namelist file: nskip > 1 and order > 3 or < 0 :-(", nskip, order
          stop
       endif

       if (nskip > 1) then
          ! TSTART is a flag to force explicitly pressure correction calculation 
          ! at run begining until we have sufficient pressure stored to apply 
          ! the first extrapolation.
          ! It depends of extrapolation order and skip.
          ! Its computation is different if nskip >= order or if nskip < order.
          if (nskip >= order) then
             tstart = (order * nskip) + 1
          else
             tstart = ((order+1) * nskip) - 1
          endif

          ! Allocate the vector where the order of pressure correction is stored
          ! example:
          ! order = 3
          ! allocate(extrap_pt(4))
          ! extrap_pt(1:4)=[1,2,3,4]
          ! store pressure 1, 2, 3, 4 => extrap_pt(1:4)=[1,2,3,4]
          ! and 1 => extrap_pt(1:4)=[2,3,4,1]
          if (.not.allocated(extrap_pt)) then
             allocate(extrap_pt(order+1))
             extrap_pt(:) = [(ii, ii=order+1,1,-1)]
          endif
       endif

       if (.not.allocated(extrap_coef)) then
          allocate(extrap_coef(order+1,nskip))
       endif
       extrap_coef(:,:) = 1._rp

       do kk=0,nskip-1
          do jj=0,order
             cc=1._rp
             do mm=0,order
                if (jj /= mm) then
                   cc = cc                                     * &
                        real(kk + 1 + mm * nskip     ,kind=rp) / &
                        real(-jj * nskip + mm * nskip,kind=rp)
                endif
             enddo
             extrap_coef(jj+1,kk+1) = cc
          enddo
       enddo

    endif

    !- Print parameters or not !
    if (present(verbose)) then
       vb = verbose
    else
       vb = .true.
    endif

    if (vb) then

       if (present(vbrank)) then
          rank = vbrank
       else
          rank = 0
       endif

       if (rank == 0) then
          write(*,*)'  Multigrid parameters:'
          write(*,*)'  - solver_prec   : ', solver_prec
          write(*,*)'  - solver_maxiter: ', solver_maxiter
          write(*,*)'  - solver_cycle  : ', trim(solver_cycle)
          write(*,*)'  - nskip         : ', nskip
          write(*,*)'  - order         : ', order
          write(*,*)'  - nsmall        : ', nsmall
          write(*,*)'  - ngather       : ', ngather
          write(*,*)'  - ns_coarsest   : ', ns_coarsest
          write(*,*)'  - ns_pre        : ', ns_pre
          write(*,*)'  - ns_post       : ', ns_post
          write(*,*)'  - autotune      : ', autotune
          write(*,*)'  - autotune_ts   : ', autotune_ts
          write(*,*)'  - relax_method  : ', trim(relax_method)
          write(*,*)'  - netcdf_output : ', netcdf_output
          write(*,*)'  - output freq   : ', output_freq
          write(*,*)'  - surf neumann  : ', surface_neumann
! these parameters are overwritten in nhmg_init()
!          write(*,*)'  - E/W periodic  : ', east_west_perio
!          write(*,*)'  - N/S periodic  : ', north_south_perio
          write(*,*)'  '

          if (nskip > 1) then
             write(*,*)'  Pressure extrapolation is activated with coefficients(order,nskip):'
             do jj=0,order
                write(*,*) extrap_coef(jj+1,:)
             enddo
             write(*,*)'  '
          endif
       endif
    endif

  end subroutine read_nhnamelist

end module mg_namelist
