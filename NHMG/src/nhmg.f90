module nhmg

  use mg_cst
  use mg_mpi
  use mg_grids
  use mg_namelist
  use mg_tictoc
  use mg_mpi_exchange
  use mg_autotune
  use mg_horiz_grids
  use mg_vert_grids
  use mg_projection
  use mg_solvers
  use mg_netcdf_out

  implicit none

  ! Variables declared bellow have implicit SAVE attribut.
  ! Values are saved when routine is finished and calls again

  ! store the number of nhmg_solve calls
  integer(kind=ip) :: tscount = 1 
  ! a counter used to decided if the solver is called or pressure extrapolation
  integer(kind=ip) :: nite    = 1   
  
  integer :: iprec1, iprec2
  integer :: halo, hl, pdx, pdy
  integer :: nx, ny, nz
  logical :: ew_perio, ns_perio

contains

  !--------------------------------------------------------------
  subroutine nhmg_init(Lm,Mm,N,NP_XI,NP_ETA,padd_X,padd_E)

    integer(kind=ip), intent(in) :: Lm,Mm,N,NP_XI,NP_ETA,padd_X,padd_E
    logical :: nx_ok, ny_ok, nz_ok
    character*60 :: msg

    call tic(1,'nhmg_init')

    nx = Lm
    ny = Mm
    nz = N
    pdx = padd_X
    pdy = padd_E
    hl = halo

    call mg_mpi_init()

    if (myrank==0) write(*,*)' nhmg_init:'

    ! check grid dimensions are in the form 2**p or 3*2**p
    nx_ok = check_value(nx, 3)
    ny_ok = check_value(ny, 3)
    nz_ok = check_value(nz, 3)
    if ((nx_ok).and.(ny_ok).and.(nz_ok)) then
       ! values are correct, nothing to say
    else
       msg = "Lm, Mm and N should be in the form 2**p or 3*2**p"
       if (myrank==0) write(*,*) msg
       call mg_mpi_abort()
    endif    

    ! check subdomains are in the form 2**p
    nx_ok = check_value(NP_XI, 1)
    ny_ok = check_value(NP_ETA, 1)
    if ((nx_ok).and.(ny_ok)) then
       ! values are correct, nothing to say
    else
       msg = "NP_XI and Np_ETA should be in the form 2**p"
       if (myrank==0) write(*,*) msg
       call mg_mpi_abort()
    endif    

    call read_nhnamelist(vbrank=myrank)

    ! Copy from nhmg module to mg_namelist module
    ! TODO: simplify the namelist, the user doesn't need
    ! to manually set east_west_perio and north_south_perio
    ! in the namelist file.
    ! This is done automatically in OCEAN/main.F
    !
    east_west_perio = ew_perio
    north_south_perio = ns_perio
    
    call define_grids(NP_XI,NP_ETA,nx,ny,nz)

    call define_neighbours()

    call print_grids()

    call toc(1,'nhmg_init')

  end subroutine nhmg_init

  !--------------------------------------------------------------
  logical function check_value(n0, odd)
    ! check whether n0 is in the form
    ! odd.eq.1 : 2**p
    ! odd.eq.3 : 2**p or 3*2**p
    integer :: n, n0, odd
    logical :: ok
    ok = .true.
    n = n0
    do while ((n.gt.odd).and.(ok))
       if (mod(n, 2).eq.0) then
          n = n/2
       else
          ok = .false.         
       endif
    enddo       
    check_value = ok
    return
  end function check_value

  !--------------------------------------------------------------
  subroutine nhmg_matrices(zxa,zya,Hza,z_r,dxa,dya)

!    integer(kind=ip), intent(in) :: nx,ny,nz
!    integer(kind=ip), intent(in) :: hl,pdx,pdy

    real(kind=rp), dimension(1-hl:nx+hl+pdx,1-hl:ny+hl+pdy,1:nz),intent(in) :: zxa,zya
    real(kind=rp), dimension(1-hl:nx+hl+pdx,1-hl:ny+hl+pdy,1:nz),intent(in) :: Hza,z_r
    real(kind=rp), dimension(0:nx+1,0:ny+1)     , optional, intent(in) :: dxa
    real(kind=rp), dimension(0:nx+1,0:ny+1)     , optional, intent(in) :: dya

    integer(kind=ip) :: i,j,k

    call tic(1,'nhmg_matrices')

    if (present(dxa) .and. present(dya)) then

       do i = 0,nx+1
          do j = 0,ny+1
             grid(1)%dx(j,i) = dxa(i,j)
             grid(1)%dy(j,i) = dya(i,j)
          enddo
       enddo

       call set_horiz_grids()

    end if

    do i = 0,nx+1
       do j = 0,ny+1
          do k = 1,nz
             grid(1)%dz(k,j,i) = Hza(i,j,k)
             grid(1)%zr(k,j,i) = z_r(i,j,k)
          enddo
       enddo
    enddo

    do i = 0,nx+1
       do j = 0,ny+1
          do k = 1, nz
             grid(1)%zxdy(k,j,i) = zxa(i,j,k) * grid(1)%dy(j,i)
             grid(1)%zydx(k,j,i) = zya(i,j,k) * grid(1)%dx(j,i)
          enddo
       enddo
    enddo

    call fill_outer_halos(grid(1)%dz,nx,ny,nz)

    if ((nite >= nskip).or.(tscount <= tstart))  then

       call set_vert_grids()

       call set_matrices()

    endif

    call toc(1,'nhmg_matrices')

  end subroutine nhmg_matrices

  !--------------------------------------------------------------
  subroutine fill_outer_halos(z,nx,ny,nz)
    real(kind=rp), dimension(1:nz,-1:ny+2,-1:nx+2), intent(inout) :: z

    integer :: i,j,k,nx,ny,nz

    if(grid(1)%neighb(1).eq.MPI_PROC_NULL) then
       ! south
       do i=-1,nx+2 ! <=extended range to fill the corners (not a bug)
          do k=1,nz
             z(k,-1,i)=z(k,1,i)
             z(k, 0,i)=z(k,1,i)
          enddo
       enddo
    endif
    if(grid(1)%neighb(3).eq.MPI_PROC_NULL) then
       ! north
       do i=-1,nx+2
          do k=1,nz
             z(k,ny+1,i)=z(k,ny,i)
             z(k,ny+2,i)=z(k,ny,i)
          enddo
       enddo
    endif
    if(grid(1)%neighb(2).eq.MPI_PROC_NULL) then
       ! east
       do j=-1,ny+2
          do k=1,nz
             z(k,j,nx+1)=z(k,j,nx)
             z(k,j,nx+2)=z(k,j,nx)
          enddo
       enddo
    endif
    if(grid(1)%neighb(4).eq.MPI_PROC_NULL) then
       ! west
       do j=-1,ny+2
          do k=1,nz
             z(k,j,-1)=z(k,j,1)
             z(k,j, 0)=z(k,j,1)
          enddo
       enddo
    endif

  end subroutine fill_outer_halos
  !--------------------------------------------------------------
  subroutine nhmg_solve(ua,va,wa)

!    integer(kind=ip), intent(in) :: nx,ny,nz
!    integer(kind=ip), intent(in) :: hl,pdx,pdy

    real(kind=rp), dimension(1-hl:nx+hl+pdx,1-hl:ny+hl+pdy,1:nz  ),intent(in)   :: ua
    real(kind=rp), dimension(1-hl:nx+hl+pdx,1-hl:ny+hl+pdy,1:nz  ),intent(in)   :: va
    real(kind=rp), dimension(1-hl:nx+hl+pdx,1-hl:ny+hl+pdy,1:nz+1),intent(inout):: wa

    integer(kind=ip) :: i,j,k, oo

    call tic(1,'nhmg_solve')

    ! tstart, corder, ptextrap are defined in mg_namelist.f90

    ! First test, nite >= nskip, is made to avoid an explicit calculation
    ! of the pressure correction.
    ! Second test, tscount <= tstart, is made to force the compuation
    ! of the pressure correction at the begining of the run.
    if ((nite >= nskip).or.(tscount <= tstart))  then

       !  Fill the rhs for the poisson equation
       do i = 1,nx
          do j = 1,ny 
             do k = 1,nz
                grid(1)%b(k,j,i) = ua(i+1,j,k) - ua(i,j,k) &
                     + va(i,j+1,k) - va(i,j,k) &
                     + wa(i,j,k+1) - wa(i,j,k)
             enddo
          enddo
       enddo

       !- auto tuning tests if autotune = .true.
       if ((tscount == autotune_ts).and.(autotune)) then
          call sb_autotune()  !- test of autotuning
       end if

       !- SOLVER -!
       call solve_p()
       !- SOLVER -!

       ! save pressure correction for extrapolation
       if ((nskip > 1).and.(mod(tscount-1,nskip)==0)) then
          grid(1)%phis(:,:,:,corder) = grid(1)%p(:,:,:)
          corder = corder + 1
          if (corder > (order + 1)) corder = 1
          if (tscount >  tstart) extrap_pt(:) = cshift(extrap_pt(:),-1)

          ! TODO: an extrapolation error could be estimated by
          ! comparison between the explicit calculation of the
          ! pressure correction and its extrapolation.
          ! error = grid(1)%p(:,:,:) - extrap_pressure(:,:,:)

       endif

       nite = 1

    else 

       grid(1)%p(:,:,:) = 0._8 !TODO: verify that was not already made

       ! extrapolation !
       do oo=1,order+1
          grid(1)%p(:,:,:) = grid(1)%p(:,:,:) + &
               extrap_coef(oo,nite) * grid(1)%phis(:,:,:,extrap_pt(oo)) 
       enddo

       nite = nite + 1

    endif

    tscount = tscount + 1

    !- APPLY PRESSURE CORRECTION -!
    call correction_uvw()
    !- APPLY PRESSURE CORRECTION -!

    if (netcdf_output) then
       call write_netcdf(grid(1)%b,vname='b',netcdf_file_name='so.nc',rank=myrank,iter=1)
       call write_netcdf(grid(1)%p,vname='p',netcdf_file_name='so.nc',rank=myrank,iter=1)
       call write_netcdf(grid(1)%r,vname='r',netcdf_file_name='so.nc',rank=myrank,iter=1)
       call write_netcdf(grid(1)%du,vname='du',netcdf_file_name='so.nc',rank=myrank,iter=1)
       call write_netcdf(grid(1)%dv,vname='dv',netcdf_file_name='so.nc',rank=myrank,iter=1)
       call write_netcdf(grid(1)%dw,vname='dw',netcdf_file_name='so.nc',rank=myrank,iter=1)
    endif

    call toc(1,'nhmg_solve')

  end subroutine nhmg_solve

  !---------------------------------------------------------------------
  ! Pnh = Non-Hydrostatic Pressure (in Pascal)
  !
  subroutine get_pnh(pnh,dt,rho0)

!    integer(kind=ip), intent(in) :: nx,ny,nz
!    integer(kind=ip), intent(in) :: hl,pdx,pdy

    real(kind=rp), dimension(1-hl:nx+hl+pdx,1-hl:ny+hl+pdy,1:nz), intent(out) :: pnh
    real(kind=rp)                                               , intent(in)  :: dt
    real(kind=rp)                                               , intent(in)  :: rho0

    integer(kind=ip) :: i, j, k
    real(kind=rp):: cff

    ! p, the solution of the Poisson equation, is the NH-pressure-time-increment
    ! p/dt is the NH pressure (up to rho0 factor)
    ! rho0*p/dt is the real NH pressure (in Pascal)
    cff = rho0/dt
    do i = 1,nx
       do j = 1,ny 
          do k = 1,nz
             pnh(i,j,k) = grid(1)%p(k,j,i)*cff
          enddo
       enddo
    enddo

  end subroutine get_pnh

!!$  !--------------------------------------------------------------
!!$  subroutine nhmg_checkdivergence(ua,va,wa,Hza)
!!$!! Move this to the Croco side
!!$!! It's here to provide a template
!!$
!!$    real(kind=rp), dimension(:,:,:), intent(in) :: ua
!!$    real(kind=rp), dimension(:,:,:), intent(in) :: va
!!$    real(kind=rp), dimension(:,:,:), intent(in) :: wa
!!$    real(kind=rp), dimension(:,:,:), intent(in) :: Hza
!!$
!!$    real(kind=rp), dimension(:,:),   pointer :: dx,dy
!!$    real(kind=rp), dimension(:,:,:), pointer :: u,v,w,dz
!!$
!!$    integer(kind=ip) :: i,j,k,is,js,ishift
!!$    integer(kind=ip) :: nx,ny,nz
!!$
!!$    integer(kind=ip), save :: iter_checkdiv=0
!!$    iter_checkdiv = iter_checkdiv + 1
!!$
!!$
!!$    nx = grid(1)%nx
!!$    ny = grid(1)%ny
!!$    nz = grid(1)%nz
!!$
!!$    dx => grid(1)%dx
!!$    dy => grid(1)%dy
!!$
!!$    ishift=2
!!$
!!$    ! need to update dz because define_matrices may not be called every time step
!!$    dz => grid(1)%dz
!!$    do k=1,nz
!!$       do j=0,ny+1
!!$          js=j+ishift
!!$          do i=0,nx+1
!!$             is=i+ishift
!!$             dz(k,j,i) = Hza(is,js,k)
!!$          enddo
!!$       enddo
!!$    enddo
!!$
!!$    ! set fluxes
!!$    u  => grid(1)%u
!!$    v  => grid(1)%v
!!$    w  => grid(1)%w
!!$
!!$    do k=1,nz
!!$       do j=1,ny
!!$          js=j+ishift
!!$          do i=1,nx+1
!!$             is=i+ishift
!!$             u(k,j,i) = ua(is,js,k) * &
!!$                  qrt * (dz(k,j,i) + dz(k,j,i-1)) * (dy(j,i)+dy(j,i-1))
!!$          enddo
!!$       enddo
!!$       do j=1,ny+1
!!$          js=j+ishift
!!$          do i=1,nx
!!$             is=i+ishift
!!$             v(k,j,i) = va(is,js,k) * &
!!$                  qrt * (dz(k,j,i) + dz(k,j-1,i)) * (dx(j,i)+dx(j-1,i))
!!$          enddo
!!$       enddo
!!$       do j=1,ny
!!$          js=j+ishift
!!$          do i=1,nx
!!$             is=i+ishift
!!$             w(k+1,j,i) = wa(is,js,k+1) * dx(j,i) * dy(j,i)
!!$          enddo
!!$       enddo
!!$    enddo
!!$    w(1,:,:) = zero
!!$
!!$!   call set_rhs()
!!$
!!$    write(*,*) 'check div',maxval(abs(grid(1)%b))
!!$
!!$    call write_netcdf(grid(1)%b,vname='rhs',netcdf_file_name='chk.nc',rank=myrank,iter=iter_checkdiv)
!!$
!!$  end subroutine nhmg_checkdivergence

!--------------------------------------------------------------

subroutine nhmg_clean()

  !   call grids_dealloc()

 if (myrank==0) then
    call print_tictoc(myrank)
 endif

end subroutine nhmg_clean

end module nhmg

