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
  use mg_diagnostics
  use mg_netcdf_out

  implicit none

! let's try to not allocate/deallocate at every call
! => allocate once in the nhmg_init

    real(kind=rp), dimension(:,:),allocatable :: ubar
    real(kind=rp), dimension(:,:),allocatable :: vbar
    real(kind=rp), dimension(:,:),allocatable :: wcorr

    integer(kind=ip) :: tscount = 1

contains

  !--------------------------------------------------------------
  subroutine nhmg_init(nx,ny,nz,npxg,npyg)
      
    integer(kind=ip), intent(in) :: nx, ny, nz
    integer(kind=ip), intent(in) :: npxg, npyg

    call mg_mpi_init()

    if (myrank==0) write(*,*)' nhmg_init:'

    call read_nhnamelist(vbrank=myrank)

    call define_grids(npxg,npyg,nx,ny,nz)

    call define_neighbours()

    call print_grids()

    allocate(ubar(1:ny,1:nx+1))
    allocate(vbar(1:ny+1,1:nx))
    allocate(wcorr(1:ny,1:nx))
    
  end subroutine nhmg_init

  !--------------------------------------------------------------
  subroutine nhmg_matrices(nx,ny,nz,hl,pdx,pdy,zxa,zya,Hza,dxa,dya)

    integer(kind=ip), intent(in) :: nx,ny,nz
    integer(kind=ip), intent(in) :: hl,pdx,pdy

    real(kind=rp), dimension(1-hl:nx+hl+pdx,1-hl:ny+hl+pdy,1:nz),intent(in) :: zxa,zya
    real(kind=rp), dimension(1-hl:nx+hl+pdx,1-hl:ny+hl+pdy,1:nz),intent(in) :: Hza
!    real(kind=rp), dimension(-1:nx+2,-1:ny+2,1:nz),intent(in) :: zr
!    real(kind=rp), dimension(-1:nx+2,-1:ny+2,1:nz),intent(in) :: Hz
    real(kind=rp), dimension(0:nx+1,0:ny+1)     , optional, intent(in) :: dxa
    real(kind=rp), dimension(0:nx+1,0:ny+1)     , optional, intent(in) :: dya

!    real(kind=rp), dimension(:,:,:), pointer :: zr,Hz
    real(kind=rp), dimension(:,:)  , pointer :: dx,dy

!!! dirty reshape arrays indexing ijk -> kji !!!
    integer(kind=ip) :: i,j,k
!    real(kind=rp), dimension(1:nz,0:ny+1,0:nx+1), target :: zrb
!    real(kind=rp), dimension(1:nz,0:ny+1,0:nx+1), target :: Hzb 
    real(kind=rp), dimension(0:ny+1,0:nx+1),      target :: dxb,dyb
!!!

    integer(kind=ip), save :: iter_matrices=0
    iter_matrices = iter_matrices + 1

!    if (myrank==0) write(*,*)' nhmg_matrices: ',iter_matrices

    call tic(1,'nhmg_matrices')

!      write(*,*)"inside nhmg : shape(zxa)=",size(zxa,dim=1),size(zxa,dim=2),size(zxa,dim=3)


    !--------------------!
    !- Horizontal grids -!
    !--------------------!
    if (present(dxa) .and. present(dya)) then

       dxb = transpose(dxa)
       dyb = transpose(dya)
       dx => dxb
       dy => dyb

       call set_horiz_grids(dx,dy)

!!$       if (check_output) then
!!$          call write_netcdf(grid(1)%dx,vname='dx',netcdf_file_name='mat.nc',rank=myrank,iter=iter_matrices)
!!$          call write_netcdf(grid(1)%dy,vname='dy',netcdf_file_name='mat.nc',rank=myrank,iter=iter_matrices)
!!$       endif

       if (associated(dx)) dx => null()
       if (associated(dy)) dy => null()

    end if

    !------------------!
    !- Vertical grids -!
    !------------------!


!!!  reshape arrays indexing ijk -> kji !!!
    do i = -1,nx+2
       do j = -1,ny+2
          do k = 1,nz
!             grid(1)%zr(k,j,i) = zr(i,j,k)
             grid(1)%dz(k,j,i) = Hza(i,j,k)
          enddo
       enddo
    enddo

       do i = 0,nx+1
          do j = 0,ny+1
             do k = 1, nz
                grid(1)%zxdy(k,j,i) = zxa(i,j,k) * grid(1)%dy(j,i)
                grid(1)%zydx(k,j,i) = zya(i,j,k) * grid(1)%dx(j,i)
                grid(1)%zx(k,j,i) = zxa(i,j,k)
                grid(1)%zy(k,j,i) = zya(i,j,k)
             enddo
          enddo
       enddo

    call fill_outer_halos(grid(1)%dz,nx,ny,nz)

    call set_vert_grids()

    call set_matrices()

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
  subroutine nhmg_solve(ua,va,wa,zwa,Hza,fill_hz)

    real(kind=rp), dimension(:,:,:), intent(in) :: ua
    real(kind=rp), dimension(:,:,:), intent(in) :: va
    real(kind=rp), dimension(:,:,:), intent(inout) :: wa    
    real(kind=rp), dimension(:,:,:), target, intent(in) :: zwa
    real(kind=rp), dimension(:,:,:), intent(in) :: Hza
    logical :: fill_hz

    real(kind=rp), dimension(:,:),   pointer :: dx,dy
    real(kind=rp), dimension(:,:,:), pointer :: u,v,w,dz,zw

    integer(kind=ip) :: i,j,k,is,js,ishift
    integer(kind=ip) :: nx,ny,nz

    integer(kind=ip), save :: iter_solve=0
    iter_solve = iter_solve + 1

    !    if (myrank==0) write(*,*)' nhmg_solve:',iter_solve

    call tic(1,'nhmg_solve')

    !    write(*,*) 'rank',myrank,'lbound(ua)',lbound(ua)
    !    write(*,*) 'rank',myrank,'ubound(ua)',ubound(ua)
    !    write(*,*) 'rank',myrank,'shape(ua)',shape(ua)

    nx = grid(1)%nx
    ny = grid(1)%ny
    nz = grid(1)%nz

    dx => grid(1)%dx
    dy => grid(1)%dy

    ishift=2

    ! need to update dz because define_matrices may not be called every time step
    dz => grid(1)%dz
    do k=1,nz
       do j=0,ny+1
          js=j+ishift
          do i=0,nx+1
             is=i+ishift
             dz(k,j,i) = Hza(is,js,k)
          enddo
       enddo
    enddo
    if (fill_hz) then
       call fill_halo(1,dz)
    endif

    ! set fluxes
    u  => grid(1)%u
    v  => grid(1)%v
    w  => grid(1)%w

    zw => zwa
    ubar(:,:) = 0.
    vbar(:,:) = 0.

    do k=1,nz
       do j=1,ny
          js=j+ishift
          do i=1,nx+1
             is=i+ishift
             u(k,j,i) = ua(is,js,k) * &
                  qrt * (dz(k,j,i) + dz(k,j,i-1)) * (dy(j,i)+dy(j,i-1))
             ubar(j,i) = ubar(j,i) + u(k,j,i)
          enddo
       enddo
       do j=1,ny+1
          js=j+ishift
          do i=1,nx
             is=i+ishift
             v(k,j,i) = va(is,js,k) * &
                  qrt * (dz(k,j,i) + dz(k,j-1,i)) * (dx(j,i)+dx(j-1,i))
             vbar(j,i) = vbar(j,i) + v(k,j,i)
          enddo
       enddo
    enddo
    if (surface_neumann)  then
       do j=1,ny
          js=j+ishift
          do i=1,nx
             is=i+ishift
             wcorr(j,i) = wa(is,js,nz+1) + ( ubar(j,i+1) - ubar(j,i) + vbar(j+1,i) - vbar(j,i) ) &
                            / (dx(j,i) * dy(j,i)) 
          enddo
       enddo
       do k=1,nz+1
          do j=1,ny
             js=j+ishift
             do i=1,nx
                is=i+ishift
                wa(is,js,k) = wa(is,js,k) - wcorr(j,i) &
                            * (zw(is,js,k   )-zw(is,js,1)) &
                            / (zw(is,js,nz+1)-zw(is,js,1))
             enddo
          enddo
       enddo
    endif 

    do k=1,nz
       do j=1,ny
          js=j+ishift
          do i=1,nx
             is=i+ishift
             w(k+1,j,i) = wa(is,js,k+1) * dx(j,i) * dy(j,i)
          enddo
       enddo
    enddo
    w(1,:,:) = zero

    !- auto tuning tests if autotune = .true.
    if ((tscount == autotune_ts).and.(autotune)) then
       call sb_autotune()  !- test of autotuning
    end if

    !- set rhs, solve for p, and compute correction for u,v,w
    call set_rhs()
    call solve_p()   

    tscount = tscount + 1

    call correction_uvw()

    if (check_output) then
       !if ((iter_solve .EQ. 1) .OR. (iter_solve .EQ. 2)) then
       call write_netcdf(grid(1)%b,vname='b',netcdf_file_name='so.nc',rank=myrank,iter=iter_solve)
       call write_netcdf(grid(1)%p,vname='p',netcdf_file_name='so.nc',rank=myrank,iter=iter_solve)
       call write_netcdf(grid(1)%r,vname='r',netcdf_file_name='so.nc',rank=myrank,iter=iter_solve)
       call write_netcdf(grid(1)%du,vname='du',netcdf_file_name='so.nc',rank=myrank,iter=iter_solve)
       call write_netcdf(grid(1)%dv,vname='dv',netcdf_file_name='so.nc',rank=myrank,iter=iter_solve)
       call write_netcdf(grid(1)%dw,vname='dw',netcdf_file_name='so.nc',rank=myrank,iter=iter_solve)
       !endif
    endif

    call toc(1,'nhmg_solve')

  end subroutine nhmg_solve

  !--------------------------------------------------------------
  subroutine nhmg_checkdivergence(ua,va,wa,Hza)

    real(kind=rp), dimension(:,:,:), intent(in) :: ua
    real(kind=rp), dimension(:,:,:), intent(in) :: va
    real(kind=rp), dimension(:,:,:), intent(in) :: wa
    real(kind=rp), dimension(:,:,:), intent(in) :: Hza

    real(kind=rp), dimension(:,:),   pointer :: dx,dy
    real(kind=rp), dimension(:,:,:), pointer :: u,v,w,dz

    integer(kind=ip) :: i,j,k,is,js,ishift
    integer(kind=ip) :: nx,ny,nz

    integer(kind=ip), save :: iter_checkdiv=0
    iter_checkdiv = iter_checkdiv + 1


    nx = grid(1)%nx
    ny = grid(1)%ny
    nz = grid(1)%nz

    dx => grid(1)%dx
    dy => grid(1)%dy

    ishift=2

    ! need to update dz because define_matrices may not be called every time step
    dz => grid(1)%dz
    do k=1,nz
       do j=0,ny+1
          js=j+ishift
          do i=0,nx+1
             is=i+ishift
             dz(k,j,i) = Hza(is,js,k)
          enddo
       enddo
    enddo

    ! set fluxes
    u  => grid(1)%u
    v  => grid(1)%v
    w  => grid(1)%w

    do k=1,nz
       do j=1,ny
          js=j+ishift
          do i=1,nx+1
             is=i+ishift
             u(k,j,i) = ua(is,js,k) * &
                  qrt * (dz(k,j,i) + dz(k,j,i-1)) * (dy(j,i)+dy(j,i-1))
          enddo
       enddo
       do j=1,ny+1
          js=j+ishift
          do i=1,nx
             is=i+ishift
             v(k,j,i) = va(is,js,k) * &
                  qrt * (dz(k,j,i) + dz(k,j-1,i)) * (dx(j,i)+dx(j-1,i))
          enddo
       enddo
       do j=1,ny
          js=j+ishift
          do i=1,nx
             is=i+ishift
             w(k+1,j,i) = wa(is,js,k+1) * dx(j,i) * dy(j,i)
          enddo
       enddo
    enddo
    w(1,:,:) = zero

    call set_rhs()

    write(*,*) 'check div',maxval(abs(grid(1)%b))

    call write_netcdf(grid(1)%b,vname='rhs',netcdf_file_name='chk.nc',rank=myrank,iter=iter_checkdiv)

  end subroutine nhmg_checkdivergence

  !--------------------------------------------------------------
  subroutine nhmg_diagnostics(ua,va,wa,Hza)

    real(kind=rp), dimension(:,:,:), intent(in) :: ua
    real(kind=rp), dimension(:,:,:), intent(in) :: va
    real(kind=rp), dimension(:,:,:), intent(in) :: wa    
    real(kind=rp), dimension(:,:,:), intent(in) :: Hza

    real(kind=rp), dimension(:,:),   pointer :: dx,dy
    real(kind=rp), dimension(:,:,:), pointer :: u,v,w,dz

    integer(kind=ip) :: i,j,k,is,js,ishift
    integer(kind=ip) :: nx,ny,nz

    integer(kind=ip), save :: iter_diag=0
    iter_diag = iter_diag + 1

    nx = grid(1)%nx
    ny = grid(1)%ny
    nz = grid(1)%nz

    dx => grid(1)%dx
    dy => grid(1)%dy

    ishift=2

    ! need to update dz because define_matrices may not be called every time step
    dz => grid(1)%dz
    do k=1,nz
       do j=0,ny+1
          js=j+ishift
          do i=0,nx+1
             is=i+ishift
             dz(k,j,i) = Hza(is,js,k)
          enddo
       enddo
    enddo

    ! set fluxes
    u  => grid(1)%u
    v  => grid(1)%v
    w  => grid(1)%w
    do k=1,nz
       do j=1,ny
          js=j+ishift
          do i=1,nx+1
             is=i+ishift
             u(k,j,i) = ua(is,js,k) * &
                  qrt * (dz(k,j,i) + dz(k,j,i-1)) * (dy(j,i)+dy(j,i-1))
          enddo
       enddo
       do j=1,ny+1
          js=j+ishift
          do i=1,nx
             is=i+ishift
             v(k,j,i) = va(is,js,k) * &
                  qrt * (dz(k,j,i) + dz(k,j-1,i)) * (dx(j,i)+dx(j-1,i))
          enddo
       enddo
       do j=1,ny
          js=j+ishift
          do i=1,nx
             is=i+ishift
             w(k+1,j,i) = wa(is,js,k+1) * &
                  dx(j,i) * dy(j,i)
          enddo
       enddo
    enddo
    w(1,:,:) = zero

    ! diagnose momentum and kinetic energy
    call diag_momentum()
    call diag_kin_energy()

    !if ((iter_diag .EQ. 1) .OR. (iter_diag .EQ. 2)) then
    call write_netcdf(grid(1)%um,vname='um',netcdf_file_name='diag.nc',rank=myrank,iter=iter_diag)
    call write_netcdf(grid(1)%vm,vname='vm',netcdf_file_name='diag.nc',rank=myrank,iter=iter_diag)
    call write_netcdf(grid(1)%wm,vname='wm',netcdf_file_name='diag.nc',rank=myrank,iter=iter_diag)
    call write_netcdf(grid(1)%ke,vname='ke',netcdf_file_name='diag.nc',rank=myrank,iter=iter_diag)
    !endif

  end subroutine nhmg_diagnostics

  !--------------------------------------------------------------
  subroutine nhmg_clean()

    real(kind=rp) :: tstart,tend,perf

    call grids_dealloc()

    call print_tictoc()

  end subroutine nhmg_clean

end module nhmg
