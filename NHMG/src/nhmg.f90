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
  subroutine nhmg_rw(nx,ny,nz,rua,rva,rwa)

    integer(kind=ip), intent(in) :: nx, ny, nz

    real(kind=rp), dimension(-1:nx+2,-1:ny+2,1:nz  ), target, intent(in) :: rua
    real(kind=rp), dimension(-1:nx+2,-1:ny+2,1:nz  ), target, intent(in) :: rva
    real(kind=rp), dimension(-1:nx+2,-1:ny+2,1:nz+1), target, intent(out):: rwa

    real(kind=rp), dimension(:,:,:), pointer :: ru,rv,rw

!!! dirty reshape arrays indexing ijk -> kji !!!
    real(kind=rp), dimension(:,:,:), allocatable, target :: rub,rvb
!!! dirty reshape arrays indexing ijk -> kji !!!

    real(kind=rp), dimension(:,:)  , pointer :: dx,dy
    real(kind=rp), dimension(:,:,:), pointer :: dz
    real(kind=rp), dimension(:,:,:), pointer :: zxdy,zydx

    real(kind=rp),dimension(nz) :: wrk
    integer(kind=ip) :: i,j,k

    integer(kind=ip), save :: iter_rw=0
    iter_rw = iter_rw + 1

    dx    => grid(1)%dx
    dy    => grid(1)%dy
    dz    => grid(1)%dz
    zxdy  => grid(1)%zxdy
    zydx  => grid(1)%zydx

!!! dirty reshape arrays indexing ijk -> kji !!!
    allocate(rub(1:nz,0:ny+1,0:nx+1))
    allocate(rvb(1:nz,0:ny+1,0:nx+1))
    do i = 1,nx+1
      do j = 0,ny+1
        do k = 1,nz
          rub(k,j,i) = rua(i,j,k)
        enddo
      enddo
    enddo
    do i = 0,nx+1
      do j = 1,ny+1
        do k = 1,nz
           rvb(k,j,i) = rva(i,j,k)
        enddo
      enddo
    enddo
    ru => rub
    rv => rvb
    call fill_halo(1,ru)
    call fill_halo(1,rv)
    call set_rurvbc2zero(ru,'u')
    call set_rurvbc2zero(rv,'v')

    if (check_output) then      
       call write_netcdf(ru,vname='ru',netcdf_file_name='rw.nc',rank=myrank,iter=iter_rw)
       call write_netcdf(rv,vname='rv',netcdf_file_name='rw.nc',rank=myrank,iter=iter_rw)
    endif
!!! 
    rw => rwa

    do i = 1,nx
       do j = 1,ny
          !
          do k = 1,nz
             wrk(k) = zxdy(k,j,i) *2.*(               &
                    ru(k,j,i  ) / ((dz(k,j,i)+dz(k,j,i-1))*(dy(j,i)+dy(j,i-1)))     &
                  + ru(k,j,i+1) / ((dz(k,j,i)+dz(k,j,i+1))*(dy(j,i)+dy(j,i+1))) )   &
                  + zydx(k,j,i) *2.*(                   &
                    rv(k,j  ,i) / ((dz(k,j,i)+dz(k,j-1,i))*(dx(j,i)+dx(j-1,i)))     &
                  + rv(k,j+1,i) / ((dz(k,j,i)+dz(k,j+1,i))*(dx(j,i)+dx(j+1,i))) )                        
          enddo
          !
          do k = 2,nz
             rw(i,j,k) =  - 0.5*(wrk(k)+wrk(k-1)) * 0.5*(dz(k,j,i)+dz(k-1,j,i))
          enddo
          rw(i,j,nz+1) =  - wrk(nz) *0.5*dz(nz,j,i)    
       enddo
    enddo

    if (check_output) then      
       call write_netcdf(rw,vname='rw',netcdf_file_name='rw.nc',rank=myrank,iter=iter_rw)
    endif

!!! dirty reshape arrays indexing kji -> ijk !!!
    deallocate(rub)
    deallocate(rvb)
!!! dirty reshape arrays indexing kji -> ijk !!!

  end subroutine nhmg_rw

  !--------------------------------------------------------------
  subroutine nhmg_rw_advection(nx,ny,nz,Huona,Hvoma,Omegaa,zwa,ua,va,wa,rwa)

    integer(kind=ip), intent(in) :: nx, ny, nz

    real(kind=rp), dimension(-1:nx+2,-1:ny+2,1:nz  ), target, intent(in) :: Huona,Hvoma
    real(kind=rp), dimension(-1:nx+2,-1:ny+2,1:nz+1), target, intent(in) :: Omegaa,zwa,wa
    real(kind=rp), dimension(-1:nx+2,-1:ny+2,1:nz  ), target, intent(in) :: ua,va
    real(kind=rp), dimension(-1:nx+2,-1:ny+2,1:nz+1), target, intent(inout):: rwa

    real(kind=rp), dimension(:,:)  , pointer :: dx,dy
    real(kind=rp), dimension(:,:,:), pointer :: zr,dz
    real(kind=rp), dimension(:,:,:), pointer :: zx,zy
    real(kind=rp), dimension(:,:,:), pointer :: Huon,Hvom,Omega,zw
    real(kind=rp), dimension(:,:,:), pointer :: u,v,w
    real(kind=rp), dimension(:,:,:), pointer :: zr_t,dt_zx,dt_zy
    real(kind=rp), dimension(:,:,:), pointer :: divx_zx,divx_zy
    real(kind=rp), dimension(:,:,:), pointer :: divy_zx,divy_zy
    real(kind=rp), dimension(:,:,:), pointer :: divs_zx,divs_zy
    real(kind=rp), dimension(:,:,:), pointer :: rw

    real(kind=rp), dimension(nz)             :: wrk
    real(kind=rp)                            :: zeta_t

    integer(kind=ip) :: i,j,k

    dx => grid(1)%dx
    dy => grid(1)%dy
    zr => grid(1)%zr
    dz => grid(1)%dz
    zx => grid(1)%zx
    zy => grid(1)%zy

    Huon => Huona
    Hvom => Hvoma
    Omega => Omegaa
    zw => zwa
    u => ua
    v => va
    w => wa
    rw => rwa

    allocate(zr_t(1:nz,0:ny+1,0:nx+1))
    allocate(dt_zx(1:nz,0:ny+1,0:nx+1))
    allocate(dt_zy(1:nz,0:ny+1,0:nx+1))
    allocate(divx_zx(1:nz,0:ny+1,0:nx+1))
    allocate(divx_zy(1:nz,0:ny+1,0:nx+1))
    allocate(divy_zx(1:nz,0:ny+1,0:nx+1))
    allocate(divy_zy(1:nz,0:ny+1,0:nx+1))
    allocate(divs_zx(1:nz,0:ny+1,0:nx+1))
    allocate(divs_zy(1:nz,0:ny+1,0:nx+1))

    !dt term
    do i = 1,nx
       do j = 1,ny
          zeta_t = 0.
          do k = 1,nz
             zeta_t = zeta_t -Huon(i+1,j,k)+Huon(i,j,k) &
                             -Hvom(i,j+1,k)+Hvom(i,j,k)
          end do
          zeta_t = zeta_t / (dx(j,i)*dy(j,i))
          do k = 1,nz
             zr_t(k,j,i) = zeta_t * (zr(k,j,i   )-zw(i,j,1)) &
                                  / (zw(i,j,nz+1)-zw(i,j,1))
          end do
       enddo
    enddo
    call fill_halo(1,zr_t)
    do i = 1,nx
       do j = 1,ny
          do k = 1,nz
             dt_zx(k,j,i) = 0.5 * (zr_t(k,j,i+1)-zr_t(k,j,i-1)) * dy(j,i)*dz(k,j,i)
             dt_zy(k,j,i) = 0.5 * (zr_t(k,j+1,i)-zr_t(k,j-1,i)) * dx(j,i)*dz(k,j,i)
          end do
       enddo
    enddo

    !divx term
    do i = 1,nx
       do j = 1,ny
          do k = 1,nz
             divx_zx(k,j,i) = Huon(i+1,j,k)*0.5*(zx(k,j,i  )+zx(k,j,i+1)) &
                  - Huon(i,j,k)*0.5*(zx(k,j,i-1)+zx(k,j,i  ))
             divx_zy(k,j,i) = Huon(i+1,j,k)*0.5*(zy(k,j,i  )+zy(k,j,i+1)) &
                  - Huon(i,j,k)*0.5*(zy(k,j,i-1)+zy(k,j,i  ))
          end do
       enddo
    enddo

    !divy term
    do i = 1,nx
       do j = 1,ny
          do k = 1,nz
             divy_zx(k,j,i) = Hvom(i,j+1,k)*0.5*(zx(k,j,i)+zx(k,j+1,i)) &
                  - Hvom(i,j,k)*0.5*(zx(k,j-1,i)+zx(k,j,i))
             divy_zy(k,j,i) = Hvom(i,j+1,k)*0.5*(zy(k,j,i)+zy(k,j+1,i)) &
                  - Hvom(i,j,k)*0.5*(zy(k,j-1,i)+zy(k,j,i))
          end do
       enddo
    enddo

    !divs term
    do i = 1,nx
       do j = 1,ny
          k=1
          divs_zx(k,j,i) = Omega(i,j,k+1)*0.5*(zx(k,j,i)+zx(k+1,j,i)) 
          divs_zy(k,j,i) = Omega(i,j,k+1)*0.5*(zy(k,j,i)+zy(k+1,j,i)) 
          do k = 2,nz-1
             divs_zx(k,j,i) = Omega(i,j,k+1)*0.5*(zx(k,j,i)+zx(k+1,j,i)) &
                  - Omega(i,j,k)*0.5*(zx(k-1,j,i)+zx(k,j,i))
             divs_zy(k,j,i) = Omega(i,j,k+1)*0.5*(zy(k,j,i)+zy(k+1,j,i)) &
                  - Omega(i,j,k)*0.5*(zy(k-1,j,i)+zy(k,j,i))
          end do
          k=nz
          divs_zx(k,j,i) = - Omega(i,j,k)*0.5*(zx(k-1,j,i)+zx(k,j,i))
          divs_zy(k,j,i) = - Omega(i,j,k)*0.5*(zy(k-1,j,i)+zy(k,j,i))
       enddo
    enddo

    !
    do i = 1,nx
       do j = 1,ny
          !
          do k = 1,nz
             wrk(k) = &
!             !dt term
!                  +0.5*(+dt_zx(k,j,i) /dz(k,j,i) *u(i  ,j,k)  &
!                        +dt_zx(k,j,i) /dz(k,j,i) *u(i+1,j,k)) &
!                  +0.5*(+dt_zy(k,j,i) /dz(k,j,i) *v(i,j  ,k)  &
!                        +dt_zy(k,j,i) /dz(k,j,i) *v(i,j+1,k)) &
!                  !or
!                  !+0.5*(+dt_zx(k,j,i) *u(i  ,j,k)  &
!                  !      +dt_zx(k,j,i) *u(i+1,j,k)) &
!                  !+0.5*(+dt_zy(k,j,i) *v(i,j  ,k)  &
!                  !      +dt_zy(k,j,i) *v(i,j+1,k)) &
             !divx term
                  +0.5*(+divx_zx(k,j,i) /dz(k,j,i) *u(i  ,j,k)  &
                        +divx_zx(k,j,i) /dz(k,j,i) *u(i+1,j,k)) &
                  +0.5*(+divx_zy(k,j,i) /dz(k,j,i) *v(i,j  ,k)  &
                        +divx_zy(k,j,i) /dz(k,j,i) *v(i,j+1,k)) &
                  !or
                  !+0.5*(+divx_zx(k,j,i) *u(i  ,j,k)  &
                  !      +divx_zx(k,j,i) *u(i+1,j,k)) &
                  !+0.5*(+divx_zy(k,j,i) *v(i,j  ,k)  &
                  !      +divx_zy(k,j,i) *v(i,j+1,k)) &
             !divy term
                  +0.5*(+divy_zx(k,j,i) /dz(k,j,i) *u(i  ,j,k)  &
                        +divy_zx(k,j,i) /dz(k,j,i) *u(i+1,j,k)) &
                  +0.5*(+divy_zy(k,j,i) /dz(k,j,i) *v(i,j  ,k)  &
                        +divy_zy(k,j,i) /dz(k,j,i) *v(i,j+1,k)) &
                  !or
                  !+0.5*(+divy_zx(k,j,i) *u(i  ,j,k)  &
                  !      +divy_zx(k,j,i) *u(i+1,j,k)) &
                  !+0.5*(+divy_zy(k,j,i) *v(i,j  ,k)  &
                  !      +divy_zy(k,j,i) *v(i,j+1,k)) &
             !divs term
                  +0.5*(+divs_zx(k,j,i) /dz(k,j,i) *u(i  ,j,k)  &
                        +divs_zx(k,j,i) /dz(k,j,i) *u(i+1,j,k)) &
                  +0.5*(+divs_zy(k,j,i) /dz(k,j,i) *v(i,j  ,k)  &
                        +divs_zy(k,j,i) /dz(k,j,i) *v(i,j+1,k))
                  !or
                  !+0.5*(+divs_zx(k,j,i) *u(i  ,j,k)  &
                  !      +divs_zx(k,j,i) *u(i+1,j,k)) &
                  !+0.5*(+divs_zy(k,j,i) *v(i,j  ,k)  &
                  !      +divs_zy(k,j,i) *v(i,j+1,k))                   
          enddo
          !
          do k = 2,nz
             rw(i,j,k) = rw(i,j,k) &
                   - 0.5*(wrk(k)+wrk(k-1)) * 0.5*(dz(k,j,i)+dz(k-1,j,i))
                  !or 
                  !- 0.5*(wrk(k)+wrk(k-1))
          enddo
          rw(i,j,nz+1) = rw(i,j,nz+1) &
               - wrk(nz) *0.5*dz(nz,j,i)  
               !or 
               !- wrk(nz)    
          !
       enddo
    enddo

    deallocate(zr_t)
    deallocate(dt_zx)
    deallocate(dt_zy)
    deallocate(divx_zx)
    deallocate(divx_zy)
    deallocate(divy_zx)
    deallocate(divy_zy)
    deallocate(divs_zx)
    deallocate(divs_zy)

  end subroutine nhmg_rw_advection

  !--------------------------------------------------------------
  subroutine nhmg_matrices(nx,ny,nz,zra,Hza,dxa,dya)

    integer(kind=ip), intent(in) :: nx, ny, nz

    real(kind=rp), dimension(0:nx+1,0:ny+1,1:nz)          , intent(in) :: zra
    real(kind=rp), dimension(0:nx+1,0:ny+1,1:nz)          , intent(in) :: Hza
    real(kind=rp), dimension(0:nx+1,0:ny+1)     , optional, intent(in) :: dxa
    real(kind=rp), dimension(0:nx+1,0:ny+1)     , optional, intent(in) :: dya

    real(kind=rp), dimension(:,:,:), pointer :: zr,Hz
    real(kind=rp), dimension(:,:)  , pointer :: dx,dy

!!! dirty reshape arrays indexing ijk -> kji !!!
    integer(kind=ip) :: i,j,k
    real(kind=rp), dimension(1:nz,0:ny+1,0:nx+1), target :: zrb
    real(kind=rp), dimension(1:nz,0:ny+1,0:nx+1), target :: Hzb 
    real(kind=rp), dimension(0:ny+1,0:nx+1),      target :: dxb,dyb
!!!

    integer(kind=ip), save :: iter_matrices=0
    iter_matrices = iter_matrices + 1

!    if (myrank==0) write(*,*)' nhmg_matrices: ',iter_matrices

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
!!! dirty reshape arrays indexing ijk -> kji !!!
    do i = 0,nx+1
       do j = 0,ny+1
          do k = 1,nz
             zrb(k,j,i) = zra(i,j,k)
             Hzb(k,j,i) = Hza(i,j,k)
          enddo
       enddo
    enddo
    zr => zrb
    Hz => Hzb
!!!

    call set_vert_grids(zr,Hz)

!!$    if (check_output) then
!!$       !if ((iter_matrices .EQ. 1) .OR. (iter_matrices .EQ. 2)) then
!!$          call write_netcdf(grid(1)%zr,vname='zr',netcdf_file_name='mat.nc',rank=myrank,iter=iter_matrices)
!!$          call write_netcdf(grid(1)%dz,vname='dz',netcdf_file_name='mat.nc',rank=myrank,iter=iter_matrices)
!!$       !endif
!!$    endif

    if (associated(zr)) zr => null()
    if (associated(Hz)) Hz => null()

    !------------!
    !- matrices -!
    !------------!
    call set_matrices()

!!$    if (check_output) then
!!$       !if ((iter_matrices .EQ. 1) .OR. (iter_matrices .EQ. 2)) then
!!$          call write_netcdf(grid(1)%cA,vname='cA',netcdf_file_name='mat.nc',rank=myrank,iter=iter_matrices)
!!$       !endif
!!$    endif

  end subroutine nhmg_matrices

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

