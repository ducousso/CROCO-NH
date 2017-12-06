module mg_vert_grids

  use mg_cst
  use mg_mpi
  use mg_tictoc
  use mg_grids
  use mg_mpi_exchange
  use mg_gather
  use mg_namelist
  use mg_netcdf_out

  implicit none

contains

  !----------------------------------------
  subroutine set_vert_grids(z_r,Hz)

    real(kind=rp), dimension(:,:,:), pointer, intent(in)  :: z_r,Hz

    integer(kind=ip) :: lev
    integer(kind=ip) :: nx,ny,nz
    integer(kind=ip) :: nxf,nxc
    integer(kind=ip) :: nyf,nyc
    integer(kind=ip) :: nzf,nzc

    real(kind=rp), dimension(:,:,:), pointer :: zr,dz

    real(kind=rp), dimension(:,:,:), pointer :: zrf,zrc 
    real(kind=rp), dimension(:,:,:), pointer :: dzf,dzc

    real(kind=rp), dimension(:,:)  , pointer :: dx,dy
    real(kind=rp), dimension(:,:,:), pointer :: dzw
    real(kind=rp), dimension(:,:,:), pointer :: Arx,Ary
    real(kind=rp), dimension(:,:,:), pointer :: zx,zy
    real(kind=rp), dimension(:,:,:), pointer :: zydx,zxdy
    real(kind=rp), dimension(:,:,:), pointer :: alpha
    real(kind=rp), dimension(:,:)  , pointer :: beta
    real(kind=rp), dimension(:,:)  , pointer :: gamu
    real(kind=rp), dimension(:,:)  , pointer :: gamv
    real(kind=rp), dimension(:,:)  , pointer :: Arz

    integer(kind=ip) :: i,j,k

!    if (myrank==0) write(*,*)'   - set vertical grids:'

    do lev = 1, nlevs

       !! fill and coarsen zr and dz

       nx=grid(lev)%nx
       ny=grid(lev)%ny
       nz=grid(lev)%nz

       if (lev == 1) then ! zr,dz from croco

          grid(lev)%zr(1:nz,0:ny+1,0:nx+1) = z_r
          grid(lev)%dz(1:nz,0:ny+1,0:nx+1) = Hz

       else               ! coarsen zr,dz (needed when directly discretizing on coarser grids)

          nxf = grid(lev-1)%nx
          nyf = grid(lev-1)%ny
          nzf = grid(lev-1)%nz

          zrf => grid(lev-1)%zr
          dzf => grid(lev-1)%dz

          if (grid(lev)%gather == 1) then
             nxc = nx/grid(lev)%ngx
             nyc = ny/grid(lev)%ngy
             nzc = nz
             allocate(zrc(1:nzc,0:nyc+1,0:nxc+1))
             allocate(dzc(1:nzc,0:nyc+1,0:nxc+1))
          else
             nxc = nx
             nyc = ny
             nzc = nz
             zrc => grid(lev)%zr
             dzc => grid(lev)%dz
          endif

          ! Call fine2coarse
          zrc(1:nzc,1:nyc,1:nxc) = eighth * (       &
               zrf(1:nzf  :2,1:nyf  :2,1:nxf  :2) + &
               zrf(1:nzf  :2,2:nyf+1:2,1:nxf  :2) + &
               zrf(1:nzf  :2,1:nyf  :2,2:nxf+1:2) + &
               zrf(1:nzf  :2,2:nyf+1:2,2:nxf+1:2) + &
               zrf(2:nzf+1:2,1:nyf  :2,1:nxf  :2) + &
               zrf(2:nzf+1:2,2:nyf+1:2,1:nxf  :2) + &
               zrf(2:nzf+1:2,1:nyf  :2,2:nxf+1:2) + &
               zrf(2:nzf+1:2,2:nyf+1:2,2:nxf+1:2) )

          ! Call fine2coarse
          dzc(1:nzc,1:nyc,1:nxc) = 2._rp * eighth * ( &
               dzf(1:nzf  :2,1:nyf  :2,1:nxf  :2) +   &
               dzf(1:nzf  :2,2:nyf+1:2,1:nxf  :2) +   &
               dzf(1:nzf  :2,1:nyf  :2,2:nxf+1:2) +   &
               dzf(1:nzf  :2,2:nyf+1:2,2:nxf+1:2) +   &
               dzf(2:nzf+1:2,1:nyf  :2,1:nxf  :2) +   &
               dzf(2:nzf+1:2,2:nyf+1:2,1:nxf  :2) +   &
               dzf(2:nzf+1:2,1:nyf  :2,2:nxf+1:2) +   &
               dzf(2:nzf+1:2,2:nyf+1:2,2:nxf+1:2) )

          if (grid(lev)%gather == 1) then
             call gather(lev,zrc,grid(lev)%zr)
             call gather(lev,dzc,grid(lev)%dz)
             deallocate(zrc)
             deallocate(dzc)
          endif

       end if

       call fill_halo(lev,grid(lev)%zr) ! special fill_halo of zr (nh=2)
       call fill_halo(lev,grid(lev)%dz) ! special fill_halo of dz (nh=2)

       if (netcdf_output) then
          call write_netcdf(grid(lev)%zr,vname='zr',netcdf_file_name='zr.nc',rank=myrank,iter=lev)
          call write_netcdf(grid(lev)%dz,vname='dz',netcdf_file_name='dz.nc',rank=myrank,iter=lev)
       endif

       !! compute derived qties

       dx    => grid(lev)%dx
       dy    => grid(lev)%dy
       zr    => grid(lev)%zr
       dz    => grid(lev)%dz

       dzw   => grid(lev)%dzw
       Arx   => grid(lev)%Arx
       Ary   => grid(lev)%Ary
       Arz   => grid(lev)%Arz
       zx    => grid(lev)%zx
       zy    => grid(lev)%zy
       zxdy  => grid(lev)%zxdy
       zydx  => grid(lev)%zydx
       alpha => grid(lev)%alpha
       beta  => grid(lev)%beta
       gamu  => grid(lev)%gamu
       gamv  => grid(lev)%gamv

       !! Cell height 
       do i = 0,nx+1
          do j = 0,ny+1
             dzw(1,j,i) = hlf * dz(1,j,i)
             do k = 2,nz
                dzw(k,j,i) = hlf * (dz(k-1,j,i) + dz(k,j,i))
             enddo
             dzw(nz+1,j,i) = hlf * dz(nz,j,i)
          enddo
       enddo

       !!  Cell faces area
       do i = 1,nx+1
          do j = 0,ny+1
             do k = 1,nz
                Arx(k,j,i) = hlf * ( dy(j,i) * dz(k,j,i) + dy(j,i-1) * dz(k,j,i-1) )
             enddo
          enddo
       enddo
       do i = 0,nx+1
          do j = 1,ny+1
             do k = 1,nz
                Ary(k,j,i) = hlf * ( dx(j,i) * dz(k,j,i) + dx(j-1,i) * dz(k,j-1,i) )
             enddo
          enddo
       enddo
       do i = 0,nx+1
          do j = 1,ny+1
             Arz(j,i) = dx(j,i) * dy(j,i)
          enddo
       enddo

       !! Slopes
       do i = 0,nx+1        ! We need zr with 2 halo points !
          do j = 0,ny+1     !
             do k = 1, nz
                zx(k,j,i) = hlf * (( zr(k,j  ,i+1) - zr(k,j  ,i-1) ) / dx(j,i) )
                zy(k,j,i) = hlf * (( zr(k,j+1,i  ) - zr(k,j-1,i  ) ) / dy(j,i) )
             enddo
          enddo
       enddo

       call set_phybound2zero(lev,zx,gt='u')
       call set_phybound2zero(lev,zy,gt='v')

       do i = 0,nx+1        ! We need zr with 2 halo points !
          do j = 0,ny+1     !
             do k = 1, nz
                zxdy(k,j,i) = hlf * (( zr(k,j  ,i+1) - zr(k,j  ,i-1) ) / dx(j,i) ) * dy(j,i)
                zydx(k,j,i) = hlf * (( zr(k,j+1,i  ) - zr(k,j-1,i  ) ) / dy(j,i) ) * dx(j,i)
             enddo
          enddo
       enddo

       call set_phybound2zero(lev,zxdy,gt='u')
       call set_phybound2zero(lev,zydx,gt='v')

       !!- Used in set_matrices and fluxes
       do i = 0,nx+1
          do j = 0,ny+1
             do k = 1, nz
                alpha(k,j,i) = one + (zxdy(k,j,i)/dy(j,i))**2 + (zydx(k,j,i)/dx(j,i))**2
             enddo
          enddo
       enddo

       do i = 0,nx+1
          do j = 0,ny+1
             gamu(j,i) = one - hlf * ( zxdy(1,j,i) / dy(j,i) )**2 / alpha(1,j,i) 
          enddo
       enddo

       do i = 0,nx+1
          do j = 0,ny+1
             gamv(j,i) = one - hlf * ( zydx(1,j,i) / dx(j,i) )**2 / alpha(1,j,i) 
          enddo
       enddo

       do i = 0,nx+1
          do j = 0,ny+1
             beta(j,i) = eighth * zxdy(1,j,i)/dy(j,i) * zydx(1,j,i)/dx(j,i) * dz(1,j,i) / alpha(1,j,i)
          enddo
       end do

       if (netcdf_output) then
          call write_netcdf(grid(lev)%dzw,vname='dzw',netcdf_file_name='dzw.nc',rank=myrank,iter=lev)
          call write_netcdf(grid(lev)%zxdy,vname='zxdy',netcdf_file_name='zxdy.nc',rank=myrank,iter=lev)
          call write_netcdf(grid(lev)%zydx,vname='zydx',netcdf_file_name='zydx.nc',rank=myrank,iter=lev)
          call write_netcdf(grid(lev)%alpha,vname='alpha',netcdf_file_name='alpha.nc',rank=myrank,iter=lev)
       endif

    enddo

  end subroutine set_vert_grids

end module mg_vert_grids
