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
  subroutine set_vert_grids()

    integer(kind=ip) :: lev
    integer(kind=ip) :: nx,ny,nz
    integer(kind=ip) :: nxf,nxc
    integer(kind=ip) :: nyf,nyc
    integer(kind=ip) :: nzf,nzc

    real(kind=rp), dimension(:,:,:), pointer :: zf,zc 
    real(kind=rp), dimension(:,:,:), pointer :: w0, wp
    real(kind=rp), dimension(:,:,:), pointer :: zxf,zxc 
    real(kind=rp), dimension(:,:,:), pointer :: zyf,zyc 
    real(kind=rp), dimension(:,:,:), pointer :: dzf,dzc

    real(kind=rp), dimension(:,:)  , pointer :: dx,dy
    real(kind=rp), dimension(:,:,:), pointer :: dz,dzw
    real(kind=rp), dimension(:,:,:), pointer :: Arx,Ary
    real(kind=rp), dimension(:,:,:), pointer :: zydx,zxdy
    real(kind=rp), dimension(:,:,:), pointer :: alpha
    real(kind=rp), dimension(:,:)  , pointer :: beta
    real(kind=rp), dimension(:,:)  , pointer :: gamu
    real(kind=rp), dimension(:,:)  , pointer :: gamv
    real(kind=rp), dimension(:,:)  , pointer :: Arz

    integer(kind=ip) :: i,j,k
    integer(kind=ip) :: i2,j2,k2,kp,ip

!    if (myrank==0) write(*,*)'   - set vertical grids:'

    do lev = 1, nlevs

       !! coarsen slopes and dz

       nx=grid(lev)%nx
       ny=grid(lev)%ny
       nz=grid(lev)%nz

       dx    => grid(lev)%dx
       dy    => grid(lev)%dy

       if (lev == 1) then 

          !! we fill slopes and dz in nhmg at the finest grid level

       else               ! coarsen slopes,dz (needed when directly discretizing on coarser grids)

          nxf = grid(lev-1)%nx
          nyf = grid(lev-1)%ny
          nzf = grid(lev-1)%nz

          dzf => grid(lev-1)%dz
          zf  => grid(lev-1)%zr          
          zxf => grid(lev-1)%zxdy
          zyf => grid(lev-1)%zydx

          if (grid(lev)%gather == 1) then
             nxc = nx/grid(lev)%ngx
             nyc = ny/grid(lev)%ngy
             nzc = nz
             allocate(dzc(1:nzc,0:nyc+1,0:nxc+1))
             allocate(zc(1:nzc,0:nyc+1,0:nxc+1))
             allocate(zxc(1:nzc,0:nyc+1,0:nxc+1))
             allocate(zyc(1:nzc,0:nyc+1,0:nxc+1))
          else
             nxc = nx
             nyc = ny
             nzc = nz
             dzc => grid(lev)%dz
             zc => grid(lev)%zr
             zxc => grid(lev)%zxdy
             zyc => grid(lev)%zydx
          endif
          if (trim(grid(lev)%coarsening_method).eq.'xyz') then
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
          
!!$          zc(1:nzc,1:nyc,1:nxc) =          eighth * ( &
!!$               dzf(1:nzf  :2,1:nyf  :2,1:nxf  :2) +   &
!!$               dzf(1:nzf  :2,2:nyf+1:2,1:nxf  :2) +   &
!!$               dzf(1:nzf  :2,1:nyf  :2,2:nxf+1:2) +   &
!!$               dzf(1:nzf  :2,2:nyf+1:2,2:nxf+1:2) +   &
!!$               dzf(2:nzf+1:2,1:nyf  :2,1:nxf  :2) +   &
!!$               dzf(2:nzf+1:2,2:nyf+1:2,1:nxf  :2) +   &
!!$               dzf(2:nzf+1:2,1:nyf  :2,2:nxf+1:2) +   &
!!$               dzf(2:nzf+1:2,2:nyf+1:2,2:nxf+1:2) )
          
          ! Call fine2coarse
          zxc(1:nzc,1:nyc,1:nxc) = 2._rp * eighth * ( &
               zxf(1:nzf  :2,1:nyf  :2,1:nxf  :2) + &
               zxf(1:nzf  :2,2:nyf+1:2,1:nxf  :2) + &
               zxf(1:nzf  :2,1:nyf  :2,2:nxf+1:2) + &
               zxf(1:nzf  :2,2:nyf+1:2,2:nxf+1:2) + &
               zxf(2:nzf+1:2,1:nyf  :2,1:nxf  :2) + &
               zxf(2:nzf+1:2,2:nyf+1:2,1:nxf  :2) + &
               zxf(2:nzf+1:2,1:nyf  :2,2:nxf+1:2) + &
               zxf(2:nzf+1:2,2:nyf+1:2,2:nxf+1:2) )

          ! Call fine2coarse
          zyc(1:nzc,1:nyc,1:nxc) = 2._rp * eighth * ( &
               zyf(1:nzf  :2,1:nyf  :2,1:nxf  :2) + &
               zyf(1:nzf  :2,2:nyf+1:2,1:nxf  :2) + &
               zyf(1:nzf  :2,1:nyf  :2,2:nxf+1:2) + &
               zyf(1:nzf  :2,2:nyf+1:2,2:nxf+1:2) + &
               zyf(2:nzf+1:2,1:nyf  :2,1:nxf  :2) + &
               zyf(2:nzf+1:2,2:nyf+1:2,1:nxf  :2) + &
               zyf(2:nzf+1:2,1:nyf  :2,2:nxf+1:2) + &
               zyf(2:nzf+1:2,2:nyf+1:2,2:nxf+1:2) )
          elseif  (trim(grid(lev)%coarsening_method).eq.'xz') then
          ! Call fine2coarse
          dzc(1:nzc,1,1:nxc) = 2._rp * qrt * ( &
               dzf(1:nzf  :2,1,1:nxf  :2) +   &
               dzf(1:nzf  :2,1,2:nxf+1:2) +   &
               dzf(2:nzf+1:2,1,1:nxf  :2) +   &
               dzf(2:nzf+1:2,1,2:nxf+1:2) )

          zc(1:nzc,1,1:nxc) =         qrt * ( &
               zf(1:nzf  :2,1,1:nxf  :2) +   &
               zf(1:nzf  :2,1,2:nxf+1:2) +   &
               zf(2:nzf+1:2,1,1:nxf  :2) +   &
               zf(2:nzf+1:2,1,2:nxf+1:2) )

          ! Call fine2coarse
          zxc(1:nzc,1,1:nxc) =   qrt * (       &
               zxf(1:nzf  :2,1,1:nxf  :2) + &
               zxf(1:nzf  :2,1,2:nxf+1:2) + &
               zxf(2:nzf+1:2,1,1:nxf  :2) + &
               zxf(2:nzf+1:2,1,2:nxf+1:2) )


!!$          ! Call fine2coarse
!!$          zyc(1:nzc,1,1:nxc) = qrt * (       &
!!$               zyf(1:nzf  :2,1,1:nxf  :2) + &
!!$               zyf(1:nzf  :2,1,2:nxf+1:2) + &
!!$               zyf(2:nzf+1:2,1,1:nxf  :2) + &
!!$               zyf(2:nzf+1:2,1,2:nxf+1:2) )
          endif
          if (grid(lev)%gather == 1) then
             call gather(lev,dzc,grid(lev)%dz)
             call gather(lev,zc, grid(lev)%zr)
             call gather(lev,zxc,grid(lev)%zxdy)
             call gather(lev,zyc,grid(lev)%zydx)
             deallocate(dzc)
             deallocate(zc)
             deallocate(zxc)
             deallocate(zyc)
          endif

       call fill_halo(lev,grid(lev)%dz)
       call fill_halo(lev,grid(lev)%zxdy)
       call fill_halo(lev,grid(lev)%zydx)
       call fill_halo(lev,grid(lev)%zr)
       end if

       !! compute derived qties

       dx    => grid(lev)%dx
       dy    => grid(lev)%dy
       dz    => grid(lev)%dz
       dzw   => grid(lev)%dzw
       Arx   => grid(lev)%Arx
       Ary   => grid(lev)%Ary
       Arz   => grid(lev)%Arz
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
       if (trim(grid(lev)%relaxation_method).eq.'xyz') then
          !!- Used in set_matrices and fluxes
          do i = 0,nx+1
             do j = 0,ny+1
                do k = 1, nz
                   alpha(k,j,i) = one + (zxdy(k,j,i)/dy(j,i))**2 + (zydx(k,j,i)/dx(j,i))**2
                enddo
             enddo
          enddo
       
       elseif (trim(grid(lev)%relaxation_method).eq.'xz') then
          do i = 0,nx+1
             do j = 0,ny+1
                do k = 1, nz
                   alpha(k,j,i) = one + (zxdy(k,j,i)/dy(j,i))**2
                enddo
             enddo
          enddo
       endif
       
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
          call write_netcdf(grid(lev)%zr,vname='zr',netcdf_file_name='zr.nc',rank=myrank,iter=lev)
       endif

       if((lev.ge.1).and.(trim(grid(lev)%coarsening_method).eq.'xz'))then
          zc => grid(lev)%zr
          zf => grid(lev-1)%zr
          w0 => grid(lev-1)%w0
          wp => grid(lev-1)%wp
          nx = grid(lev-1)%nx
          nz = grid(lev-1)%nz
          j = 1
          j2 = 1
          do i=1,nx
             i2 = (i+1)/2
             ip = i2-(mod(i,2)*2-1)
             do k=1,nz-1
                k2 = (k+1)/2
                kp = k2-(mod(k,2)*2-1)
                if(k.eq.1)kp=2
                w0(k,j,i) = (zf(k,j,i)-zc(kp,j2,i2)) / (zc(k2,j2,i2)-zc(kp,j2,i2))
                wp(k,j,i) = (zf(k,j,i)-zc(kp,j2,ip)) / (zc(k2,j2,ip)-zc(kp,j2,ip))
             enddo
          enddo          
       endif
    enddo

  end subroutine set_vert_grids

end module mg_vert_grids
