module mg_intergrids

  use mg_cst
  use mg_mpi
  use mg_tictoc
  use mg_namelist
  use mg_grids
  use mg_mpi_exchange
  use mg_gather

  implicit none

contains

  !------------------------------------------------------------
  subroutine fine2coarse(lev)

    ! coarsen grid(lev)%r to grid(lev+1)%b and set grid(lev+1)%p=0

    integer(kind=ip), intent(in) :: lev

    real(kind=rp),dimension(:,:,:),pointer :: r
    real(kind=rp),dimension(:,:,:),pointer :: b

    integer(kind=ip) :: nx, ny, nz

    call tic(lev,'fine2coarse')

    nx = grid(lev+1)%nx
    ny = grid(lev+1)%ny
    nz = grid(lev+1)%nz

    r => grid(lev)%r

    if (grid(lev+1)%gather == 1) then
       !- b here is sub level rhs, we will gather at the end
       !- in the level+1 rhs
       b => grid(lev+1)%dummy3
       !- ngx and ngy are the number of processes being gathered in x and y
       !- values are 1 or 2
       nx = grid(lev+1)%nx / grid(lev+1)%ngx
       ny = grid(lev+1)%ny / grid(lev+1)%ngy
    else
       b => grid(lev+1)%b
    endif

    if (trim(grid(lev+1)%coarsening_method).eq.'xyz') then
       call fine2coarse_xyz(r,b,nx,ny,nz)

    elseif (trim(grid(lev+1)%coarsening_method).eq.'xz') then
       call fine2coarse_xz(r,b,nx,ny,nz)

    elseif (trim(grid(lev+1)%coarsening_method).eq.'yz') then
       call fine2coarse_yz(r,b,nx,ny,nz)

    end if

    if (grid(lev+1)%gather == 1) then
       r => grid(lev+1)%dummy3
       b => grid(lev+1)%b
       call gather(lev+1,r,b)
    endif

    call fill_halo(lev+1,grid(lev+1)%b)

    grid(lev+1)%p = zero

    call toc(lev,'fine2coarse')

  end subroutine fine2coarse

  !------------------------------------------------------------
  subroutine coarse2fine(lev)

    ! interpolate grid(lev+1)%p to grid(lev)%r and add it to grid(lev)%p

    integer(kind=ip), intent(in) :: lev

    real(kind=rp),dimension(:,:,:),pointer :: rf,zf
    real(kind=rp),dimension(:,:,:),pointer :: pc,zc

    integer(kind=ip) :: nxc, nyc, nzc

    integer(kind=ip) :: kk, jj, ii

    call tic(lev,'coarse2fine')

    nxc = grid(lev+1)%nx
    nyc = grid(lev+1)%ny
    nzc = grid(lev+1)%nz

    pc => grid(lev+1)%p

    zf => grid(lev)%zr
    zc => grid(lev+1)%zr

    if (grid(lev+1)%gather == 1) then
       rf => grid(lev+1)%dummy3
       call split(lev+1,pc,rf)
       pc => grid(lev+1)%dummy3
       nxc = grid(lev+1)%nx / grid(lev+1)%ngx
       nyc = grid(lev+1)%ny / grid(lev+1)%ngy
       nzc = grid(lev+1)%nz       
    endif

    rf => grid(lev)%r

    if (trim(grid(lev+1)%coarsening_method).eq.'xyz') then
       call coarse2fine_xyz(rf,pc,nxc,nyc,nzc)
       
    elseif (trim(grid(lev+1)%coarsening_method).eq.'xz') then
       call coarse2fine_xz(rf,grid(lev)%w0,grid(lev)%wp,pc,zf,zc,nxc,nyc,nzc,lev)
       
    elseif (trim(grid(lev+1)%coarsening_method).eq.'yz') then
       call coarse2fine_yz(rf,grid(lev)%w0,grid(lev)%wp,pc,zf,zc,nxc,nyc,nzc,lev)
       
       
    else
       write(*,*)"found no coarsening => STOP"
       stop
    endif

    call fill_halo(lev,grid(lev)%r)

    !!NG sep 2018
    !! We use do-loops to avoid stack overflow with Intel compiler !!
	!! It's avoid also a copy in memory due to the fact that we use pointers
	!! (not necessary considered as contiguous in memory for compilers)
	!! rather than allocatable arrays in grid structure (derived type).
    !! old command => grid(lev)%p = grid(lev)%p + grid(lev)%r
    do ii = lbound(grid(lev)%p,dim=3), ubound(grid(lev)%p,dim=3)
       do jj = lbound(grid(lev)%p,dim=2), ubound(grid(lev)%p,dim=2)
          do kk = lbound(grid(lev)%p,dim=1), ubound(grid(lev)%p,dim=1)
             grid(lev)%p(kk,jj,ii) = grid(lev)%p(kk,jj,ii) + grid(lev)%r(kk,jj,ii)
          enddo
       enddo
    enddo

    call toc(lev,'coarse2fine')

  end subroutine coarse2fine

  !------------------------------------------------------------
  subroutine fine2coarse_xyz(x,y,nx,ny,nz)
    !
    ! Fine2coarse 'x' from fine level l1 to 'y' on coarse level l2=l1+1
    real(kind=rp),dimension(:,:,:),pointer,intent(in) :: x
    real(kind=rp),dimension(:,:,:),pointer,intent(out) :: y
    integer(kind=ip), intent(in) :: nx, ny, nz
    ! local
    integer(kind=ip) :: i,j,k,i2,j2,k2

    !! No division by 8 because we work in volume integrated form
    !! See Briggs et al., 'Tutorial on Multi Grid'
    do i2=1,nx
       i=2*i2-1
       do j2=1,ny
          j=2*j2-1
          do k2=1,nz
             k=2*k2-1
             y(k2,j2,i2) = x(k  ,j,i)+x(k  ,j,i+1)+x(k  ,j+1,i)+x(k  ,j+1,i+1) &
                         + x(k+1,j,i)+x(k+1,j,i+1)+x(k+1,j+1,i)+x(k+1,j+1,i+1)
          enddo
       enddo
    enddo

  end subroutine fine2coarse_xyz

  !------------------------------------------------------------
  subroutine fine2coarse_xz(x,y,nx,ny,nz)
    !
    ! Fine2coarse 'x' from fine level l1 to 'y' on coarse level l2=l1+1
    real(kind=rp),dimension(:,:,:),pointer,intent(in) :: x
    real(kind=rp),dimension(:,:,:),pointer,intent(out) :: y
    integer(kind=ip), intent(in) :: nx, ny, nz
    ! local
    integer(kind=ip) :: i,j,k,i2,j2,k2

    j = 1
    do i2=1,nx
       i=2*i2-1
       do k2=1,nz
          k=2*k2-1
          y(k2,j,i2) = x(k  ,j,i)+x(k  ,j,i+1)+x(k+1,j,i)+x(k+1,j,i+1)
       enddo
    enddo

  end subroutine fine2coarse_xz

  !------------------------------------------------------------
  subroutine fine2coarse_yz(x,y,nx,ny,nz)
    !
    ! Fine2coarse 'x' from fine level l1 to 'y' on coarse level l2=l1+1
    real(kind=rp),dimension(:,:,:),pointer,intent(in) :: x
    real(kind=rp),dimension(:,:,:),pointer,intent(out) :: y
    integer(kind=ip), intent(in) :: nx, ny, nz
    ! local
    integer(kind=ip) :: i,j,k,i2,j2,k2

    i = 1
    do j2=1,ny
       j=2*j2-1
       do k2=1,nz
          k=2*k2-1
          y(k2,j2,i) = x(k  ,j,i)+x(k  ,j+1,i)+x(k+1,j,i)+x(k+1,j+1,i)
       enddo
    enddo

  end subroutine fine2coarse_yz

  !------------------------------------------------------------
  subroutine coarse2fine_xyz(xf,xc,nx,ny,nz)

    real(kind=rp),dimension(:,:,:),pointer,intent(out) :: xf
    real(kind=rp),dimension(:,:,:),pointer,intent(in)  :: xc
    integer(kind=ip),intent(in) :: nx, ny, nz

    ! local
    integer(kind=ip) :: i,j,k,i2,j2,k2,kp
    real(kind=rp) :: a,b,c,d,e,f,g

    integer(kind=rp) :: dirichlet_flag

    if (surface_neumann) then
       dirichlet_flag = 0
    else
       dirichlet_flag = 1
    endif

    !
    ! weights for bilinear in (i,j), nearest in k
    a = 9._8 / 16._8
    b = 3._8 / 16._8
    c = 1._8 / 16._8
    !
    ! weights for trilinear in (i,j,k)
    d = 27._8 / 64._8
    e =  9._8 / 64._8
    f =  3._8 / 64._8
    g =  1._8 / 64._8
    !
!!$    a = 1.
!!$    b = 0.
!!$    c = 0.
!!$    d = 1.
!!$    e = 0.
!!$    f = 0.
!!$    g = 0.
    
    do i2=1,nx
       i=2*i2-1
       do j2=1,ny
          j=2*j2-1

          ! bottom level
          k  = 1
          k2 = 1
          xf(k  ,j  ,i  ) =  &
               + a * xc(k2,j2  ,i2) + c * xc(k2,j2-1,i2-1) &
               + b * xc(k2,j2-1,i2) + b * xc(k2,j2  ,i2-1)
          xf(k  ,j+1,i  ) =  &
               + a * xc(k2,j2  ,i2) + c * xc(k2,j2+1,i2-1) &
               + b * xc(k2,j2+1,i2) + b * xc(k2,j2  ,i2-1)
          xf(k  ,j  ,i+1) =  &
               + a * xc(k2,j2  ,i2) + c * xc(k2,j2-1,i2+1) &
               + b * xc(k2,j2-1,i2) + b * xc(k2,j2  ,i2+1)
          xf(k  ,j+1,i+1) =  &
               + a * xc(k2,j2  ,i2) + c * xc(k2,j2+1,i2+1) &
               + b * xc(k2,j2+1,i2) + b * xc(k2,j2  ,i2+1)

          ! interior level
          do k=2,nz*2-1
             k2 = ((k+1)/2)
             kp = k2-(mod(k,2)*2-1)
             xf(k  ,j  ,i  ) =  &
                  + d * xc(k2,j2  ,i2) + f * xc(k2,j2-1,i2-1) &
                  + e * xc(k2,j2-1,i2) + e * xc(k2,j2  ,i2-1) &
                  + e * xc(kp,j2  ,i2) + g * xc(kp,j2-1,i2-1) &
                  + f * xc(kp,j2-1,i2) + f * xc(kp,j2  ,i2-1)
             xf(k  ,j+1,i  ) =  &
                  + d * xc(k2,j2  ,i2) + f * xc(k2,j2+1,i2-1) &
                  + e * xc(k2,j2+1,i2) + e * xc(k2,j2  ,i2-1) &
                  + e * xc(kp,j2  ,i2) + g * xc(kp,j2+1,i2-1) &
                  + f * xc(kp,j2+1,i2) + f * xc(kp,j2  ,i2-1)
             xf(k  ,j  ,i+1) = &
                  + d * xc(k2,j2  ,i2) + f * xc(k2,j2-1,i2+1) &
                  + e * xc(k2,j2-1,i2) + e * xc(k2,j2  ,i2+1) &
                  + e * xc(kp,j2  ,i2) + g * xc(kp,j2-1,i2+1) &
                  + f * xc(kp,j2-1,i2) + f * xc(kp,j2  ,i2+1)
             xf(k  ,j+1,i+1) = &
                  + d * xc(k2,j2  ,i2) + f * xc(k2,j2+1,i2+1) &
                  + e * xc(k2,j2+1,i2) + e * xc(k2,j2  ,i2+1) &
                  + e * xc(kp,j2  ,i2) + g * xc(kp,j2+1,i2+1) &
                  + f * xc(kp,j2+1,i2) + f * xc(kp,j2  ,i2+1)
          enddo

          ! top level
          k = nz*2
          k2 = nz
          xf(k  ,j  ,i  ) =  (1-hlf*dirichlet_flag)  * (     &
               a * xc(k2,j2  ,i2) + c * xc(k2,j2-1,i2-1) +   &
               b * xc(k2,j2-1,i2) + b * xc(k2,j2  ,i2-1)   )
          xf(k  ,j+1,i  ) =  (1-hlf*dirichlet_flag) * (      &
               a * xc(k2,j2  ,i2) + c * xc(k2,j2+1,i2-1) +   &
               b * xc(k2,j2+1,i2) + b * xc(k2,j2  ,i2-1)   )
          xf(k  ,j  ,i+1) =  (1-hlf*dirichlet_flag)  * (     &
               a * xc(k2,j2  ,i2) + c * xc(k2,j2-1,i2+1) +   &
               b * xc(k2,j2-1,i2) + b * xc(k2,j2  ,i2+1)   ) 
          xf(k  ,j+1,i+1) =  (1-hlf*dirichlet_flag)  * (     &
               a * xc(k2,j2  ,i2) + c * xc(k2,j2+1,i2+1) +   &
               b * xc(k2,j2+1,i2) + b * xc(k2,j2  ,i2+1)   )
       enddo
    enddo

  end subroutine coarse2fine_xyz

  !------------------------------------------------------------
  subroutine coarse2fine_xz(xf,w0,wp,xc,zf,zc,nx,ny,nz,lev)

    real(kind=rp),dimension(:,:,:),pointer,intent(out) :: xf
    real(kind=rp),dimension(:,:,:),pointer,intent(in)  :: xc,zf,zc,w0,wp
    integer(kind=ip),intent(in) :: nx, ny, nz,lev

    ! local
    integer(kind=ip) :: i,j,k,i2,j2,k2,kp,ip
    real(kind=rp) :: a,b,c,d,e,f,g,ww0,wwp

    integer(kind=rp) :: dirichlet_flag

    if (surface_neumann) then
       dirichlet_flag = 0
    else
       dirichlet_flag = 1
    endif

    ! 
    j2 = 1
    j  = 1

    a = 9./16.
    b = 3./16.
    c = 1./16.
    d = 3./4.
    e = 1./4.

!    if (1<0) then
    if(lev.lt.1)then
       do i=1,nx*2
          i2 = (i+1)/2
          ip = i2-(mod(i,2)*2-1)
          do k=1,nz*2-1
             k2 = (k+1)/2
             kp = k2-(mod(k,2)*2-1)
             if(k.eq.1)kp=2

             ! bilinear interpolation using depths of rho points
             ! as opposed to indices (which yields 0.25 - 0.75 coeff)
             ! weights are precomputed in mg_vert_grids.f90
             !
             xf(k  ,j  ,i  ) =  0.0625*( &
                  + (    w0(k,j,i)*9+    wp(k,j,i)*3)*xc(k2,j2,i2) &
                  + (    w0(k,j,i)  +    wp(k,j,i)*3)*xc(k2,j2,ip) &
                  + ((1-w0(k,j,i))*9+(1-wp(k,j,i))*3)*xc(kp,j2,i2) &
                  + ((1-w0(k,j,i))  +(1-wp(k,j,i))*3)*xc(kp,j2,ip) )
          enddo
          xf(k  ,j  ,i  ) =  (1-hlf*dirichlet_flag)    &
               * ( d*xc(k2,j2,i2) + e*xc(k2,j2,ip) )
       enddo

    else
       ! this piece of code is never used (see condition above)
       ! it is kept in case someone wants to use it
       do i=1,nx*2
          i2 = (i+1)/2
          ip = i2-(mod(i,2)*2-1)
          do k=1,nz*2-1
             k2 = (k+1)/2
             kp = k2-(mod(k,2)*2-1)
             if(k.eq.1)kp=2
             ! bilinear interpolation based on indices location
             xf(k  ,j  ,i  ) =  &
                  +     a*xc(k2,j2  ,i2) + b*xc(k2,j2  ,ip) &
                  +     b*xc(kp,j2  ,i2) + c*xc(kp,j2  ,ip)

          enddo
          xf(k  ,j  ,i  ) =  (1-hlf*dirichlet_flag)    &
               * ( d*xc(k2,j2,i2) + e*xc(k2,j2,ip) )
       enddo

    endif
    
  end subroutine coarse2fine_xz

  !------------------------------------------------------------
  subroutine coarse2fine_yz(xf,w0,wp,xc,zf,zc,nx,ny,nz,lev)

    real(kind=rp),dimension(:,:,:),pointer,intent(out) :: xf
    real(kind=rp),dimension(:,:,:),pointer,intent(in)  :: xc,zf,zc,w0,wp
    integer(kind=ip),intent(in) :: nx, ny, nz,lev

    ! local
    integer(kind=ip) :: i,j,k,i2,j2,k2,kp,jp
    real(kind=rp) :: a,b,c,d,e,f,g,ww0,wwp

    integer(kind=rp) :: dirichlet_flag

    if (surface_neumann) then
       dirichlet_flag = 0
    else
       dirichlet_flag = 1
    endif

    ! 
    i2 = 1
    i  = 1

    a = 9./16.
    b = 3./16.
    c = 1./16.
    d = 3./4.
    e = 1./4.

!    if (1<0) then
    if(lev.lt.1)then
       do j=1,ny*2
          j2 = (j+1)/2
          jp = j2-(mod(j,2)*2-1)
          do k=1,nz*2-1
             k2 = (k+1)/2
             kp = k2-(mod(k,2)*2-1)
             if(k.eq.1)kp=2

             ! bilinear interpolation using depths of rho points
             ! as opposed to indices (which yields 0.25 - 0.75 coeff)
             ! weights are precomputed in mg_vert_grids.f90
             !
             xf(k  ,j  ,i  ) =  0.0625*( &
                  + (    w0(k,j,i)*9+    wp(k,j,i)*3)*xc(k2,j2,i2) &
                  + (    w0(k,j,i)  +    wp(k,j,i)*3)*xc(k2,jp,i2) &
                  + ((1-w0(k,j,i))*9+(1-wp(k,j,i))*3)*xc(kp,j2,i2) &
                  + ((1-w0(k,j,i))  +(1-wp(k,j,i))*3)*xc(kp,jp,i2) )
          enddo
          xf(k  ,j  ,i  ) =  (1-hlf*dirichlet_flag)    &
               * ( d*xc(k2,j2,i2) + e*xc(k2,jp,i2) )
       enddo

    else
       ! this piece of code is never used (see condition above)
       ! it is kept in case someone wants to use it
       do j=1,ny*2
          j2 = (j+1)/2
          jp = j2-(mod(j,2)*2-1)
          do k=1,nz*2-1
             k2 = (k+1)/2
             kp = k2-(mod(k,2)*2-1)
             if(k.eq.1)kp=2
             ! bilinear interpolation based on indices location
             xf(k  ,j  ,i  ) =  &
                  +     a*xc(k2,j2  ,i2) + b*xc(k2,jp  ,i2) &
                  +     b*xc(kp,j2  ,i2) + c*xc(kp,jp  ,i2)

          enddo
          xf(k  ,j  ,i  ) =  (1-hlf*dirichlet_flag)    &
               * ( d*xc(k2,j2,i2) + e*xc(k2,jp,i2) )
       enddo

    endif
    
  end subroutine coarse2fine_yz

end module mg_intergrids
