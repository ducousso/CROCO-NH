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
  !---------------!
  !- FINE2COARSE -! fine to coarse grid
  !------------------------------------------------------------
  subroutine fine2coarse(lev)

    ! coarsen grid(lev)%r to grid(lev+1)%b and set grid(lev+1)%p=0

    integer(kind=ip), intent(in) :: lev

    real(kind=rp),dimension(:,:,:),pointer :: r
    real(kind=rp),dimension(:,:,:),pointer :: b

    integer(kind=ip) :: nx, ny, nz

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
!       if(myrank == 0) write(*,*)"gather lev=",lev+1,"nx,ny,nz=",nx,ny,nz
    else
       b => grid(lev+1)%b
!       if(myrank == 0) write(*,*)"F2C   lev=",lev+1,"nx,ny,nz=",nx,ny,nz
    endif


    if ((aggressive).and.(lev==1)) then
       call fine2coarse_aggressive(r,b,nx,ny,nz)

    elseif (grid(lev)%nz == 1) then
       call fine2coarse_2D(r,b,nx,ny)

    else
       call tic(lev,'fine2coarse_3D')

       call fine2coarse_3D(r,b,nx,ny,nz)

      call toc(lev,'fine2coarse_3D')

    end if

    if (grid(lev+1)%gather == 1) then
!       if(myrank == 0) write(*,*)" *** dummy3(1,1,1)=",b(1,1:ny,1:nx)
       r => grid(lev+1)%dummy3
       b => grid(lev+1)%b
       call gather(lev+1,r,b)
!       if(myrank == 0) write(*,*)" *** after gather **** b(1,1,1)=",grid(lev+1)%b(1,1,1)
    endif

    call fill_halo(lev+1,grid(lev+1)%b)

    grid(lev+1)%p = zero

  end subroutine fine2coarse

  !----------------------------------------
  subroutine fine2coarse_aggressive(x,y,nx,ny,nz)

    real(kind=rp)   , dimension(:,:,:), intent(in)    :: x !fine
    real(kind=rp)   , dimension(:,:,:), intent(inout) :: y ! coarse
    integer(kind=ip)                  , intent(in)    :: nx, ny, nz

    ! local
    integer(kind=ip):: i,j,k,k2

    do k=1,nz
       k2=(k-1)/8+1
       if(mod(k,8).eq.1)then
          do j=1,ny
             do i=1,nx               
                y(i,j,k2) = x(i,j,k) * eighth
             enddo
          enddo
       else
          do j=1,ny
             do i=1,nx               
                y(i,j,k2) = y(i,j,k2)+x(i,j,k) * eighth
             enddo
          enddo
       endif
    enddo

  end subroutine fine2coarse_aggressive

  !------------------------------------------------------------
  subroutine fine2coarse_2D(x,y,nx,ny)

    real(kind=rp),dimension(:,:,:),pointer,intent(in) :: x
    real(kind=rp),dimension(:,:,:),pointer,intent(out) :: y
    integer(kind=ip), intent(in) :: nx, ny

    integer(kind=ip) :: i,j,i2,j2
    integer(kind=ip) :: d

    d = size(x,1) ! vertical dimension of the fine level, can 2 or 1

   if(d==1)then
       ! x was already 2D
       do i2=1,nx
          i=2*i2-1
          do j2=1,ny
             j=2*j2-1      
             y(1,j2,i2) = (x(1,j,i)+x(1,j,i+1)+x(1,j+1,i)+x(1,j+1,i+1))
          enddo
       enddo
    else
       ! x was 3D
       do i2=1,nx
          i=2*i2-1
          do j2=1,ny
             j=2*j2-1     
             y(1,j2,i2) = (x(1,j,i)+x(1,j,i+1)+x(1,j+1,i)+x(1,j+1,i+1)&
                          +x(2,j,i)+x(2,j,i+1)+x(2,j+1,i)+x(2,j+1,i+1))
          enddo
       enddo
    endif

  end subroutine fine2coarse_2D

  !------------------------------------------------------------
  subroutine fine2coarse_3D(x,y,nx,ny,nz)
    !
    ! Fine2coarse 'x' from fine level l1 to 'y' on coarse level l2=l1+1
    real(kind=rp),dimension(:,:,:),pointer,intent(in) :: x
    real(kind=rp),dimension(:,:,:),pointer,intent(out) :: y
    integer(kind=ip), intent(in) :: nx, ny, nz
    ! local
    integer(kind=ip) :: i,j,k,i2,j2,k2
    real(kind=rp):: z

    do i2=1,nx
       i=2*i2-1
       do j2=1,ny
          j=2*j2-1
          do k2=1,nz
             k=2*k2-1
             z = x(k,j,i)  +x(k,j,i+1)  +x(k,j+1,i)  +x(k,j+1,i+1) &
                  + x(k+1,j,i)+x(k+1,j,i+1)+x(k+1,j+1,i)+x(k+1,j+1,i+1)
             y(k2,j2,i2) = z
          enddo
       enddo
    enddo

  end subroutine fine2coarse_3D

  !---------------!
  !- COARSE2FINE -! coarse to fine grid
  !------------------------------------------------------------
  subroutine coarse2fine(lev)

    ! interpolate grid(lev+1)%p to grid(lev)%r and add it to grid(lev)%p

    integer(kind=ip), intent(in) :: lev

    real(kind=rp),dimension(:,:,:),pointer :: rf
    real(kind=rp),dimension(:,:,:),pointer :: pc

    integer(kind=ip) :: nxc, nyc, nzc

    nxc = grid(lev+1)%nx
    nyc = grid(lev+1)%ny
    nzc = grid(lev+1)%nz

    pc => grid(lev+1)%p

    if (grid(lev+1)%gather == 1) then
       !       if(myrank==0)write(*,*)"SPLIT!!!"
       rf => grid(lev+1)%dummy3
       call split(lev+1,pc,rf)
       pc => grid(lev+1)%dummy3
       nxc = grid(lev+1)%nx / grid(lev+1)%ngx
       nyc = grid(lev+1)%ny / grid(lev+1)%ngy
       nzc = grid(lev+1)%nz       
    endif

    rf => grid(lev)%r

    if (trim(interp_type)=='nearest')  then

       if ((aggressive).and.(lev==1)) then

          call coarse2fine_aggressive(rf,pc,nxc,nyc,nzc)

       elseif (grid(lev)%nz == 1) then
          call coarse2fine_2D_nearest(rf,pc,nxc,nyc)
       else
          call coarse2fine_3D_nearest(rf,pc,nxc,nyc,nzc)
       end if

    elseif ( trim(interp_type)=='linear')then

       if ((aggressive).and.(lev==1)) then

          call coarse2fine_aggressive(rf,pc,nxc,nyc,nzc)

       elseif (grid(lev)%nz == 1) then

          call coarse2fine_2D_linear(rf,pc,nxc,nyc)
       else
          call coarse2fine_3D_linear(rf,pc,nxc,nyc,nzc)

       end if

    endif

    call fill_halo(lev,grid(lev)%r)

    grid(lev)%p = grid(lev)%p + grid(lev)%r

  end subroutine coarse2fine

  !------------------------------------------------------------
  subroutine coarse2fine_aggressive(x,y,nx,ny,nz)

    real(kind=rp),dimension(:,:,:),pointer,intent(in)  :: x
    real(kind=rp),dimension(:,:,:),pointer,intent(out) :: y
    integer(kind=ip),intent(in) :: nx, ny, nz

    !TODO
    integer(kind=ip) ::idum ! line to remove
    idum = nx               ! line to remove
    idum = ny               ! line to remove
    idum = nz               ! line to remove
    y = x                   ! line to remove
    write(*,*)'Error:  coarse2fine_aggressive not available yet !'
    stop -1
    !TODO

  end subroutine coarse2fine_aggressive

  !------------------------------------------------------------
  subroutine coarse2fine_2D_nearest(xf,xc,nx,ny)

    real(kind=rp),dimension(:,:,:),pointer,intent(out):: xf
    real(kind=rp),dimension(:,:,:),pointer,intent(in) :: xc
    integer(kind=ip),intent(in) :: nx, ny

    ! local
    integer(kind=ip) :: i,j,i2,j2
    integer(kind=ip) :: d

    d = size(xf,1) ! vertical dimension of the fine level, can 2 or 1

    if(d==1)then
       ! xf is also 2D
       do i2=1,nx
          i=2*i2-1
          do j2=1,ny
             j=2*j2-1
             xf(1,j  ,i  ) = xc(1,j2,i2)
             xf(1,j+1,i  ) = xc(1,j2,i2)
             xf(1,j  ,i+1) = xc(1,j2,i2)
             xf(1,j+1,i+1) = xc(1,j2,i2)
          enddo
       enddo
    else
       ! xf is 3D
       do i2=1,nx
          i=2*i2-1
          do j2=1,ny
             j=2*j2-1
             xf(1,j  ,i  ) = xc(1,j2,i2)
             xf(2,j  ,i  ) = xc(1,j2,i2)
             xf(1,j+1,i  ) = xc(1,j2,i2)
             xf(2,j+1,i  ) = xc(1,j2,i2)
             xf(1,j  ,i+1) = xc(1,j2,i2)
             xf(2,j  ,i+1) = xc(1,j2,i2)
             xf(1,j+1,i+1) = xc(1,j2,i2)
             xf(2,j+1,i+1) = xc(1,j2,i2)
          enddo
       enddo
    endif

  end subroutine coarse2fine_2D_nearest

  !------------------------------------------------------------
  subroutine coarse2fine_2D_linear(xf,xc,nx,ny)

    real(kind=rp),dimension(:,:,:),pointer,intent(out) :: xf
    real(kind=rp),dimension(:,:,:),pointer,intent(in)  :: xc
    integer(kind=ip),intent(in) :: nx, ny

    ! local
  integer(kind=ip) :: i,j,i2,j2,k,k2
  real(kind=rp) :: a,b,c
  !
  ! weights for bilinear in (i,j)
  a = 9._8 / 16._8
  b = 3._8 / 16._8
  c = 1._8 / 16._8

  k  = 1
  k2 = 1

  do i2=1,nx
     i=2*i2-1
     do j2=1,ny
        j=2*j2-1
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
     enddo
  enddo


  end subroutine coarse2fine_2D_linear

  !------------------------------------------------------------
  subroutine coarse2fine_3D_nearest(xf,xc,nx,ny,nz)

    real(kind=rp),dimension(:,:,:),pointer,intent(out) :: xf
    real(kind=rp),dimension(:,:,:),pointer,intent(in)  :: xc
    integer(kind=ip),intent(in) :: nx, ny, nz

    ! local
    integer(kind=ip) :: i,j,k,i2,j2,k2
    ! 
    do i2=1,nx
       i=2*i2-1
       do j2=1,ny
          j=2*j2-1
          do k2=1,nz
             k=2*k2-1
             xf(k  ,j  ,i  ) = xc(k2,j2,i2)
             xf(k+1,j  ,i  ) = xc(k2,j2,i2)
             xf(k  ,j+1,i  ) = xc(k2,j2,i2)
             xf(k+1,j+1,i  ) = xc(k2,j2,i2)
             xf(k  ,j  ,i+1) = xc(k2,j2,i2)
             xf(k+1,j  ,i+1) = xc(k2,j2,i2)
             xf(k  ,j+1,i+1) = xc(k2,j2,i2)
             xf(k+1,j+1,i+1) = xc(k2,j2,i2)
          enddo
       enddo
    enddo

  end subroutine coarse2fine_3D_nearest

  !------------------------------------------------------------
  subroutine coarse2fine_3D_linear(xf,xc,nx,ny,nz)

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
             ! kp = k2+1 for k=2,4 ..
             ! kp = k2-1 for k=3,5 ..
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
          xf(k  ,j  ,i  ) =  (1-hlf*dirichlet_flag)  * ( &
               a * xc(k2,j2  ,i2) + c * xc(k2,j2-1,i2-1) +   &
               b * xc(k2,j2-1,i2) + b * xc(k2,j2  ,i2-1)   )
          xf(k  ,j+1,i  ) =  (1-hlf*dirichlet_flag) * ( &
               a * xc(k2,j2  ,i2) + c * xc(k2,j2+1,i2-1) +   &
               b * xc(k2,j2+1,i2) + b * xc(k2,j2  ,i2-1)   )
          xf(k  ,j  ,i+1) =  (1-hlf*dirichlet_flag)     * ( &
               a * xc(k2,j2  ,i2) + c * xc(k2,j2-1,i2+1) +   &
               b * xc(k2,j2-1,i2) + b * xc(k2,j2  ,i2+1)   ) 
          xf(k  ,j+1,i+1) =  (1-hlf*dirichlet_flag)      * ( &
               a * xc(k2,j2  ,i2) + c * xc(k2,j2+1,i2+1) +   &
               b * xc(k2,j2+1,i2) + b * xc(k2,j2  ,i2+1)   )
       enddo
    enddo

  end subroutine coarse2fine_3D_linear

end module mg_intergrids
