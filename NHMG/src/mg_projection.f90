module mg_projection

  use mg_cst
  use mg_mpi
  use mg_tictoc
  use mg_namelist
  use mg_grids
  use mg_mpi_exchange
  use mg_gather
  use mg_netcdf_out

  implicit none

contains
  !-----------------------------------------------------------------------------------
  subroutine set_matrices()

    ! Define matrix coefficients cA
    ! Coefficients are stored in order of diagonals
    ! cA(1,:,:,:)      -> p(k,j,i)
    ! cA(2,:,:,:)      -> p(k-1,j,i)
    ! cA(3,:,:,:)      -> p(k+1,j-1,i)
    ! cA(4,:,:,:)      -> p(k,j-1,i)
    ! cA(5,:,:,:)      -> p(k-1,j-1,i)
    ! cA(6,:,:,:)      -> p(k+1,j,i-1)
    ! cA(7,:,:,:)      -> p(k,j,i-1)
    ! cA(8,:,:,:)      -> p(k-1,j,i-1)

    integer(kind=ip) :: lev
    integer(kind=ip) :: k,j,i
    integer(kind=ip) :: k2,j2,i2
    integer(kind=ip) :: nx,ny,nz

    real(kind=rp), dimension(:,:),     pointer :: dx,dy
    real(kind=rp), dimension(:,:),     pointer :: dxu,dyv
    real(kind=rp), dimension(:,:,:),   pointer :: dzw
    real(kind=rp), dimension(:,:,:),   pointer :: Arx,Ary
    real(kind=rp), dimension(:,:)  ,   pointer :: Arz
    real(kind=rp), dimension(:,:,:),   pointer :: zxdy,zydx
    real(kind=rp), dimension(:,:,:),   pointer :: alpha
    real(kind=rp), dimension(:,:),     pointer :: beta
    real(kind=rp), dimension(:,:),     pointer :: gamu
    real(kind=rp), dimension(:,:),     pointer :: gamv
    real(kind=rp), dimension(:,:,:,:), pointer :: cA,cAf

    integer(kind=ip) :: dirichlet_flag 
    integer(kind=ip) :: cffx, cffy

    if (surface_neumann) then
       dirichlet_flag = 0
    else
       dirichlet_flag = 1
    endif

    do lev = 1, nlevs

       nx = grid(lev)%nx
       ny = grid(lev)%ny
       nz = grid(lev)%nz

       dx    => grid(lev)%dx    !
       dy    => grid(lev)%dy    !
       dxu   => grid(lev)%dxu   !
       dyv   => grid(lev)%dyv   !
       dzw   => grid(lev)%dzw   !
       Arx   => grid(lev)%Arx   !
       Ary   => grid(lev)%Ary   !
       Arz   => grid(lev)%Arz   !
       alpha => grid(lev)%alpha !
       beta  => grid(lev)%beta  !
       gamu  => grid(lev)%gamu  !
       gamv  => grid(lev)%gamv  !
       zxdy  => grid(lev)%zxdy  !
       zydx  => grid(lev)%zydx  !
       cA    => grid(lev)%cA    !

       if(lev.ge.1)then
       !! interaction coeff with neighbours !!

       !---------------!
       !- lower level -!
       !---------------!
       k = 1
       do i = 1,nx
          do j = 1,ny+1
             ! couples with k+1,j-1
             cA(3,k,j,i) = qrt * ( zydx(k+1,j,i) + zydx(k,j-1,i) )
             ! couples with j-1
             cA(4,k,j,i) = hlf * (gamv(j,i) + gamv(j-1,i)) * Ary(k,j,i) / dyv(j,i) &
                  - qrt * ( zydx(k,j-1,i) - zydx(k,j,i) )
!             cA(4,k,j,i) =  Ary(k,j,i) / dyv(j,i) 
          enddo
       enddo

       do i = 1,nx+1
          do j = 1,ny
             ! couples with k+1,i-1
             cA(6,k,j,i) = qrt * ( zxdy(k+1,j,i) + zxdy(k,j,i-1) )
             ! couples with i-1
             cA(7,k,j,i) = hlf * (gamu(j,i) + gamu(j,i-1))  * Arx(k,j,i) / dxu(j,i) &
                  - qrt * ( zxdy(k,j,i-1) - zxdy(k,j,i) )
!             cA(7,k,j,i) = Arx(k,j,i) / dxu(j,i)
          enddo
       enddo

       do i = 1,nx+1
          do j = 0,ny
             ! only for k==1, couples with j+1,i-1
             cA(5,k,j,i) = beta(j,i-1) + beta(j+1,i)
          enddo
       enddo

       do i = 1,nx+1
          do j = 1,ny+1
             ! only for k==1, couples with j-1,i-1
             cA(8,k,j,i) = - beta(j,i-1) - beta(j-1,i)
          enddo
       enddo

       !-------------------!
       !- interior levels -!
       !-------------------!
       do i = 1,nx
          do j = 1,ny
             do k = 2,nz-1 
                ! couples with k-1
                cA(2,k,j,i) = &
                     ( Arz(j,i) / dzw(k,j,i) ) * &
                     hlf * (alpha(k,j,i)+alpha(k-1,j,i))
             enddo
          enddo
       enddo

       do i = 1,nx
          do j = 1,ny+1
             do k = 2,nz-1 
                ! couples with k+1,j-1
                cA(3,k,j,i) = qrt * ( zydx(k+1,j,i) + zydx(k,j-1,i) )
                ! couples with j-1
                cA(4,k,j,i) =  Ary(k,j,i) / dyv(j,i) 
                ! couples with k-1,j-1
                cA(5,k,j,i) = - qrt * ( zydx(k-1,j,i) + zydx(k,j-1,i) )
             enddo
          enddo
       enddo

       do i = 1,nx+1
          do j = 1,ny 
             do k = 2,nz-1 
                ! couples with k+1,i-1
                cA(6,k,j,i) = qrt * ( zxdy(k+1,j,i) + zxdy(k,j,i-1) )
                ! couples with i-1
                cA(7,k,j,i) = Arx(k,j,i) / dxu(j,i) 
                ! couples with k-1,i-1
                cA(8,k,j,i) = - qrt * ( zxdy(k-1,j,i) + zxdy(k,j,i-1) )
             enddo
          enddo
       enddo

       !---------------!
       !- upper level -!
       !---------------!
       k = nz
       do i = 1,nx    
          do j = 1,ny 
             ! couples with k-1
             cA(2,k,j,i) = (Arz(j,i)/dzw(k,j,i)) * hlf * ( alpha(k,j,i) + alpha(k-1,j,i) )
          enddo
       enddo

       do i = 1,nx
          do j = 1,ny+1
             ! couples with j-1
             cA(4,k,j,i) = Ary(k,j,i) / dyv(j,i) &
                  + qrt * ( - zydx(k,j-1,i) + zydx(k,j,i) ) *(2*dirichlet_flag-1) 
             ! couples with k-1,j-1
             cA(5,k,j,i) = - qrt * ( zydx(k-1,j,i) + zydx(k,j-1,i) )
          enddo
       enddo

       do i = 1,nx+1
          do j = 1,ny 
             ! with Neumann BC, the CA(7,:,:) has flip signed on the slope term
             ! this will be in the paper
             ! couples with i-1
             cA(7,k,j,i) = Arx(k,j,i) / dxu(j,i) &
                  + qrt * ( -zxdy(k,j,i-1) + zxdy(k,j,i) )*(2*dirichlet_flag-1) 
             ! couples with k-1,i-1
             cA(8,k,j,i) = - qrt * ( zxdy(k-1,j,i) + zxdy(k,j,i-1) )
          enddo
       enddo

       call fill_halo(lev,cA)

       !! self-interaction coeff !!
       if (trim(grid(lev)%relaxation_method).eq.'xyz') then
       do i = 1,nx
          do j = 1,ny

             k = 1 !lower level
             cA(1,k,j,i) = &
                  - Arz(j,i) / dzw(k+1,j,i) * hlf * (alpha(k+1,j,i) + alpha(k,j,i)) &
                  - Arx(k,j,i  )/dxu(j,i  ) * hlf * (gamu(j,i) + gamu(j  ,i-1)) &
                  - Arx(k,j,i+1)/dxu(j,i+1) * hlf * (gamu(j,i) + gamu(j  ,i+1)) &
                  - Ary(k,j  ,i)/dyv(j  ,i) * hlf * (gamv(j,i) + gamv(j-1,i  )) &
                  - Ary(k,j+1,i)/dyv(j+1,i) * hlf * (gamv(j,i) + gamv(j+1,i  ))

             do k = 2,nz-1 !interior levels
                cA(1,k,j,i) = &
                     - Arz(j,i) / dzw(k+1,j,i) * hlf * (alpha(k+1,j,i) + alpha(k,j,i)) &
                     - Arz(j,i) / dzw(k  ,j,i) * hlf * (alpha(k-1,j,i) + alpha(k,j,i)) &
                     - Arx(k,j,i  )/dxu(j,i  )  &
                     - Arx(k,j,i+1)/dxu(j,i+1)  &
                     - Ary(k,j  ,i)/dyv(j  ,i)  &
                     - Ary(k,j+1,i)/dyv(j+1,i)
             enddo

             k=nz ! upper level
             cA(1,k,j,i) = &
                  - Arz(j,i) / dzw(k+1,j,i) * alpha(k,j,i) * dirichlet_flag &
                  - Arz(j,i) / dzw(k  ,j,i) * hlf * (alpha(k-1,j,i) + alpha(k,j,i)) &
                  - Arx(k,j,i  )/dxu(j,i  )  &
                  - Arx(k,j,i+1)/dxu(j,i+1)  &
                  - Ary(k,j  ,i)/dyv(j  ,i)  &
                  - Ary(k,j+1,i)/dyv(j+1,i)

          enddo
       enddo
       elseif (trim(grid(lev)%relaxation_method).eq.'xz') then
       do i = 1,nx
          j = 1

             k = 1 !lower level
             cA(1,k,j,i) = &
                  - Arz(j,i) / dzw(k+1,j,i) * hlf * (alpha(k+1,j,i) + alpha(k,j,i)) &
                  - Arx(k,j,i  )/dxu(j,i  ) * hlf * (gamu(j,i) + gamu(j  ,i-1)) &
                  - Arx(k,j,i+1)/dxu(j,i+1) * hlf * (gamu(j,i) + gamu(j  ,i+1)) 

             do k = 2,nz-1 !interior levels
                cA(1,k,j,i) = &
                     - Arz(j,i) / dzw(k+1,j,i) * hlf * (alpha(k+1,j,i) + alpha(k,j,i)) &
                     - Arz(j,i) / dzw(k  ,j,i) * hlf * (alpha(k-1,j,i) + alpha(k,j,i)) &
                     - Arx(k,j,i  )/dxu(j,i  )  &
                     - Arx(k,j,i+1)/dxu(j,i+1)  
             enddo

             k=nz ! upper level
             cA(1,k,j,i) = &
                  - Arz(j,i) / dzw(k+1,j,i) * alpha(k,j,i) * dirichlet_flag &
                  - Arz(j,i) / dzw(k  ,j,i) * hlf * (alpha(k-1,j,i) + alpha(k,j,i)) &
                  - Arx(k,j,i  )/dxu(j,i  )  &
                  - Arx(k,j,i+1)/dxu(j,i+1)  

       enddo
       elseif (trim(grid(lev)%relaxation_method).eq.'yz') then
       i = 1
          do j = 1,ny

             k = 1 !lower level
             cA(1,k,j,i) = &
                  - Arz(j,i) / dzw(k+1,j,i) * hlf * (alpha(k+1,j,i) + alpha(k,j,i)) &
                  - Ary(k,j  ,i)/dyv(j  ,i) * hlf * (gamv(j,i) + gamv(j-1,i  )) &
                  - Ary(k,j+1,i)/dyv(j+1,i) * hlf * (gamv(j,i) + gamv(j+1,i  ))

             do k = 2,nz-1 !interior levels
                cA(1,k,j,i) = &
                     - Arz(j,i) / dzw(k+1,j,i) * hlf * (alpha(k+1,j,i) + alpha(k,j,i)) &
                     - Arz(j,i) / dzw(k  ,j,i) * hlf * (alpha(k-1,j,i) + alpha(k,j,i)) &
                     - Ary(k,j  ,i)/dyv(j  ,i)  &
                     - Ary(k,j+1,i)/dyv(j+1,i)
             enddo

             k=nz ! upper level
             cA(1,k,j,i) = &
                  - Arz(j,i) / dzw(k+1,j,i) * alpha(k,j,i) * dirichlet_flag &
                  - Arz(j,i) / dzw(k  ,j,i) * hlf * (alpha(k-1,j,i) + alpha(k,j,i)) &
                  - Ary(k,j  ,i)/dyv(j  ,i)  &
                  - Ary(k,j+1,i)/dyv(j+1,i)

          enddo

       endif

    else ! if lev.eq.1
       cAf    => grid(lev-1)%cA    !

      do i2=1,nx
       i=2*i2-1
       do j2=1,ny
          j=2*j2-1
          do k2=2,nz
             k=2*k2-1
             cA(2,k2,j2,i2) = cAf(2,k,j,i)+cAf(2,k,j,i+1)+cAf(2,k,j+1,i)+cAf(2,k,j+1,i+1)&
                  +cAf(3,k-1,j+1,i)+cAf(3,k-1,j+1,i+1) &
                  +cAf(5,k  ,j+1,i)+cAf(5,k  ,j+1,i+1) &
                  +cAf(6,k-1,j,i+1)+cAf(6,k-1,j+1,i+1) &
                  +cAf(8,k  ,j,i+1)+cAf(8,k  ,j+1,i+1)
          enddo
       enddo
    enddo
      do i2=1,nx
       i=2*i2-1
       do j2=1,ny+1
          j=2*j2-1
          do k2=1,nz
             k=2*k2-1
             cA(3,k2,j2,i2) = cAf(3,k+1,j,i)+cAf(3,k+1,j,i+1)
             cA(4,k2,j2,i2) = cAf(4,k,j,i)+cAf(4,k,j,i+1)+cAf(4,k+1,j,i)+cAf(4,k+1,j,i+1)&
                  +cAf(3,k  ,j,i)+cAf(3,k  ,j,i+1) &
                  +cAf(5,k+1,j,i)+cAf(5,k+1,j,i+1)
             cA(5,k2,j2,i2) = cAf(5,k,j,i)+cAf(5,k,j,i+1)
          enddo
       enddo
    enddo
      do i2=1,nx+1
       i=2*i2-1
       do j2=1,ny
          j=2*j2-1
          do k2=1,nz
             k=2*k2-1
             cA(6,k2,j2,i2) = cAf(6,k+1,j,i)+cAf(6,k+1,j+1,i)
             cA(7,k2,j2,i2) = cAf(7,k,j,i)+cAf(7,k,j+1,i)+cAf(7,k+1,j,i)+cAf(7,k+1,j+1,i)&
                  +cAf(6,k  ,j,i)+cAf(6,k  ,j+1,i) &
                  +cAf(8,k+1,j,i)+cAf(8,k+1,j+1,i)
             cA(8,k2,j2,i2) = cAf(8,k,j,i)+cAf(8,k,j+1,i)
          enddo
       enddo
    enddo
    ! bottom cross coefficients
    k2=1
    k=2*k2-1
    do i2=1,nx
       i=2*i2-1
       do j2=1,ny
          j=2*j2-1
          cA(5,k2,j2,i2) = cAf(5,k,j+1,i)
          cA(8,k2,j2,i2) = cAf(8,k,j,i)
          cA(4,k2,j2,i2) = cA(4,k2,j2,i2) + cAf(5,k,j-1,i+1) + cAf(8,k,j,i+1)
          cA(7,k2,j2,i2) = cA(7,k2,j2,i2) + cAf(5,k,j,i) + cAf(8,k,j+1,i)
       enddo
    enddo
    
    
    !cA = cA*0.5    
!!$    do i=1,nx
!!$       do j=1,ny
!!$          k=1
!!$             cA(1,k,j,i) = -(            cA(2,k+1,j  ,i  ) &
!!$                            +cA(3,k,j,i)                   &
!!$                            +cA(4,k,j,i)+cA(4,k  ,j+1,i  ) &
!!$                                        +cA(5,k+1,j+1,i  ) &
!!$                            +cA(6,k,j,i)                   &
!!$                            +cA(7,k,j,i)+cA(7,k  ,j  ,i+1) &
!!$                                        +cA(8,k+1,j  ,i+1) &
!!$                            +cA(5,k,j,i)+cA(5,k  ,j-1,i+1) &
!!$                            +cA(8,k,j,i)+cA(8,k  ,j+1,i+1) )
!!$          do k=2,nz-1
!!$             cA(1,k,j,i) = -(cA(2,k,j,i)+cA(2,k+1,j  ,i  ) &
!!$                            +cA(3,k,j,i)+cA(3,k-1,j+1,i  ) &
!!$                            +cA(4,k,j,i)+cA(4,k  ,j+1,i  ) &
!!$                            +cA(5,k,j,i)+cA(5,k+1,j+1,i  ) &
!!$                            +cA(6,k,j,i)+cA(6,k-1,j  ,i+1) &
!!$                            +cA(7,k,j,i)+cA(7,k  ,j  ,i+1) &
!!$                            +cA(8,k,j,i)+cA(8,k+1,j  ,i+1) )
!!$          enddo
!!$          k=nz
!!$          cA(1,k,j,i) = -(cA(2,k,j,i) &
!!$               + Arz(j,i) / dzw(k+1,j,i)* alpha(k,j,i)  * dirichlet_flag&
!!$                         +cA(4,k,j,i)+cA(4,k,j+1,i) &
!!$                         +cA(7,k,j,i)+cA(7,k,j,i+1))
!!$       enddo
!!$    enddo

    k2=nz
    do k2=1,nz
       k=2*k2-1
       do i2=1,nx
          i=2*i2-1
          do j2=1,ny
             j=2*j2-1
             cA(1,k2,j2,i2) = &
                  +cAf(1,k  ,j,i)+cAf(1,k  ,j,i+1)+cAf(1,k  ,j+1,i)+cAf(1,k  ,j+1,i+1)&
                  +cAf(1,k+1,j,i)+cAf(1,k+1,j,i+1)+cAf(1,k+1,j+1,i)+cAf(1,k+1,j+1,i+1)&
            +2*(  +cAf(2,k+1,j,i)+cAf(2,k+1,j,i+1)+cAf(2,k+1,j+1,i)+cAf(2,k+1,j+1,i+1)&
                  +cAf(3,k,j+1,i)+cAf(3,k,j+1,i+1) &
                  +cAf(4,k,j+1,i)+cAf(4,k,j+1,i+1)+cAf(4,k+1,j+1,i)+cAf(4,k+1,j+1,i+1)&
                  +cAf(5,k+1,j+1,i)+cAf(5,k+1,j+1,i+1) &
                  +cAf(6,k,j,i+1)+cAf(6,k,j+1,i+1) &
                  +cAf(7,k,j,i+1)+cAf(7,k,j+1,i+1)+cAf(7,k+1,j,i+1)+cAf(7,k+1,j+1,i+1)&
                  +cAf(8,k+1,j,i+1)+cAf(8,k+1,j+1,i+1) )
             if(k2.eq.1)then
                cA(1,k2,j2,i2) = cA(1,k2,j2,i2) &
            +2*(   cAf(5,k,j,i+1)+cAf(8,k,j+1,i+1))
             endif
             !cA(1,k2,j2,i2) = cA(1,k2,j2,i2)*0.5
          enddo
       enddo
    enddo

     call fill_halo(lev,cA)
     cA = cA*0.5
!!$     cA(3,:,:,:) = 2*cA(3,:,:,:)
!!$     cA(5,:,:,:) = 2*cA(5,:,:,:)
!!$     cA(6,:,:,:) = 2*cA(6,:,:,:)
!!$     cA(8,:,:,:) = 2*cA(8,:,:,:)
!!$     cA(5,1,:,:) = 2*cA(5,1,:,:)
!!$     cA(8,1,:,:) = 2*cA(8,1,:,:)
 
    endif
       
       if (netcdf_output) then
          if (myrank==0) write(*,*)'       write cA in a netcdf file'
          call write_netcdf(grid(lev)%cA,vname='ca',netcdf_file_name='cA.nc',rank=myrank,iter=lev)
       endif

    enddo

  end subroutine set_matrices

  !-------------------------------------------------------------------------     
  subroutine correction_uvw()

    !! u,v,w are fluxes, the correction is T*grad(p)

    integer(kind=ip):: k, j, i
    integer(kind=ip):: nx, ny, nz

    real(kind=rp) :: gamma
    real(kind=rp), dimension(:,:)  , pointer :: dx,dy
    real(kind=rp), dimension(:,:)  , pointer :: dxu,dyv
    real(kind=rp), dimension(:,:)  , pointer :: Arz
    real(kind=rp), dimension(:,:,:), pointer :: dzw
    real(kind=rp), dimension(:,:,:), pointer :: Arx,Ary
    real(kind=rp), dimension(:,:,:), pointer :: zxdy,zydx
    real(kind=rp), dimension(:,:,:), pointer :: alpha
    real(kind=rp), dimension(:,:)  , pointer :: beta
    real(kind=rp), dimension(:,:,:), pointer :: p
    real(kind=rp), dimension(:,:,:), pointer :: px,py,pz
    real(kind=rp), dimension(:,:,:), pointer :: du,dv,dw

    integer(kind=ip) :: dirichlet_flag
    real(kind=rp) :: cff
    
!    if (myrank==0) write(*,*)'   - compute pressure gradient and translate to fluxes'

    if (surface_neumann) then
       dirichlet_flag = 0
    else
       dirichlet_flag = 1
    endif

    nx = grid(1)%nx
    ny = grid(1)%ny
    nz = grid(1)%nz

    dx    => grid(1)%dx
    dy    => grid(1)%dy
    dxu   => grid(1)%dxu
    dyv   => grid(1)%dyv
    dzw   => grid(1)%dzw
    Arx   => grid(1)%Arx
    Ary   => grid(1)%Ary
    Arz   => grid(1)%Arz
    alpha => grid(1)%alpha
    beta  => grid(1)%beta
    zxdy  => grid(1)%zxdy
    zydx  => grid(1)%zydx
    p     => grid(1)%p

    !! Pressure gradient -

    px => grid(1)%px
    py => grid(1)%py
    pz => grid(1)%pz

    if (trim(grid(1)%relaxation_method).eq.'yz')then
       do i = 1,nx+1
          do j = 0,ny+1
             do k = 1,nz
                px(k,j,i) = 0.
             enddo
          enddo
       enddo
    else
       do i = 1,nx+1
          do j = 0,ny+1
             cff = 1./dxu(j,i)
             do k = 1,nz
                px(k,j,i) = - (p(k,j,i)-p(k,j,i-1))*cff
             enddo
          enddo
       enddo
    endif
    if (trim(grid(1)%relaxation_method).eq.'xz')then
       do i = 0,nx+1
          do j = 0,ny+1 
             do k = 1,nz
                py(k,j,i) = 0.
             enddo
          enddo
       enddo
    else
       do i = 0,nx+1
          do j = 1,ny+1
             cff=1./dyv(j,i)
             do k = 1,nz
                py(k,j,i) = -(p(k,j,i)-p(k,j-1,i))*cff
             enddo
          enddo
       enddo
    endif

    do i = 0,nx+1
       do j = 0,ny+1

          k = 1 !bottom pressure gradient is undefined
          pz(k,j,i) = 9999999999.

          do k = 2,nz !interior levels
             pz(k,j,i) = - (p(k,j,i)-p(k-1,j,i))/dzw(k,j,i)
          enddo

          k = nz+1 !surface
          pz(k,j,i) =  p(k-1,j,i)/dzw(k,j,i) * dirichlet_flag

       enddo
    enddo

    !! Correction for U -

    du => grid(1)%du

    if (trim(grid(1)%relaxation_method).eq.'yz')then
       do i = 1,nx+1  
          do j = 1,ny
             do k = 1,nz
                du(k,j,i) = 0.
             enddo
          enddo
       enddo
    else
       do i = 1,nx+1  
          do j = 1,ny 
             k = 1
             gamma = one - qrt * ( &
                  (zxdy(k,j,i  )/dy(j,i  ))**2/alpha(k,j,i  ) + &
                  (zxdy(k,j,i-1)/dy(j,i-1))**2/alpha(k,j,i-1) )
             du(k,j,i) = gamma * Arx(k,j,i) * px(k,j,i) &
                  - qrt * ( &
                  + zxdy(k,j,i  ) * dzw(k+1,j,i  ) * pz(k+1,j,i  ) &
                  + zxdy(k,j,i-1) * dzw(k+1,j,i-1) * pz(k+1,j,i-1) )  &
                  - beta(j,i-1)   * dyv(j  ,i-1)   * py(k,j  ,i-1) &
                  - beta(j,i-1)   * dyv(j+1,i-1)   * py(k,j+1,i-1) &
                  - beta(j,i  )   * dyv(j  ,i  )   * py(k,j  ,i  ) &
                  - beta(j,i  )   * dyv(j+1,i  )   * py(k,j+1,i  )

             do k = 2,nz-1 
                du(k,j,i) = Arx(k,j,i) * px(k,j,i) &
                     - qrt * ( &
                     + zxdy(k,j,i  ) * dzw(k  ,j,i  ) * pz(k  ,j,i  ) &
                     + zxdy(k,j,i  ) * dzw(k+1,j,i  ) * pz(k+1,j,i  ) &
                     + zxdy(k,j,i-1) * dzw(k  ,j,i-1) * pz(k  ,j,i-1) &
                     + zxdy(k,j,i-1) * dzw(k+1,j,i-1) * pz(k+1,j,i-1) )
             enddo

             k = nz
             du(k,j,i) = Arx(k,j,i) * px(k,j,i) &
                  - qrt * ( &
                  + zxdy(k,j,i  ) *       dzw(k  ,j,i  ) * pz(k  ,j,i  ) &
                  + zxdy(k,j,i  ) * two * dzw(k+1,j,i  ) * pz(k+1,j,i  ) * dirichlet_flag &
                  + zxdy(k,j,i-1) *       dzw(k  ,j,i-1) * pz(k  ,j,i-1) &
                  + zxdy(k,j,i-1) * two * dzw(k+1,j,i-1) * pz(k+1,j,i-1) * dirichlet_flag )
          enddo
       enddo
    endif
    !! Correction for V - 

    dv => grid(1)%dv

    if (trim(grid(1)%relaxation_method).eq.'xz')then
       do i = 0,nx+1
          do j = 0,ny+1 
             do k = 1,nz
                dv(k,j,i) = 0.
             enddo
          enddo
       enddo
    else
       do i = 1,nx
          do j = 1,ny+1
             k = 1
             gamma = one - qrt * (  &
                  (zydx(k,j  ,i)/dx(j  ,i))**2/alpha(k,j  ,i  ) + &
                  (zydx(k,j-1,i)/dx(j-1,i))**2/alpha(k,j-1,i) )
             dv(k,j,i) = gamma * Ary(k,j,i) * py(k,j,i) &
                  - qrt * ( &
                  + zydx(k,j  ,i) * dzw(k+1,j  ,i) * pz(k+1,j  ,i) &
                  + zydx(k,j-1,i) * dzw(k+1,j-1,i) * pz(k+1,j-1,i) ) &
                  - beta(j-1,i)   * dxu(j-1,i  )   * px(k,j-1,i  ) &
                  - beta(j-1,i)   * dxu(j-1,i+1)   * px(k,j-1,i+1) &
                  - beta(j  ,i)   * dxu(j  ,i  )   * px(k,j  ,i  ) &
                  - beta(j  ,i)   * dxu(j  ,i+1)   * px(k,j  ,i+1)

             do k = 2,nz-1
                dv(k,j,i) =  Ary(k,j,i) * py(k,j,i) &
                     - qrt * ( &
                     + zydx(k,j  ,i) * dzw(k  ,j  ,i) * pz(k  ,j  ,i) &
                     + zydx(k,j  ,i) * dzw(k+1,j  ,i) * pz(k+1,j  ,i) &
                     + zydx(k,j-1,i) * dzw(k  ,j-1,i) * pz(k  ,j-1,i) &
                     + zydx(k,j-1,i) * dzw(k+1,j-1,i) * pz(k+1,j-1,i) )
             enddo

             k = nz
             dv(k,j,i) = Ary(k,j,i) * py(k,j,i) &
                  - qrt * ( &
                  + zydx(k,j  ,i)       * dzw(k  ,j  ,i) * pz(k  ,j  ,i) &
                  + zydx(k,j  ,i) * two * dzw(k+1,j  ,i) * pz(k+1,j  ,i) * dirichlet_flag &
                  + zydx(k,j-1,i)       * dzw(k  ,j-1,i) * pz(k  ,j-1,i) &
                  + zydx(k,j-1,i) * two * dzw(k+1,j-1,i) * pz(k+1,j-1,i) * dirichlet_flag) 

          enddo
       enddo
    endif
    !! Correction for W -

    dw => grid(1)%dw

    do i = 1,nx
       do j = 1,ny

          do k = 2,nz
             dw(k,j,i) =  hlf * (alpha(k-1,j,i) + alpha(k,j,i)) * Arz(j,i) * pz(k,j,i) &
                  - qrt * ( &
                  + zxdy(k  ,j,i) * dxu(j,i  ) * px(k  ,j,i  ) &
                  + zxdy(k  ,j,i) * dxu(j,i+1) * px(k  ,j,i+1) &
                  + zxdy(k-1,j,i) * dxu(j,i  ) * px(k-1,j,i  ) &
                  + zxdy(k-1,j,i) * dxu(j,i+1) * px(k-1,j,i+1) ) &
                  - qrt * ( &
                  + zydx(k  ,j,i) * dyv(j  ,i) * py(k  ,j  ,i) &
                  + zydx(k  ,j,i) * dyv(j+1,i) * py(k  ,j+1,i) &
                  + zydx(k-1,j,i) * dyv(j  ,i) * py(k-1,j  ,i) &
                  + zydx(k-1,j,i) * dyv(j+1,i) * py(k-1,j+1,i) )
          enddo

          k = nz+1 
          dw(k,j,i) = alpha(k-1,j,i) * Arz(j,i) * pz(k,j,i) &
               - hlf * ( &
               + zxdy(k-1,j,i) * dxu(j,i  ) * px(k-1,j,i  ) &
               + zxdy(k-1,j,i) * dxu(j,i+1) * px(k-1,j,i+1) ) &
               - hlf * ( &
               + zydx(k-1,j,i) * dyv(j  ,i) * py(k-1,j  ,i) &
               + zydx(k-1,j,i) * dyv(j+1,i) * py(k-1,j+1,i) )

          ! for Neumann BC, dw at the top level should be exactly 0
          dw(k,j,i) = dw(k,j,i) * dirichlet_flag
       enddo
    enddo

  end subroutine correction_uvw

end module mg_projection

