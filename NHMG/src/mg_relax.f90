module mg_relax

  use mg_cst
  use mg_mpi
  use mg_tictoc
  use mg_namelist
  use mg_grids
  use mg_mpi_exchange

  implicit none

  ! this module provides
  !
  ! - relax(lev, nsweeps)
  ! - compute_residual(lev,res)  => mg(lev)%r + computes the its L2-norm
  !
  ! which are wrappers for
  !
  ! - relax_xyz_GS 
  ! - relax_xyz_RB <= recommended in 3D
  ! - relax_xyz_FC
  ! - relax_xyz_IRB
  !
  ! - relax_xz_GS <= recommended in 2D xz
  ! - relax_xz_RB
  !
  ! - compute_residual_xyz
  ! - compute_residual_xz
  !
  ! 
  
  
contains

  !----------------------------------------
  subroutine relax(lev,nsweeps)
    integer(kind=ip), intent(in):: lev
    integer(kind=ip), intent(in):: nsweeps

    real(kind=rp),dimension(:,:,:), pointer:: p
    real(kind=rp),dimension(:,:,:), pointer:: b
    real(kind=rp),dimension(:,:,:,:), pointer:: cA

    integer(kind=ip) :: nx, ny, nz

    call tic(lev,'relax')

    p  => grid(lev)%p
    b  => grid(lev)%b
    cA => grid(lev)%cA

    nx = grid(lev)%nx
    ny = grid(lev)%ny
    nz = grid(lev)%nz

    if (trim(grid(lev)%relaxation_method).eq.'xyz') then
       select case(trim(relax_method))
       case('Gauss-Seidel','GS')
          call relax_xyz_GS(lev,p,b,cA,nsweeps,nx,ny,nz)

       case('Red-Black','RB')
          call relax_xyz_RB(lev,p,b,cA,nsweeps,nx,ny,nz)

       case('Four-Color','FC')
          call relax_xyz_FC(lev,p,b,cA,nsweeps,nx,ny,nz)

       case('isoRB','IRB')
          call relax_xyz_IRB(lev,p,b,cA,nsweeps,nx,ny,nz)
       end select

       
    elseif (trim(grid(lev)%relaxation_method).eq.'xz') then
       select case(trim(relax_method))
       case('Gauss-Seidel','GS')
          call relax_xz_GS(lev,p,b,cA,nsweeps,nx,ny,nz)

       case('Red-Black','RB')
          call relax_xz_RB(lev,p,b,cA,nsweeps,nx,ny,nz)
       end select

    elseif (trim(grid(lev)%relaxation_method).eq.'yz') then
       select case(trim(relax_method))
       case('Gauss-Seidel','GS')
          call relax_yz_GS(lev,p,b,cA,nsweeps,nx,ny,nz)

       case('Red-Black','RB')
          call relax_yz_RB(lev,p,b,cA,nsweeps,nx,ny,nz)
       end select

    end if

    call toc(lev,'relax')
    
  end subroutine relax

  !----------------------------------------
  subroutine compute_residual(lev,res)
    integer(kind=ip), intent(in) :: lev
    real(kind=rp)   , intent(out):: res

    real(kind=rp),dimension(:,:,:)  , pointer:: p
    real(kind=rp),dimension(:,:,:)  , pointer:: b
    real(kind=rp),dimension(:,:,:)  , pointer:: r
    real(kind=rp),dimension(:,:,:,:), pointer:: cA

    integer(kind=ip) :: nx, ny, nz
    real(kind=rp) ::resloc

    call tic(lev,'residual')
    
    p  => grid(lev)%p
    b  => grid(lev)%b
    r  => grid(lev)%r
    cA => grid(lev)%cA

    nx = grid(lev)%nx
    ny = grid(lev)%ny
    nz = grid(lev)%nz

    if (trim(grid(lev)%relaxation_method).eq.'xyz') then
       call compute_residual_xyz(res,p,b,r,cA,nx,ny,nz)

    elseif (trim(grid(lev)%relaxation_method).eq.'xz') then
       call compute_residual_xz(res,p,b,r,cA,nx,ny,nz)
      
    elseif (trim(grid(lev)%relaxation_method).eq.'yz') then
       call compute_residual_yz(res,p,b,r,cA,nx,ny,nz)
      
    end if

    call fill_halo(lev,r)

    if (lev >-1) then
       resloc=res
       call global_sum(lev,resloc,res)
       res = sqrt(res)
    else
       res = -999._rp
    endif

    call toc(lev,'residual')

  end subroutine compute_residual

 !----------------------------------------
  subroutine relax_xyz_GS(lev,p,b,cA,nsweeps,nx,ny,nz)

    integer(kind=ip)                        , intent(in)   :: lev
    integer(kind=ip)                        , intent(in)   :: nsweeps
    integer(kind=ip)                        , intent(in)   :: nx, ny, nz

    real(kind=rp),dimension(:,:,:)  , pointer, intent(inout):: p
    real(kind=rp),dimension(:,:,:)  , pointer, intent(in)   :: b
    real(kind=rp),dimension(:,:,:,:), pointer, intent(in)   :: cA

    integer(kind=ip)            :: i,j,it

    ! add a loop on smoothing
    do it = 1,nsweeps
       do i = 1, nx
          do j = 1, ny
             call linerelax_xyz(p,b,cA,i,j,nz)
          enddo
       enddo
       call fill_halo(lev,p)
    enddo

  end subroutine relax_xyz_GS

 !----------------------------------------
  subroutine relax_xyz_RB(lev,p,b,cA,nsweeps,nx,ny,nz)
! This is the recommended relaxation method for production

    integer(kind=ip)                        , intent(in)   :: lev
    integer(kind=ip)                        , intent(in)   :: nsweeps
    integer(kind=ip)                        , intent(in)   :: nx, ny, nz

    real(kind=rp),dimension(:,:,:)  , pointer, intent(inout):: p
    real(kind=rp),dimension(:,:,:)  , pointer, intent(in)   :: b
    real(kind=rp),dimension(:,:,:,:), pointer, intent(in)   :: cA

    integer(kind=ip) :: i,j,it
    integer(kind=ip) :: rb

    do it = 1,nsweeps
       do rb = 1, 2 ! Red black loop
          do i = 1, nx
             do j = 1+mod(i+rb,2),ny,2
                call linerelax_xyz(p,b,cA,i,j,nz)
             enddo
             call fill_halo(lev,p)
          enddo
       enddo
    enddo

  end subroutine relax_xyz_RB

 !----------------------------------------
  subroutine relax_xyz_FC(lev,p,b,cA,nsweeps,nx,ny,nz)
! This is the recommended relaxation method for MPI debugging with topography

    integer(kind=ip)                        , intent(in)   :: lev
    integer(kind=ip)                        , intent(in)   :: nsweeps
    integer(kind=ip)                        , intent(in)   :: nx, ny, nz

    real(kind=rp),dimension(:,:,:)  , pointer, intent(inout):: p
    real(kind=rp),dimension(:,:,:)  , pointer, intent(in)   :: b
    real(kind=rp),dimension(:,:,:,:), pointer, intent(in)   :: cA

    integer(kind=ip)            :: i,j,it
    integer(kind=ip)            :: fc1,fc2

    do it = 1,nsweeps
       do fc1 = 1, 2
          do fc2 = 1, 2
             do i = 1 + mod(fc1-1,2), nx, 2
                do j = 1 + mod(fc2-1,2), ny, 2
                   call linerelax_xyz(p,b,cA,i,j,nz)
                enddo
             enddo
             call fill_halo(lev,p)
          enddo
       enddo
    enddo

  end subroutine relax_xyz_FC

 !----------------------------------------
  subroutine relax_xyz_IRB(lev,p,b,cA,nsweeps,nx,ny,nz)

    integer(kind=ip)                        , intent(in)   :: lev
    integer(kind=ip)                        , intent(in)   :: nsweeps
    integer(kind=ip)                        , intent(in)   :: nx, ny, nz

    real(kind=rp),dimension(:,:,:)  , pointer, intent(inout):: p
    real(kind=rp),dimension(:,:,:)  , pointer, intent(in)   :: b
    real(kind=rp),dimension(:,:,:,:), pointer, intent(in)   :: cA

    integer(kind=ip) :: i,j,k,it
    integer(kind=ip) :: rb

    real(kind=rp) :: rhs, omega

    omega = 1.1
    
    do it = 1,nsweeps
!       do rb = 1, 2 ! Red black loop
          do i = 1, nx
             do j =  1,ny!+mod(i+rb,2),ny,2
                do k = 1, nz
                   call comp_rhs_xyz(rhs,b,p,cA,nz,k,j,i)
                   p(k,j,i) = p(k,j,i)*(1.-omega) + omega*rhs/ca(1,k,j,i)                   
                enddo
             enddo
          enddo
          call fill_halo(lev,p)
!       enddo
    enddo

  end subroutine relax_xyz_IRB
  
 !----------------------------------------
  subroutine comp_rhs_xyz(rhs,b,p,cA,nz,k,j,i)
    real(kind=rp),                             intent(out):: rhs
    real(kind=rp),dimension(:,:,:)  , pointer, intent(in) :: b,p
    real(kind=rp),dimension(:,:,:,:), pointer, intent(in) :: cA
    integer(kind=ip),                          intent(in) :: nz,k,j,i

    if(k.eq.1)then !lower level
       rhs  = b(k,j,i)                                             &
         - cA(2,k+1,j,i)*p(k+1,j,i)                                &
         - cA(3,k,j,i)*p(k+1,j-1,i)                                &
         - cA(4,k,j,i)*p(k  ,j-1,i) - cA(4,k  ,j+1,i)*p(k  ,j+1,i) &
         - cA(5,k+1,j+1,i)*p(k+1,j+1,i)                            &
         - cA(6,k,j,i)*p(k+1,j,i-1)                                &
         - cA(7,k,j,i)*p(k  ,j,i-1) - cA(7,k  ,j,i+1)*p(k  ,j,i+1) &
         - cA(8,k+1,j,i+1)*p(k+1,j,i+1)                            &
!!  Special cross terms 
         - cA(5,k,j,i)*p(k,j+1,i-1) - cA(5,k,j-1,i+1)*p(k,j-1,i+1) &
         - cA(8,k,j,i)*p(k,j-1,i-1) - cA(8,k,j+1,i+1)*p(k,j+1,i+1)

    elseif(k.eq.nz)then !upper level
       rhs = b(k,j,i)                                                 &
            - cA(2,k,j,i)*p(k-1,j,i)                                  &
            - cA(3,k-1,j+1,i)*p(k-1,j+1,i)                            &
            - cA(4,k,j,i)*p(k  ,j-1,i) - cA(4,k  ,j+1,i)*p(k  ,j+1,i) &
            - cA(5,k,j,i)*p(k-1,j-1,i)                                &
            - cA(6,k-1,j,i+1)*p(k-1,j,i+1)                            &
            - cA(7,k,j,i)*p(k  ,j,i-1) - cA(7,k  ,j,i+1)*p(k  ,j,i+1) &
            - cA(8,k,j,i)*p(k-1,j,i-1) 

    else !interior levels
       rhs = b(k,j,i) &
            - cA(2,k,j,i)*p(k-1,j,i)   - cA(2,k+1,j,i)*p(k+1,j,i)     &
            - cA(3,k,j,i)*p(k+1,j-1,i) - cA(3,k-1,j+1,i)*p(k-1,j+1,i) &
            - cA(4,k,j,i)*p(k  ,j-1,i) - cA(4,k  ,j+1,i)*p(k  ,j+1,i) &
            - cA(5,k,j,i)*p(k-1,j-1,i) - cA(5,k+1,j+1,i)*p(k+1,j+1,i) &
            - cA(6,k,j,i)*p(k+1,j,i-1) - cA(6,k-1,j,i+1)*p(k-1,j,i+1) &
            - cA(7,k,j,i)*p(k  ,j,i-1) - cA(7,k  ,j,i+1)*p(k  ,j,i+1) &
            - cA(8,k,j,i)*p(k-1,j,i-1) - cA(8,k+1,j,i+1)*p(k+1,j,i+1) 
    endif

  end subroutine comp_rhs_xyz

 !----------------------------------------
  subroutine relax_xz_GS(lev,p,b,cA,nsweeps,nx,ny,nz)
! In 2D this is the recommended relaxation method for production

    integer(kind=ip)                        , intent(in)   :: lev
    integer(kind=ip)                        , intent(in)   :: nsweeps
    integer(kind=ip)                        , intent(in)   :: nx, ny, nz

    real(kind=rp),dimension(:,:,:)  , pointer, intent(inout):: p
    real(kind=rp),dimension(:,:,:)  , pointer, intent(in)   :: b
    real(kind=rp),dimension(:,:,:,:), pointer, intent(in)   :: cA

    integer(kind=ip)            :: i,j,it

    j = 1
    do it = 1,nsweeps
       do i = 1,nx
          call linerelax_xz(p,b,cA,i,j,nz)
       enddo
       call fill_halo(lev,p)          
    enddo

  end subroutine relax_xz_GS

 !----------------------------------------
  subroutine relax_yz_GS(lev,p,b,cA,nsweeps,nx,ny,nz)
! In 2D this is the recommended relaxation method for production

    integer(kind=ip)                        , intent(in)   :: lev
    integer(kind=ip)                        , intent(in)   :: nsweeps
    integer(kind=ip)                        , intent(in)   :: nx, ny, nz

    real(kind=rp),dimension(:,:,:)  , pointer, intent(inout):: p
    real(kind=rp),dimension(:,:,:)  , pointer, intent(in)   :: b
    real(kind=rp),dimension(:,:,:,:), pointer, intent(in)   :: cA

    integer(kind=ip)            :: i,j,it

    i = 1
    do it = 1,nsweeps
       do j = 1,ny
          call linerelax_yz(p,b,cA,i,j,nz)
       enddo
       call fill_halo(lev,p)          
    enddo

  end subroutine relax_yz_GS

 !----------------------------------------
  subroutine relax_xz_RB(lev,p,b,cA,nsweeps,nx,ny,nz)

    integer(kind=ip)                        , intent(in)   :: lev
    integer(kind=ip)                        , intent(in)   :: nsweeps
    integer(kind=ip)                        , intent(in)   :: nx, ny, nz

    real(kind=rp),dimension(:,:,:)  , pointer, intent(inout):: p
    real(kind=rp),dimension(:,:,:)  , pointer, intent(in)   :: b
    real(kind=rp),dimension(:,:,:,:), pointer, intent(in)   :: cA

    integer(kind=ip) :: i,j,k,it
    integer(kind=ip) :: rb
    real(kind=rp) :: rhs, omega

    omega = 0.9
    
    j = 1
    do it = 1,nsweeps
       do rb = 1, 2 ! Red black loop
          do i = 1, nx
             do k = 1+mod(i+rb,2),nz,2
                call comp_rhs_xz(rhs,b,p,cA,nz,k,j,i)
                p(k,j,i) = p(k,j,i)*(1.-omega) + omega*rhs/ca(1,k,j,i)                   
             enddo
          enddo
       call fill_halo(lev,p)
       enddo
    enddo

  end subroutine relax_xz_RB

 !----------------------------------------
  subroutine relax_yz_RB(lev,p,b,cA,nsweeps,nx,ny,nz)

    integer(kind=ip)                        , intent(in)   :: lev
    integer(kind=ip)                        , intent(in)   :: nsweeps
    integer(kind=ip)                        , intent(in)   :: nx, ny, nz

    real(kind=rp),dimension(:,:,:)  , pointer, intent(inout):: p
    real(kind=rp),dimension(:,:,:)  , pointer, intent(in)   :: b
    real(kind=rp),dimension(:,:,:,:), pointer, intent(in)   :: cA

    integer(kind=ip) :: i,j,k,it
    integer(kind=ip) :: rb
    real(kind=rp) :: rhs, omega

    omega = 0.9
    
    i = 1
    do it = 1,nsweeps
       do rb = 1, 2 ! Red black loop
          do j = 1, ny
             do k = 1+mod(j+rb,2),nz,2
                call comp_rhs_yz(rhs,b,p,cA,nz,k,j,i)
                p(k,j,i) = p(k,j,i)*(1.-omega) + omega*rhs/ca(1,k,j,i)                   
             enddo
          enddo
       call fill_halo(lev,p)
       enddo
    enddo

  end subroutine relax_yz_RB

  !----------------------------------------
  subroutine comp_rhs_xz(rhs,b,p,cA,nz,k,j,i)
    real(kind=rp),                             intent(out):: rhs
    real(kind=rp),dimension(:,:,:)  , pointer, intent(in) :: b,p
    real(kind=rp),dimension(:,:,:,:), pointer, intent(in) :: cA
    integer(kind=ip),                          intent(in) :: nz,k,j,i

    if(k.eq.1)then !lower level
       rhs  = b(k,j,i)                                             &
         - cA(2,k+1,j,i)*p(k+1,j,i)                                &
         - cA(6,k,j,i)*p(k+1,j,i-1)                                &
         - cA(7,k,j,i)*p(k  ,j,i-1) - cA(7,k  ,j,i+1)*p(k  ,j,i+1) &
         - cA(8,k+1,j,i+1)*p(k+1,j,i+1)                           ! &

    elseif(k.eq.nz)then !upper level
       rhs = b(k,j,i)                                                 &
            - cA(2,k,j,i)*p(k-1,j,i)                                  &
            - cA(6,k-1,j,i+1)*p(k-1,j,i+1)                            &
            - cA(7,k,j,i)*p(k  ,j,i-1) - cA(7,k  ,j,i+1)*p(k  ,j,i+1) &
            - cA(8,k,j,i)*p(k-1,j,i-1) 

    else !interior levels
       rhs = b(k,j,i) &
            - cA(2,k,j,i)*p(k-1,j,i)   - cA(2,k+1,j,i)*p(k+1,j,i)     &
            - cA(6,k,j,i)*p(k+1,j,i-1) - cA(6,k-1,j,i+1)*p(k-1,j,i+1) &
            - cA(7,k,j,i)*p(k  ,j,i-1) - cA(7,k  ,j,i+1)*p(k  ,j,i+1) &
            - cA(8,k,j,i)*p(k-1,j,i-1) - cA(8,k+1,j,i+1)*p(k+1,j,i+1) 
    endif

  end subroutine comp_rhs_xz
 
  !----------------------------------------
  subroutine comp_rhs_yz(rhs,b,p,cA,nz,k,j,i)
    real(kind=rp),                             intent(out):: rhs
    real(kind=rp),dimension(:,:,:)  , pointer, intent(in) :: b,p
    real(kind=rp),dimension(:,:,:,:), pointer, intent(in) :: cA
    integer(kind=ip),                          intent(in) :: nz,k,j,i

    if(k.eq.1)then !lower level
       rhs  = b(k,j,i)                                             &
         - cA(2,k+1,j,i)*p(k+1,j,i)                                &
         - cA(3,k,j,i)*p(k+1,j-1,i)                                &
         - cA(4,k,j,i)*p(k  ,j-1,i) - cA(4,k  ,j+1,i)*p(k  ,j+1,i) &
         - cA(5,k+1,j+1,i)*p(k+1,j+1,i)                           ! &
!         - cA(6,k,j,i)*p(k+1,j,i-1)                                &
!         - cA(7,k,j,i)*p(k  ,j,i-1) - cA(7,k  ,j,i+1)*p(k  ,j,i+1) &
!         - cA(8,k+1,j,i+1)*p(k+1,j,i+1)                            &
!!  Special cross terms 
!         - cA(5,k,j,i)*p(k,j+1,i-1) - cA(5,k,j-1,i+1)*p(k,j-1,i+1) &
!         - cA(8,k,j,i)*p(k,j-1,i-1) - cA(8,k,j+1,i+1)*p(k,j+1,i+1)

    elseif(k.eq.nz)then !upper level
       rhs = b(k,j,i)                                                 &
            - cA(2,k,j,i)*p(k-1,j,i)                                  &
            - cA(3,k-1,j+1,i)*p(k-1,j+1,i)                            &
            - cA(4,k,j,i)*p(k  ,j-1,i) - cA(4,k  ,j+1,i)*p(k  ,j+1,i) &
            - cA(5,k,j,i)*p(k-1,j-1,i)                               ! &
!            - cA(6,k-1,j,i+1)*p(k-1,j,i+1)                            &
!            - cA(7,k,j,i)*p(k  ,j,i-1) - cA(7,k  ,j,i+1)*p(k  ,j,i+1) &
!            - cA(8,k,j,i)*p(k-1,j,i-1) 

    else !interior levels
       rhs = b(k,j,i) &
            - cA(2,k,j,i)*p(k-1,j,i)   - cA(2,k+1,j,i)*p(k+1,j,i)     &
            - cA(3,k,j,i)*p(k+1,j-1,i) - cA(3,k-1,j+1,i)*p(k-1,j+1,i) &
            - cA(4,k,j,i)*p(k  ,j-1,i) - cA(4,k  ,j+1,i)*p(k  ,j+1,i) &
            - cA(5,k,j,i)*p(k-1,j-1,i) - cA(5,k+1,j+1,i)*p(k+1,j+1,i) !&
!            - cA(6,k,j,i)*p(k+1,j,i-1) - cA(6,k-1,j,i+1)*p(k-1,j,i+1) &
!            - cA(7,k,j,i)*p(k  ,j,i-1) - cA(7,k  ,j,i+1)*p(k  ,j,i+1) &
!            - cA(8,k,j,i)*p(k-1,j,i-1) - cA(8,k+1,j,i+1)*p(k+1,j,i+1) 
    endif

  end subroutine comp_rhs_yz
 
  !----------------------------------------
  subroutine linerelax_xyz(p,b,cA,i,j,nz)

    real(kind=rp),dimension(:,:,:)  , pointer, intent(inout):: p
    real(kind=rp),dimension(:,:,:)  , pointer, intent(in)   :: b
    real(kind=rp),dimension(:,:,:,:), pointer, intent(in)   :: cA

    integer(kind=ip)                          , intent(in)  :: i
    integer(kind=ip)                          , intent(in)  :: j
    integer(kind=ip)                          , intent(in)  :: nz

    !- Local -!
    integer(kind=ip) :: k
    real(kind=rp), dimension(nz) :: rhs, d, ud

    ! Coefficients are stored in order of diagonals
    ! cA(1,:,:,:)      -> p(k,j,i)
    ! cA(2,:,:,:)      -> p(k-1,j,i)
    ! cA(3,:,:,:)      -> p(k+1,j-1,i)
    ! cA(4,:,:,:)      -> p(k,j-1,i)
    ! cA(5,:,:,:)      -> p(k-1,j-1,i)
    ! cA(6,:,:,:)      -> p(k+1,j,i-1)
    ! cA(7,:,:,:)      -> p(k,j,i-1)
    ! cA(8,:,:,:)      -> p(k-1,j,i-1)

    k=1 !lower level
    rhs(k) = b(k,j,i)                                              &
         - cA(3,k,j,i)*p(k+1,j-1,i)                                &
         - cA(4,k,j,i)*p(k  ,j-1,i) - cA(4,k  ,j+1,i)*p(k  ,j+1,i) &
         - cA(5,k+1,j+1,i)*p(k+1,j+1,i)                            &
         - cA(6,k,j,i)*p(k+1,j,i-1)                                &
         - cA(7,k,j,i)*p(k  ,j,i-1) - cA(7,k  ,j,i+1)*p(k  ,j,i+1) &
         - cA(8,k+1,j,i+1)*p(k+1,j,i+1)                            &
!!  Special cross terms 
         - cA(5,k,j,i)*p(k,j+1,i-1) - cA(5,k,j-1,i+1)*p(k,j-1,i+1) &
         - cA(8,k,j,i)*p(k,j-1,i-1) - cA(8,k,j+1,i+1)*p(k,j+1,i+1)

    d(k)   = cA(1,k,j,i)
    ud(k)  = cA(2,k+1,j,i)

    do k = 2,nz-1 !interior levels
       rhs(k) = b(k,j,i) &
            - cA(3,k,j,i)*p(k+1,j-1,i) - cA(3,k-1,j+1,i)*p(k-1,j+1,i) &
            - cA(4,k,j,i)*p(k  ,j-1,i) - cA(4,k  ,j+1,i)*p(k  ,j+1,i) &
            - cA(5,k,j,i)*p(k-1,j-1,i) - cA(5,k+1,j+1,i)*p(k+1,j+1,i) &
            - cA(6,k,j,i)*p(k+1,j,i-1) - cA(6,k-1,j,i+1)*p(k-1,j,i+1) &
            - cA(7,k,j,i)*p(k  ,j,i-1) - cA(7,k  ,j,i+1)*p(k  ,j,i+1) &
            - cA(8,k,j,i)*p(k-1,j,i-1) - cA(8,k+1,j,i+1)*p(k+1,j,i+1) 
       d(k)   = cA(1,k,j,i)
       ud(k)  = cA(2,k+1,j,i)
    enddo

    k=nz !upper level
    rhs(k) = b(k,j,i)                                              &
         - cA(3,k-1,j+1,i)*p(k-1,j+1,i) &
         - cA(4,k,j,i)*p(k  ,j-1,i) - cA(4,k  ,j+1,i)*p(k  ,j+1,i) &
         - cA(5,k,j,i)*p(k-1,j-1,i)                                &
         - cA(6,k-1,j,i+1)*p(k-1,j,i+1) &
         - cA(7,k,j,i)*p(k  ,j,i-1) - cA(7,k  ,j,i+1)*p(k  ,j,i+1) &
         - cA(8,k,j,i)*p(k-1,j,i-1) 
    d(k)   = cA(1,k,j,i)

    call tridiag(nz,d,ud,rhs,p(:,j,i)) ! solve 1D tridiagonal system

  end subroutine linerelax_xyz

  !----------------------------------------
  subroutine linerelax_xz(p,b,cA,i,j,nz)

    real(kind=rp),dimension(:,:,:)  , pointer, intent(inout):: p
    real(kind=rp),dimension(:,:,:)  , pointer, intent(in)   :: b
    real(kind=rp),dimension(:,:,:,:), pointer, intent(in)   :: cA

    integer(kind=ip)                          , intent(in)  :: i
    integer(kind=ip)                          , intent(in)  :: j
    integer(kind=ip)                          , intent(in)  :: nz

    !- Local -!
    integer(kind=ip) :: k
    real(kind=rp), dimension(nz) :: rhs, d, ud

    k=1 !lower level
    rhs(k) = b(k,j,i)                                              &
         - cA(6,k,j,i)*p(k+1,j,i-1)                                &
         - cA(7,k,j,i)*p(k  ,j,i-1) - cA(7,k  ,j,i+1)*p(k  ,j,i+1) &
         - cA(8,k+1,j,i+1)*p(k+1,j,i+1)                            

    d(k)   = cA(1,k,j,i)
    ud(k)  = cA(2,k+1,j,i)

    do k = 2,nz-1 !interior levels
       rhs(k) = b(k,j,i) &
            - cA(6,k,j,i)*p(k+1,j,i-1) - cA(6,k-1,j,i+1)*p(k-1,j,i+1) &
            - cA(7,k,j,i)*p(k  ,j,i-1) - cA(7,k  ,j,i+1)*p(k  ,j,i+1) &
            - cA(8,k,j,i)*p(k-1,j,i-1) - cA(8,k+1,j,i+1)*p(k+1,j,i+1) 
       d(k)   = cA(1,k,j,i)
       ud(k)  = cA(2,k+1,j,i)
    enddo

    k=nz !upper level
    rhs(k) = b(k,j,i)                                              &
         - cA(6,k-1,j,i+1)*p(k-1,j,i+1) &
         - cA(7,k,j,i)*p(k  ,j,i-1) - cA(7,k  ,j,i+1)*p(k  ,j,i+1) &
         - cA(8,k,j,i)*p(k-1,j,i-1) 
    d(k)   = cA(1,k,j,i)

    call tridiag(nz,d,ud,rhs,p(:,j,i)) ! solve 1D tridiagonal system
    
  end subroutine linerelax_xz

  !----------------------------------------
  subroutine linerelax_yz(p,b,cA,i,j,nz)

    real(kind=rp),dimension(:,:,:)  , pointer, intent(inout):: p
    real(kind=rp),dimension(:,:,:)  , pointer, intent(in)   :: b
    real(kind=rp),dimension(:,:,:,:), pointer, intent(in)   :: cA

    integer(kind=ip)                          , intent(in)  :: i
    integer(kind=ip)                          , intent(in)  :: j
    integer(kind=ip)                          , intent(in)  :: nz

    !- Local -!
    integer(kind=ip) :: k
    real(kind=rp), dimension(nz) :: rhs, d, ud

    k=1 !lower level
    rhs(k) = b(k,j,i)                                              &
         - cA(3,k,j,i)*p(k+1,j-1,i)                                &
         - cA(4,k,j,i)*p(k  ,j-1,i) - cA(4,k  ,j+1,i)*p(k  ,j+1,i) &
         - cA(5,k+1,j+1,i)*p(k+1,j+1,i)                            !&
!         - cA(6,k,j,i)*p(k+1,j,i-1)                                &
!         - cA(7,k,j,i)*p(k  ,j,i-1) - cA(7,k  ,j,i+1)*p(k  ,j,i+1) &
!         - cA(8,k+1,j,i+1)*p(k+1,j,i+1)                            &
!!  Special cross terms 
!         - cA(5,k,j,i)*p(k,j+1,i-1) - cA(5,k,j-1,i+1)*p(k,j-1,i+1) &
!         - cA(8,k,j,i)*p(k,j-1,i-1) - cA(8,k,j+1,i+1)*p(k,j+1,i+1)

    d(k)   = cA(1,k,j,i)
    ud(k)  = cA(2,k+1,j,i)

    do k = 2,nz-1 !interior levels
       rhs(k) = b(k,j,i) &
            - cA(3,k,j,i)*p(k+1,j-1,i) - cA(3,k-1,j+1,i)*p(k-1,j+1,i) &
            - cA(4,k,j,i)*p(k  ,j-1,i) - cA(4,k  ,j+1,i)*p(k  ,j+1,i) &
            - cA(5,k,j,i)*p(k-1,j-1,i) - cA(5,k+1,j+1,i)*p(k+1,j+1,i) !&
!            - cA(6,k,j,i)*p(k+1,j,i-1) - cA(6,k-1,j,i+1)*p(k-1,j,i+1) &
!            - cA(7,k,j,i)*p(k  ,j,i-1) - cA(7,k  ,j,i+1)*p(k  ,j,i+1) &
!            - cA(8,k,j,i)*p(k-1,j,i-1) - cA(8,k+1,j,i+1)*p(k+1,j,i+1) 
       d(k)   = cA(1,k,j,i)
       ud(k)  = cA(2,k+1,j,i)
    enddo

    k=nz !upper level
    rhs(k) = b(k,j,i)                                              &
         - cA(3,k-1,j+1,i)*p(k-1,j+1,i) &
         - cA(4,k,j,i)*p(k  ,j-1,i) - cA(4,k  ,j+1,i)*p(k  ,j+1,i) &
         - cA(5,k,j,i)*p(k-1,j-1,i)                                !&
!         - cA(6,k-1,j,i+1)*p(k-1,j,i+1) &
!         - cA(7,k,j,i)*p(k  ,j,i-1) - cA(7,k  ,j,i+1)*p(k  ,j,i+1) &
!         - cA(8,k,j,i)*p(k-1,j,i-1) 
    d(k)   = cA(1,k,j,i)

    call tridiag(nz,d,ud,rhs,p(:,j,i)) ! solve 1D tridiagonal system
    
  end subroutine linerelax_yz

  !----------------------------------------
  subroutine tridiag(l,d,dd,b,xc)
    !     Axc = b
    !     Solve tridiagonal system

    integer                   ,intent(in)  :: l
    real(kind=rp),dimension(l),intent(in)  :: d,b
    real(kind=rp),dimension(l),intent(in)  :: dd
    real(kind=rp),dimension(l),intent(out) :: xc

    integer                  :: k
    real(kind=rp),dimension(l):: gam
    real(kind=rp)             :: bet

    bet   = one / d(1)
    xc(1) = b(1)*bet
    do k=2,l
       gam(k)= dd(k-1)*bet
       bet     = one /(d(k)-dd(k-1)*gam(k))
       xc(k) = (b(k)-dd(k-1)*xc(k-1))*bet
    enddo
    do k=l-1,1,-1
       xc(k) = xc(k)-gam(k+1)*xc(k+1)
    enddo

  end subroutine tridiag

  !----------------------------------------
  subroutine compute_residual_xyz(res,p,b,r,cA,nx,ny,nz)

    real(kind=rp)                            , intent(out)  :: res
    real(kind=rp),dimension(:,:,:)  , pointer, intent(inout):: p
    real(kind=rp),dimension(:,:,:)  , pointer, intent(in)   :: b
    real(kind=rp),dimension(:,:,:)  , pointer, intent(inout)   :: r
    real(kind=rp),dimension(:,:,:,:), pointer, intent(in)   :: cA
    integer(kind=ip)                        , intent(in)   :: nx, ny, nz

    ! Coefficients are stored in order of diagonals
    ! cA(1,:,:,:)      -> p(k,j,i)
    ! cA(2,:,:,:)      -> p(k-1,j,i)
    ! cA(3,:,:,:)      -> p(k+1,j-1,i)
    ! cA(4,:,:,:)      -> p(k,j-1,i)
    ! cA(5,:,:,:)      -> p(k-1,j-1,i)
    ! cA(6,:,:,:)      -> p(k+1,j,i-1)
    ! cA(7,:,:,:)      -> p(k,j,i-1)
    ! cA(8,:,:,:)      -> p(k-1,j,i-1)

    integer(kind=ip)           :: i,j,k

    res = zero

    do i = 1,nx
       do j = 1,ny

          k=1 !lower level
          r(k,j,i) = b(k,j,i)                                           &
               - cA(1,k,j,i)*p(k,j,i)                                   &
               - cA(2,k+1,j,i)*p(k+1,j,i)                               &
               - cA(3,k,j,i)*p(k+1,j-1,i)                               &
               - cA(4,k,j,i)*p(k  ,j-1,i) - cA(4,k  ,j+1,i)*p(k  ,j+1,i)&
               - cA(5,k+1,j+1,i)*p(k+1,j+1,i)                           &
               - cA(6,k,j,i)*p(k+1,j,i-1)                               &
               - cA(7,k,j,i)*p(k  ,j,i-1) - cA(7,k  ,j,i+1)*p(k  ,j,i+1)&
               - cA(8,k+1,j,i+1)*p(k+1,j,i+1)                           &
!!  Special cross terms 
               - cA(5,k,j,i)*p(k,j+1,i-1) - cA(5,k,j-1,i+1)*p(k,j-1,i+1)&
               - cA(8,k,j,i)*p(k,j-1,i-1) - cA(8,k,j+1,i+1)*p(k,j+1,i+1)

          res = res+r(k,j,i)*r(k,j,i)

          do k = 2,nz-1 !interior levels
             r(k,j,i) = b(k,j,i)                                           &
                  - cA(1,k,j,i)*p(k,j,i)                                   &
                  - cA(2,k,j,i)*p(k-1,j,i)   - cA(2,k+1,j,i)*p(k+1,j,i)    &
                  - cA(3,k,j,i)*p(k+1,j-1,i) - cA(3,k-1,j+1,i)*p(k-1,j+1,i)&
                  - cA(4,k,j,i)*p(k  ,j-1,i) - cA(4,k  ,j+1,i)*p(k  ,j+1,i)&
                  - cA(5,k,j,i)*p(k-1,j-1,i) - cA(5,k+1,j+1,i)*p(k+1,j+1,i)&
                  - cA(6,k,j,i)*p(k+1,j,i-1) - cA(6,k-1,j,i+1)*p(k-1,j,i+1)&
                  - cA(7,k,j,i)*p(k  ,j,i-1) - cA(7,k  ,j,i+1)*p(k  ,j,i+1)&
                  - cA(8,k,j,i)*p(k-1,j,i-1) - cA(8,k+1,j,i+1)*p(k+1,j,i+1)

             res = res+r(k,j,i)*r(k,j,i)
          enddo

          k=nz !upper level
          r(k,j,i) = b(k,j,i)                                           &
               - cA(1,k,j,i)*p(k,j,i)                                   &
               - cA(2,k,j,i)*p(k-1,j,i)                                 &
               - cA(3,k-1,j+1,i)*p(k-1,j+1,i)                           &
               - cA(4,k,j,i)*p(k  ,j-1,i) - cA(4,k  ,j+1,i)*p(k  ,j+1,i)&
               - cA(5,k,j,i)*p(k-1,j-1,i)                               &
               - cA(6,k-1,j,i+1)*p(k-1,j,i+1)                           &
               - cA(7,k,j,i)*p(k  ,j,i-1) - cA(7,k  ,j,i+1)*p(k  ,j,i+1)&
               - cA(8,k,j,i)*p(k-1,j,i-1)

          res = res+r(k,j,i)*r(k,j,i)

       enddo
    enddo

  end subroutine compute_residual_xyz

  !----------------------------------------
  subroutine compute_residual_xz(res,p,b,r,cA,nx,ny,nz)

    real(kind=rp)                            , intent(out)  :: res
    real(kind=rp),dimension(:,:,:)  , pointer, intent(inout):: p
    real(kind=rp),dimension(:,:,:)  , pointer, intent(in)   :: b
    real(kind=rp),dimension(:,:,:)  , pointer, intent(inout)   :: r
    real(kind=rp),dimension(:,:,:,:), pointer, intent(in)   :: cA
    integer(kind=ip)                        , intent(in)   :: nx, ny, nz

    integer(kind=ip)           :: i,j,k

    res = zero

    do i = 1,nx
       j=1

          k=1 !lower level
          r(k,j,i) = b(k,j,i)                                           &
               - cA(1,k,j,i)*p(k,j,i)                                   &
               - cA(2,k+1,j,i)*p(k+1,j,i)                               &
               - cA(6,k,j,i)*p(k+1,j,i-1)                               &
               - cA(7,k,j,i)*p(k  ,j,i-1) - cA(7,k  ,j,i+1)*p(k  ,j,i+1)&
               - cA(8,k+1,j,i+1)*p(k+1,j,i+1)                           

          res = res+r(k,j,i)*r(k,j,i)

          do k = 2,nz-1 !interior levels
             r(k,j,i) = b(k,j,i)                                           &
                  - cA(1,k,j,i)*p(k,j,i)                                   &
                  - cA(2,k,j,i)*p(k-1,j,i)   - cA(2,k+1,j,i)*p(k+1,j,i)    &
                  - cA(6,k,j,i)*p(k+1,j,i-1) - cA(6,k-1,j,i+1)*p(k-1,j,i+1)&
                  - cA(7,k,j,i)*p(k  ,j,i-1) - cA(7,k  ,j,i+1)*p(k  ,j,i+1)&
                  - cA(8,k,j,i)*p(k-1,j,i-1) - cA(8,k+1,j,i+1)*p(k+1,j,i+1)

             res = res+r(k,j,i)*r(k,j,i)
          enddo

          k=nz !upper level
          r(k,j,i) = b(k,j,i)                                           &
               - cA(1,k,j,i)*p(k,j,i)                                   &
               - cA(2,k,j,i)*p(k-1,j,i)                                 &
               - cA(6,k-1,j,i+1)*p(k-1,j,i+1)                           &
               - cA(7,k,j,i)*p(k  ,j,i-1) - cA(7,k  ,j,i+1)*p(k  ,j,i+1)&
               - cA(8,k,j,i)*p(k-1,j,i-1)

          res = res+r(k,j,i)*r(k,j,i)

    enddo

  end subroutine compute_residual_xz

  !----------------------------------------
  subroutine compute_residual_yz(res,p,b,r,cA,nx,ny,nz)

    real(kind=rp)                            , intent(out)  :: res
    real(kind=rp),dimension(:,:,:)  , pointer, intent(inout):: p
    real(kind=rp),dimension(:,:,:)  , pointer, intent(in)   :: b
    real(kind=rp),dimension(:,:,:)  , pointer, intent(inout)   :: r
    real(kind=rp),dimension(:,:,:,:), pointer, intent(in)   :: cA
    integer(kind=ip)                        , intent(in)   :: nx, ny, nz

    integer(kind=ip)           :: i,j,k

    res = zero

    i = 1
       do j = 1,ny

          k=1 !lower level
          r(k,j,i) = b(k,j,i)                                           &
               - cA(1,k,j,i)*p(k,j,i)                                   &
               - cA(2,k+1,j,i)*p(k+1,j,i)                               &
               - cA(3,k,j,i)*p(k+1,j-1,i)                               &
               - cA(4,k,j,i)*p(k  ,j-1,i) - cA(4,k  ,j+1,i)*p(k  ,j+1,i)&
               - cA(5,k+1,j+1,i)*p(k+1,j+1,i)                           !&
!               - cA(6,k,j,i)*p(k+1,j,i-1)                               &
!               - cA(7,k,j,i)*p(k  ,j,i-1) - cA(7,k  ,j,i+1)*p(k  ,j,i+1)&
!               - cA(8,k+1,j,i+1)*p(k+1,j,i+1)                           &
!!  Special cross terms 
!               - cA(5,k,j,i)*p(k,j+1,i-1) - cA(5,k,j-1,i+1)*p(k,j-1,i+1)&
!               - cA(8,k,j,i)*p(k,j-1,i-1) - cA(8,k,j+1,i+1)*p(k,j+1,i+1)

          res = res+r(k,j,i)*r(k,j,i)

          do k = 2,nz-1 !interior levels
             r(k,j,i) = b(k,j,i)                                           &
                  - cA(1,k,j,i)*p(k,j,i)                                   &
                  - cA(2,k,j,i)*p(k-1,j,i)   - cA(2,k+1,j,i)*p(k+1,j,i)    &
                  - cA(3,k,j,i)*p(k+1,j-1,i) - cA(3,k-1,j+1,i)*p(k-1,j+1,i)&
                  - cA(4,k,j,i)*p(k  ,j-1,i) - cA(4,k  ,j+1,i)*p(k  ,j+1,i)&
                  - cA(5,k,j,i)*p(k-1,j-1,i) - cA(5,k+1,j+1,i)*p(k+1,j+1,i)!&
!                  - cA(6,k,j,i)*p(k+1,j,i-1) - cA(6,k-1,j,i+1)*p(k-1,j,i+1)&
!                  - cA(7,k,j,i)*p(k  ,j,i-1) - cA(7,k  ,j,i+1)*p(k  ,j,i+1)&
!                  - cA(8,k,j,i)*p(k-1,j,i-1) - cA(8,k+1,j,i+1)*p(k+1,j,i+1)

             res = res+r(k,j,i)*r(k,j,i)
          enddo

          k=nz !upper level
          r(k,j,i) = b(k,j,i)                                           &
               - cA(1,k,j,i)*p(k,j,i)                                   &
               - cA(2,k,j,i)*p(k-1,j,i)                                 &
               - cA(3,k-1,j+1,i)*p(k-1,j+1,i)                           &
               - cA(4,k,j,i)*p(k  ,j-1,i) - cA(4,k  ,j+1,i)*p(k  ,j+1,i)!&
!               - cA(5,k,j,i)*p(k-1,j-1,i)                               &
!               - cA(6,k-1,j,i+1)*p(k-1,j,i+1)                           &
!               - cA(7,k,j,i)*p(k  ,j,i-1) - cA(7,k  ,j,i+1)*p(k  ,j,i+1)&
!               - cA(8,k,j,i)*p(k-1,j,i-1)

          res = res+r(k,j,i)*r(k,j,i)

       enddo


  end subroutine compute_residual_yz

end module mg_relax
