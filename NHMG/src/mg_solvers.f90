module mg_solvers

  use mg_cst
  use mg_mpi
  use mg_tictoc
  use mg_namelist
  use mg_grids
  use mg_intergrids
  use mg_relax
  use mg_netcdf_out

  implicit none

contains

  !---------------------------------------------------------------------
  subroutine solve_p()

    integer(kind=ip) :: nite
    integer(kind=ip) :: nx,ny,nz

    real(kind=rp)    :: rnorm,bnorm,res0,conv,rnorm0
    real(kind=rp), dimension(:,:,:), pointer :: p,b,r

    real(kind=lg) :: tstart,tend,perf
    real(kind=rp) :: rnxg,rnyg,rnzg
    real(kind=rp) :: rnpxg,rnpyg

    integer, save :: count = 0
    logical :: verbose

    verbose = .false.
    if (mod(count,10)==0) verbose=.true.
    if (autotune)  verbose=.true.
    count = count+1
    if ((myrank==0).and.verbose) write(*,*)'     ---------------'


    ! if commented, we used the previous pressure as first guess for the current projection
    ! grid(1)%p(:,:,:) = zero

    p  => grid(1)%p
    b  => grid(1)%b
    r  => grid(1)%r

    nx = grid(1)%nx
    ny = grid(1)%ny
    nz = grid(1)%nz

    call tic(1,'solve')
    call cpu_time(tstart)

    nite=0

    res0 = sum(grid(1)%b(1:nz,1:ny,1:nx)**2)
    call global_sum(1,res0,bnorm)
    bnorm = sqrt(bnorm)

    ! residual returns both 'r' and its norm
    call compute_residual(1,rnorm) 
    res0   = rnorm/bnorm
    rnorm0 = res0

!    if (myrank == 0) write(100,*) rnorm0, nite

    do while ((nite < solver_maxiter).and.(res0 > solver_prec))

       call Fcycle()

       call compute_residual(1,rnorm)
       rnorm = rnorm/bnorm
       conv = res0/rnorm ! error reduction after this iteration
       res0 = rnorm

       nite = nite+1
       if ((myrank == 0).and.verbose) write(*,10) nite, rnorm, conv
!       if (myrank == 0) write(100,*) rnorm, conv

!!$       if (netcdf_output) then
!!$          call write_netcdf(grid(1)%p,vname='p',netcdf_file_name='p.nc',rank=myrank,iter=nite)
!!$          call write_netcdf(grid(1)%r,vname='r',netcdf_file_name='r.nc',rank=myrank,iter=nite)
!!$       endif

    enddo

    call cpu_time(tend)
    call toc(1,'solve')

    if ((myrank == 0).and.verbose) then
       rnpxg=real(grid(1)%npx,kind=rp)
       rnpyg=real(grid(1)%npy,kind=rp)
       rnxg=real(grid(1)%nx,kind=rp)*rnpxg
       rnyg=real(grid(1)%ny,kind=rp)*rnpyg
       rnzg=real(grid(1)%nz,kind=rp)
       ! the rescaled time should be expressed in terms of error reduction,
       ! therefore the ratio rnorm/rnorm0 [the rnorm0 was missing prior Dec 11th]
       perf = (tend-tstart)*(rnpxg*rnpyg)/(-log(rnorm/rnorm0)/log(10._rp))/(rnxg*rnyg*rnzg)
       write(*,*)'     --- summary ---'
       write(*,'(A,F8.3,A)')"     time spent to solve :",tend-tstart," s"
       write(*,'(A,E10.3)') "     rescaled performance:",perf
       write(*,*)'     ---------------'
    end if

10  format("     ite = ",I2,": res = ",E10.3," / conv = ",F10.3)

  end subroutine solve_p

  !---------------------------------------------------------------------
  subroutine Fcycle()

    integer(kind=ip):: lev,maxlev

    call tic(1,'Fcycle')

    maxlev=nlevs

    do lev=1,maxlev-1
       call fine2coarse(lev)
       ! this step can be drop if we pass on output argument the intend array
       ! call fine2coarse(lev,grid(lev)%b) for Vcycle
       ! call fine2coarse(lev,grid(lev)%r) for Fcycle
       grid(lev+1)%r=grid(lev+1)%b
    enddo

    call relax(maxlev, ns_coarsest)

    do lev=maxlev-1,1,-1
       call coarse2fine(lev) 
       call Vcycle(lev)
    enddo

    call toc(1,'Fcycle')

  end subroutine Fcycle

  !----------------------------------------
  subroutine Vcycle(lev1)

    integer(kind=ip), intent(in) :: lev1
    integer(kind=ip)             :: lev, nlevs0
    real(kind=rp)                :: rnorm

    nlevs0=nlevs


    do lev=lev1,nlevs0-1
       call relax(lev,ns_pre)
       call compute_residual(lev,rnorm)
       call fine2coarse(lev)
    enddo

    call relax(nlevs0,ns_coarsest)

    do lev=nlevs0-1,lev1,-1
       call coarse2fine(lev)
       call relax(lev,ns_post)
    enddo

  end subroutine Vcycle


  !----------------------------------------
  subroutine Vcycle2(lev1,lev2)
    ! partial V-cycle: go down level = lev2

    integer(kind=ip), intent(in) :: lev1,lev2
    integer(kind=ip)             :: lev, nlevs0
    real(kind=rp)                :: rnorm

    nlevs0=nlevs

    do lev=lev1,lev2-1
       call relax(lev,ns_pre)
       call compute_residual(lev,rnorm)
       call fine2coarse(lev)
    enddo

    call relax(lev2,ns_coarsest)

    do lev=lev2-1,lev1,-1
       call coarse2fine(lev)
       call relax(lev,ns_post)
    enddo

  end subroutine Vcycle2

  !---------------------------------------------------------------------
  subroutine norm(lev,x,y,nx,ny,nz,res)

    use mg_mpi_exchange
    integer(kind=ip) :: lev,i,j,k
    integer(kind=ip) :: nx,ny,nz
    real(kind=rp) :: r,res
    real(kind=rp),dimension(:,:,:)  , pointer :: x,y

    r = zero

    do i=1,nx
       do j=1,ny
          do k=1,nz
             r=r+x(k,j,i)*y(k,j,i)
          enddo
       enddo
    enddo

    call global_sum(lev,r,res)

  end subroutine norm

  !---------------------------------------------------------------------
  subroutine testgalerkin(lev)

    real(kind=rp) :: norm_c,norm_f,dummy
    integer(kind=ip) :: lev,nx,ny,nz,i,j,k
    character(len=16) :: filen


    integer(kind=ip) :: npx,npy,pi,pj
    real(kind=rp) :: x,y,z,cff

    nx = grid(lev)%nx
    ny = grid(lev)%ny
    nz = grid(lev)%nz

    npx = grid(1)%npx
    npy = grid(1)%npy

    pj = myrank/npx
    pi = mod(myrank,npx)


    call random_number(grid(lev)%p)!
    do i=1,nx
       x=(1._rp*i-0.5_rp+pi*nx)/(npx*nx) -0.3_rp
       do j=1,ny
          y=(1._rp*j-0.5_rp+pj*ny)/(npy*ny)-0.4_rp
          do k=1,nz
             z=(1._rp*k-0.5_rp)/nz-0.2_rp
             cff = exp( - (x*x+y*y+z*z)*30._rp )
             !             grid(lev)%p(k,j,i)= cff
             !!NG grid(lev)%p(k,j,i)= grid(lev)%p(k,j,i)*grid(lev)%rmask(j,i)
             !             grid(lev)%p(k,j,i)= 1._rp*grid(lev)%rmask(j,i)
             !             grid(lev)%p(k,j,i)= (i)*(nz+0.5-k)*1._rp*grid(lev)%rmask(j,i)
          enddo
       enddo
    enddo
    !    grid(lev)%p(2,:,:)=0._rp
    call fill_halo(lev,grid(lev)%p)

    !    write(filen,'("p_",i1,".nc")') lev
    !    call write_netcdf(grid(lev)%p,vname='p',netcdf_file_name=filen,rank=myrank)


    grid(lev)%b = zero
    call compute_residual(lev,dummy)    
    call norm(lev,grid(lev)%p,grid(lev)%r,nx,ny,nz,norm_c)

    if (netcdf_output) then
       write(filen,'("p_",i1,".nc")') lev
       call write_netcdf(grid(lev)%p,vname='p',netcdf_file_name=filen,rank=myrank)

       write(filen,'("r_",i1,".nc")') lev
       call write_netcdf(grid(lev)%r,vname='r',netcdf_file_name=filen,rank=myrank)
    endif

    grid(lev-1)%p = zero
    call coarse2fine(lev-1) ! interpolate p to r and add r to p

    if (netcdf_output) then
       write(filen,'("p_",i1,".nc")') lev-1
       call write_netcdf(grid(lev-1)%p,vname='p',netcdf_file_name=filen,rank=myrank)
    endif

    !    grid(lev-1)%p(:,:,:)= 1._rp
    grid(lev-1)%b = zero
    call compute_residual(lev-1,dummy)

    if (netcdf_output) then
       write(filen,'("r_",i1,".nc")') lev-1
       call write_netcdf(grid(lev-1)%r,vname='r',netcdf_file_name=filen,rank=myrank)
    endif

    nx = grid(lev-1)%nx
    ny = grid(lev-1)%ny
    nz = grid(lev-1)%nz

    call norm(lev-1,grid(lev-1)%p,grid(lev-1)%r,nx,ny,nz,norm_f)

    if (myrank==0) then
       write(*,*)"======== lev ",lev,"==========="
       write(*,*)"norm coarse = ",norm_c
       write(*,*)"norm fine   = ",norm_f/4
       write(*,*)"ratio       = ",norm_c/norm_f*4
    endif

  end subroutine testgalerkin

end module mg_solvers
