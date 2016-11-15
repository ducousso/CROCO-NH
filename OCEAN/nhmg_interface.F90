#include "cppdefs.h"
#ifdef NHMG
module nhmg_interface

!  use mg_mpi
!  use mg_grids
!  use mg_namelist
!  use mg_tictoc
!  use mg_mpi_exchange
!  use mg_horiz_grids
!  use mg_netcdf_out
!  use mg_set_bbc
!  use mg_compute_fluxes
!  use mg_btbc_coupling
!  use mg_compute_rhs
!  use mg_define_matrices
!  use mg_solvers
!  use mg_correct_uvw
!  use mg_compute_barofrc

  use nhmg

  implicit none

contains

  !--------------------------------------------------------------
  subroutine nhmg_init(nx,ny,nz,npxg,npyg,dxa,dya)
      
    integer(kind=ip), intent(in) :: nx, ny, nz
    integer(kind=ip), intent(in) :: npxg, npyg

    real(kind=rp), dimension(0:nx+1,0:ny+1), intent(in) :: dxa, dya
    real(kind=rp), dimension(0:ny+1,0:nx+1), target     :: dxb, dyb
    real(kind=rp), dimension(:,:)          , pointer    :: dx, dy

    if (myrank==0) write(*,*)' nhmg_init:'

    call mg_mpi_init()

    call read_nhnamelist(vbrank=myrank)

    call define_grids(npxg,npyg,nx,ny,nz)

    call define_neighbours()

    dxb = transpose(dxa)
    dyb = transpose(dya)
    dx => dxb
    dy => dyb
    call fill_horiz_grids(dx,dy)

    call print_grids()

  end subroutine nhmg_init

  !--------------------------------------------------------------
  subroutine nhmg_bbc(nx,ny,nz,zra,zwa,ua,va,wa)

    integer(kind=ip), intent(in) :: nx, ny, nz

    real(kind=rp), dimension(0:nx+1,0:ny+1,1:nz), target, intent(in) :: zra
    real(kind=rp), dimension(0:nx+1,0:ny+1,0:nz), target, intent(in) :: zwa
    real(kind=rp), dimension(1:nx+1,0:ny+1,1:nz), target, intent(in) :: ua
    real(kind=rp), dimension(0:nx+1,1:ny+1,1:nz), target, intent(in) :: va
    real(kind=rp), dimension(0:nx+1,0:ny+1,0:nz), target, intent(inout) :: wa

    real(kind=rp), dimension(:,:,:), pointer :: zr,zw
    real(kind=rp), dimension(:,:,:), pointer :: u,v,w

!!! dirty reshape arrays indexing ijk -> kji !!!
    real(kind=rp), dimension(:,:,:), allocatable, target :: zrb,zwb 
    real(kind=rp), dimension(:,:,:), allocatable, target :: ub,vb,wb
!!!
    integer(kind=ip) :: i,j,k

    integer(kind=ip), save :: iter_bbc=0
    iter_bbc = iter_bbc + 1

    if (myrank==0) write(*,*)' nhmg_bbc:'

!!! dirty reshape arrays indexing ijk -> kji !!!
    allocate(zrb(1:nz,0:ny+1,0:nx+1))
    allocate(zwb(0:nz,0:ny+1,0:nx+1))
    allocate( ub(1:nz,0:ny+1,0:nx+1))
    allocate( vb(1:nz,0:ny+1,0:nx+1))
    allocate( wb(0:nz,0:ny+1,0:nx+1))
    !-UB-!
    do i = 1,nx+1
      do j = 0,ny+1
        do k = 1,nz
          ub(k,j,i) = ua(i,j,k)
        enddo
      enddo
    enddo
    i = 0
    do j = 0,ny+1
       do k = 1,nz
          ub(k,j,i) = 0._rp
       enddo
    enddo
    !-VB-!
    do i = 0,nx+1
      do j = 1,ny+1
        do k = 1,nz
          vb(k,j,i) = va(i,j,k)
        enddo
      enddo
    enddo
    j = 0
    do i = 0,nx+1
       do k = 1,nz
          vb(k,j,i) = 0._rp
       enddo
    enddo
    !-WB-!
    do i = 0,nx+1
      do j = 0,ny+1
        do k = 0,nz
          wb(k,j,i) = wa(i,j,k)
        enddo
      enddo
    enddo
    do i = 0,nx+1
      do j = 0,ny+1
        do k = 1,nz
          zrb(k,j,i) = zra(i,j,k)
        enddo
      enddo
    enddo
    do i = 0,nx+1
      do j = 0,ny+1
        do k = 0,nz
          zwb(k,j,i) = zwa(i,j,k)
        enddo
      enddo
    enddo
    zr => zrb
    zw => zwb
    u => ub
    v => vb
    w => wb
!!!

    call fill_halo(1,u,lbc_null='u')
    call fill_halo(1,v,lbc_null='v')
    call fill_halo(1,w)

    call set_bbc(zr,zw,u,v,w)

!!! dirty reshape arrays indexing kji -> ijk !!!
   do i = 0,nx+1
      do j = 0,ny+1
        do k = 0,nz
          wa(i,j,k) = w(k,j,i)
        enddo
      enddo
    enddo
    deallocate(zrb)
    deallocate(zwb)
    deallocate(ub)
    deallocate(vb)
    deallocate(wb)
!!!

    if (associated(zr)) zr => null()
    if (associated(zw)) zw => null()
    if (associated(u)) u => null()
    if (associated(v)) v => null()
    if (associated(w)) w => null()

  end subroutine nhmg_bbc

  !--------------------------------------------------------------
  subroutine nhmg_fluxes(nx,ny,nz,zra,zwa,ua,va,wa,ufa,vfa)

    integer(kind=ip), intent(in) :: nx, ny, nz

    real(kind=rp), dimension(0:nx+1,0:ny+1,1:nz), target, intent(in) :: zra
    real(kind=rp), dimension(0:nx+1,0:ny+1,0:nz), target, intent(in) :: zwa
    real(kind=rp), dimension(1:nx+1,0:ny+1,1:nz), target, intent(in) :: ua
    real(kind=rp), dimension(0:nx+1,1:ny+1,1:nz), target, intent(in) :: va
    real(kind=rp), dimension(0:nx+1,0:ny+1,0:nz), target, intent(in) :: wa
    real(kind=rp), dimension(1:nx+1,0:ny+1,1:nz), target, intent(out):: ufa
    real(kind=rp), dimension(0:nx+1,1:ny+1,1:nz), target, intent(out):: vfa

    real(kind=rp), dimension(:,:,:), pointer :: zr,zw
    real(kind=rp), dimension(:,:,:), pointer :: u,v,w
    real(kind=rp), dimension(:,:,:), pointer :: uf,vf

!!! dirty reshape arrays indexing ijk -> kji !!!
    real(kind=rp), dimension(:,:,:), allocatable, target :: zrb,zwb 
    real(kind=rp), dimension(:,:,:), allocatable, target :: ub,vb,wb
    real(kind=rp), dimension(:,:,:), allocatable, target :: ufb,vfb
!!!

    integer(kind=ip) :: i,j,k

    integer(kind=ip), save :: iter_fluxes=-1
    iter_fluxes = iter_fluxes + 1

    if (myrank==0) write(*,*)' nhmg_fluxes:'

    call tic(1,'nhmg_fluxes')

!!! dirty reshape arrays indexing ijk -> kji !!!
    allocate(zrb(1:nz,0:ny+1,0:nx+1))
    allocate(zwb(0:nz,0:ny+1,0:nx+1))
    allocate(ub(1:nz,0:ny+1,  nx+1))
    allocate(vb(1:nz,  ny+1,0:nx+1))
    allocate(wb(0:nz,0:ny+1,0:nx+1))
    do i = 1,nx+1
      do j = 0,ny+1
        do k = 1,nz
          ub(k,j,i) = ua(i,j,k)
        enddo
      enddo
    enddo
    do i = 0,nx+1
      do j = 1,ny+1
        do k = 1,nz
          vb(k,j,i) = va(i,j,k)
        enddo
      enddo
    enddo
    do i = 0,nx+1
      do j = 0,ny+1
        do k = 0,nz
          wb(k,j,i) = wa(i,j,k)
        enddo
      enddo
    enddo
    do i = 0,nx+1
      do j = 0,ny+1
        do k = 1,nz
          zrb(k,j,i) = zra(i,j,k)
        enddo
      enddo
    enddo
    do i = 0,nx+1
      do j = 0,ny+1
        do k = 0,nz
          zwb(k,j,i) = zwa(i,j,k)
        enddo
      enddo
    enddo
    zr => zrb
    zw => zwb
    u => ub
    v => vb
    w => wb

    allocate(ufb(1:nz,0:ny+1,  nx+1))
    allocate(vfb(1:nz,  ny+1,0:nx+1))
    uf => ufb
    vf => vfb
!!!

!    u => ua
!    v => va
!    w => wa
!    uf => ufa
!    vf => vfa

!    if (check_output) then
!       call write_netcdf(u,vname='u',netcdf_file_name='fl.nc',rank=myrank,iter=iter_fluxes)
!       call write_netcdf(v,vname='v',netcdf_file_name='fl.nc',rank=myrank,iter=iter_fluxes)
!       call write_netcdf(w,vname='w',netcdf_file_name='fl.nc',rank=myrank,iter=iter_fluxes)
!    endif

    call compute_fluxes(zr,zw,u,v,w,uf,vf)

!    if (check_output) then
!       call write_netcdf(uf,vname='uf',netcdf_file_name='fl.nc',rank=myrank,iter=iter_fluxes)
!       call write_netcdf(vf,vname='vf',netcdf_file_name='fl.nc',rank=myrank,iter=iter_fluxes)
!    endif

!!! dirty reshape arrays indexing kji -> ijk !!!
   do i = 1,nx+1
      do j = 0,ny+1
        do k = 1,nz
          ufa(i,j,k) = uf(k,j,i)
        enddo
      enddo
    enddo
    do i = 0,nx+1
      do j = 1,ny+1
        do k = 1,nz
          vfa(i,j,k) = vf(k,j,i)
        enddo
      enddo
    enddo
    deallocate(zrb)
    deallocate(zwb)
    deallocate(ub)
    deallocate(vb)
    deallocate(wb)
    deallocate(ufb)
    deallocate(vfb)
!!!

    if (associated(zr)) zr => null()
    if (associated(zw)) zw => null()
    if (associated(u)) u => null()
    if (associated(v)) v => null()
    if (associated(w)) w => null()
    if (associated(uf)) uf => null()
    if (associated(vf)) vf => null()

    call toc(1,'nhmg_fluxes')	
 
  end subroutine nhmg_fluxes

  !--------------------------------------------------------------
  subroutine nhmg_coupling(nx,ny,nz,zra,zwa,uf_bara,vf_bara,ua,va,wa,ufa,vfa)

    integer(kind=ip), intent(in) :: nx, ny, nz

    real(kind=rp), dimension(1:nx+1,0:ny+1),      target, intent(in) :: uf_bara
    real(kind=rp), dimension(0:nx+1,1:ny+1),      target, intent(in) :: vf_bara
    real(kind=rp), dimension(0:nx+1,0:ny+1,1:nz), target, intent(in) :: zra
    real(kind=rp), dimension(0:nx+1,0:ny+1,0:nz), target, intent(in) :: zwa
    real(kind=rp), dimension(1:nx+1,0:ny+1,1:nz), target, intent(inout) :: ua
    real(kind=rp), dimension(0:nx+1,1:ny+1,1:nz), target, intent(inout) :: va
    real(kind=rp), dimension(0:nx+1,0:ny+1,0:nz), target, intent(inout) :: wa
    real(kind=rp), dimension(1:nx+1,0:ny+1,1:nz), target, optional, intent(out):: ufa
    real(kind=rp), dimension(0:nx+1,1:ny+1,1:nz), target, optional, intent(out):: vfa

    real(kind=rp), dimension(:,:),   pointer :: uf_bar,vf_bar
    real(kind=rp), dimension(:,:,:), pointer :: zr,zw
    real(kind=rp), dimension(:,:,:), pointer :: u,v,w
    real(kind=rp), dimension(:,:,:), pointer :: uf,vf

!!! dirty reshape arrays indexing ijk -> kji !!!
    real(kind=rp), dimension(:,:),   allocatable, target :: uf_barb,vf_barb
    real(kind=rp), dimension(:,:,:), allocatable, target :: zrb,zwb 
    real(kind=rp), dimension(:,:,:), allocatable, target :: ub,vb,wb
    real(kind=rp), dimension(:,:,:), allocatable, target :: ufb,vfb
!!! 

    real(kind=rp), dimension(:,:,:), pointer :: Tmp3Darray

    integer(kind=ip) :: i,j,k

    integer(kind=ip), save :: iter_coupling=0
    iter_coupling = iter_coupling + 1

    if (myrank==0) write(*,*)' nhmg_coupling:'

    call tic(1,'nhmg_coupling')

!!! dirty reshape arrays indexing ijk -> kji !!!
    allocate(uf_barb(0:ny+1,  nx+1))
    allocate(vf_barb(  ny+1,0:nx+1))
    allocate(zrb(1:nz,0:ny+1,0:nx+1))
    allocate(zwb(0:nz,0:ny+1,0:nx+1))
    allocate(ub(1:nz,0:ny+1,  nx+1))
    allocate(vb(1:nz,  ny+1,0:nx+1))
    allocate(wb(0:nz,0:ny+1,0:nx+1))
    do i = 1,nx+1
      do j = 0,ny+1
          uf_barb(j,i) = uf_bara(i,j)
      enddo
    enddo
    allocate(Tmp3Darray(1:nz,0:ny+1, 0:nx+1))
    Tmp3Darray(:,:,:) = 0._rp
    Tmp3Darray(1,0:ny+1,1:nx+1)=uf_barb(0:ny+1,1:nx+1)
    call fill_halo(1,Tmp3Darray,lbc_null='u')
    uf_barb(0:ny+1,1:nx+1)=Tmp3Darray(1,0:ny+1,1:nx+1)
    do i = 0,nx+1
      do j = 1,ny+1
          vf_barb(j,i) = vf_bara(i,j)
      enddo
    enddo
    Tmp3Darray(:,:,:) = 0._rp
    Tmp3Darray(1,1:ny+1,0:nx+1)=vf_barb(1:ny+1,0:nx+1)
    call fill_halo(1,Tmp3Darray,lbc_null='v')
    vf_barb(1:ny+1,0:nx+1)=Tmp3Darray(1,1:ny+1,0:nx+1)
    deallocate(Tmp3Darray)
    do i = 0,nx+1
      do j = 0,ny+1
        do k = 1,nz
          zrb(k,j,i) = zra(i,j,k)
        enddo
      enddo
    enddo
    do i = 0,nx+1
      do j = 0,ny+1
        do k = 0,nz
          zwb(k,j,i) = zwa(i,j,k)
        enddo
      enddo
    enddo
    do i = 1,nx+1
      do j = 0,ny+1
        do k = 1,nz
          ub(k,j,i) = ua(i,j,k)
        enddo
      enddo
    enddo
    do i = 0,nx+1
      do j = 1,ny+1
        do k = 1,nz
          vb(k,j,i) = va(i,j,k)
        enddo
      enddo
    enddo
    do i = 0,nx+1
      do j = 0,ny+1
        do k = 0,nz
          wb(k,j,i) = wa(i,j,k)
        enddo
      enddo
    enddo
    uf_bar => uf_barb
    vf_bar => vf_barb
    zr => zrb
    zw => zwb
    u => ub
    v => vb
    w => wb

    call fill_halo(1,w)
!!!

!    zr => zra
!    zw => zwa
!    uf_bar => uf_bara
!    vf_bar => vf_bara
!    u => ua
!    v => va
!    w => wa

       if (check_output) then
          call write_netcdf(uf_bar,vname='uf_bar',netcdf_file_name='co.nc',rank=myrank,iter=iter_coupling)
          call write_netcdf(vf_bar,vname='vf_bar',netcdf_file_name='co.nc',rank=myrank,iter=iter_coupling)
          call write_netcdf(u,vname='uin',netcdf_file_name='co.nc',rank=myrank,iter=iter_coupling)
          call write_netcdf(v,vname='vin',netcdf_file_name='co.nc',rank=myrank,iter=iter_coupling)
          call write_netcdf(w,vname='win',netcdf_file_name='co.nc',rank=myrank,iter=iter_coupling)
       endif

    if ((present(ufa)).and.(present(vfa))) then
 
       allocate(ufb(1:nz,0:ny+1,  nx+1))
       allocate(vfb(1:nz,  ny+1,0:nx+1))
       uf => ufb
       vf => vfb

!       uf => ufa
!       vf => vfa

       call btbc_coupling(zr,zw,uf_bar,vf_bar,u,v,w,uf,vf)

    else

       call btbc_coupling(zr,zw,uf_bar,vf_bar,u,v,w)

    endif

       if (check_output) then
          call write_netcdf(u,vname='uout',netcdf_file_name='co.nc',rank=myrank,iter=iter_coupling)
          call write_netcdf(v,vname='vout',netcdf_file_name='co.nc',rank=myrank,iter=iter_coupling)
          call write_netcdf(w,vname='wout',netcdf_file_name='co.nc',rank=myrank,iter=iter_coupling)
          if ((present(ufa)).and.(present(vfa))) then
          call write_netcdf(uf,vname='uf',netcdf_file_name='co.nc',rank=myrank,iter=iter_coupling)
          call write_netcdf(vf,vname='vf',netcdf_file_name='co.nc',rank=myrank,iter=iter_coupling)
          endif
       endif

!!! dirty reshape arrays indexing kji -> ijk !!!
    do i = 1,nx+1
      do j = 0,ny+1
        do k = 1,nz
          ua(i,j,k) = u(k,j,i)
        enddo
      enddo
    enddo
    do i = 0,nx+1
      do j = 1,ny+1
        do k = 1,nz
          va(i,j,k) = v(k,j,i)
        enddo
      enddo
    enddo
    do i = 0,nx+1
      do j = 0,ny+1
        do k = 0,nz
          wa(i,j,k) = w(k,j,i)
        enddo
      enddo
    enddo
    if ((present(ufa)).and.(present(vfa))) then
    do i = 1,nx+1
      do j = 0,ny+1
        do k = 1,nz
          ufa(i,j,k) = uf(k,j,i)
        enddo
      enddo
    enddo
    do i = 0,nx+1
      do j = 1,ny+1
        do k = 1,nz
          vfa(i,j,k) = vf(k,j,i)
        enddo
      enddo
    enddo
    deallocate(ufb)
    deallocate(vfb)
    endif
    deallocate(uf_barb)
    deallocate(vf_barb)
    deallocate(zrb)
    deallocate(zwb)
    deallocate(ub)
    deallocate(vb)
    deallocate(wb)
!!!

    call toc(1,'nhmg_coupling')

  end subroutine nhmg_coupling

  !--------------------------------------------------------------
  subroutine nhmg_matrices(nx,ny,nz,zetaa,ha,hc,theta_b,theta_s)

    integer(kind=ip), intent(in) :: nx, ny, nz

    real(kind=rp), dimension(0:nx+1,0:ny+1), intent(in) :: zetaa, ha
    real(kind=rp),                           intent(in) :: hc, theta_b, theta_s
    real(kind=rp), dimension(0:ny+1,0:nx+1), target     :: zetab, hb
    real(kind=rp), dimension(:,:)          , pointer    :: zeta, h

    integer(kind=ip), save :: iter_matrices=-1
    iter_matrices = iter_matrices + 1

    if (myrank==0) write(*,*)' nhmg_matrices:'

    zetab = transpose(zetaa)
    hb = transpose(ha)

    zeta => zetab
    h => hb

    nhhc      = hc
    nhtheta_b = theta_b
    nhtheta_s = theta_s

    call define_matrices(zeta,h)

    if (check_output) then
       if ((iter_matrices .EQ. 199) .OR. (iter_matrices .EQ. 200) .OR. &
           (iter_matrices .EQ. 999) .OR. (iter_matrices .EQ. 1000) .OR. &
           (iter_matrices .EQ. 1999) .OR. (iter_matrices .EQ. 2000) .OR. &
           (iter_matrices .GE. 3499)) then
       call write_netcdf(grid(1)%cA,vname='cA',netcdf_file_name='so.nc',rank=myrank,iter=iter_matrices)
       endif
    endif

  end subroutine nhmg_matrices

  !--------------------------------------------------------------
  subroutine nhmg_solve(nx,ny,nz,zra,zwa,ua,va,wa,dt,rua,rva)

    integer(kind=ip), intent(in) :: nx, ny, nz

    real(kind=rp), dimension(0:nx+1,0:ny+1,1:nz), target, intent(in) :: zra
    real(kind=rp), dimension(0:nx+1,0:ny+1,0:nz), target, intent(in) :: zwa
    real(kind=rp), dimension(1:nx+1,0:ny+1,1:nz), target, intent(inout) :: ua
    real(kind=rp), dimension(0:nx+1,1:ny+1,1:nz), target, intent(inout) :: va
    real(kind=rp), dimension(0:nx+1,0:ny+1,0:nz), target, intent(inout) :: wa
    real(kind=rp),                                   optional, intent(in) :: dt
    real(kind=rp), dimension(1:nx+1,0:ny+1), target, optional, intent(out):: rua
    real(kind=rp), dimension(0:nx+1,1:ny+1), target, optional, intent(out):: rva

    real(kind=rp), dimension(:,:,:), pointer :: zr,zw
    real(kind=rp), dimension(:,:,:), pointer :: u, v, w
    real(kind=rp), dimension(:,:)  , pointer :: ru, rv

!!! dirty reshape arrays indexing
    real(kind=rp), dimension(:,:,:), allocatable, target :: zrb,zwb 
    real(kind=rp), dimension(:,:,:), allocatable, target :: ub, vb, wb
!!! 

    real(kind=rp)    :: tol
    integer(kind=ip) :: maxite

    integer(kind=ip) :: i,j,k

    integer(kind=ip), save :: iter_solve=-1
    iter_solve = iter_solve + 1

    if (myrank==0) write(*,*)' nhmg_solve:'

    call tic(1,'nhmg_solve')

    tol    = solver_prec    
    maxite = solver_maxiter 

!!! dirty reshape arrays indexing ijk -> kji !!!
    allocate(zrb(1:nz,0:ny+1,0:nx+1))
    allocate(zwb(0:nz,0:ny+1,0:nx+1))
    allocate( ub(1:nz,0:ny+1,0:nx+1))
    allocate( vb(1:nz,0:ny+1,0:nx+1))
    allocate( wb(0:nz,0:ny+1,0:nx+1))
    do i = 0,nx+1
      do j = 0,ny+1
        do k = 1,nz
          zrb(k,j,i) = zra(i,j,k)
        enddo
      enddo
    enddo
    do i = 0,nx+1
      do j = 0,ny+1
        do k = 0,nz
          zwb(k,j,i) = zwa(i,j,k)
        enddo
      enddo
    enddo
    do i = 1,nx+1
      do j = 0,ny+1
        do k = 1,nz
          ub(k,j,i) = ua(i,j,k)
        enddo
      enddo
    enddo
    i = 0
    do j = 0,ny+1
       do k = 1,nz
          ub(k,j,i) = 0._rp
       enddo
    enddo
    do i = 0,nx+1
      do j = 1,ny+1
        do k = 1,nz
          vb(k,j,i) = va(i,j,k)
        enddo
      enddo
    enddo
    j = 0
    do i = 0,nx+1
       do k = 1,nz
          vb(k,j,i) = 0._rp
       enddo
    enddo
    do i = 0,nx+1
       do j = 0,ny+1
          do k = 0,nz
             wb(k,j,i) = wa(i,j,k)
          enddo
       enddo
    enddo
    zr => zrb
    zw => zwb
    u => ub
    v => vb
    w => wb
!!! 

    call fill_halo(1,u,lbc_null='u')
    call fill_halo(1,v,lbc_null='v')
    call fill_halo(1,w)
    !    u => ua
    !    v => va
    !    w => wa

       if (check_output) then
!       if ((iter_solve .EQ. 199) .OR. (iter_solve .EQ. 200) .OR. &
!           (iter_solve .EQ. 999) .OR. (iter_solve .EQ. 1000) .OR. &
!           (iter_solve .EQ. 1999) .OR. (iter_solve .EQ. 2000) .OR. &
!           (iter_solve .GE. 3499)) then
          call write_netcdf(u,vname='uin',netcdf_file_name='so.nc',rank=myrank,iter=iter_solve)
          call write_netcdf(v,vname='vin',netcdf_file_name='so.nc',rank=myrank,iter=iter_solve)
          call write_netcdf(w,vname='win',netcdf_file_name='so.nc',rank=myrank,iter=iter_solve)
!          endif
       endif

    !- step 1 - 
    call compute_rhs(zr,zw,u,v,w)

    if (check_output) then
!       if ((iter_solve .EQ. 199) .OR. (iter_solve .EQ. 200) .OR. &
!           (iter_solve .EQ. 999) .OR. (iter_solve .EQ. 1000) .OR. &
!           (iter_solve .EQ. 1999) .OR. (iter_solve .EQ. 2000) .OR. &
!           (iter_solve .GE. 3499)) then
       call write_netcdf(grid(1)%b,vname='b',netcdf_file_name='so.nc',rank=myrank,iter=iter_solve)
!       endif
    endif

    !- step 2 -
    call solve_p(tol,maxite)

    if (check_output) then
!       if ((iter_solve .EQ. 199) .OR. (iter_solve .EQ. 200) .OR. &
!           (iter_solve .EQ. 999) .OR. (iter_solve .EQ. 1000) .OR. &
!           (iter_solve .EQ. 1999) .OR. (iter_solve .EQ. 2000) .OR. &
!           (iter_solve .GE. 3499)) then
       call write_netcdf(grid(1)%p,vname='p',netcdf_file_name='so.nc',rank=myrank,iter=iter_solve)
       call write_netcdf(grid(1)%r,vname='r',netcdf_file_name='so.nc',rank=myrank,iter=iter_solve)
!       endif
    endif

    !- step 3 -
    call correct_uvw(zr,zw,u,v,w)

       if (check_output) then
!       if ((iter_solve .EQ. 199) .OR. (iter_solve .EQ. 200) .OR. &
!           (iter_solve .EQ. 999) .OR. (iter_solve .EQ. 1000) .OR. &
!           (iter_solve .EQ. 1999) .OR. (iter_solve .EQ. 2000) .OR. &
!           (iter_solve .GE. 3499)) then
          call write_netcdf(u,vname='uout',netcdf_file_name='so.nc',rank=myrank,iter=iter_solve)
          call write_netcdf(v,vname='vout',netcdf_file_name='so.nc',rank=myrank,iter=iter_solve)
          call write_netcdf(w,vname='wout',netcdf_file_name='so.nc',rank=myrank,iter=iter_solve)
!          endif
       endif

    !- step 4 -
    if ((present(dt)).and.(present(rua)).and.(present(rva))) then

       ru => rua
       rv => rva

       call compute_barofrc(zr,zw,dt,ru,rv)

       if (check_output) then
!       if ((iter_solve .EQ. 199) .OR. (iter_solve .EQ. 200) .OR. &
!           (iter_solve .EQ. 999) .OR. (iter_solve .EQ. 1000) .OR. &
!           (iter_solve .EQ. 1999) .OR. (iter_solve .EQ. 2000) .OR. &
!           (iter_solve .GE. 3499)) then
          call write_netcdf(ru,vname='ru',netcdf_file_name='so.nc',rank=myrank,iter=iter_solve)
          call write_netcdf(rv,vname='rv',netcdf_file_name='so.nc',rank=myrank,iter=iter_solve)
!          endif
       endif

    endif

    !- step 5 -
    call compute_rhs(zr,zw,u,v,w)

    if (check_output) then
!       if ((iter_solve .EQ. 199) .OR. (iter_solve .EQ. 200) .OR. &
!           (iter_solve .EQ. 999) .OR. (iter_solve .EQ. 1000) .OR. &
!           (iter_solve .EQ. 1999) .OR. (iter_solve .EQ. 2000) .OR. &
!           (iter_solve .GE. 3499)) then
       call write_netcdf(grid(1)%b,vname='bout',netcdf_file_name='so.nc',rank=myrank,iter=iter_solve)
!       endif
    endif

!!! dirty reshape arrays indexing kji -> ijk !!!
    do i = 1,nx+1
       do j = 0,ny+1
          do k = 1,nz
             ua(i,j,k) = ub(k,j,i)
          enddo
       enddo
    enddo
    do i = 0,nx+1
       do j = 1,ny+1
          do k = 1,nz
             va(i,j,k) = vb(k,j,i)
          enddo
       enddo
    enddo
    do i = 0,nx+1
       do j = 0,ny+1
          do k = 0,nz
             wa(i,j,k) = wb(k,j,i)
          enddo
       enddo
    enddo
    deallocate(zrb)
    deallocate(zwb)
    deallocate(ub)
    deallocate(vb)
    deallocate(wb)
!!!

    if (associated(u)) u => null()
    if (associated(v)) v => null()
    if (associated(w)) w => null()

    call toc(1,'nhmg_solve')	

  end subroutine nhmg_solve

  !--------------------------------------------------------------
  subroutine nhmg_clean()

    real(kind=rp) :: tstart,tend,perf

    call cpu_time(tstart)

    call grids_dealloc()

    call print_tictoc()

    call cpu_time(tend)

    if (myrank == 0) write(*,*)'nhmg_clean time:',tend-tstart

  end subroutine nhmg_clean

end module nhmg_interface
#else
        module nhmg_interface_empty
        end module
#endif
