#include "cppdefs.h"
#if defined NBQ && defined MPI

 subroutine MPI_nbq_Setup(imax,jmax,kmax)
!------------------------------------------------------------------------------
!                       NBQ Module: prepares MPI exchanges
!------------------------------------------------------------------------------

  use module_parallel_nbq
  use module_nbq  !!!ONLY,......
  implicit none
# include "mpi_roms.h"
  integer,intent(in) :: imax,jmax,kmax
  par%rank = mynode
  par%comm2d = MPI_COMM_WORLD
  par%tvoisin = MPI_PROC_NULL
  if (WEST_INTER_NBQ)    par%tvoisin(ouest)     = p_W
  if (EAST_INTER_NBQ)    par%tvoisin(est)       = p_E
  if (SOUTH_INTER_NBQ)   par%tvoisin(sud)       = p_S
  if (NORTH_INTER_NBQ)   par%tvoisin(nord)      = p_N
  if (SOUTH_INTER_NBQ .and. WEST_INTER_NBQ) par%tvoisin(sudouest)  = p_SW
  if (NORTH_INTER_NBQ .and. EAST_INTER_NBQ) par%tvoisin(nordest)   = p_NE
  if (SOUTH_INTER_NBQ .and. EAST_INTER_NBQ) par%tvoisin(sudest)    = p_SE
  if (NORTH_INTER_NBQ .and. WEST_INTER_NBQ) par%tvoisin(nordouest) = p_NW
  
# ifdef NBQ_OUT
  write(100+mynode,*) "***_INTER", &
       NORTH_INTER_NBQ,SOUTH_INTER_NBQ,EAST_INTER_NBQ,WEST_INTER_NBQ
  write(200+mynode,*) " par%tvoisin=", par%tvoisin
# endif
 
  !print *,mynode,":",par%tvoisin 
  !print *,mynode,"imax,jmax,kmax=",imax,jmax,kmax 
  
  !call create_echange_qdm_nbq_a(imax,jmax,kmax) 
  call create_echange_qdmU_nbq_a(imax,jmax,kmax)
  call create_echange_qdmV_nbq_a(imax,jmax,kmax)
  call create_echange_qdmW_nbq_a(imax,jmax,kmax)
  call create_echange_div_nbq_a(imax,jmax,kmax)
  !call create_echange_rhs2_nh(imax,jmax,kmax) !No longer used
  
  ! Initalize persistant communications
  call Persistant_init
  
 end subroutine MPI_nbq_Setup

#else
  subroutine MPI_nbq_Setup_empty
  end subroutine MPI_nbq_Setup_empty
#endif
