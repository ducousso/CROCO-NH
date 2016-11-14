#include "cppdefs.h"
subroutine grid_exchange
#if defined NBQ  && defined MPI
  use module_parallel_nbq
  use module_nh
  implicit none
  integer,parameter :: TAG1=1000, TAG2=2000,   TAG3=3000,  TAG4=4000
  integer,parameter :: TAG5=5000, TAG6=6000,   TAG7=7000,   TAG8=8000
  integer,parameter :: TAG9=9000, TAG10=10000

  integer,parameter :: TAG11=11000, TAG12=12000,   TAG13=13000,  TAG14=14000
  integer,parameter :: TAG15=15000, TAG16=16000,   TAG17=17000,  TAG18=18000
  integer,parameter :: TAG19=19000, TAG20=20000

  integer,parameter :: TAG21=21000, TAG22=22000,   TAG23=23000,  TAG24=24000
  integer,parameter :: TAG25=25000, TAG26=26000,   TAG27=27000,  TAG28=28000
  integer,parameter :: TAG29=29000, TAG30=30000

  integer,parameter :: TAG31=31000, TAG32=32000,   TAG33=33000,  TAG34=34000
  integer,parameter :: TAG35=35000, TAG36=36000,   TAG37=37000,  TAG38=38000
  integer,parameter :: TAG39=39000, TAG40=40000

  integer,parameter :: TAG41=41000, TAG42=42000,   TAG43=43000,  TAG44=44000
  integer,parameter :: TAG45=45000, TAG46=46000,   TAG47=47000,  TAG48=48000
  integer,parameter :: TAG49=49000, TAG50=50000

  integer,parameter :: TAG51=51000, TAG52=52000,   TAG53=53000,  TAG54=54000
  integer,parameter :: TAG55=55000, TAG56=56000,   TAG57=57000,  TAG58=58000
  integer,parameter :: TAG59=59000, TAG60=60000

  double precision,dimension(istru_nh-1:iend_nh+1) 	:: ftr_ouest
  integer,dimension(256) 				:: tabreq
  integer,dimension(MPI_STATUS_SIZE,256) 		:: tstatus
  integer 						:: nbreq, ierror
  
  integer :: i,j,k
!**********************************************************************
!
! MPI NEEDS:
!
! gdepth_u, coefa_u and coefb_u: 
!                we need here ( istru_nh-1,[jstr_nh:jend_nh],[0:N+1] )
!                             ( iendu_nh+1,[jstr_nh:jend_nh],[0:N+1] )               
! gdepth_v, coefa_v and coefb_v: 
!                we need here ( [istr_nh:iend_nh],jstrv_nh-1,[0:N+1] )
!                             ( [istr_nh:iend_nh],jendv_nh+1,[0:N+1] ) 
!  
!**********************************************************************
if (doalloc) then
    ! U variables -----------------------------------------------------
    ! ---  gdepth_u --- ouest-est
    allocate(gdepth_uS%ouest(jend_nh-jstr_nh+1,N+2))
    allocate(  gdepth_uS%est(jend_nh-jstr_nh+1,N+2))
    allocate(gdepth_uR%ouest(jend_nh-jstr_nh+1,N+2))
    allocate(  gdepth_uR%est(jend_nh-jstr_nh+1,N+2))
    ! ---  gdepth_u --- sud-nord
    allocate(gdepth_uS%sud(iendu_nh-istru_nh+1,N+2))
    allocate(  gdepth_uS%nord(iendu_nh-istru_nh+1,N+2))
    allocate(gdepth_uR%sud(iendu_nh-istru_nh+1,N+2))
    allocate(  gdepth_uR%nord(iendu_nh-istru_nh+1,N+2))
    
    ! ---  coefa_u --- ouest-est
    allocate(coefa_uS%ouest(jend_nh-jstr_nh+1,N+2))
    allocate( coefa_uS%est (jend_nh-jstr_nh+1,N+2))
    allocate(coefa_uR%ouest(jend_nh-jstr_nh+1,N+2))
    allocate(  coefa_uR%est(jend_nh-jstr_nh+1,N+2))
    ! ---  coefa_u --- sud-nord
    allocate(coefa_uS%sud(iendu_nh-istru_nh+1,N+2))
    allocate( coefa_uS%nord (iendu_nh-istru_nh+1,N+2))
    allocate(coefa_uR%sud(iendu_nh-istru_nh+1,N+2))
    allocate(  coefa_uR%nord(iendu_nh-istru_nh+1,N+2))
    
    ! ---  coefb_u --- ouest-est
    allocate(coefb_uS%ouest(jend_nh-jstr_nh+1,N+2))
    allocate(  coefb_uS%est(jend_nh-jstr_nh+1,N+2))
    allocate(coefb_uR%ouest(jend_nh-jstr_nh+1,N+2))
    allocate(  coefb_uR%est(jend_nh-jstr_nh+1,N+2))
    ! ---  coefb_u --- sud-nord
    allocate(coefb_uS%sud(iendu_nh-istru_nh+1,N+2))
    allocate(  coefb_uS%nord(iendu_nh-istru_nh+1,N+2))
    allocate(coefb_uR%sud(iendu_nh-istru_nh+1,N+2))
    allocate(  coefb_uR%nord(iendu_nh-istru_nh+1,N+2))
    sz1 = (jend_nh-jstr_nh+1)*(N+2)   ! WEST-EST boundary
    sz3 = (iendu_nh-istru_nh+1)*(N+2) ! SOUTH-NORTH boundary
    
    
    ! V variables -----------------------------------------------------
    ! ---  gdepth_v --- ouest-est
    allocate(gdepth_vS%ouest(jendv_nh-jstrv_nh+1,N+2))
    allocate(  gdepth_vS%est(jendv_nh-jstrv_nh+1,N+2))
    allocate(gdepth_vR%ouest(jendv_nh-jstrv_nh+1,N+2))
    allocate(  gdepth_vR%est(jendv_nh-jstrv_nh+1,N+2))
    ! ---  gdepth_v --- sud-nord
    allocate(gdepth_vS%sud(iend_nh-istr_nh+1,N+2))
    allocate(  gdepth_vS%nord(iend_nh-istr_nh+1,N+2))
    allocate(gdepth_vR%sud(iend_nh-istr_nh+1,N+2))
    allocate(  gdepth_vR%nord(iend_nh-istr_nh+1,N+2))
    
    ! ---  coefa_u --- ouest-est
    allocate(coefa_vS%ouest(jendv_nh-jstrv_nh+1,N+2))
    allocate(  coefa_vS%est(jendv_nh-jstrv_nh+1,N+2))
    allocate(coefa_vR%ouest(jendv_nh-jstrv_nh+1,N+2))
    allocate(  coefa_vR%est(jendv_nh-jstrv_nh+1,N+2))
    ! ---  coefa_u --- sud-nord
    allocate(coefa_vS%sud(iend_nh-istr_nh+1,N+2))
    allocate(  coefa_vS%nord(iend_nh-istr_nh+1,N+2))
    allocate(coefa_vR%sud(iend_nh-istr_nh+1,N+2))
    allocate(  coefa_vR%nord(iend_nh-istr_nh+1,N+2))
    
    ! ---  coefb_u --- ouest-est
    allocate(coefb_vS%ouest(jendv_nh-jstrv_nh+1,N+2))
    allocate(  coefb_vS%est(jendv_nh-jstrv_nh+1,N+2))
    allocate(coefb_vR%ouest(jendv_nh-jstrv_nh+1,N+2))
    allocate(  coefb_vR%est(jendv_nh-jstrv_nh+1,N+2))
    ! ---  coefb_u --- sud-nord
    allocate(coefb_vS%sud(iend_nh-istr_nh+1,N+2))
    allocate(  coefb_vS%nord(iend_nh-istr_nh+1,N+2))
    allocate(coefb_vR%sud(iend_nh-istr_nh+1,N+2))
    allocate(  coefb_vR%nord(iend_nh-istr_nh+1,N+2))
    sz2 = (jendv_nh-jstrv_nh+1)*(N+2) ! WEST-EST boundary
    sz4 = (iend_nh-istr_nh+1)*(N+2)   ! SOUTH-NORTH boundary
    
    ! Corner -----------------------------------------------------
    ! U variables -----------------------------------------------------
    ! ---  gdepth_u
    !	---  gdepth_u --- nord-est<-->sud-ouest	
    allocate(gdepth_uS%nordest(1,N+2))
    allocate(gdepth_uS%sudouest(1,N+2))
    allocate(gdepth_uR%nordest(1,N+2))
    allocate(gdepth_uR%sudouest(1,N+2))
    !	---  gdepth_u --- sud-est<-->nord-ouest	
    allocate(gdepth_uS%sudest(1,N+2))
    allocate(gdepth_uS%nordouest(1,N+2))
    allocate(gdepth_uR%sudest(1,N+2))
    allocate(gdepth_uR%nordouest(1,N+2))

    ! ---  coefa_u
    !	---  coefa_u --- nord-est<-->sud-ouest	
    allocate(coefa_uS%nordest(1,N+2))
    allocate(coefa_uS%sudouest(1,N+2))
    allocate(coefa_uR%nordest(1,N+2))
    allocate(coefa_uR%sudouest(1,N+2))
    !	---  coefa_u --- sud-est<-->nord-ouest	
    allocate(coefa_uS%sudest(1,N+2))
    allocate(coefa_uS%nordouest(1,N+2))
    allocate(coefa_uR%sudest(1,N+2))
    allocate(coefa_uR%nordouest(1,N+2))

    ! ---  coefb_u
    !	---  coefb_u --- nord-est<-->sud-ouest	
    allocate(coefb_uS%nordest(1,N+2))
    allocate(coefb_uS%sudouest(1,N+2))
    allocate(coefb_uR%nordest(1,N+2))
    allocate(coefb_uR%sudouest(1,N+2))
    !	---  coefb_u --- sud-est<-->nord-ouest	
    allocate(coefb_uS%sudest(1,N+2))
    allocate(coefb_uS%nordouest(1,N+2))
    allocate(coefb_uR%sudest(1,N+2))
    allocate(coefb_uR%nordouest(1,N+2))
   
   
    ! V variables -----------------------------------------------------
    ! ---  gdepth_v
    !	---  gdepth_v --- nord-est<-->sud-ouest	
    allocate(gdepth_vS%nordest(1,N+2))
    allocate(gdepth_vS%sudouest(1,N+2))
    allocate(gdepth_vR%nordest(1,N+2))
    allocate(gdepth_vR%sudouest(1,N+2))
    !	---  gdepth_v --- sud-est<-->nord-ouest	
    allocate(gdepth_vS%sudest(1,N+2))
    allocate(gdepth_vS%nordouest(1,N+2))
    allocate(gdepth_vR%sudest(1,N+2))
    allocate(gdepth_vR%nordouest(1,N+2))
    ! ---  coefa_v 
    ! 	---  coefa_v --- nord-est<-->sud-ouest	
    allocate(coefa_vS%nordest(1,N+2))
    allocate(coefa_vS%sudouest(1,N+2))
    allocate(coefa_vR%nordest(1,N+2))
    allocate(coefa_vR%sudouest(1,N+2))
    ! 	---  coefa_v --- sud-est<-->nord-ouest	
    allocate(coefa_vS%sudest(1,N+2))
    allocate(coefa_vS%nordouest(1,N+2))
    allocate(coefa_vR%sudest(1,N+2))
    allocate(coefa_vR%nordouest(1,N+2))
    
    ! ---  coefb_v
    ! 	---  coefb_v --- nord-est<-->sud-ouest	
    allocate(coefb_vS%nordest(1,N+2))
    allocate(coefb_vS%sudouest(1,N+2))
    allocate(coefb_vR%nordest(1,N+2))
    allocate(coefb_vR%sudouest(1,N+2))
    ! 	---  coefb_v --- sud-est<-->nord-ouest	
    allocate(coefb_vS%sudest(1,N+2))
    allocate(coefb_vS%nordouest(1,N+2))
    allocate(coefb_vR%sudest(1,N+2))
    allocate(coefb_vR%nordouest(1,N+2))
    
    doalloc=.FALSE.
  endif


!    print *,par%tvoisin(:)
!    print *,"N=",N
! !   ! Number of exchange
!    call mpi_finalize(ierr)
!    stop 'ici...'
  nbreq=0

  !******************************************************************
  !		 U point variables
  !******************************************************************
  
  !******************************************************************
  !		 WEST direction
  !******************************************************************
  if (par%tvoisin(ouest) /= MPI_PROC_NULL) then
  !-------- gdepth_u exchange ---------------------------------------
    gdepth_uS%ouest = gdepth_u(istru_nh,jstr_nh:jend_nh,0:N+1)
    nbreq=nbreq+1
    call MPI_ISEND(gdepth_uS%ouest, sz1, MPI_DOUBLE_PRECISION, &
      par%tvoisin(ouest), TAG1, par%comm2d, tabreq(nbreq), ierror)
    nbreq=nbreq+1
    call MPI_IRECV(gdepth_uR%ouest, sz1, MPI_DOUBLE_PRECISION, &
      par%tvoisin(ouest), TAG2, par%comm2d, tabreq(nbreq), ierror)
  !-------- coefa_u exchange ---------------------------------------
    coefa_uS%ouest = coefa_u(istru_nh,jstr_nh:jend_nh,0:N+1)
    nbreq=nbreq+1
    call MPI_ISEND(coefa_uS%ouest, sz1, MPI_DOUBLE_PRECISION, &
      par%tvoisin(ouest), TAG3, par%comm2d, tabreq(nbreq), ierror)
    nbreq=nbreq+1
    call MPI_IRECV(coefa_uR%ouest, sz1, MPI_DOUBLE_PRECISION, &
      par%tvoisin(ouest), TAG4, par%comm2d, tabreq(nbreq), ierror)
   !-------- coefb_u exchange ---------------------------------------     
    coefb_uS%ouest = coefb_u(istru_nh,jstr_nh:jend_nh,0:N+1)
    nbreq=nbreq+1
    call MPI_ISEND(coefb_uS%ouest, sz1, MPI_DOUBLE_PRECISION, &
      par%tvoisin(ouest), TAG5, par%comm2d, tabreq(nbreq), ierror)
    nbreq=nbreq+1
    call MPI_IRECV(coefb_uR%ouest, sz1, MPI_DOUBLE_PRECISION, &
      par%tvoisin(ouest), TAG6, par%comm2d, tabreq(nbreq), ierror)
  endif
  
  
  !******************************************************************
  !		 EAST direction
  !******************************************************************
   if (par%tvoisin(est) /= MPI_PROC_NULL) then
  !-------- gdepth_u exchange ---------------------------------------
    gdepth_uS%est   = gdepth_u(iendu_nh,jstr_nh:jend_nh,0:N+1)
    nbreq=nbreq+1
    call MPI_ISEND(gdepth_uS%est, sz1, MPI_DOUBLE_PRECISION, &
      par%tvoisin(est), TAG2, par%comm2d, tabreq(nbreq), ierror)
    nbreq=nbreq+1
    call MPI_IRECV(gdepth_uR%est, sz1, MPI_DOUBLE_PRECISION, &
      par%tvoisin(est), TAG1, par%comm2d, tabreq(nbreq), ierror)
  !-------- coefa_u exchange ---------------------------------------
    coefa_uS%est   = coefa_u(iendu_nh,jstr_nh:jend_nh,0:N+1)
    nbreq=nbreq+1
    call MPI_ISEND(coefa_uS%est, sz1, MPI_DOUBLE_PRECISION, &
      par%tvoisin(est), TAG4, par%comm2d, tabreq(nbreq), ierror)
    nbreq=nbreq+1
    call MPI_IRECV(coefa_uR%est, sz1, MPI_DOUBLE_PRECISION, &
      par%tvoisin(est), TAG3, par%comm2d, tabreq(nbreq), ierror)
  !-------- coefb_u exchange ---------------------------------------     
    coefb_uS%est   = coefb_u(iendu_nh,jstr_nh:jend_nh,0:N+1)
    nbreq=nbreq+1
    call MPI_ISEND(coefb_uS%est, sz1, MPI_DOUBLE_PRECISION, &
      par%tvoisin(est), TAG6, par%comm2d, tabreq(nbreq), ierror)
    nbreq=nbreq+1
    call MPI_IRECV(coefb_uR%est, sz1, MPI_DOUBLE_PRECISION, &
      par%tvoisin(est), TAG5, par%comm2d, tabreq(nbreq), ierror)
  endif
  
  
  !******************************************************************
  !		 NORTH direction
  !******************************************************************
  if (par%tvoisin(nord) /= MPI_PROC_NULL) then
  !-------- gdepth_u exchange ---------------------------------------
    gdepth_uS%nord   = gdepth_u(istru_nh:iendu_nh,jend_nh,0:N+1)
    nbreq=nbreq+1
    call MPI_ISEND(gdepth_uS%nord, sz3, MPI_DOUBLE_PRECISION, &
      par%tvoisin(nord), TAG14, par%comm2d, tabreq(nbreq), ierror)
    nbreq=nbreq+1
    call MPI_IRECV(gdepth_uR%nord, sz3, MPI_DOUBLE_PRECISION, &
      par%tvoisin(nord), TAG13, par%comm2d, tabreq(nbreq), ierror)
  !-------- coefa_u exchange ---------------------------------------
    coefa_uS%nord = coefa_u(istru_nh:iendu_nh,jend_nh,0:N+1)
    nbreq=nbreq+1
    call MPI_ISEND(coefa_uS%nord, sz3, MPI_DOUBLE_PRECISION, &
      par%tvoisin(nord), TAG16, par%comm2d, tabreq(nbreq), ierror)
    nbreq=nbreq+1
    call MPI_IRECV(coefa_uR%nord, sz3, MPI_DOUBLE_PRECISION, &
      par%tvoisin(nord), TAG15, par%comm2d, tabreq(nbreq), ierror)
  !-------- coefb_u exchange ---------------------------------------     
    coefb_uS%nord = coefb_u(istru_nh:iendu_nh,jend_nh,0:N+1)
    nbreq=nbreq+1
    call MPI_ISEND(coefb_uS%nord, sz3, MPI_DOUBLE_PRECISION, &
      par%tvoisin(nord), TAG18, par%comm2d, tabreq(nbreq), ierror)
    nbreq=nbreq+1
    call MPI_IRECV(coefb_uR%nord, sz3, MPI_DOUBLE_PRECISION, &
      par%tvoisin(nord), TAG17, par%comm2d, tabreq(nbreq), ierror)
  endif

  !******************************************************************
  !		 SOUTH direction
  !******************************************************************
  if (par%tvoisin(sud) /= MPI_PROC_NULL) then
  !-------- gdepth_u exchange ---------------------------------------
    gdepth_uS%sud = gdepth_u(istru_nh:iendu_nh,jstr_nh,0:N+1)
    nbreq=nbreq+1
    call MPI_ISEND(gdepth_uS%sud, sz3, MPI_DOUBLE_PRECISION, &
      par%tvoisin(sud), TAG13, par%comm2d, tabreq(nbreq), ierror)
    nbreq=nbreq+1
    call MPI_IRECV(gdepth_uR%sud, sz3, MPI_DOUBLE_PRECISION, &
      par%tvoisin(sud), TAG14, par%comm2d, tabreq(nbreq), ierror)
  !-------- coefa_u exchange ---------------------------------------
    coefa_uS%sud   = coefa_u(istru_nh:iendu_nh,jstr_nh,0:N+1)
    nbreq=nbreq+1
    call MPI_ISEND(coefa_uS%sud, sz3, MPI_DOUBLE_PRECISION, &
      par%tvoisin(sud), TAG15, par%comm2d, tabreq(nbreq), ierror)
    nbreq=nbreq+1
    call MPI_IRECV(coefa_uR%sud, sz3, MPI_DOUBLE_PRECISION, &
      par%tvoisin(sud), TAG16, par%comm2d, tabreq(nbreq), ierror)
  !-------- coefb_u exchange ---------------------------------------     
    coefb_uS%sud   = coefb_u(istru_nh:iendu_nh,jstr_nh,0:N+1)
    nbreq=nbreq+1
    call MPI_ISEND(coefb_uS%sud, sz3, MPI_DOUBLE_PRECISION, &
      par%tvoisin(sud), TAG17, par%comm2d, tabreq(nbreq), ierror)
    nbreq=nbreq+1
    call MPI_IRECV(coefb_uR%sud, sz3, MPI_DOUBLE_PRECISION, &
      par%tvoisin(sud), TAG18, par%comm2d, tabreq(nbreq), ierror)
  endif 

  
  !******************************************************************
  !********************* V Point variables    ***********************
  
  !******************************************************************
  !		 WEST direction
  !******************************************************************
  if (par%tvoisin(ouest) /= MPI_PROC_NULL) then
  !-------- gdepth_v exchange ---------------------------------------
    gdepth_vS%ouest = gdepth_v(istr_nh,jstrv_nh:jendv_nh,0:N+1)
    nbreq=nbreq+1
    call MPI_ISEND(gdepth_vS%ouest, sz2, MPI_DOUBLE_PRECISION, &
      par%tvoisin(ouest), TAG19, par%comm2d, tabreq(nbreq), ierror)
    nbreq=nbreq+1
    call MPI_IRECV(gdepth_vR%ouest, sz2, MPI_DOUBLE_PRECISION, &
      par%tvoisin(ouest), TAG20, par%comm2d, tabreq(nbreq), ierror)
  !-------- coefa_v exchange ---------------------------------------
    coefa_vS%ouest = coefa_v(istr_nh,jstrv_nh:jendv_nh,0:N+1)
    nbreq=nbreq+1
    call MPI_ISEND(coefa_vS%ouest, sz2, MPI_DOUBLE_PRECISION, &
      par%tvoisin(ouest), TAG21, par%comm2d, tabreq(nbreq), ierror)
    nbreq=nbreq+1
    call MPI_IRECV(coefa_vR%ouest, sz2, MPI_DOUBLE_PRECISION, &
      par%tvoisin(ouest), TAG22, par%comm2d, tabreq(nbreq), ierror)
  !-------- coefb_v exchange ---------------------------------------
    coefb_vS%ouest = coefb_v(istr_nh,jstrv_nh:jendv_nh,0:N+1)
    nbreq=nbreq+1
    call MPI_ISEND(coefb_vS%ouest, sz2, MPI_DOUBLE_PRECISION, &
      par%tvoisin(ouest), TAG23, par%comm2d, tabreq(nbreq), ierror)
    nbreq=nbreq+1
    call MPI_IRECV(coefb_vR%ouest, sz2, MPI_DOUBLE_PRECISION, &
      par%tvoisin(ouest), TAG24, par%comm2d, tabreq(nbreq), ierror)
  endif

  !******************************************************************
  !		 EAST direction
  !******************************************************************
  if (par%tvoisin(est) /= MPI_PROC_NULL) then
  !-------- gdepth_v exchange ---------------------------------------
    gdepth_vS%est   = gdepth_v(iend_nh,jstrv_nh:jendv_nh,0:N+1)
    nbreq=nbreq+1
    call MPI_ISEND(gdepth_vS%est, sz2, MPI_DOUBLE_PRECISION, &
      par%tvoisin(est), TAG20, par%comm2d, tabreq(nbreq), ierror)
    nbreq=nbreq+1
    call MPI_IRECV(gdepth_vR%est, sz2, MPI_DOUBLE_PRECISION, &
      par%tvoisin(est), TAG19, par%comm2d, tabreq(nbreq), ierror)
  !-------- coefa_v exchange ---------------------------------------
    coefa_vS%est   = coefa_v(iend_nh,jstrv_nh:jendv_nh,0:N+1)
    nbreq=nbreq+1
    call MPI_ISEND(coefa_vS%est, sz2, MPI_DOUBLE_PRECISION, &
      par%tvoisin(est), TAG22, par%comm2d, tabreq(nbreq), ierror)
    nbreq=nbreq+1
    call MPI_IRECV(coefa_vR%est, sz2, MPI_DOUBLE_PRECISION, &
      par%tvoisin(est), TAG21, par%comm2d, tabreq(nbreq), ierror)
  !-------- coefb_v exchange ---------------------------------------
    coefb_vS%est   = coefb_v(iend_nh,jstrv_nh:jendv_nh,0:N+1)
    nbreq=nbreq+1
    call MPI_ISEND(coefb_vS%est, sz2, MPI_DOUBLE_PRECISION, &
      par%tvoisin(est), TAG24, par%comm2d, tabreq(nbreq), ierror)
    nbreq=nbreq+1
    call MPI_IRECV(coefb_vR%est, sz2, MPI_DOUBLE_PRECISION, &
      par%tvoisin(est), TAG23, par%comm2d, tabreq(nbreq), ierror)
  endif

  !******************************************************************
  !		 NORTH direction
  !******************************************************************
  if (par%tvoisin(nord) /= MPI_PROC_NULL) then
  !-------- gdepth_v exchange ---------------------------------------
    gdepth_vS%nord   = gdepth_v(istr_nh:iend_nh,jendv_nh,0:N+1)
    nbreq=nbreq+1
    call MPI_ISEND(gdepth_vS%nord, sz4, MPI_DOUBLE_PRECISION, &
      par%tvoisin(nord), TAG26, par%comm2d, tabreq(nbreq), ierror)
    nbreq=nbreq+1
    call MPI_IRECV(gdepth_vR%nord, sz4, MPI_DOUBLE_PRECISION, &
      par%tvoisin(nord), TAG25, par%comm2d, tabreq(nbreq), ierror)
  !-------- coefa_v exchange ---------------------------------------
    coefa_vS%nord   = coefa_v(istr_nh:iend_nh,jendv_nh,0:N+1)
    nbreq=nbreq+1
    call MPI_ISEND(coefa_vS%nord, sz4, MPI_DOUBLE_PRECISION, &
      par%tvoisin(nord), TAG28, par%comm2d, tabreq(nbreq), ierror)
    nbreq=nbreq+1
    call MPI_IRECV(coefa_vR%nord, sz4, MPI_DOUBLE_PRECISION, &
      par%tvoisin(nord), TAG27, par%comm2d, tabreq(nbreq), ierror)
  !-------- coefb_v exchange ---------------------------------------
    coefb_vS%nord   = coefb_v(istr_nh:iend_nh,jendv_nh,0:N+1)
    nbreq=nbreq+1
    call MPI_ISEND(coefb_vS%nord, sz4, MPI_DOUBLE_PRECISION, &
      par%tvoisin(nord), TAG30, par%comm2d, tabreq(nbreq), ierror)
    nbreq=nbreq+1
    call MPI_IRECV(coefb_vR%nord, sz4, MPI_DOUBLE_PRECISION, &
      par%tvoisin(nord), TAG29, par%comm2d, tabreq(nbreq), ierror)
  endif

  !******************************************************************
  !		 SOUTH direction
  !******************************************************************
  if (par%tvoisin(sud) /= MPI_PROC_NULL) then
  !-------- gdepth_v exchange ---------------------------------------
    gdepth_vS%sud   = gdepth_v(istr_nh:iend_nh,jstrv_nh,0:N+1)
    nbreq=nbreq+1
    call MPI_ISEND(gdepth_vS%sud, sz4, MPI_DOUBLE_PRECISION, &
      par%tvoisin(sud), TAG25, par%comm2d, tabreq(nbreq), ierror)
    nbreq=nbreq+1
    call MPI_IRECV(gdepth_vR%sud, sz4, MPI_DOUBLE_PRECISION, &
      par%tvoisin(sud), TAG26, par%comm2d, tabreq(nbreq), ierror)
  !-------- coefa_v exchange ---------------------------------------
    coefa_vS%sud   = coefa_v(istr_nh:iend_nh,jstrv_nh,0:N+1)
    nbreq=nbreq+1
    call MPI_ISEND(coefa_vS%sud, sz4, MPI_DOUBLE_PRECISION, &
      par%tvoisin(sud), TAG27, par%comm2d, tabreq(nbreq), ierror)
    nbreq=nbreq+1
    call MPI_IRECV(coefa_vR%sud, sz4, MPI_DOUBLE_PRECISION, &
      par%tvoisin(sud), TAG28, par%comm2d, tabreq(nbreq), ierror)
  !-------- coefb_v exchange ---------------------------------------
    coefb_vS%sud   = coefb_v(istr_nh:iend_nh,jstrv_nh,0:N+1)
    nbreq=nbreq+1
    call MPI_ISEND(coefb_vS%sud, sz4, MPI_DOUBLE_PRECISION, &
      par%tvoisin(sud), TAG29, par%comm2d, tabreq(nbreq), ierror)
    nbreq=nbreq+1
    call MPI_IRECV(coefb_vR%sud, sz4, MPI_DOUBLE_PRECISION, &
      par%tvoisin(sud), TAG30, par%comm2d, tabreq(nbreq), ierror)
  endif

  !******************************************************************
  !		 CORNER direction
  !******************************************************************
  if (par%tvoisin(sudest) /= MPI_PROC_NULL) then
!   write(1000+par%rank,*) "sudest=",coefa_u(iendu_nh,jstr_nh,0:N+1)  ! *0+(par%rank+1)*10
  !-------- gdepth_u exchange ---------------------------------------
    gdepth_uS%sudest(1,:) = gdepth_u(iendu_nh,jstr_nh,0:N+1)
    nbreq=nbreq+1
    call MPI_ISEND(gdepth_uS%sudest, N+2, MPI_DOUBLE_PRECISION, &
      par%tvoisin(sudest), TAG33, par%comm2d, tabreq(nbreq), ierror)
    nbreq=nbreq+1
    call MPI_IRECV(gdepth_uR%sudest, N+2, MPI_DOUBLE_PRECISION, &
      par%tvoisin(sudest), TAG34, par%comm2d, tabreq(nbreq), ierror)
  !-------- coefa_u exchange ---------------------------------------
    coefa_uS%sudest(1,:) = coefa_u(iendu_nh,jstr_nh,0:N+1)  ! *0+(par%rank+1)*10
    nbreq=nbreq+1
    call MPI_ISEND(coefa_uS%sudest, N+2, MPI_DOUBLE_PRECISION, &
      par%tvoisin(sudest), TAG31, par%comm2d, tabreq(nbreq), ierror)
    nbreq=nbreq+1
    call MPI_IRECV(coefa_uR%sudest, N+2, MPI_DOUBLE_PRECISION, &
      par%tvoisin(sudest), TAG32, par%comm2d, tabreq(nbreq), ierror)
  !-------- coefb_u exchange ---------------------------------------     
    coefb_uS%sudest(1,:) = coefb_u(iendu_nh,jstr_nh,0:N+1)
    nbreq=nbreq+1
    call MPI_ISEND(coefb_uS%sudest, N+2, MPI_DOUBLE_PRECISION, &
      par%tvoisin(sudest), TAG39, par%comm2d, tabreq(nbreq), ierror)
    nbreq=nbreq+1
    call MPI_IRECV(coefb_uR%sudest, N+2, MPI_DOUBLE_PRECISION, &
      par%tvoisin(sudest), TAG40, par%comm2d, tabreq(nbreq), ierror)

  !-------- gdepth_v exchange ---------------------------------------
    gdepth_vS%sudest(1,:) = gdepth_v(iend_nh,jstrv_nh,0:N+1)
    nbreq=nbreq+1
    call MPI_ISEND(gdepth_vS%sudest, N+2, MPI_DOUBLE_PRECISION, &
      par%tvoisin(sudest), TAG43, par%comm2d, tabreq(nbreq), ierror)
    nbreq=nbreq+1
    call MPI_IRECV(gdepth_vR%sudest, N+2, MPI_DOUBLE_PRECISION, &
      par%tvoisin(sudest), TAG44, par%comm2d, tabreq(nbreq), ierror)
  !-------- coefa_v exchange ---------------------------------------
    coefa_vS%sudest(1,:) = coefa_v(iend_nh,jstrv_nh,0:N+1)
    nbreq=nbreq+1
    call MPI_ISEND(coefa_vS%sudest, N+2, MPI_DOUBLE_PRECISION, &
      par%tvoisin(sudest), TAG45, par%comm2d, tabreq(nbreq), ierror)
    nbreq=nbreq+1
    call MPI_IRECV(coefa_vR%sudest, N+2, MPI_DOUBLE_PRECISION, &
      par%tvoisin(sudest), TAG46, par%comm2d, tabreq(nbreq), ierror)
  !-------- coefb_v exchange ---------------------------------------     
    coefb_vS%sudest(1,:) = coefb_v(iend_nh,jstrv_nh,0:N+1)
    nbreq=nbreq+1
    call MPI_ISEND(coefb_vS%sudest, N+2, MPI_DOUBLE_PRECISION, &
      par%tvoisin(sudest), TAG47, par%comm2d, tabreq(nbreq), ierror)
    nbreq=nbreq+1
    call MPI_IRECV(coefb_vR%sudest, N+2, MPI_DOUBLE_PRECISION, &
      par%tvoisin(sudest), TAG48, par%comm2d, tabreq(nbreq), ierror)
  endif

  if (par%tvoisin(sudouest) /= MPI_PROC_NULL) then
!   write(1000+par%rank,*) "sudouest=",coefa_u(istru_nh,jstr_nh,0:N+1)  ! *0+(par%rank+1)*10
  !-------- gdepth_u exchange ---------------------------------------
    gdepth_uS%sudouest(1,:) = gdepth_u(istru_nh,jstr_nh,0:N+1)
    nbreq=nbreq+1
    call MPI_ISEND(gdepth_uS%sudouest, N+2, MPI_DOUBLE_PRECISION, &
      par%tvoisin(sudouest), TAG35, par%comm2d, tabreq(nbreq), ierror)
    nbreq=nbreq+1
    call MPI_IRECV(gdepth_uR%sudouest, N+2, MPI_DOUBLE_PRECISION, &
      par%tvoisin(sudouest), TAG36, par%comm2d, tabreq(nbreq), ierror)
  !-------- coefa_u exchange ---------------------------------------
    coefa_uS%sudouest(1,:) = coefa_u(istru_nh,jstr_nh,0:N+1)  ! *0+(par%rank+1)*10
    nbreq=nbreq+1
    call MPI_ISEND(coefa_uS%sudouest, N+2, MPI_DOUBLE_PRECISION, &
      par%tvoisin(sudouest), TAG37, par%comm2d, tabreq(nbreq), ierror)
    nbreq=nbreq+1
    call MPI_IRECV(coefa_uR%sudouest, N+2, MPI_DOUBLE_PRECISION, &
      par%tvoisin(sudouest), TAG38, par%comm2d, tabreq(nbreq), ierror)
  !-------- coefb_u exchange ---------------------------------------     
    coefb_uS%sudouest(1,:) = coefb_u(istru_nh,jstr_nh,0:N+1)
    nbreq=nbreq+1
    call MPI_ISEND(coefb_uS%sudouest, N+2, MPI_DOUBLE_PRECISION, &
      par%tvoisin(sudouest), TAG41, par%comm2d, tabreq(nbreq), ierror)
    nbreq=nbreq+1
    call MPI_IRECV(coefb_uR%sudouest, N+2, MPI_DOUBLE_PRECISION, &
      par%tvoisin(sudouest), TAG42, par%comm2d, tabreq(nbreq), ierror)

  !-------- gdepth_v exchange ---------------------------------------
    gdepth_vS%sudouest(1,:) = gdepth_v(istr_nh,jstrv_nh,0:N+1)
    nbreq=nbreq+1
    call MPI_ISEND(gdepth_vS%sudouest, N+2, MPI_DOUBLE_PRECISION, &
      par%tvoisin(sudouest), TAG49, par%comm2d, tabreq(nbreq), ierror)
    nbreq=nbreq+1
    call MPI_IRECV(gdepth_vR%sudouest, N+2, MPI_DOUBLE_PRECISION, &
      par%tvoisin(sudouest), TAG50, par%comm2d, tabreq(nbreq), ierror)
  !-------- coefa_v exchange ---------------------------------------
    coefa_vS%sudouest(1,:) = coefa_v(istr_nh,jstrv_nh,0:N+1)
    nbreq=nbreq+1
    call MPI_ISEND(coefa_vS%sudouest, N+2, MPI_DOUBLE_PRECISION, &
      par%tvoisin(sudouest), TAG51, par%comm2d, tabreq(nbreq), ierror)
    nbreq=nbreq+1
    call MPI_IRECV(coefa_vR%sudouest, N+2, MPI_DOUBLE_PRECISION, &
      par%tvoisin(sudouest), TAG52, par%comm2d, tabreq(nbreq), ierror)
  !-------- coefb_v exchange ---------------------------------------     
    coefb_vS%sudouest(1,:) = coefb_v(istr_nh,jstrv_nh,0:N+1)
    nbreq=nbreq+1
    call MPI_ISEND(coefb_vS%sudouest, N+2, MPI_DOUBLE_PRECISION, &
      par%tvoisin(sudouest), TAG53, par%comm2d, tabreq(nbreq), ierror)
    nbreq=nbreq+1
    call MPI_IRECV(coefb_vR%sudouest, N+2, MPI_DOUBLE_PRECISION, &
      par%tvoisin(sudouest), TAG54, par%comm2d, tabreq(nbreq), ierror)
  endif

  if (par%tvoisin(nordouest) /= MPI_PROC_NULL) then
!   write(1000+par%rank,*) "nordouest=",coefa_u(istru_nh,jend_nh,0:N+1)  ! *0+(par%rank+1)*10
  !-------- gdepth_u exchange ---------------------------------------
    gdepth_uS%nordouest(1,:) = gdepth_u(istru_nh,jend_nh,0:N+1)
    nbreq=nbreq+1
    call MPI_ISEND(gdepth_uS%nordouest, N+2, MPI_DOUBLE_PRECISION, &
      par%tvoisin(nordouest), TAG34, par%comm2d, tabreq(nbreq), ierror)
    nbreq=nbreq+1
    call MPI_IRECV(gdepth_uR%nordouest, N+2, MPI_DOUBLE_PRECISION, &
      par%tvoisin(nordouest), TAG33, par%comm2d, tabreq(nbreq), ierror)
  !-------- coefa_u exchange ---------------------------------------
    coefa_uS%nordouest(1,:) = coefa_u(istru_nh,jend_nh,0:N+1)  ! *0+(par%rank+1)*10
    nbreq=nbreq+1
    call MPI_ISEND(coefa_uS%nordouest, N+2, MPI_DOUBLE_PRECISION, &
      par%tvoisin(nordouest), TAG32, par%comm2d, tabreq(nbreq), ierror)
    nbreq=nbreq+1
    call MPI_IRECV(coefa_uR%nordouest, N+2, MPI_DOUBLE_PRECISION, &
      par%tvoisin(nordouest), TAG31, par%comm2d, tabreq(nbreq), ierror)
  !-------- coefb_u exchange ---------------------------------------     
    coefb_uS%nordouest(1,:) = coefb_u(istru_nh,jend_nh,0:N+1)
    nbreq=nbreq+1
    call MPI_ISEND(coefb_uS%nordouest, N+2, MPI_DOUBLE_PRECISION, &
      par%tvoisin(nordouest), TAG40, par%comm2d, tabreq(nbreq), ierror)
    nbreq=nbreq+1
    call MPI_IRECV(coefb_uR%nordouest, N+2, MPI_DOUBLE_PRECISION, &
      par%tvoisin(nordouest), TAG39, par%comm2d, tabreq(nbreq), ierror)

  !-------- gdepth_v exchange ---------------------------------------
    gdepth_vS%nordouest(1,:) = gdepth_v(istr_nh,jendv_nh,0:N+1)
    nbreq=nbreq+1
    call MPI_ISEND(gdepth_vS%nordouest, N+2, MPI_DOUBLE_PRECISION, &
      par%tvoisin(nordouest), TAG44, par%comm2d, tabreq(nbreq), ierror)
    nbreq=nbreq+1
    call MPI_IRECV(gdepth_vR%nordouest, N+2, MPI_DOUBLE_PRECISION, &
      par%tvoisin(nordouest), TAG43, par%comm2d, tabreq(nbreq), ierror)
  !-------- coefa_v exchange ---------------------------------------
    coefa_vS%nordouest(1,:) = coefa_v(istr_nh,jendv_nh,0:N+1)
    nbreq=nbreq+1
    call MPI_ISEND(coefa_vS%nordouest, N+2, MPI_DOUBLE_PRECISION, &
      par%tvoisin(nordouest), TAG46, par%comm2d, tabreq(nbreq), ierror)
    nbreq=nbreq+1
    call MPI_IRECV(coefa_vR%nordouest, N+2, MPI_DOUBLE_PRECISION, &
      par%tvoisin(nordouest), TAG45, par%comm2d, tabreq(nbreq), ierror)
  !-------- coefb_v exchange ---------------------------------------     
    coefb_vS%nordouest(1,:) = coefb_v(istr_nh,jendv_nh,0:N+1)
    nbreq=nbreq+1
    call MPI_ISEND(coefb_vS%nordouest, N+2, MPI_DOUBLE_PRECISION, &
      par%tvoisin(nordouest), TAG48, par%comm2d, tabreq(nbreq), ierror)
    nbreq=nbreq+1
    call MPI_IRECV(coefb_vR%nordouest, N+2, MPI_DOUBLE_PRECISION, &
      par%tvoisin(nordouest), TAG47, par%comm2d, tabreq(nbreq), ierror)
  endif

  if (par%tvoisin(nordest) /= MPI_PROC_NULL) then
!   write(1000+par%rank,*) "nordest=",coefa_u(iendu_nh,jend_nh,0:N+1)  ! *0+(par%rank+1)*10
  !-------- gdepth_u exchange ---------------------------------------
    gdepth_uS%nordest(1,:) = gdepth_u(iendu_nh,jend_nh,0:N+1)
    nbreq=nbreq+1
    call MPI_ISEND(gdepth_uS%nordest, N+2, MPI_DOUBLE_PRECISION, &
      par%tvoisin(nordest), TAG36, par%comm2d, tabreq(nbreq), ierror)
    nbreq=nbreq+1
    call MPI_IRECV(gdepth_uR%nordest, N+2, MPI_DOUBLE_PRECISION, &
      par%tvoisin(nordest), TAG35, par%comm2d, tabreq(nbreq), ierror)
  !-------- coefa_u exchange ---------------------------------------
    coefa_uS%nordest(1,:) = coefa_u(iendu_nh,jend_nh,0:N+1)  ! *0+(par%rank+1)*10
    nbreq=nbreq+1
    call MPI_ISEND(coefa_uS%nordest, N+2, MPI_DOUBLE_PRECISION, &
      par%tvoisin(nordest), TAG38, par%comm2d, tabreq(nbreq), ierror)
    nbreq=nbreq+1
    call MPI_IRECV(coefa_uR%nordest, N+2, MPI_DOUBLE_PRECISION, &
      par%tvoisin(nordest), TAG37, par%comm2d, tabreq(nbreq), ierror)
  !-------- coefb_u exchange ---------------------------------------     
    coefb_uS%nordest(1,:) = coefb_u(iendu_nh,jend_nh,0:N+1)
    nbreq=nbreq+1
    call MPI_ISEND(coefb_uS%nordest, N+2, MPI_DOUBLE_PRECISION, &
      par%tvoisin(nordest), TAG42, par%comm2d, tabreq(nbreq), ierror)
    nbreq=nbreq+1
    call MPI_IRECV(coefb_uR%nordest, N+2, MPI_DOUBLE_PRECISION, &
      par%tvoisin(nordest), TAG41, par%comm2d, tabreq(nbreq), ierror)

  !-------- gdepth_v exchange ---------------------------------------
    gdepth_vS%nordest(1,:) = gdepth_v(iend_nh,jendv_nh,0:N+1)
    nbreq=nbreq+1
    call MPI_ISEND(gdepth_vS%nordest, N+2, MPI_DOUBLE_PRECISION, &
      par%tvoisin(nordest), TAG50, par%comm2d, tabreq(nbreq), ierror)
    nbreq=nbreq+1
    call MPI_IRECV(gdepth_vR%nordest, N+2, MPI_DOUBLE_PRECISION, &
      par%tvoisin(nordest), TAG49, par%comm2d, tabreq(nbreq), ierror)
  !-------- coefa_v exchange ---------------------------------------
    coefa_vS%nordest(1,:) = coefa_v(iend_nh,jendv_nh,0:N+1)
    nbreq=nbreq+1
    call MPI_ISEND(coefa_vS%nordest, N+2, MPI_DOUBLE_PRECISION, &
      par%tvoisin(nordest), TAG52, par%comm2d, tabreq(nbreq), ierror)
    nbreq=nbreq+1
    call MPI_IRECV(coefa_vR%nordest, N+2, MPI_DOUBLE_PRECISION, &
      par%tvoisin(nordest), TAG51, par%comm2d, tabreq(nbreq), ierror)
  !-------- coefb_v exchange ---------------------------------------     
    coefb_vS%nordest(1,:) = coefb_v(iend_nh,jendv_nh,0:N+1)
    nbreq=nbreq+1
    call MPI_ISEND(coefb_vS%nordest, N+2, MPI_DOUBLE_PRECISION, &
      par%tvoisin(nordest), TAG54, par%comm2d, tabreq(nbreq), ierror)
    nbreq=nbreq+1
    call MPI_IRECV(coefb_vR%nordest, N+2, MPI_DOUBLE_PRECISION, &
      par%tvoisin(nordest), TAG53, par%comm2d, tabreq(nbreq), ierror)
  endif

    
  !! Waiting ROOM
  CALL MPI_WAITALL(nbreq, tabreq(1:nbreq), tstatus(:,1:nbreq), ierror)   
!  write(100+par%rank,*) "ierror=",ierror
!  write(100+par%rank,*) "tstatus=",tstatus

  if (par%tvoisin(ouest) /= MPI_PROC_NULL) then
      gdepth_u(istru_nh-1,jstr_nh:jend_nh,0:N+1)  = gdepth_uR%ouest
       coefa_u(istru_nh-1,jstr_nh:jend_nh,0:N+1)  = coefa_uR%ouest
       coefb_u(istru_nh-1,jstr_nh:jend_nh,0:N+1)  = coefb_uR%ouest
      
      gdepth_v(istr_nh-1,jstrv_nh:jendv_nh,0:N+1)  = gdepth_vR%ouest
       coefa_v(istr_nh-1,jstrv_nh:jendv_nh,0:N+1)  = coefa_vR%ouest
       coefb_v(istr_nh-1,jstrv_nh:jendv_nh,0:N+1)  = coefb_vR%ouest
  endif
  if (par%tvoisin(  est) /= MPI_PROC_NULL) then
      gdepth_u(iendu_nh+1,jstr_nh:jend_nh,0:N+1)  = gdepth_uR%est
       coefa_u(iendu_nh+1,jstr_nh:jend_nh,0:N+1)  = coefa_uR%est
       coefb_u(iendu_nh+1,jstr_nh:jend_nh,0:N+1)  = coefb_uR%est
      
      gdepth_v(iend_nh+1,jstrv_nh:jendv_nh,0:N+1)  = gdepth_vR%est
       coefa_v(iend_nh+1,jstrv_nh:jendv_nh,0:N+1)  = coefa_vR%est
       coefb_v(iend_nh+1,jstrv_nh:jendv_nh,0:N+1)  = coefb_vR%est
  endif

  
  if (par%tvoisin(nord) /= MPI_PROC_NULL) then
      gdepth_u(istru_nh:iendu_nh,jend_nh+1,0:N+1)  = gdepth_uR%nord
       coefa_u(istru_nh:iendu_nh,jend_nh+1,0:N+1)  = coefa_uR%nord
       coefb_u(istru_nh:iendu_nh,jend_nh+1,0:N+1)  = coefb_uR%nord
      
       gdepth_v(istr_nh:iend_nh,jendv_nh+1,0:N+1)  = gdepth_vR%nord
        coefa_v(istr_nh:iend_nh,jendv_nh+1,0:N+1)  = coefa_vR%nord
        coefb_v(istr_nh:iend_nh,jendv_nh+1,0:N+1)  = coefb_vR%nord
  endif
  if (par%tvoisin(  sud) /= MPI_PROC_NULL) then
      gdepth_u(istru_nh:iendu_nh,jstr_nh-1,0:N+1)  = gdepth_uR%sud
       coefa_u(istru_nh:iendu_nh,jstr_nh-1,0:N+1)  = coefa_uR%sud
       coefb_u(istru_nh:iendu_nh,jstr_nh-1,0:N+1)  = coefb_uR%sud
      
      gdepth_v(istr_nh:iend_nh,jstrv_nh-1,0:N+1)  = gdepth_vR%sud
       coefa_v(istr_nh:iend_nh,jstrv_nh-1,0:N+1)  = coefa_vR%sud
       coefb_v(istr_nh:iend_nh,jstrv_nh-1,0:N+1)  = coefb_vR%sud
  endif

  !******************************************************************
  !		 CORNER direction
  !******************************************************************
  if (par%tvoisin(sudest) /= MPI_PROC_NULL) then
!   write(2000+par%rank,*) "sudest=",coefa_uR%sudest(1,:)
       gdepth_u(iendu_nh+1,jstr_nh-1,0:N+1)  = gdepth_uR%sudest(1,:)
        coefa_u(iendu_nh+1,jstr_nh-1,0:N+1)  =  coefa_uR%sudest(1,:)
        coefb_u(iendu_nh+1,jstr_nh-1,0:N+1)  =  coefb_uR%sudest(1,:)

       gdepth_v(iend_nh+1,jstrv_nh-1,0:N+1)  = gdepth_vR%sudest(1,:)
        coefa_v(iend_nh+1,jstrv_nh-1,0:N+1)  =  coefa_vR%sudest(1,:)
        coefb_v(iend_nh+1,jstrv_nh-1,0:N+1)  =  coefb_vR%sudest(1,:)
  endif
  if (par%tvoisin(sudouest) /= MPI_PROC_NULL) then
!   write(2000+par%rank,*) "sudouest=",coefa_uR%sudouest(1,:)
       gdepth_u(istru_nh-1,jstr_nh-1,0:N+1)  = gdepth_uR%sudouest(1,:)
        coefa_u(istru_nh-1,jstr_nh-1,0:N+1)  =  coefa_uR%sudouest(1,:)
        coefb_u(istru_nh-1,jstr_nh-1,0:N+1)  =  coefb_uR%sudouest(1,:)

       gdepth_v(istr_nh-1,jstrv_nh-1,0:N+1)  = gdepth_vR%sudouest(1,:)
        coefa_v(istr_nh-1,jstrv_nh-1,0:N+1)  =  coefa_vR%sudouest(1,:)
        coefb_v(istr_nh-1,jstrv_nh-1,0:N+1)  =  coefb_vR%sudouest(1,:)
  endif
  if (par%tvoisin(nordouest) /= MPI_PROC_NULL) then
!   write(2000+par%rank,*) "nordouest=",coefa_uR%nordouest(1,:)
       gdepth_u(istru_nh-1,jend_nh+1,0:N+1)  = gdepth_uR%nordouest(1,:)
        coefa_u(istru_nh-1,jend_nh+1,0:N+1)  =  coefa_uR%nordouest(1,:)
        coefb_u(istru_nh-1,jend_nh+1,0:N+1)  =  coefb_uR%nordouest(1,:)

       gdepth_v(istr_nh-1,jendv_nh+1,0:N+1)  = gdepth_vR%nordouest(1,:)
        coefa_v(istr_nh-1,jendv_nh+1,0:N+1)  =  coefa_vR%nordouest(1,:)
        coefb_v(istr_nh-1,jendv_nh+1,0:N+1)  =  coefb_vR%nordouest(1,:)
  endif
  if (par%tvoisin(nordest) /= MPI_PROC_NULL) then
!   write(2000+par%rank,*) "nordest=",coefa_uR%nordest(1,:)
       gdepth_u(iendu_nh+1,jend_nh+1,0:N+1)  = gdepth_uR%nordest(1,:)
        coefa_u(iendu_nh+1,jend_nh+1,0:N+1)  =  coefa_uR%nordest(1,:)
        coefb_u(iendu_nh+1,jend_nh+1,0:N+1)  =  coefb_uR%nordest(1,:)

       gdepth_v(iend_nh+1,jendv_nh+1,0:N+1)  = gdepth_vR%nordest(1,:)
        coefa_v(iend_nh+1,jendv_nh+1,0:N+1)  =  coefa_vR%nordest(1,:)
        coefb_v(iend_nh+1,jendv_nh+1,0:N+1)  =  coefb_vR%nordest(1,:)
  endif
  
  
!   do i=istru_nh-1,iendu_nh+1
!    do j=jstr_nh-1,jend_nh+1
!     do k=0,N+1
!       write(6000+par%rank,*) i,j,k,coefa_u(i,j,k)
!     enddo
!    enddo
!   enddo
    
  
!  print *,"istru_nh,iendu_nh,jstr_nh,jend_nh,N=",istru_nh,iendu_nh,jstr_nh,jend_nh,N
#endif  
end subroutine grid_exchange
