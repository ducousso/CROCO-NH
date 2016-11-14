#include "cppdefs.h"
#if defined NBQ && defined MPI
!------------------------------------------------------------------------------
!                               NHOMS
!                Non Hydrostatic Ocean Modeling System      
!------------------------------------------------------------------------------
!
!> @note <a href="http://poc.obs-mip.fr/auclair/WOcean.fr/SNH/index_snh_home.htm"> Main web documentation </a>
!
! DESCRIPTION: 
!
!> @brief
!
!> @details 
!
!
! REVISION HISTORY:
!
!> @authors
!> @date 2015 January
!> @todo
!
!------------------------------------------------------------------------------

      subroutine parallele_nbq(ichoix)

!__________________________________________________________________________________
!
!                               SNH2012.14      
!                 Non-Hydrostatic & Non-Boussinesq Version  
! Laboratoire d'Aerologie, 14 Avenue Edouard Belin, F-31400 Toulouse
! http://poc.obs-mip.fr/auclair/WOcean.fr/SNH/index_snh_home.htm  
!
!__________________________________________________________________________________


!      use module_parameter
!      use module_parameter_nh
!      use module_parallele
!      use module_principal , only :  mask_u, mask_v, mask_t, imax, jmax, kmax &
!			  , iteration3d, iteration2d
      use module_nh , only : ijk2lmom_nh, time_omp_nh, ijk2lq_nh, rhs2_nh
      use module_nbq
      use module_parallel_nbq
!      use module_profile
      implicit none 
      integer,intent(in) ::ichoix
      integer :: i, j, k, bcl
      double precision :: tmptime1,tmptime2
      logical,save :: firstpass=.true.
      integer :: vois
      integer(MPI_ADDRESS_KIND) :: addr1
!       double precision,dimension(0:size(qdm_nbq_a(:,vnnew_nbq))) :: qdm_tmp1,qdm_tmp2
      integer :: sizeqdm, szsend, szrecv, debsend,debrecv
      integer :: kmax,imax,jmax
#define LOCALLM Lmmpi
#define LOCALMM Mmmpi
       
      imax=LOCALLM+1
      jmax=LOCALMM+1
     


! MPI_RSEND_INIT(buf, count, datatype, dest, tag, comm, request)
      if (ichoix.eq.5) then
!****************************************************************************************
! Echanges des variables "boucle NBQ" qunatité de mouvement :: ENVOI
!****************************************************************************************
	  nbreq_qdm=1
	  do bcl=1, 8
	   vois=lvoisin(bcl)
	   if (par%tvoisin(vois) /= mpi_proc_null) then 
	   call MPI_TYPE_SIZE(ech_qdm_nbq(vois)%send, szsend,ierr)
	   call MPI_TYPE_SIZE(ech_qdm_nbq(vois)%recv, szrecv,ierr) 
           call MPI_TYPE_GET_EXTENT(ech_qdm_nbq(vois)%send, debsend, szsend, ierr)
           call MPI_TYPE_GET_EXTENT(ech_qdm_nbq(vois)%recv, debrecv, szrecv, ierr)
            call MPI_IRECV(qdm_nbq_a(1,vnnew_nbq),  1, ech_qdm_nbq(vois)%recv, par%tvoisin(vois), &
	      tagqdm_Recv(vois), par%comm2d, tabreq_qdm(nbreq_qdm), ierr)
 	     nbreq_qdm=nbreq_qdm+1
 	     call MPI_IRSEND(qdm_nbq_a(1,vnnew_nbq),  1, ech_qdm_nbq(vois)%send, par%tvoisin(vois), &
               tagqdm_Send(vois), par%comm2d, tabreq_qdm(nbreq_qdm), ierr)
  	     nbreq_qdm=nbreq_qdm+1	
           endif
	  enddo
      endif
      if (ichoix.eq.15) then
!****************************************************************************************
! Echanges des variables "boucle NBQ" qunatité de mouvement :: RECEPTION/WAIT
!****************************************************************************************
!----------------------------------------------------------------------------------------
!  Salle d'attente
!----------------------------------------------------------------------------------------
	nbreq_qdm = nbreq_qdm-1
	CALL MPI_WAITALL(nbreq_qdm, tabreq_qdm(1:nbreq_qdm), tstatus(:,1:nbreq_qdm), IERR)   
        nbreq_qdm=0
       endif
       
       
      if (ichoix.eq.51) then ! U only
!****************************************************************************************
! Echanges des variables "boucle NBQ" qunatité de mouvement U :: ENVOI
!****************************************************************************************
	  !print *,mynode,"startall 51",p_nbreq_qdmU(vnnew_nbq)
	  !write(100+mynode,*) "startall 51",p_nbreq_qdmU(vnnew_nbq)
	  if (p_nbreq_qdmU(vnnew_nbq)> 0 ) call MPI_STARTALL(p_nbreq_qdmU(vnnew_nbq), &
			    p_tabreq_qdmU(1:p_nbreq_qdmU(vnnew_nbq),vnnew_nbq), &
			    ierr)
      endif
      if (ichoix.eq.151) then
!****************************************************************************************
! Echanges des variables "boucle NBQ" qunatité de mouvement :: RECEPTION/WAIT
!****************************************************************************************
!----------------------------------------------------------------------------------------
!  Salle d'attente
!----------------------------------------------------------------------------------------
	tstatus=0
        !print *,mynode,"waitall 51",p_nbreq_qdmU(vnnew_nbq)
        !do bcl=1,p_nbreq_qdmU(vnnew_nbq)
	!   print *,"     1:",mynode,tstatus(:,bcl)
	!enddo
	if (p_nbreq_qdmU(vnnew_nbq)> 0 ) CALL MPI_WAITALL(p_nbreq_qdmU(vnnew_nbq), &
			     p_tabreq_qdmU(1:p_nbreq_qdmU(vnnew_nbq),vnnew_nbq), &
			     tstatus(:,1:p_nbreq_qdmU(vnnew_nbq)), IERR)   
        !print *,mynode,"waitall 51 Ok..."," IERR=",IERR
        !do bcl=1,p_nbreq_qdmU(vnnew_nbq)
	!   print *,"     2:",mynode,tstatus(:,bcl)
	!enddo
	endif
       
      if (ichoix.eq.52) then ! V only
!****************************************************************************************
! Echanges des variables "boucle NBQ" qunatité de mouvement V :: ENVOI
!****************************************************************************************
!
	  if (p_nbreq_qdmV(vnnew_nbq)> 0 ) call MPI_STARTALL(p_nbreq_qdmV(vnnew_nbq), &
			    p_tabreq_qdmV(1:p_nbreq_qdmV(vnnew_nbq),vnnew_nbq), &
			    ierr)
      endif
      if (ichoix.eq.152) then
!****************************************************************************************
! Echanges des variables "boucle NBQ" qunatité de mouvement V:: RECEPTION/WAIT
!****************************************************************************************
!----------------------------------------------------------------------------------------
!  Salle d'attente
!----------------------------------------------------------------------------------------
	tstatus=0
        !print *,"waitall 52"
	if (p_nbreq_qdmV(vnnew_nbq)> 0 ) CALL MPI_WAITALL(p_nbreq_qdmV(vnnew_nbq), &
			     p_tabreq_qdmV(1:p_nbreq_qdmV(vnnew_nbq),vnnew_nbq), &
			     tstatus(:,1:p_nbreq_qdmV(vnnew_nbq)), IERR)   
        !print *,"waitall 52 Ok..."
	endif

      if (ichoix.eq.53) then ! W only
!****************************************************************************************
! Echanges des variables "boucle NBQ" qunatité de mouvement U :: ENVOI
!****************************************************************************************
	  if (p_nbreq_qdmW(vnnew_nbq)> 0 ) call MPI_STARTALL(p_nbreq_qdmW(vnnew_nbq), &
			    p_tabreq_qdmW(1:p_nbreq_qdmW(vnnew_nbq),vnnew_nbq), &
			    ierr)
      endif
      if (ichoix.eq.153) then
!****************************************************************************************
! Echanges des variables "boucle NBQ" qunatité de mouvement :: RECEPTION/WAIT
!****************************************************************************************
!----------------------------------------------------------------------------------------
!  Salle d'attente
!----------------------------------------------------------------------------------------
        !print *,"waitall 53"
	tstatus=0
	if (p_nbreq_qdmW(vnnew_nbq)> 0 ) CALL MPI_WAITALL(p_nbreq_qdmW(vnnew_nbq), &
			     p_tabreq_qdmW(1:p_nbreq_qdmW(vnnew_nbq),vnnew_nbq), &
			     tstatus(:,1:p_nbreq_qdmW(vnnew_nbq)), IERR)   
        !print *,"waitall 53 Ok"

       endif
       
!****************************************************************************************
!****************************************************************************************
      if (ichoix.eq.7) then  
!****************************************************************************************
! Echanges des variables "boucle NBQ" divergence :: ENVOIE
!****************************************************************************************
	  if (p_nbreq_mv(dnrhs_nbq)> 0 ) call MPI_STARTALL(p_nbreq_mv(dnrhs_nbq), &
			    p_tabreq_mv(1:p_nbreq_mv(vnnew_nbq),dnrhs_nbq), &
			    ierr)
      endif
      if (ichoix.eq.17) then
!****************************************************************************************
! Echanges des variables "boucle NBQ" divergence :: ENVOIE
!****************************************************************************************
!----------------------------------------------------------------------------------------
!  Salle d'attente
!----------------------------------------------------------------------------------------
        !print *,"waitall 7"
	tstatus=0
	if (p_nbreq_mv(dnrhs_nbq)> 0 ) CALL MPI_WAITALL(p_nbreq_mv(dnrhs_nbq), &
			     p_tabreq_mv(1:p_nbreq_mv(dnrhs_nbq),dnrhs_nbq), &
			     tstatus(:,1:p_nbreq_mv(dnrhs_nbq)), IERR)   
        !print *,"waitall mv OK"

      endif
!****************************************************************************************
!****************************************************************************************

!****************************************************************************************
!****************************************************************************************
      if (ichoix.eq.8) then  
!****************************************************************************************
! !.....Echanges de RHS2_NH contenant la condition cinematique au fond:
!****************************************************************************************
	  nbreq_rhs=1
	  do bcl=1,8
	   vois=lvoisin(bcl)
	   if (par%tvoisin(vois) /= mpi_proc_null) then 
            call MPI_IRECV(rhs2_nh(1),  1, ech_rhs2_nh(vois)%recv, par%tvoisin(vois), &
	      tagrhs_Recv(vois), par%comm2d, tabreq_rhs(nbreq_rhs), ierr)
 	     nbreq_rhs=nbreq_rhs+1
 	     call MPI_IRSEND(rhs2_nh(1),  1, ech_rhs2_nh(vois)%send, par%tvoisin(vois), &
               tagrhs_Send(vois), par%comm2d, tabreq_rhs(nbreq_rhs), ierr)
  	     nbreq_rhs=nbreq_rhs+1	
           endif  
	  enddo
      endif
      if (ichoix.eq.18) then
!****************************************************************************************
! !.....Echanges de RHS2_NH contenant la condition cinematique au fond:
!****************************************************************************************
!----------------------------------------------------------------------------------------
!  Salle d'attente
!----------------------------------------------------------------------------------------
	nbreq_rhs = nbreq_rhs-1
	CALL MPI_WAITALL(nbreq_rhs, tabreq_rhs(1:nbreq_rhs), tstatus(:,1:nbreq_rhs), IERR)   
        nbreq_rhs=0
      !print *,"waitall rhs OK."
      endif
!****************************************************************************************
!****************************************************************************************


       end subroutine parallele_nbq
#else
      subroutine parallele_nbq(ichoix)
      implicit none
      integer,intent(in) :: ichoix
      end subroutine parallele_nbq
#endif
