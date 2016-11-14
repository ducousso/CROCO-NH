#include "cppdefs.h"
#if defined NBQ && defined MPI

module module_parallel_nbq
!------------------------------------------------------------------------------
!                       NBQ Module for MPI-exchanges
!------------------------------------------------------------------------------

      implicit none
      integer, parameter :: ouest=1,est=2,nord=3,sud=4,haut=5,bas=6
      integer, parameter :: sudouest=7,sudest=8,nordouest=9,nordest=10
      integer, parameter :: ouestest=1,nordsud=2
      include 'mpif.h'    

# include "param_F90.h"
# include "scalars_F90.h"

  type echblock
	  integer :: send
	  integer :: recv
  end type echblock
  type infopar_croco
         integer ::  comm2d                 !COMMUNICATEUR GLOBAL
         integer ::  rank
         integer,dimension(10)                      ::  tvoisin         !LE NUMERO DES VOISINS DANS L'ORDRE(O,E,S,N)
  end type

  type (infopar_croco) :: par
  type (echblock),dimension(10) :: ech_qdm_nbq, ech_div_nbq, ech_rhs2_nh
  type (echblock),dimension(10) :: ech_qdmU_nbq, ech_qdmV_nbq, ech_qdmW_nbq
  integer,dimension(8),parameter :: liste_voisin = &
      (/ ouest, est, nord, sud, sudouest, sudest, nordouest, nordest /)
  integer :: ierr

  !! For 3D arrays (grid varibles exchanges) 
  type boundarybuff
	double precision,dimension(:,:),allocatable :: ouest,est,sud,nord,nordest, &
			sudest,sudouest,nordouest
  end type boundarybuff
  
  type (boundarybuff) 			:: gdepth_uS, gdepth_uR
  type (boundarybuff) 			:: gdepth_vS, gdepth_vR
  type (boundarybuff) 			:: coefa_uS,  coefa_uR
  type (boundarybuff) 			:: coefa_vS,  coefa_vR
  type (boundarybuff) 			:: coefb_uS,  coefb_uR
  type (boundarybuff) 			:: coefb_vS,  coefb_vR
  integer 				:: sz1, sz2, sz3, sz4, sz5, sz6
  logical,save 				:: doalloc=.TRUE.
 
      integer,dimension(8),parameter ::  lvoisin  =    (/ ouest,   est,   nord,  sud,        sudouest, sudest, nordouest, nordest /)
      integer,dimension(10),parameter :: tagqdm_Send = (/ 55000, 55010, 56010, 56000, 0, 0,  155000, 515010,    516000, 516010  /)
      integer,dimension(10),parameter :: tagqdm_Recv = (/ 55010, 55000, 56000, 56010, 0, 0,  516010, 516000,    515010, 155000  /)
      integer,dimension(10),parameter :: tagdiv_Send = (/ 75000, 75010, 76010, 76000, 0, 0,  255000, 715010,    716000, 716010  /)
      integer,dimension(10),parameter :: tagdiv_Recv = (/ 75010, 75000, 76000, 76010, 0, 0,  716010, 716000,    715010, 255000  /)
      integer,dimension(10),parameter :: tagrhs_Send = (/ 85000, 85010, 86010, 86000, 0, 0,  355000, 815010,    816000, 816010  /)
      integer,dimension(10),parameter :: tagrhs_Recv = (/ 85010, 85000, 86000, 86010, 0, 0,  816010, 816000,    815010, 355000  /)
      integer,dimension(10),parameter :: tagqdmU_Send= (/ 55001, 55011, 56011, 56001, 0, 0,  155001, 515011,    516001, 516011  /)
      integer,dimension(10),parameter :: tagqdmU_Recv= (/ 55011, 55001, 56001, 56011, 0, 0,  516011, 516001,    515011, 155001  /)
      integer,dimension(10),parameter :: tagqdmV_Send= (/ 55002, 55012, 56012, 56002, 0, 0,  155002, 515012,    516002, 516012  /)
      integer,dimension(10),parameter :: tagqdmV_Recv= (/ 55012, 55002, 56002, 56012, 0, 0,  516012, 516002,    515012, 155002  /)
      integer,dimension(10),parameter :: tagqdmW_Send= (/ 55003, 55013, 56013, 56003, 0, 0,  155003, 515013,    516003, 516013  /)
      integer,dimension(10),parameter :: tagqdmW_Recv= (/ 55013, 55003, 56003, 56013, 0, 0,  516013, 516003,    515013, 155003  /)

      integer :: nbreq_qdm=1,nbreq_mv=1,nbreq_rhs=1
      integer :: nbreq_qdmU=1,  nbreq_qdmV=1,  nbreq_qdmW=1

      ! Pour les echanges :: qdm et mv 
      integer,dimension(18) :: tabreq_qdm,tabreq_mv,tabreq_rhs
      integer,dimension(18) :: tabreq_qdmU, tabreq_qdmV, tabreq_qdmW
 
      ! Persistants messages
      integer,dimension(0:2) :: p_nbreq_qdmU=1, p_nbreq_qdmV=1, p_nbreq_qdmW=1 
      integer,dimension(0:1) :: p_nbreq_mv=1
      integer,dimension(80,0:2) :: p_tabreq_qdmU, p_tabreq_qdmV, p_tabreq_qdmW
      integer,dimension(80,0:1) :: p_tabreq_mv

contains 

!-------------------------------------------------------------------------------------
!-------------------------------------------------------------------------------------
!                 ROUTINES QDM_NBQ
!-------------------------------------------------------------------------------------
!-------------------------------------------------------------------------------------
!--------------------------------------------------------------------------
subroutine borne_echange_qdm_nbq_a(voisin,imax,jmax,kmax, &
	  ideb_s,ifin_s,jdeb_s,jfin_s,kdeb_s,kfin_s, &
	  ideb_r,ifin_r,jdeb_r,jfin_r,kdeb_r,kfin_r )
  use module_nh , only : istr_nh,iend_nh,jstr_nh,jend_nh,istru_nh,iendu_nh,jstrv_nh,jendv_nh
  implicit none
      integer, parameter :: ouest=1,est=2,nord=3,sud=4,haut=5,bas=6
      integer, parameter :: sudouest=7,sudest=8,nordouest=9,nordest=10
      integer, parameter :: ouestest=1,nordsud=2
  integer,intent(in) :: voisin	  
  integer,intent(in) :: imax,jmax,kmax
  integer,dimension(3),intent(out) :: ideb_s,ifin_s,jdeb_s,jfin_s,kdeb_s,kfin_s
  integer,dimension(3),intent(out) :: ideb_r,ifin_r,jdeb_r,jfin_r,kdeb_r,kfin_r

  select case(voisin)
  !!! (/ U, V, W /)
  case(ouest) !........................
      ! Envoie
      ideb_s=(/ istru_nh,      istr_nh,   istr_nh    /)
      ifin_s=(/ istru_nh,      istr_nh,   istr_nh    /)
      jdeb_s=(/  jstr_nh,     jstrv_nh,   jstr_nh    /)
      jfin_s=(/  jend_nh,     jendv_nh,   jend_nh    /)
      kdeb_s=(/ 1,    1,    0    /)
      kfin_s=(/ kmax, kmax, kmax /)
      ! Reception
      ideb_r=(/ istru_nh-1,     istr_nh-1,    istr_nh-1  /)
      ifin_r=(/ istru_nh-1,     istr_nh-1,    istr_nh-1  /)
      jdeb_r=(/  jstr_nh,        jstrv_nh,      jstr_nh  /)
      jfin_r=(/  jend_nh,        jendv_nh,      jend_nh  /)  
      kdeb_r=(/ 1,    1,    0    /)
      kfin_r=(/ kmax, kmax, kmax /)

      case(est)
      ! Envoie
      ideb_s=(/ iendu_nh,  iend_nh, iend_nh /)
      ifin_s=(/ iendu_nh,  iend_nh, iend_nh /)
      jdeb_s=(/  jstr_nh, jstrv_nh, jstr_nh /)
      jfin_s=(/  jend_nh, jendv_nh, jend_nh /)
      kdeb_s=(/ 1,    1,    0    /)
      kfin_s=(/ kmax, kmax, kmax /)
      ! Reception
      ideb_r=(/ iendu_nh+1,  iend_nh+1, iend_nh+1  /)
      ifin_r=(/ iendu_nh+1,  iend_nh+1, iend_nh+1  /)
      jdeb_r=(/  jstr_nh,     jstrv_nh,   jstr_nh  /)
      jfin_r=(/  jend_nh,     jendv_nh,   jend_nh  /)
      kdeb_r=(/ 1,      1,      0       /)
      kfin_r=(/ kmax,   kmax,   kmax    /)

  case(nord) !........................
      ! Envoie
      ideb_s=(/ istru_nh,     istr_nh,    istr_nh    /)
      ifin_s=(/ iendu_nh,     iend_nh,    iend_nh    /)
      jdeb_s=(/  jend_nh,    jendv_nh,    jend_nh    /)
      jfin_s=(/  jend_nh,    jendv_nh,    jend_nh   /)
      kdeb_s=(/ 1,    1,    0    /)
      kfin_s=(/ kmax, kmax, kmax /)
      ! Reception
      ideb_r=(/ istru_nh,       istr_nh,      istr_nh       /)
      ifin_r=(/ iendu_nh,       iend_nh,      iend_nh       /)
      jdeb_r=(/  jend_nh+1,     jendv_nh+1,   jend_nh+1     /)
      jfin_r=(/  jend_nh+1,     jendv_nh+1,   jend_nh+1     /)
      kdeb_r=(/ 1,      1,      0       /)
      kfin_r=(/ kmax,   kmax,   kmax    /)
  
   case(sud) !........................ 
      ideb_s=(/ istru_nh,    istr_nh,      istr_nh  /)
      ifin_s=(/ iendu_nh,    iend_nh,      iend_nh  /)
      jdeb_s=(/  jstr_nh,   jstrv_nh,      jstr_nh  /)
      jfin_s=(/  jstr_nh,   jstrv_nh,      jstr_nh  /)
      kdeb_s=(/ 1,    1,       0 /)
      kfin_s=(/ kmax, kmax, kmax /)
      ! Reception
      ideb_r=(/ istru_nh,     istr_nh,      istr_nh      /)
      ifin_r=(/ iendu_nh,     iend_nh,      iend_nh      /)
      jdeb_r=(/  jstr_nh-1,  jstrv_nh-1,    jstr_nh-1    /)
      jfin_r=(/  jstr_nh-1,  jstrv_nh-1,    jstr_nh-1    /)
      kdeb_r=(/ 1,    1,    0    /)
      kfin_r=(/ kmax, kmax, kmax /)
      
      !!! Les Coins..............................
   case(sudouest)
!       ! Envoie
      ideb_s=(/ istru_nh,     istr_nh,      istr_nh    /) 
      ifin_s=(/ istru_nh,     istr_nh,      istr_nh    /) 
      jdeb_s=(/  jstr_nh,    jstrv_nh,      jstr_nh    /) 
      jfin_s=(/  jstr_nh,    jstrv_nh,      jstr_nh    /) 
      kdeb_s=(/ 1,    1,    0    /)
      kfin_s=(/ kmax, kmax, kmax /)
!       ! Reception
      ideb_r=(/ istru_nh-1,     istr_nh-1,      istr_nh-1    /) 
      ifin_r=(/ istru_nh-1,     istr_nh-1,      istr_nh-1    /) 
      jdeb_r=(/  jstr_nh-1,    jstrv_nh-1,      jstr_nh-1    /) 
      jfin_r=(/  jstr_nh-1,    jstrv_nh-1,      jstr_nh-1    /) 
      kdeb_r=(/ 1,    1,    0    /)
      kfin_r=(/ kmax, kmax, kmax /)
   case(sudest)
       ! Envoie
      ideb_s=(/ iendu_nh,  iend_nh, iend_nh /) 
      ifin_s=(/ iendu_nh,  iend_nh, iend_nh /) 
      jdeb_s=(/  jstr_nh,    jstrv_nh,      jstr_nh    /) 
      jfin_s=(/  jstr_nh,    jstrv_nh,      jstr_nh    /) 
      kdeb_s=(/ 1,    1,    0    /)
      kfin_s=(/ kmax, kmax, kmax /)
!       ! Reception
      ideb_r=(/ iendu_nh+1,     iend_nh+1,      iend_nh+1      /) 
      ifin_r=(/ iendu_nh+1,     iend_nh+1,      iend_nh+1      /) 
      jdeb_r=(/  jstr_nh-1,    jstrv_nh-1,      jstr_nh-1      /) 
      jfin_r=(/  jstr_nh-1,    jstrv_nh-1,      jstr_nh-1      /) 
      kdeb_r=(/ 1,      1,      0      /)
      kfin_r=(/ kmax,   kmax,   kmax   /)
   case(nordouest)
!       ! Envoie
      ideb_s=(/ istru_nh,     istr_nh,      istr_nh     /) 
      ifin_s=(/ istru_nh,     istr_nh,      istr_nh     /) 
      jdeb_s=(/  jend_nh,    jendv_nh,      jend_nh     /) 
      jfin_s=(/  jend_nh,    jendv_nh,      jend_nh     /) 
      kdeb_s=(/ 1,    1,    0     /)
      kfin_s=(/ kmax, kmax, kmax  /)
!       ! Reception
      ideb_r=(/ istru_nh-1,     istr_nh-1,      istr_nh-1      /) 
      ifin_r=(/ istru_nh-1,     istr_nh-1,      istr_nh-1      /) 
      jdeb_r=(/  jend_nh+1,    jendv_nh+1,      jend_nh+1      /) 
      jfin_r=(/  jend_nh+1,    jendv_nh+1,      jend_nh+1      /) 
      kdeb_r=(/ 1,      1,      0      /)
      kfin_r=(/ kmax,   kmax,   kmax   /)
   case(nordest)
      ! Envoie
      ideb_s=(/ iendu_nh,  iend_nh, iend_nh    /) 
      ifin_s=(/ iendu_nh,  iend_nh, iend_nh    /) 
      jdeb_s=(/  jend_nh, jendv_nh, jend_nh     /) 
      jfin_s=(/  jend_nh, jendv_nh, jend_nh     /) 
      kdeb_s=(/ 1,    1,    0       /)
      kfin_s=(/ kmax, kmax, kmax    /)
      ! Reception
      ideb_r=(/ iendu_nh+1,  iend_nh+1, iend_nh+1 /) 
      ifin_r=(/ iendu_nh+1,  iend_nh+1, iend_nh+1 /) 
      jdeb_r=(/  jend_nh+1, jendv_nh+1, jend_nh+1 /) 
      jfin_r=(/  jend_nh+1, jendv_nh+1, jend_nh+1 /) 
      kdeb_r=(/ 1,      1,      0      /)
      kfin_r=(/ kmax,   kmax,   kmax   /)
   case default
      call mpi_finalize(ierr)
      stop 'borne_echange_qdm_nbq_a voisin inconu'
   end select
    

end subroutine 	borne_echange_qdm_nbq_a  
!--------------------------------------------------------------------------

!--------------------------------------------------------------------------
subroutine get_index_ghost_qdm_nbq_a(ideb, ifin, jdeb, jfin, kdeb, kfin, &
			    l_index, ii)
  use module_nh , only : ijk2lmom_nh
  implicit none
  integer,dimension(3),intent(in) :: ideb,ifin,jdeb,jfin,kdeb,kfin
  integer,dimension(:),intent(inout) :: l_index
  integer,intent(out) :: ii
  integer :: var, i, j, k, l_nbq
     ii=0
     do var=1,3 ! u,v,w ==> i=3,2,2
      do i=ideb(var),ifin(var)
	do k=kdeb(var),kfin(var)
        do j=jdeb(var),jfin(var)
           l_nbq=ijk2lmom_nh(i,j,k,var)
          if (l_nbq > 0) then
            ii=ii+1
            l_index(ii)=l_nbq
           endif
         enddo    
	 enddo
	 enddo
     enddo	 
end subroutine get_index_ghost_qdm_nbq_a
!--------------------------------------------------------------------------

!--------------------------------------------------------------------------
subroutine getblocks_qdm_nbq_a(voisin,imax,jmax,kmax,nbelt,nbblock,blockdeb,blocklength,sendrecv)
  use module_qsort
  implicit none
  integer, intent(in) :: voisin,nbelt,imax,jmax,kmax,sendrecv
  integer, intent(out) :: nbblock
  integer,dimension(nbelt), intent(out) :: blockdeb
  integer,dimension(nbelt), intent(out) :: blocklength
  !
  integer,dimension(nbelt) :: idi, idi_tri
  integer,dimension(3) :: ideb_s,ifin_s,jdeb_s,jfin_s,kdeb_s,kfin_s
  integer,dimension(3) :: ideb_r,ifin_r,jdeb_r,jfin_r,kdeb_r,kfin_r
  integer :: ii, bcl,bcl2
  integer :: intex
  !
  call borne_echange_qdm_nbq_a(voisin,imax,jmax,kmax, &
	  ideb_s,ifin_s,jdeb_s,jfin_s,kdeb_s,kfin_s, &
	  ideb_r,ifin_r,jdeb_r,jfin_r,kdeb_r,kfin_r )
	  
  ii=0
  if(sendrecv == 1) then !! sendrecv==1 => send
    call get_index_ghost_qdm_nbq_a(ideb_s, ifin_s,  &
                                  jdeb_s, jfin_s, kdeb_s, kfin_s, &
				  idi, ii)
   else
   call get_index_ghost_qdm_nbq_a(ideb_r, ifin_r,  &
                                  jdeb_r, jfin_r, kdeb_r, kfin_r, &
				  idi, ii)
  endif

  idi_tri(1:ii)=idi(1:ii) 
  call getblocks(ii,idi_tri,nbblock,blockdeb,blocklength)
  blockdeb(1:nbblock)=idi_tri(blockdeb(1:nbblock))-1
  blocklength = blocklength+1
end subroutine getblocks_qdm_nbq_a
!--------------------------------------------------------------------------


!--------------------------------------------------------------------------
subroutine create_echange_qdm_nbq_a(imax,jmax,kmax)
!
! Echanges des variables "boucle NBQ" quantité de mouvement :: 
! creation d'un type mpi d'echange
!
  use module_nh , only : ijk2lmom_nh, ijk2lq_nh, rhs2_nh,l2imom_nh,l2jmom_nh,l2kmom_nh, &
			 istr_nh,iend_nh,jstr_nh
!  use module_nbq
  use module_qsort
  implicit none
  integer,intent(in) :: imax,jmax,kmax
  integer :: bcl, bcl2
  integer :: nbblock
  integer,dimension((max(imax,jmax)+2)*(kmax+2)*4) :: blocklength
  integer,dimension((max(imax,jmax)+2)*(kmax+2)*4) :: blockdeb, blocktype
  integer :: ftrouestnb
  integer :: ftrnordnb
  integer,dimension(8) :: szmax 

  ftrouestnb=(max(imax,jmax+2))*(kmax+2)*4
  ftrnordnb =(max(imax,jmax)+2)*(kmax+2)*4
  szmax = (/ ftrouestnb, ftrouestnb, ftrnordnb, ftrnordnb, &
             ftrouestnb, ftrouestnb, ftrnordnb, ftrnordnb /)  
             
   do bcl=1, 8
!!Type Envoie
      nbblock=0
      call getblocks_qdm_nbq_a(liste_voisin(bcl), imax, jmax, kmax, &
 	        	      szmax(bcl),nbblock,blockdeb,blocklength,1)
        call MPI_Type_indexed( nbblock, blocklength(1:nbblock),	blockdeb(1:nbblock), MPI_DOUBLE_PRECISION, &
		      ech_qdm_nbq(liste_voisin(bcl))%send,ierr)                        
      call mpi_type_commit(ech_qdm_nbq(liste_voisin(bcl))%send,ierr)
!!Type Reception
     call getblocks_qdm_nbq_a(liste_voisin(bcl), imax, jmax, kmax, &
	        	      szmax(bcl),nbblock,blockdeb,blocklength,0)
      call MPI_Type_indexed( nbblock, blocklength,	blockdeb, MPI_DOUBLE_PRECISION, &
		      ech_qdm_nbq(liste_voisin(bcl))%recv,ierr)                        
      call mpi_type_commit(ech_qdm_nbq(liste_voisin(bcl))%recv,ierr)
   enddo	
   
end subroutine create_echange_qdm_nbq_a
!--------------------------------------------------------------------------

!--------------------------------------------------------------------------
!     Echange U
!--------------------------------------------------------------------------
subroutine get_index_ghost_qdmU_nbq_a(ideb, ifin, jdeb, jfin, kdeb, kfin, &
			    l_index, ii)
  use module_nh , only : ijk2lmom_nh
  implicit none
  integer,dimension(3),intent(in) :: ideb,ifin,jdeb,jfin,kdeb,kfin
  integer,dimension(:),intent(inout) :: l_index
  integer,intent(out) :: ii
  integer :: var, i, j, k, l_nbq
     ii=0
     var=1 !,3 ! u,v,w ==> i=3,2,2
      do i=ideb(var),ifin(var)
	do k=kdeb(var),kfin(var)
        do j=jdeb(var),jfin(var)
           l_nbq=ijk2lmom_nh(i,j,k,var)
          if (l_nbq > 0) then
            ii=ii+1
            l_index(ii)=l_nbq
           endif
         enddo    
	 enddo
      enddo	 
end subroutine get_index_ghost_qdmU_nbq_a
!--------------------------------------------------------------------------

!--------------------------------------------------------------------------
subroutine getblocks_qdmU_nbq_a(voisin,imax,jmax,kmax,nbelt,nbblock,blockdeb,blocklength,sendrecv)
  use module_qsort
  implicit none
  integer, intent(in) :: voisin,nbelt,imax,jmax,kmax,sendrecv
  integer, intent(out) :: nbblock
  integer,dimension(nbelt), intent(out) :: blockdeb
  integer,dimension(nbelt), intent(out) :: blocklength
  !
  integer,dimension(nbelt) :: idi, idi_tri
  integer,dimension(3) :: ideb_s,ifin_s,jdeb_s,jfin_s,kdeb_s,kfin_s
  integer,dimension(3) :: ideb_r,ifin_r,jdeb_r,jfin_r,kdeb_r,kfin_r
  integer :: ii, bcl,bcl2
  integer :: intex
  !
  call borne_echange_qdm_nbq_a(voisin,imax,jmax,kmax, &
	  ideb_s,ifin_s,jdeb_s,jfin_s,kdeb_s,kfin_s, &
	  ideb_r,ifin_r,jdeb_r,jfin_r,kdeb_r,kfin_r )
	  
  ii=0
  if(sendrecv == 1) then !! sendrecv==1 => send
    call get_index_ghost_qdmU_nbq_a(ideb_s, ifin_s,  &
                                  jdeb_s, jfin_s, kdeb_s, kfin_s, &
				  idi, ii)
   else
   call get_index_ghost_qdmU_nbq_a(ideb_r, ifin_r,  &
                                  jdeb_r, jfin_r, kdeb_r, kfin_r, &
				  idi, ii)
  endif

  idi_tri(1:ii)=idi(1:ii) 
  call getblocks(ii,idi_tri,nbblock,blockdeb,blocklength)
  blockdeb(1:nbblock)=idi_tri(blockdeb(1:nbblock))-1
  blocklength = blocklength+1
end subroutine getblocks_qdmU_nbq_a
!--------------------------------------------------------------------------

!--------------------------------------------------------------------------
subroutine create_echange_qdmU_nbq_a(imax,jmax,kmax)
!
! Echanges des variables "boucle NBQ" quantité de mouvement :: 
! creation d'un type mpi d'echange
!
  use module_nh , only : ijk2lmom_nh, ijk2lq_nh, rhs2_nh,l2imom_nh,l2jmom_nh,l2kmom_nh, &
			 istr_nh,iend_nh,jstr_nh
!  use module_nbq
  use module_qsort
  implicit none
  integer,intent(in) :: imax,jmax,kmax
  integer :: bcl, bcl2
  integer :: nbblock
  integer,dimension((max(imax,jmax)+2)*(kmax+2)*4) :: blocklength
  integer,dimension((max(imax,jmax)+2)*(kmax+2)*4) :: blockdeb, blocktype
  integer :: ftrouestnb
  integer :: ftrnordnb
  integer,dimension(8) :: szmax 

  ftrouestnb=(max(imax,jmax+2))*(kmax+2)*4
  ftrnordnb =(max(imax,jmax)+2)*(kmax+2)*4
  szmax = (/ ftrouestnb, ftrouestnb, ftrnordnb, ftrnordnb, &
             ftrouestnb, ftrouestnb, ftrnordnb, ftrnordnb /)  
             
   do bcl=1, 8
!!Type Envoie
      nbblock=0
      call getblocks_qdmU_nbq_a(liste_voisin(bcl), imax, jmax, kmax, &
 	        	      szmax(bcl),nbblock,blockdeb,blocklength,1)
        call MPI_Type_indexed( nbblock, blocklength(1:nbblock),	blockdeb(1:nbblock), MPI_DOUBLE_PRECISION, &
		      ech_qdmU_nbq(liste_voisin(bcl))%send,ierr)                        
      call mpi_type_commit(ech_qdmU_nbq(liste_voisin(bcl))%send,ierr)
!!Type Reception
     call getblocks_qdmU_nbq_a(liste_voisin(bcl), imax, jmax, kmax, &
	        	      szmax(bcl),nbblock,blockdeb,blocklength,0)
      call MPI_Type_indexed( nbblock, blocklength,	blockdeb, MPI_DOUBLE_PRECISION, &
		      ech_qdmU_nbq(liste_voisin(bcl))%recv,ierr)                        
      call mpi_type_commit(ech_qdmU_nbq(liste_voisin(bcl))%recv,ierr)
   enddo	
   
end subroutine create_echange_qdmU_nbq_a
!--------------------------------------------------------------------------

!--------------------------------------------------------------------------
!     Echange V
!--------------------------------------------------------------------------
subroutine get_index_ghost_qdmV_nbq_a(ideb, ifin, jdeb, jfin, kdeb, kfin, &
			    l_index, ii)
  use module_nh , only : ijk2lmom_nh
  implicit none
  integer,dimension(3),intent(in) :: ideb,ifin,jdeb,jfin,kdeb,kfin
  integer,dimension(:),intent(inout) :: l_index
  integer,intent(out) :: ii
  integer :: var, i, j, k, l_nbq
     ii=0
     var=2 !,3 ! u,v,w ==> i=3,2,2
      do i=ideb(var),ifin(var)
	do k=kdeb(var),kfin(var)
        do j=jdeb(var),jfin(var)
           l_nbq=ijk2lmom_nh(i,j,k,var)
!              write(200+par%rank,*) i,j,k," ---",ii," -- ",l_nbq
          if (l_nbq > 0) then
            ii=ii+1
            l_index(ii)=l_nbq
           endif
         enddo    
	 enddo
      enddo	 
end subroutine get_index_ghost_qdmV_nbq_a
!--------------------------------------------------------------------------

!--------------------------------------------------------------------------
subroutine getblocks_qdmV_nbq_a(voisin,imax,jmax,kmax,nbelt,nbblock,blockdeb,blocklength,sendrecv)
  use module_qsort
  implicit none
  integer, intent(in) :: voisin,nbelt,imax,jmax,kmax,sendrecv
  integer, intent(out) :: nbblock
  integer,dimension(nbelt), intent(out) :: blockdeb
  integer,dimension(nbelt), intent(out) :: blocklength
  !
  integer,dimension(nbelt) :: idi, idi_tri
  integer,dimension(3) :: ideb_s,ifin_s,jdeb_s,jfin_s,kdeb_s,kfin_s
  integer,dimension(3) :: ideb_r,ifin_r,jdeb_r,jfin_r,kdeb_r,kfin_r
  integer :: ii, bcl,bcl2
  integer :: intex
  !
  call borne_echange_qdm_nbq_a(voisin,imax,jmax,kmax, &
	  ideb_s,ifin_s,jdeb_s,jfin_s,kdeb_s,kfin_s, &
	  ideb_r,ifin_r,jdeb_r,jfin_r,kdeb_r,kfin_r )
	  
  ii=0
  if(sendrecv == 1) then !! sendrecv==1 => send
    call get_index_ghost_qdmV_nbq_a(ideb_s, ifin_s,  &
                                  jdeb_s, jfin_s, kdeb_s, kfin_s, &
				  idi, ii)
   else
   call get_index_ghost_qdmV_nbq_a(ideb_r, ifin_r,  &
                                  jdeb_r, jfin_r, kdeb_r, kfin_r, &
				  idi, ii)
  endif

  idi_tri(1:ii)=idi(1:ii) 
  call getblocks(ii,idi_tri,nbblock,blockdeb,blocklength)
  blockdeb(1:nbblock)=idi_tri(blockdeb(1:nbblock))-1
  blocklength = blocklength+1
end subroutine getblocks_qdmV_nbq_a
!--------------------------------------------------------------------------

!--------------------------------------------------------------------------
subroutine create_echange_qdmV_nbq_a(imax,jmax,kmax)
!
! Echanges des variables "boucle NBQ" quantité de mouvement :: 
! creation d'un type mpi d'echange
!
  use module_nh , only : ijk2lmom_nh, ijk2lq_nh, rhs2_nh,l2imom_nh,l2jmom_nh,l2kmom_nh, &
			 istr_nh,iend_nh,jstr_nh
!  use module_nbq
  use module_qsort
  implicit none
  integer,intent(in) :: imax,jmax,kmax
  integer :: bcl, bcl2
  integer :: nbblock
  integer,dimension((max(imax,jmax)+2)*(kmax+2)*4) :: blocklength
  integer,dimension((max(imax,jmax)+2)*(kmax+2)*4) :: blockdeb, blocktype
  integer :: ftrouestnb
  integer :: ftrnordnb
  integer,dimension(8) :: szmax 

  ftrouestnb=(max(imax,jmax+2))*(kmax+2)*4
  ftrnordnb =(max(imax,jmax)+2)*(kmax+2)*4
  szmax = (/ ftrouestnb, ftrouestnb, ftrnordnb, ftrnordnb, &
             ftrouestnb, ftrouestnb, ftrnordnb, ftrnordnb /)  
             
   do bcl=1, 8
!!Type Envoie
      nbblock=0
      call getblocks_qdmV_nbq_a(liste_voisin(bcl), imax, jmax, kmax, &
 	        	      szmax(bcl),nbblock,blockdeb,blocklength,1)
        call MPI_Type_indexed( nbblock, blocklength(1:nbblock),	blockdeb(1:nbblock), MPI_DOUBLE_PRECISION, &
		      ech_qdmV_nbq(liste_voisin(bcl))%send,ierr)                        
      call mpi_type_commit(ech_qdmV_nbq(liste_voisin(bcl))%send,ierr)
!!Type Reception
     call getblocks_qdmV_nbq_a(liste_voisin(bcl), imax, jmax, kmax, &
	        	      szmax(bcl),nbblock,blockdeb,blocklength,0)
      call MPI_Type_indexed( nbblock, blocklength,	blockdeb, MPI_DOUBLE_PRECISION, &
		      ech_qdmV_nbq(liste_voisin(bcl))%recv,ierr)                        
      call mpi_type_commit(ech_qdmV_nbq(liste_voisin(bcl))%recv,ierr)
   enddo	
   
end subroutine create_echange_qdmV_nbq_a
!--------------------------------------------------------------------------

!--------------------------------------------------------------------------
!     Echange W
!--------------------------------------------------------------------------
subroutine get_index_ghost_qdmW_nbq_a(ideb, ifin, jdeb, jfin, kdeb, kfin, &
			    l_index, ii)
  use module_nh , only : ijk2lmom_nh
  implicit none
  integer,dimension(3),intent(in) :: ideb,ifin,jdeb,jfin,kdeb,kfin
  integer,dimension(:),intent(inout) :: l_index
  integer,intent(out) :: ii
  integer :: var, i, j, k, l_nbq
     ii=0
     var=3 !,3 ! u,v,w ==> i=3,2,2
      do i=ideb(var),ifin(var)
	do k=kdeb(var),kfin(var)
        do j=jdeb(var),jfin(var)
           l_nbq=ijk2lmom_nh(i,j,k,var)
          if (l_nbq > 0) then
            ii=ii+1
            l_index(ii)=l_nbq
           endif
         enddo    
	 enddo
      enddo 
end subroutine get_index_ghost_qdmW_nbq_a
!--------------------------------------------------------------------------

!--------------------------------------------------------------------------
subroutine getblocks_qdmW_nbq_a(voisin,imax,jmax,kmax,nbelt,nbblock,blockdeb,blocklength,sendrecv)
  use module_qsort
  implicit none
  integer, intent(in) :: voisin,nbelt,imax,jmax,kmax,sendrecv
  integer, intent(out) :: nbblock
  integer,dimension(nbelt), intent(out) :: blockdeb
  integer,dimension(nbelt), intent(out) :: blocklength
  !
  integer,dimension(nbelt) :: idi, idi_tri
  integer,dimension(3) :: ideb_s,ifin_s,jdeb_s,jfin_s,kdeb_s,kfin_s
  integer,dimension(3) :: ideb_r,ifin_r,jdeb_r,jfin_r,kdeb_r,kfin_r
  integer :: ii, bcl,bcl2
  integer :: intex
  !
  call borne_echange_qdm_nbq_a(voisin,imax,jmax,kmax, &
	  ideb_s,ifin_s,jdeb_s,jfin_s,kdeb_s,kfin_s, &
	  ideb_r,ifin_r,jdeb_r,jfin_r,kdeb_r,kfin_r )
	  
  ii=0
  if(sendrecv == 1) then !! sendrecv==1 => send
    call get_index_ghost_qdmW_nbq_a(ideb_s, ifin_s,  &
                                  jdeb_s, jfin_s, kdeb_s, kfin_s, &
				  idi, ii)
   else
   call get_index_ghost_qdmW_nbq_a(ideb_r, ifin_r,  &
                                  jdeb_r, jfin_r, kdeb_r, kfin_r, &
				  idi, ii)
  endif

  idi_tri(1:ii)=idi(1:ii) 
  call getblocks(ii,idi_tri,nbblock,blockdeb,blocklength)
  blockdeb(1:nbblock)=idi_tri(blockdeb(1:nbblock))-1
  blocklength = blocklength+1
end subroutine getblocks_qdmW_nbq_a
!--------------------------------------------------------------------------

!--------------------------------------------------------------------------
subroutine create_echange_qdmW_nbq_a(imax,jmax,kmax)
!
! Echanges des variables "boucle NBQ" quantité de mouvement :: 
! creation d'un type mpi d'echange
!
  use module_nh , only : ijk2lmom_nh, ijk2lq_nh, rhs2_nh,l2imom_nh,l2jmom_nh,l2kmom_nh, &
			 istr_nh,iend_nh,jstr_nh
!  use module_nbq
  use module_qsort
  implicit none
  integer,intent(in) :: imax,jmax,kmax
  integer :: bcl, bcl2
  integer :: nbblock
  integer,dimension((max(imax,jmax)+2)*(kmax+2)*4) :: blocklength
  integer,dimension((max(imax,jmax)+2)*(kmax+2)*4) :: blockdeb, blocktype
  integer :: ftrouestnb
  integer :: ftrnordnb
  integer,dimension(8) :: szmax 

  ftrouestnb=(max(imax,jmax+2))*(kmax+2)*4
  ftrnordnb =(max(imax,jmax)+2)*(kmax+2)*4
  szmax = (/ ftrouestnb, ftrouestnb, ftrnordnb, ftrnordnb, &
             ftrouestnb, ftrouestnb, ftrnordnb, ftrnordnb /)  
             
   do bcl=1, 8
!!Type Envoie
      nbblock=0
      call getblocks_qdmW_nbq_a(liste_voisin(bcl), imax, jmax, kmax, &
 	        	      szmax(bcl),nbblock,blockdeb,blocklength,1)
        call MPI_Type_indexed( nbblock, blocklength(1:nbblock),	blockdeb(1:nbblock), MPI_DOUBLE_PRECISION, &
		      ech_qdmW_nbq(liste_voisin(bcl))%send,ierr)                        
      call mpi_type_commit(ech_qdmW_nbq(liste_voisin(bcl))%send,ierr)
!!Type Reception
     call getblocks_qdmW_nbq_a(liste_voisin(bcl), imax, jmax, kmax, &
	        	      szmax(bcl),nbblock,blockdeb,blocklength,0)
      call MPI_Type_indexed( nbblock, blocklength,	blockdeb, MPI_DOUBLE_PRECISION, &
		      ech_qdmW_nbq(liste_voisin(bcl))%recv,ierr)                        
      call mpi_type_commit(ech_qdmW_nbq(liste_voisin(bcl))%recv,ierr)
   enddo	
   
end subroutine create_echange_qdmW_nbq_a
!--------------------------------------------------------------------------



!-------------------------------------------------------------------------------------
!-------------------------------------------------------------------------------------
!                 ROUTINES DIV_NBQ
!-------------------------------------------------------------------------------------
!-------------------------------------------------------------------------------------
!--------------------------------------------------------------------------
subroutine borne_echange_div_nbq_a(voisin,imax,jmax,kmax, &
	  ideb_s,ifin_s,jdeb_s,jfin_s,kdeb_s,kfin_s, &
	  ideb_r,ifin_r,jdeb_r,jfin_r,kdeb_r,kfin_r )
  use module_nh , only : istr_nh,iend_nh,jstr_nh,jend_nh, &
	    istru_nh,iendu_nh,jstrv_nh,jendv_nh
!  use module_parallele 	  
  implicit none
  integer,intent(in) :: voisin	  
  integer,intent(in) :: imax,jmax,kmax
  integer,dimension(3),intent(out) :: ideb_s,ifin_s,jdeb_s,jfin_s,kdeb_s,kfin_s
  integer,dimension(3),intent(out) :: ideb_r,ifin_r,jdeb_r,jfin_r,kdeb_r,kfin_r
  
  select case(voisin)
   case(ouest) !........................
      ! Envoie
      ideb_s=(/ istr_nh, 0, 0 /); jdeb_s=(/ jstr_nh, 0, 0 /);     kdeb_s=(/ 1   , 0, 0 /) 
      ifin_s=(/ istr_nh, 0, 0 /); jfin_s=(/ jend_nh, 0, 0 /);     kfin_s=(/ kmax, 0, 0 /)
      
      ! Reception
      ideb_r=(/ istr_nh-1, 0, 0 /); jdeb_r=(/ jstr_nh, 0, 0 /);     kdeb_r=(/ 1 ,   0, 0 /)
      ifin_r=(/ istr_nh-1, 0, 0 /); jfin_r=(/ jend_nh, 0, 0 /);     kfin_r=(/ kmax, 0, 0 /)

   case(est)
      ! Envoie
      ideb_s=(/ iend_nh,   0, 0  /); jdeb_s=(/ jstr_nh,    0, 0 /);   kdeb_s=(/ 1,    0, 0 /)
      ifin_s=(/ iend_nh,   0, 0  /); jfin_s=(/ jend_nh, 0, 0 /);   kfin_s=(/ kmax, 0, 0 /)
      ! Reception
      ideb_r=(/ iend_nh+1, 0, 0  /); jdeb_r=(/ jstr_nh,    0, 0 /);   kdeb_r=(/ 1,    0, 0 /) 
      ifin_r=(/ iend_nh+1, 0, 0  /); jfin_r=(/ jend_nh, 0, 0 /);   kfin_r=(/ kmax, 0, 0 /)
           
  case(sud)
      ! Envoie
      ideb_s=(/ istr_nh,    0, 0 /);  jdeb_s=(/ jstr_nh, 0, 0  /);kdeb_s=(/ 1,    0, 0 /)
      ifin_s=(/ iend_nh,    0, 0 /);  jfin_s=(/ jstr_nh, 0, 0  /);kfin_s=(/ kmax, 0, 0 /)    
      ! Reception
      ideb_r=(/ istr_nh,    0, 0 /);  jdeb_r=(/ jstr_nh-1, 0, 0  /);kdeb_r=(/ 1,    0, 0 /)
      ifin_r=(/ iend_nh,    0, 0 /);  jfin_r=(/ jstr_nh-1, 0, 0  /);kfin_r=(/ kmax, 0, 0 /)

  case(nord)
      ! Envoie
      ideb_s=(/ istr_nh,    0, 0 /); jdeb_s=(/ jend_nh,    0, 0 /);kdeb_s=(/ 1,    0, 0 /)
      ifin_s=(/ iend_nh,    0, 0 /); jfin_s=(/ jend_nh,    0, 0 /);kfin_s=(/ kmax, 0, 0 /)
!       ! Reception
      ideb_r=(/ istr_nh,    0, 0 /);  jdeb_r=(/ jend_nh+1, 0, 0 /); kdeb_r=(/ 1,    0, 0 /)
      ifin_r=(/ iend_nh,    0, 0 /);  jfin_r=(/ jend_nh+1, 0, 0 /); kfin_r=(/ kmax, 0, 0 /)
      
       
  case(sudouest) 
!       ! Envoie
      ideb_s=(/ istr_nh,   0, 0 /);jdeb_s=(/ jstr_nh, 0, 0 /);kdeb_s=(/ 1   , 0, 0 /)
      ifin_s=(/ istr_nh,   0, 0 /);jfin_s=(/ jstr_nh, 0, 0 /);kfin_s=(/ kmax, 0, 0 /)
!       ! Reception
      ideb_r=(/ istr_nh-1, 0, 0 /);jdeb_r=(/ jstr_nh-1, 0, 0 /);kdeb_r=(/ 1,    0, 0 /)
      ifin_r=(/ istr_nh-1, 0, 0 /);jfin_r=(/ jstr_nh-1, 0, 0 /);kfin_r=(/ kmax, 0, 0 /)
      
   case(sudest)
       ! Envoie
      ideb_s=(/ iend_nh,   0, 0 /);  jdeb_s=(/ jstr_nh, 0, 0 /); kdeb_s=(/    1, 0, 0 /)
      ifin_s=(/ iend_nh,   0, 0 /);  jfin_s=(/ jstr_nh, 0, 0 /); kfin_s=(/ kmax, 0, 0 /)   
!      ! Reception
      ideb_r=(/ iend_nh+1, 0, 0 /);jdeb_r=(/ jstr_nh-1, 0, 0 /); kdeb_r=(/ 1,    0, 0 /)
      ifin_r=(/ iend_nh+1, 0, 0 /);jfin_r=(/ jstr_nh-1, 0, 0 /); kfin_r=(/ kmax, 0, 0 /)
 
   case(nordouest)
!       ! Envoie
      ideb_s=(/ istr_nh,   0, 0 /);jdeb_s=(/ jend_nh,   0, 0 /);kdeb_s=(/ 1,    0, 0 /)
      ifin_s=(/ istr_nh,   0, 0 /);jfin_s=(/ jend_nh,   0, 0 /);kfin_s=(/ kmax, 0, 0 /)     
!       ! Reception
      ideb_r=(/ istr_nh-1, 0, 0 /);jdeb_r=(/ jend_nh+1, 0, 0 /);kdeb_r=(/ 1,    0, 0 /)
      ifin_r=(/ istr_nh-1, 0, 0 /);jfin_r=(/ jend_nh+1, 0, 0 /);kfin_r=(/ kmax, 0, 0 /)

   case(nordest)
      ! Envoie
      ideb_s=(/ iend_nh, 0,   0 /);jdeb_s=(/ jend_nh,   0, 0 /);kdeb_s=(/ 1,     0, 0 /)
      ifin_s=(/ iend_nh, 0,   0 /);jfin_s=(/ jend_nh,   0, 0 /);kfin_s=(/ kmax,  0, 0 /)
!     ! Reception
      ideb_r=(/ iend_nh+1, 0, 0 /);jdeb_r=(/ jend_nh+1, 0, 0 /);kdeb_r=(/ 1,     0, 0 /)
      ifin_r=(/ iend_nh+1, 0, 0 /);jfin_r=(/ jend_nh+1, 0, 0 /); kfin_r=(/ kmax, 0, 0 /)

   case default
      call mpi_finalize(ierr)
      stop 'borne_echange_qdm_nbq_a voisin inconu'
   end select

end subroutine 	borne_echange_div_nbq_a  
!--------------------------------------------------------------------------

!--------------------------------------------------------------------------
subroutine get_index_ghost_div_nbq_a(ideb, ifin, jdeb, jfin, kdeb, kfin, &
			    l_index, ii)
  use module_nh , only : ijk2lq_nh
  implicit none
  integer,dimension(3),intent(in) :: ideb,ifin,jdeb,jfin,kdeb,kfin
  integer,dimension(:),intent(inout) :: l_index
  integer,intent(out) :: ii
  integer :: var, i, j, k, l_nbq
     ii=0
     var=1 !! inutile mais pour garder la me syntax que get_index_ghost_qdm_nbq_a
     do i=ideb(var),ifin(var)
        do k=kdeb(var),kfin(var)
           do j=jdeb(var),jfin(var)
              l_nbq=ijk2lq_nh(i,j,k)
              if (l_nbq > 0) then
                  ii=ii+1
                  l_index(ii)=l_nbq  
              endif
           enddo    
        enddo
      enddo
!      write(3200+par%rank,*)
end subroutine get_index_ghost_div_nbq_a
!--------------------------------------------------------------------------

!--------------------------------------------------------------------------
subroutine getblocks_div_nbq_a(voisin,imax,jmax,kmax,nbelt,nbblock,blockdeb,blocklength,sendrecv)
  use module_qsort
  implicit none
  integer, intent(in) :: voisin,nbelt,imax,jmax,kmax,sendrecv
  integer, intent(out) :: nbblock
  integer,dimension(nbelt), intent(out) :: blockdeb
  integer,dimension(nbelt), intent(out) :: blocklength
  !
  integer,dimension(nbelt) :: idi, idi_tri
  integer,dimension(3) :: ideb_s,ifin_s,jdeb_s,jfin_s,kdeb_s,kfin_s
  integer,dimension(3) :: ideb_r,ifin_r,jdeb_r,jfin_r,kdeb_r,kfin_r
  integer :: ii, bcl,bcl2
  integer :: intex
  !
  nbblock=0
  call borne_echange_div_nbq_a(voisin,imax,jmax,kmax, &
	  ideb_s,ifin_s,jdeb_s,jfin_s,kdeb_s,kfin_s, &
	  ideb_r,ifin_r,jdeb_r,jfin_r,kdeb_r,kfin_r )
	  
  ii=0
  if(sendrecv == 1) then !! sendrecv==1 => send
    call get_index_ghost_div_nbq_a(ideb_s, ifin_s,  &
                                  jdeb_s, jfin_s, kdeb_s, kfin_s, &
				  idi, ii)  
   else
   call get_index_ghost_div_nbq_a(ideb_r, ifin_r,  &
                                  jdeb_r, jfin_r, kdeb_r, kfin_r, &
				  idi, ii)
  endif

  idi_tri(1:ii)=idi(1:ii) !qsort(idi(1:ii)) !.......

  call getblocks(ii,idi_tri,nbblock,blockdeb,blocklength)				 
  blockdeb(1:nbblock)=(idi_tri(blockdeb(1:nbblock))-1)
  blocklength = (blocklength+1)
end subroutine getblocks_div_nbq_a

!--------------------------------------------------------------------------
subroutine create_echange_div_nbq_a(imax,jmax,kmax)
!
! Echanges des variables "boucle NBQ" divergence :: 
! creation d'un type mpi d'echange
!
  use module_nh , only : ijk2lmom_nh, ijk2lq_nh, rhs2_nh,l2imom_nh,l2jmom_nh,l2kmom_nh
!  use module_nbq
  use module_qsort
  implicit none
  integer,intent(in) :: imax,jmax,kmax
  integer :: bcl,bcl2
  integer :: nbblock
  integer,dimension((max(imax,jmax)+2)*(kmax+2)*4) :: blocklength
  integer,dimension((max(imax,jmax)+2)*(kmax+2)*4) :: blockdeb, blocktype
  integer :: ftrouestnb
  integer :: ftrnordnb
  integer,dimension(8) :: szmax 

  ftrouestnb=(max(imax,jmax+2))*(kmax+2)*4
  ftrnordnb =(max(imax,jmax)+2)*(kmax+2)*4
  szmax = (/ ftrouestnb, ftrouestnb, ftrnordnb, ftrnordnb, &
             ftrouestnb, ftrouestnb, ftrnordnb, ftrnordnb /)  
             
   do bcl=1, 8 
!!Type Envoie
      call getblocks_div_nbq_a(liste_voisin(bcl), imax, jmax, kmax, &
 	        	      szmax(bcl),nbblock,blockdeb,blocklength,1)
     if (par%tvoisin(bcl) /= MPI_PROC_NULL) then
      endif
      
      call MPI_Type_indexed( nbblock, blocklength(1:nbblock),	blockdeb(1:nbblock), MPI_DOUBLE_PRECISION, &
		      ech_div_nbq(liste_voisin(bcl))%send,ierr)                        
      call mpi_type_commit(ech_div_nbq(liste_voisin(bcl))%send,ierr)
!!Type Reception
     call getblocks_div_nbq_a(liste_voisin(bcl), imax, jmax, kmax, &
	        	      szmax(bcl),nbblock,blockdeb,blocklength,0)
      if (par%tvoisin(bcl) /= MPI_PROC_NULL) then
      do bcl2=1,nbblock
      enddo
      endif
     call MPI_Type_indexed( nbblock, blocklength,	blockdeb, MPI_DOUBLE_PRECISION, &
		      ech_div_nbq(liste_voisin(bcl))%recv,ierr)                        
      call mpi_type_commit(ech_div_nbq(liste_voisin(bcl))%recv,ierr)
   enddo	
   
end subroutine create_echange_div_nbq_a
!--------------------------------------------------------------------------

!--------------------------------------------------------------------------
! Initialise Persistant Communications
subroutine  Persistant_init
      use module_nbq, only : qdm_nbq_a, div_nbq_a
  integer :: bcl, szsend, szrecv, vois, vnnew_nbq, coef

  szsend=0
  szrecv=0
! Init qdmU exchange-----------------------------------------------------------
          do vnnew_nbq=0,2
	  coef=(vnnew_nbq+1)*10
	  p_nbreq_qdmU(vnnew_nbq)=1
  	  do bcl=1, 8
	   vois=lvoisin(bcl)
	   if (par%tvoisin(vois) /= mpi_proc_null) then 
	   call MPI_TYPE_SIZE(ech_qdmU_nbq(vois)%send, szsend,ierr)
	   call MPI_TYPE_SIZE(ech_qdmU_nbq(vois)%recv, szrecv,ierr) 
 	   !print *,par%rank,"szsend=",szsend," voisin",par%tvoisin(vois),"  --",MPI_PROC_NULL
 	   !print *,par%rank,"szrecv=",szrecv," voisin",par%tvoisin(vois)
	   if (szsend >0) then
	      call MPI_RECV_INIT(qdm_nbq_a(1,vnnew_nbq),  1, ech_qdmU_nbq(vois)%recv, par%tvoisin(vois), &
			     tagqdmU_Recv(vois)*coef, par%comm2d, &
			     p_tabreq_qdmU(p_nbreq_qdmU(vnnew_nbq),vnnew_nbq), ierr)
	      p_nbreq_qdmU(vnnew_nbq)=p_nbreq_qdmU(vnnew_nbq)+1
	      call MPI_SEND_INIT(qdm_nbq_a(1,vnnew_nbq),  1, ech_qdmU_nbq(vois)%send, par%tvoisin(vois), &
			     tagqdmU_Send(vois)*coef, par%comm2d, &
			     p_tabreq_qdmU(p_nbreq_qdmU(vnnew_nbq),vnnew_nbq), ierr)
	      p_nbreq_qdmU(vnnew_nbq)=p_nbreq_qdmU(vnnew_nbq)+1
  	   endif
           endif
	  enddo
	  p_nbreq_qdmU(vnnew_nbq)= p_nbreq_qdmU(vnnew_nbq)-1
	  enddo
!--------------------------------------------------------------------------------------

! Init qdmV exchange-----------------------------------------------------------
          do vnnew_nbq=0,2
	  coef=(vnnew_nbq+1)*10
	  p_nbreq_qdmV(vnnew_nbq)=1
  	  do bcl=1, 8
	   vois=lvoisin(bcl)
	   if (par%tvoisin(vois) /= mpi_proc_null) then 
	   call MPI_TYPE_SIZE(ech_qdmV_nbq(vois)%send, szsend,ierr)
	   call MPI_TYPE_SIZE(ech_qdmV_nbq(vois)%recv, szrecv,ierr) 
! 	   print *,par%rank,"szsend=",szsend," voisin",bcl
	   if (szsend >0) then
	      call MPI_RECV_INIT(qdm_nbq_a(1,vnnew_nbq),  1, ech_qdmV_nbq(vois)%recv, par%tvoisin(vois), &
			     tagqdmV_Recv(vois)*coef, par%comm2d, &
			     p_tabreq_qdmV(p_nbreq_qdmV(vnnew_nbq),vnnew_nbq), ierr)
	      p_nbreq_qdmV(vnnew_nbq)=p_nbreq_qdmV(vnnew_nbq)+1
	      call MPI_SEND_INIT(qdm_nbq_a(1,vnnew_nbq),  1, ech_qdmV_nbq(vois)%send, par%tvoisin(vois), &
			     tagqdmV_Send(vois)*coef, par%comm2d, &
			     p_tabreq_qdmV(p_nbreq_qdmV(vnnew_nbq),vnnew_nbq), ierr)
	      p_nbreq_qdmV(vnnew_nbq)=p_nbreq_qdmV(vnnew_nbq)+1
  	   endif
           endif
	  enddo
	  p_nbreq_qdmV(vnnew_nbq)= p_nbreq_qdmV(vnnew_nbq)-1
	  enddo
!--------------------------------------------------------------------------------------

! Init qdmW exchange-----------------------------------------------------------
          do vnnew_nbq=0,2
	  coef=(vnnew_nbq+1)*10
	  p_nbreq_qdmW(vnnew_nbq)=1
  	  do bcl=1, 8
	   vois=lvoisin(bcl)
	   if (par%tvoisin(vois) /= mpi_proc_null) then 
	   call MPI_TYPE_SIZE(ech_qdmW_nbq(vois)%send, szsend,ierr)
	   call MPI_TYPE_SIZE(ech_qdmW_nbq(vois)%recv, szrecv,ierr) 
! 	   print *,par%rank,"szsend=",szsend," voisin",bcl
	   if (szsend >0) then
	      call MPI_RECV_INIT(qdm_nbq_a(1,vnnew_nbq),  1, ech_qdmW_nbq(vois)%recv, par%tvoisin(vois), &
			     tagqdmW_Recv(vois)*coef, par%comm2d, &
			     p_tabreq_qdmW(p_nbreq_qdmW(vnnew_nbq),vnnew_nbq), ierr)
	      p_nbreq_qdmW(vnnew_nbq)=p_nbreq_qdmW(vnnew_nbq)+1
	      call MPI_SEND_INIT(qdm_nbq_a(1,vnnew_nbq),  1, ech_qdmW_nbq(vois)%send, par%tvoisin(vois), &
			     tagqdmW_Send(vois)*coef, par%comm2d, &
			     p_tabreq_qdmW(p_nbreq_qdmW(vnnew_nbq),vnnew_nbq), ierr)
	      p_nbreq_qdmW(vnnew_nbq)=p_nbreq_qdmW(vnnew_nbq)+1
  	   endif
           endif
	  enddo
	  p_nbreq_qdmW(vnnew_nbq)= p_nbreq_qdmW(vnnew_nbq)-1
	  enddo
!--------------------------------------------------------------------------------------

! Init DIV exchange-----------------------------------------------------------
          do vnnew_nbq=0,1 !!! dnrhs_nbq in [0,1]
	  coef=(vnnew_nbq+1)*10
	  p_nbreq_mv(vnnew_nbq)=1
  	  do bcl=1, 8
	   vois=lvoisin(bcl)
	   if (par%tvoisin(vois) /= mpi_proc_null) then 
	   call MPI_TYPE_SIZE(ech_div_nbq(vois)%send, szsend,ierr)
	   call MPI_TYPE_SIZE(ech_div_nbq(vois)%recv, szrecv,ierr) 
! 	   print *,par%rank,"szsend=",szsend," voisin",bcl
	   if (szsend >0) then
	      call MPI_RECV_INIT(div_nbq_a(1,vnnew_nbq),  1, ech_div_nbq(vois)%recv, par%tvoisin(vois), &
			     tagdiv_Recv(vois)*coef, par%comm2d, &
			     p_tabreq_mv(p_nbreq_mv(vnnew_nbq),vnnew_nbq), ierr)
	      p_nbreq_mv(vnnew_nbq)=p_nbreq_mv(vnnew_nbq)+1
	      call MPI_SEND_INIT(div_nbq_a(1,vnnew_nbq),  1, ech_div_nbq(vois)%send, par%tvoisin(vois), &
			     tagdiv_Send(vois)*coef, par%comm2d, &
			     p_tabreq_mv(p_nbreq_mv(vnnew_nbq),vnnew_nbq), ierr)
	      p_nbreq_mv(vnnew_nbq)=p_nbreq_mv(vnnew_nbq)+1
  	   endif
           endif
	  enddo
	  p_nbreq_mv(vnnew_nbq)= p_nbreq_mv(vnnew_nbq)-1
	  enddo
!--------------------------------------------------------------------------------------


! 	  call MPI_Barrier(MPI_COMM_WORLD,ierr)
!           call mpi_Finalize(ierr)
!           stop 'toto'
          
          
end subroutine  Persistant_init 	  
!--------------------------------------------------------------------------
      
end module module_parallel_nbq

#else
 module module_parallel_nbq
  implicit none
  integer, parameter :: ouest=1,est=2,nord=3,sud=4,haut=5,bas=6
  integer, parameter :: sudouest=7,sudest=8,nordouest=9,nordest=10
  integer, parameter :: ouestest=1,nordsud=2

  integer,parameter :: MPI_PROC_NULL=-2

  type infopar_croco
         integer ::  comm2d                 !COMMUNICATEUR GLOBAL
         integer ::  rank
         integer,dimension(10)                      ::  tvoisin
  end type 

!  integer,dimension(8),parameter :: liste_voisin = &
!      (/ ouest, est, nord, sud, sudouest, sudest, nordouest, nordest /)
  integer :: ierr,mynode
  type(infopar_croco) :: par

 end module module_parallel_nbq
#endif
