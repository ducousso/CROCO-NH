#include "cppdefs.h"
#ifdef NBQ
      module module_nbq

      implicit none

!__________________________________________________________________________________
!
!                               SNH2012.14      
!                 Non-Hydrostatic & Non-Boussinesq Kernel Version  
! Laboratoire d Aerologie, 14 Avenue Edouard Belin, F-31400 Toulouse
! http://poc.obs-mip.fr/auclair/WOcean.fr/SNH/index_snh_home.htm  
!
!__________________________________________________________________________________


! debut module ne pas toucher à cette ligne
      integer  :: nmq_nbq,nmcont_nbq
      integer  :: nmv_nbq,nmmom_nbq,nmprod_nbq

!**********************************************************************
! Logical
!**********************************************************************
      logical             ::                                          &
       WEST_INTER_NBQ                                                 &
      ,EAST_INTER_NBQ                                                 &
      ,SOUTH_INTER_NBQ                                                &
      ,NORTH_INTER_NBQ                                     
      
!**********************************************************************
!.....Flags et dimensions
!**********************************************************************
      integer             ::                                          &
       iteration_nbq_max                                              &
      ,iteration_nbq                                                  &
      ,l_nbq                                                          &
      ,l1_nbq                                                         &
      ,ifl_nbq                                                        &
      ,slip_nbq          

      integer             ::                                          &
       neqcorrt_nbq
                                
      double precision    ::                                          &
       ifl_filtre_nbq                                                 &
      ,fl_nbq                                                         &
      ,cw_int_nbq                                      !CXA

      integer             ::                                          &
       rnnew_nbq &
      ,rnrhs_nbq &
      ,rnstp_nbq &
      ,vnnew_nbq &
      ,vnrhs_nbq &
      ,vnstp_nbq &
      ,dnrhs_nbq &
      ,dnstp_nbq 

!**********************************************************************
!.....double precision
!**********************************************************************

!.....Variables 0D:
      double precision    ::                                          &
       soundspeed_nbq                                                 &
      ,soundspeed2_nbq                                                &
      ,time_nbq           

!....Variables algébriques mode NBQ:
      double precision,dimension(:),allocatable     ::                &
       dqdmdt_nbq_a                                                   &
      ,rhssum_nbq_a
      double precision,dimension(:,:),allocatable     ::              &
       qdm_nbq_a                                                      &
      ,rhp_nbq_a                                                      &
      ,div_nbq_a                                                      &
      ,rhp_bq_a


!....Tableaux de travail NBQ:
      double precision,dimension(:),allocatable    ::                 &
       rhs1_nbq                                                       &
      ,rhs2_nbq                                                       &
      ,rhs1r_nbq                                                      &
      ,rhsd2_nbq                                                      &
      ,visc2_nbq_a     

!.....Variables mode EXT:  
      double precision,dimension(:,:,:),allocatable   ::              &
       dqdmdtext_nbq_u                                                &
      ,dqdmdtext_nbq_v                                                &
      ,qdm_u_sum                                                      &
      ,qdm_v_sum                                                      &
      ,qdm_u_ext                                                      &
      ,qdm_v_ext

!.....Variables mode INT:
      double precision,dimension(:,:,:,:),allocatable   ::            &
       dqdmdtint_nbq_u                                                &
      ,dqdmdtint_nbq_v                                                &
      ,dqdmdtint_nbq_w                                                &
      ,qdm_u                                                          &
      ,qdm_v                                                          &
      ,qdm_w                                                          &
      ,qdm_u_int                                                      &
      ,qdm_v_int                                                      &
      ,qdm_w_int

!.....Pour echanges "parallele":
      double precision ,dimension(:,:,:),allocatable    ::            &
      qdm_nbq_u                                                       &
     ,qdm_nbq_v                                                       &
     ,qdm_nbq_w                                                       &
     ,rhp_nbq_t        

!.....Pour partie implicite:
      integer::                                                       &
      nzmimp_nbq                                                      &
     ,nzcimp_nbq                                                      &
     ,l1imp_nbq                                                       &
     ,neqmimp_nbq                                                     &
     ,neqcimp_nbq                                                     &
     ,nnz_nbq                                                         &
     ,nindkun_nbq                                                     &
     ,ifl_imp_nbq

      double precision,dimension(:), allocatable ::                   &
      cimpv_nbq                                                       &
     ,mimpv_nbq                                                       &
     ,rhsimp_nbq                                                      &
     ,rhsimp2_nbq                                                     &
     ,qdmimp_nbq                                                      &
     ,pdv_nbq                                                         &
     ,puv_nbq                                                         &
     ,plv_nbq                                                         &
     ,pdvint_nbq                                                      &
     ,puvint_nbq                                                      &
     ,plvint_nbq                                                      &
     ,puvsave_nbq                                                     &
     ,plvsave_nbq                                                     &
     ,pdvsave_nbq                                                     &
     ,rhsint_nbq


      integer,dimension(:), allocatable ::                            &
      cimpi_nbq                                                       &
     ,cimpj_nbq                                                       &
     ,mimpi_nbq                                                       &
     ,mimpj_nbq                                                       &
     ,pimpi_nbq                                                       &
     ,pimpj_nbq                                                       &
     ,iw1_nbq                                                         &
     ,iw2_nbq                                                         &
     ,indkun_nbq

      integer ::                                                      &
      ptri_nbq


      integer :: nbsendOUEST, idiOUEST=1
      integer :: nbsendEST, idiEST=1
      integer :: nbsendNORD, idiNORD=1
      integer :: nbsendSUD, idiSUD=1
      integer :: nbsendOUEST_qdm, idiOUEST_qdm=1
      integer :: nbsendEST_qdm, idiEST_qdm=1
      integer :: nbsendNORD_qdm, idiNORD_qdm=1
      integer :: nbsendSUD_qdm, idiSUD_qdm=1
      integer :: nbsendOUEST_mv, idiOUEST_mv=1
      integer :: nbsendEST_mv, idiEST_mv=1
      integer :: nbsendNORD_mv, idiNORD_mv=1
      integer :: nbsendSUD_mv, idiSUD_mv=1

      integer :: nbsendSUDOUEST, idiSUDOUEST=1
      integer :: nbsendSUDEST, idiSUDEST=1
      integer :: nbsendNORDEST, idiNORDEST=1
      integer :: nbsendNORDOUEST, idiNORDOUEST=1

      integer :: nbsendSUDOUEST_qdm, idiSUDOUEST_qdm=1
      integer :: nbsendSUDEST_qdm, idiSUDEST_qdm=1
      integer :: nbsendNORDEST_qdm, idiNORDEST_qdm=1
      integer :: nbsendNORDOUEST_qdm, idiNORDOUEST_qdm=1

      integer :: nbsendSUDOUEST_mv, idiSUDOUEST_mv=1
      integer :: nbsendSUDEST_mv, idiSUDEST_mv=1
      integer :: nbsendNORDEST_mv, idiNORDEST_mv=1
      integer :: nbsendNORDOUEST_mv, idiNORDOUEST_mv=1

      integer,dimension(:,:), allocatable :: tstatus

      integer,parameter :: TAGOUEST_qdm=55000, TAGEST_qdm=55010, TAGSUD_qdm=56000, TAGNORD_qdm=56010
      integer,parameter :: TAGSUDOUEST_qdm=155000, TAGSUDEST_qdm=515010, TAGNORDOUEST_qdm=516000, TAGNORDEST_qdm=516010
      integer,parameter :: TAGOUEST_mv=75000, TAGEST_mv=75010, TAGSUD_mv=76000, TAGNORD_mv=76010
      integer,parameter :: TAGSUDOUEST_mv=715000, TAGSUDEST_mv=715010, TAGNORDOUEST_mv=716000, TAGNORDEST_mv=716010
      integer,parameter :: TAGOUEST_rhs=85000, TAGEST_rhs=85010, TAGSUD_rhs=86000, TAGNORD_rhs=86010
      integer,parameter :: TAGSUDOUEST_rhs=815000, TAGSUDEST_rhs=815010, TAGNORDOUEST_rhs=816000, TAGNORDEST_rhs=816010
! fin module ne pas toucher à cette ligne                        
     
      
      contains   !--------------------------------------------------------------------
      !--------------------------------------------------------------------
      subroutine alloc_module_nbq(MPI_STATUS_SIZE)
      implicit none
#include "param_F90.h"
#include "def_bounds.h"

      integer,intent(in) :: MPI_STATUS_SIZE
      integer :: imax,jmax

      imax=LOCALLM
      jmax=LOCALMM

      !
      nmq_nbq    = (imax+2)*(jmax+2)*(N+1)
      nmv_nbq    = (imax+2)*(jmax+2)*(N+1)*4
      nmprod_nbq = (imax+2)*(jmax+2)*(N+1)*10
      nmcont_nbq = nmq_nbq*20
      nmmom_nbq  = nmv_nbq*20


!....Variables algébriques mode NBQ:
       allocate(  dqdmdt_nbq_a      (1:nmv_nbq)      )
       allocate(  qdm_nbq_a         (1:nmv_nbq,-2:2) )
       allocate(  rhp_bq_a          (1:nmq_nbq,0:2)  )
       allocate(  rhp_nbq_a         (1:nmq_nbq,-1:2) )
       allocate(  rhssum_nbq_a      (1:nmv_nbq)      )        
       allocate(  div_nbq_a         (1:nmq_nbq,0:1)  )     
            
!....Tableaux de travail NBQ:
       allocate(  rhs1_nbq          (1:nmv_nbq)  )
       allocate(  rhs1r_nbq         (1:nmv_nbq)  )
       allocate(  rhs2_nbq          (1:nmv_nbq)  )
       allocate(  rhsd2_nbq         (1:nmv_nbq)  )   
       allocate(  visc2_nbq_a       (1:nmv_nbq)  )  

!.....Variables mode EXT:
       allocate(  dqdmdtext_nbq_u   (GLOBAL_2D_ARRAY,1)    )
       allocate(  dqdmdtext_nbq_v   (GLOBAL_2D_ARRAY,1)    )
       allocate(  qdm_u_ext         (GLOBAL_2D_ARRAY,0:2)  )
       allocate(  qdm_v_ext         (GLOBAL_2D_ARRAY,0:2)  )
       allocate(  qdm_u_sum         (GLOBAL_2D_ARRAY,-1:2) )
       allocate(  qdm_v_sum         (GLOBAL_2D_ARRAY,-1:2) )

!.....Variables mode INT:
       allocate(  dqdmdtint_nbq_u   (GLOBAL_2D_ARRAY,0:N+1,1)    )
       allocate(  dqdmdtint_nbq_v   (GLOBAL_2D_ARRAY,0:N+1,1)    )
       allocate(  dqdmdtint_nbq_w   (GLOBAL_2D_ARRAY,-1:N+1,1)   )
       allocate(  qdm_u             (GLOBAL_2D_ARRAY,0:N+1,0:2)  )
       allocate(  qdm_v             (GLOBAL_2D_ARRAY,0:N+1,0:2)  )
       allocate(  qdm_w             (GLOBAL_2D_ARRAY,-1:N+1,0:2) )
       allocate(  qdm_u_int         (GLOBAL_2D_ARRAY,0:N+1,0:2)  )
       allocate(  qdm_v_int         (GLOBAL_2D_ARRAY,0:N+1,0:2)  )
       allocate(  qdm_w_int         (GLOBAL_2D_ARRAY,-1:N+1,0:2) )

!.....Pour echanges "parallele":
       allocate(  qdm_nbq_u         (GLOBAL_2D_ARRAY,0:N+1)   )     
       allocate(  qdm_nbq_v         (GLOBAL_2D_ARRAY,0:N+1)   )     
       allocate(  qdm_nbq_w         (GLOBAL_2D_ARRAY,-1:N+1)  )      
       allocate(  rhp_nbq_t         (GLOBAL_2D_ARRAY,0:N+1)   )  

!......Pour partie implicit:
       allocate(rhsimp_nbq       (nmv_nbq)      )    
       allocate(rhsimp2_nbq      (nmv_nbq)      )   
       allocate(rhsint_nbq       (nmv_nbq)      )
       allocate(qdmimp_nbq       (nmv_nbq)      )    
       allocate(cimpv_nbq        (0:nmcont_nbq) )    
       allocate(mimpv_nbq        (nmmom_nbq)    )    
       allocate(cimpi_nbq        (nmq_nbq)      )   
       allocate(cimpj_nbq        (nmcont_nbq)   )   
       allocate(mimpi_nbq        (nmv_nbq)      )   
       allocate(mimpj_nbq        (nmmom_nbq)    )   
       allocate(pimpi_nbq        (nmv_nbq)      )   
       allocate(pimpj_nbq        (nmprod_nbq)   )  
       allocate(iw1_nbq          (nmv_nbq)      )  
       allocate(iw2_nbq          (nmv_nbq)      )  
       allocate(indkun_nbq       (nmv_nbq)      )   

       allocate(pdv_nbq          (nmv_nbq)      )
       allocate(puv_nbq          (nmv_nbq)      )
       allocate(plv_nbq          (nmv_nbq)      )
       allocate(pdvint_nbq       (nmv_nbq)      )
       allocate(puvint_nbq       (nmv_nbq)      )
       allocate(plvint_nbq       (nmv_nbq)      )

       allocate(pdvsave_nbq      (nmv_nbq)      )
       allocate(puvsave_nbq      (nmv_nbq)      )
       allocate(plvsave_nbq      (nmv_nbq)      )


      allocate( tstatus(MPI_STATUS_SIZE,60) )
      
      end subroutine alloc_module_nbq

      end module module_nbq

#else
      module module_nbq_empty
      end module
#endif      
