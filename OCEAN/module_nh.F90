#include "cppdefs.h"
#ifdef NBQ
      module module_nh


!__________________________________________________________________________
!
!                               SNH2012.14      
!                 Non-Hydrostatic & Non-Boussinesq Kernel Version  
! Laboratoire d Aerologie, 14 Avenue Edouard Belin, F-31400 Toulouse
! http://poc.obs-mip.fr/auclair/WOcean.fr/SNH/index_snh_home.htm  
!
!__________________________________________________________________________


! debut module ne pas toucher a cette ligne

!.....nombre moyen de termes par ligne pour les matrices : 3D!
      integer, parameter  :: ntcont_nh=14,ntmom_nh=6,ntw_nh=5  

!.....taille du vecteur q ==> lignes de cont:
!     frontieres necessaires pour l utilisation parallele.
      integer  :: nmq_nh

!.....taille du vecteur v ==> lignes de mom:
      integer  :: nmv_nh

!.....taille max de la matrice cont:
      integer  :: nmcont_nh

!.....taille max de la matrice mom:
      integer  :: nmmom_nh

!.....taille max de la matrice prod:
      integer  :: nmw_nh

!**********************************************************************
!.....Flags et dimensions "integer"
!**********************************************************************
      integer  ::                                                     &
         istr_nh                                                      &
        ,jstr_nh                                                      &
        ,iend_nh                                                      &
        ,jend_nh                                                      &
        ,istru_nh                                                     &
        ,jstru_nh                                                     &
        ,istrv_nh                                                     &
        ,jstrv_nh                                                     &
        ,iendu_nh                                                     &
        ,jendu_nh                                                     &
        ,iendv_nh                                                     & 
        ,jendv_nh                                                     & 
        ,istrq_nh                                                     &
        ,iendq_nh                                                     &
        ,jstrq_nh                                                     &
        ,jendq_nh
 
      integer  ::                                                     &
         ifl_nh                                                       &
        ,ifl_solv_nh                                                  &
        ,l_nh                                                         &
        ,l1_nh                                                        &
        ,l2_nh                                                        &
        ,nnz_nh         (10)                                          &
        ,nzeq_nh                                                      &
        ,nzcont_nh                                                    &
#ifdef NBQ_CONS0  
        ,nzcorr_nh                                                    &
#endif             
        ,nzmom_nh                                                     &   
        ,neqcont_nh                                                   &  
        ,neqmom_nh      (0:3)                                         &
        ,neqq_nh        (0:7)                                         &
        ,nequ_nh        (0:7)                                         &
        ,neqv_nh        (0:7)                                         &
        ,neqw_nh        (0:7)                                      
        
#ifdef NBQ_CONS4
      integer  ::                                                     &
         neqcine_nh                                                   &
        ,nzcine_nh                                                    
#endif    

      integer,dimension(:),allocatable ::                             &        
         conti_nh                                                     &   
        ,cont_nnz_nh                                                  & 
        ,contj_nh      

#ifdef NBQ_CONS0                                                 
      integer,dimension(:),allocatable ::                             &  
         corri_nh                                                     & 
        ,corrj_nh         
#endif                   

#ifdef NBQ_CONS4
      integer,dimension(:),allocatable ::                             &  
         cinei_nh                                                     &
        ,cinej_nh
#endif


!**********************************************************************
!.....Variables Real
!**********************************************************************
      real ::      &
       period_exp  &
      ,for_a_exp   &
      ,dg_exp      &
      ,hmax_exp    &
      ,amp_exp

!**********************************************************************
!.....Tableaux "integer"
!**********************************************************************
      integer, dimension(:), allocatable     ::                       &
         l2iq_nh        &  
        ,l2jq_nh        &  
        ,l2kq_nh        &  
        ,l2imom_nh      &  
        ,l2jmom_nh      &  
        ,l2kmom_nh      &  
        ,momi_nh        &  
        ,momj_nh           
        
      integer, dimension(:,:,:), allocatable    ::                    &
         ijk2lq_nh      &  
        ,mijk2lq_nh        

      integer, dimension(:,:,:,:), allocatable  ::                    &
         ijk2lmom_nh    &  
        ,mijk2lmom_nh      

!**********************************************************************
!.....Tableaux: double precision
!**********************************************************************                                            
        
      double precision, dimension(:), allocatable      ::             &
         contv_nh       &  
        ,momv_nh        &  
        ,momvg_nh       &  
        ,rhs1_nh        &  
        ,rhs2_nh      

#ifdef NBQ_CONS4
      double precision, dimension(:), allocatable      ::             &
         cinev_nh                                                     & 
        ,rhscine2_nh        
#endif

#ifdef NBQ_CONS0
      double precision, dimension(:), allocatable      ::             & 
         corrv_nh                                                     
#endif
 
      double precision, dimension(:,:), allocatable    ::             &
         coriolis_nh_t                                                

#ifdef NBQ_CONS4
      double precision, dimension(:,:), allocatable    ::             &
         rhscine_nh          
#endif
        
      double precision, dimension(:,:,:), allocatable   ::            &
         coefa_u        &  
        ,coefb_u        &  
        ,coefa_v        &  
        ,coefb_v        &  
        ,gdepth_u       &  
        ,gdepth_v       

        
      double precision, dimension(:,:,:,:), allocatable     ::        &
         div_nh_t          

      double precision                                                &
         time_omp_nh    (100)                                        


! fin module ne pas toucher a cette ligne
         
         contains!----------------------------------------------------
!CXA         subroutine alloc_module_nh(imax,jmax,kmax,nbdom_world)
         subroutine alloc_module_nh()
         implicit none

# include "param_F90.h"
#include "def_bounds.h"

         integer :: imax,jmax,nbdom_world

      imax=LOCALLM
      jmax=LOCALMM

#ifdef MPI
         nbdom_world=NNODES
#else
         nbdom_world=1
#endif

         nmq_nh=(imax+4)*(jmax+4)*(N+1)
         nmv_nh=(imax+4)*(jmax+4)*N                &
               +(imax+4)*(jmax+4)*N                &
               +(imax+4)*(jmax+4)*(N+1)*2.

         nmw_nh    = nmv_nh*ntw_nh
         nmcont_nh = nmq_nh*ntcont_nh
         nmmom_nh  = nmv_nh*ntmom_nh

! Variables communes SNH / SNBQ
         allocate(conti_nh        (nmq_nh)                   )  

#ifdef NBQ_CONS4
         allocate(cinei_nh        (nmq_nh)                   )  
#endif

#ifdef NBQ_CONS0
         allocate(corri_nh        (nmq_nh)                   ) 
#endif 

         allocate(cont_nnz_nh     (nmq_nh)                   )  
         allocate(contj_nh        (nmcont_nh)                )  
#ifdef NBQ_CONS4
         allocate(cinej_nh        (nmcont_nh)                ) 
#endif
#ifdef NBQ_CONS0 
         allocate(corrj_nh        (nmcont_nh)                ) 
#endif
         allocate(l2iq_nh         (nmq_nh)                   ) 
         allocate(l2jq_nh         (nmq_nh)                   ) 
         allocate(l2kq_nh         (nmq_nh)                   )  
         allocate(l2imom_nh       (nmv_nh)                   )  
         allocate(l2jmom_nh       (nmv_nh)                   )  
         allocate(l2kmom_nh       (nmv_nh)                   )   
         allocate(momi_nh         (nmv_nh)                   )  
         allocate(momj_nh         (nmmom_nh)                 )  
         allocate(ijk2lq_nh       (GLOBAL_2D_ARRAY,0:N+1)    )   
         allocate(mijk2lq_nh      (GLOBAL_2D_ARRAY,0:N+1)    )
         allocate(ijk2lmom_nh     (GLOBAL_2D_ARRAY,0:N+1,4)  )   
         allocate(mijk2lmom_nh    (GLOBAL_2D_ARRAY,0:N+1,4)  )
         allocate(contv_nh        (0:nmcont_nh)              ) 
#ifdef NBQ_CONS4
         allocate(cinev_nh        (0:nmcont_nh)              )  
#endif
#ifdef NBQ_CONS0
         allocate(corrv_nh        (0:nmcont_nh)              )  
#endif
         allocate(momv_nh         (nmmom_nh)                 )  
         allocate(momvg_nh        (nmmom_nh)                 ) 
         allocate(rhs1_nh         (nmv_nh)                   )  
#ifdef NBQ_CONS4
         allocate(rhscine_nh      (nmq_nh,0:2)               )  
         allocate(rhscine2_nh      (nmv_nh)                  )
#endif
         allocate(rhs2_nh         (nmq_nh)                   )  
         allocate(coriolis_nh_t   (GLOBAL_2D_ARRAY)          )  
         allocate(coefa_u         (GLOBAL_2D_ARRAY,0:N+1)    )  
         allocate(coefb_u         (GLOBAL_2D_ARRAY,0:N+1)    ) 
         allocate(coefa_v         (GLOBAL_2D_ARRAY,0:N+1)    )  
         allocate(coefb_v         (GLOBAL_2D_ARRAY,0:N+1)    )  
         allocate(gdepth_u        (GLOBAL_2D_ARRAY,0:N+1)    ) 
         allocate(gdepth_v        (GLOBAL_2D_ARRAY,0:N+1)    )  
         allocate(div_nh_t        (GLOBAL_2D_ARRAY,0:N,2)    )  

! 
!        
         end subroutine alloc_module_nh

        end module module_nh  
#else
        module module_nh_empty
        end module
#endif
