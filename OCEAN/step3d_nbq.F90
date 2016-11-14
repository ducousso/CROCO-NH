#include "cppdefs.h"
#ifdef NBQ
!
!======================================================================
!                      NBQ-Mode for NH-modeling
!                            Main Routine
!======================================================================
!
!> @note Main web documentation at:
! http://poc.obs-mip.fr/auclair/WOcean.fr/SNH/index_snh_home.htm
!
! DESCRIPTION: 
!
!> @brief SNBQ driver : Non-hydrostatic algorithm with the 
!                       Non-boussinesq solver.
!
!> @details NBQ time step. See SNBQ web pages :
!  http://poc.obs-mip.fr/auclair/WOcean.fr/SNH/Restricted/NH-NBQ/Html_pages
!    Algorithme_NBQ.htm      --> SNBQ algorithm:               
!    Algebrique_SNBQ.htm     --> SNBQ algebric representation
!    Couplage_Numerique.htm  --> Numerical coupling
!    Couplage_Modes_SNBQ.htm --> Coupling:
!    Couplage_Split_NBQ.htm  --> Coupling Splitting
!
! REVISION HISTORY:
!
!> @authors
!> @date 2015 January
!> @todo
!
!======================================================================
!
      subroutine step3d_nbq (Istr,Iend,Jstr,Jend, WORK)

      use module_nh 
      use module_nbq

      implicit none

# include "param_F90.h"
# include "scalars_F90.h"
# include "ocean2d.h"
# include "ocean3d.h"
# include "grid.h"
# include "nbq.h"
# ifdef MPI
      include 'mpif.h'
# endif
      integer Istr,Iend,Jstr,Jend,i,j,k
      real    WORK(PRIVATE_2D_SCRATCH_ARRAY)
      real :: dum_s
# ifndef MPI
      integer mynode
      mynode=0
# endif
# undef DEBUG
!
!-------------------------------------------------------------------
!       Store internal mode density field
!-------------------------------------------------------------------
!
!       call density_nbq(4)
!
#  ifdef ACOUS
      if (iic.eq.1) call density_nbq(10)  ! Initialize Acoustic waves
#  endif

!-------------------------------------------------------------------
!       Initializes velocity
!-------------------------------------------------------------------
!
# ifdef KH_INST
      if (iic.eq.1.and.iif.eq.1) then
        do l_nbq = 1,nequ_nh(7)
          i=l2imom_nh(l_nbq)
          j=l2jmom_nh(l_nbq)
          k=l2kmom_nh(l_nbq)
          qdm_nbq_a(l_nbq,vnnew_nbq)=rho0*u(i,j,k,nrhs)*hz_half(i,j,k)
        enddo
        call parallele_nbq(51)
        call parallele_nbq(151)
        qdm_nbq_a(:,vnrhs_nbq)=qdm_nbq_a(:,vnnew_nbq)
        qdm_nbq_a(:,vnstp_nbq)=qdm_nbq_a(:,vnnew_nbq)
      endif
# endif
!
!-------------------------------------------------------------------
!  Get internal and external forcing terms for nbq equations:
!  ru+rubar (or lambda_ext+lambda_int)
!  dzdt*rhosurf
!-------------------------------------------------------------------
!
      call ru_nbq(1)
      call density_nbq(1)

!-------------------------------------------------------------------
!       Implicit part: system setup
!-------------------------------------------------------------------
!
# ifdef NBQ_IMP
      if (iif.eq.1.and.ifl_imp_nbq.eq.1) call implicit_nbq (1)
# endif
!
!*******************************************************************
!*******************************************************************
!              NBQ mode iteration (main loop)
!*******************************************************************
!*******************************************************************

      do iteration_nbq=1,iteration_nbq_max

# ifdef DEBUG
        print *,'STEP3D_NBQ: it_nbq = ',iteration_nbq
# endif


!.......1st iteration NBQ: treats NBQ
        if (iteration_nbq>1) then
!        Receive
# ifndef NBQ_IMP
          call parallele_nbq(151)
          call parallele_nbq(152)
          call parallele_nbq(153)
# else
          call parallele_nbq(153)
# endif
!        Momentum equation: switch time indices (move forward)
         call ru_nbq(7)
        endif
!
!-------------------------------------------------------------------
!      Compute divergence term (AMUX):
!          qdm_nbq_a ==> div_nbq_a
!-------------------------------------------------------------------
!
        call density_nbq(6)
!
!-------------------------------------------------------------------
!      Compute Second viscosity (product mat*vect):
!            div_nbq_a ==> rhsd2_nbq
!-------------------------------------------------------------------
!
        call viscous_nbq (1)
!
!-------------------------------------------------------------------
!      Message passing , indice switch (iif=1)
!-------------------------------------------------------------------
!
!  Send
        call parallele_nbq(7) 
!       call parallele_nbq(17) 
!
!-------------------------------------------------------------------
!      Acoustic wave emission
!-------------------------------------------------------------------
!
# ifdef ACOUS
        call density_nbq(11)
# endif
!
!-------------------------------------------------------------------
!      Compute pressure gradient and gravity terms (AMUX)
!                rhp_nbq_a  ==> rhs1_nbq
!-------------------------------------------------------------------
!
        call ru_nbq(6)
!
!-------------------------------------------------------------------
!      Horizontal Momentum equation: leapfrog time stepping
!         If explicit: (x,y,z) is dealt with here
!-------------------------------------------------------------------
!
!  XI-Direction:
!
        do l_nbq = nequ_nh(1)+1,nequ_nh(6)
          dum_s = soundspeed_nbq**2  * rhs1_nbq(l_nbq)                 &
                - visc2_nbq_a(l_nbq) * rhsd2_nbq(l_nbq)   
          qdm_nbq_a(l_nbq,vnnew_nbq) = qdm_nbq_a(l_nbq,vnstp_nbq)      & 
                                       + 2.*dtnbq*(                    &
                                              dum_s                    &
                                            + dqdmdt_nbq_a(l_nbq) )  
          rhssum_nbq_a(l_nbq) = rhssum_nbq_a(l_nbq)  +  dum_s  
          
        enddo 
!
!  U-momentum open boundary conditions
!
# ifdef OBC_NBQ
        call unbq_bc_tile (Istr,Iend,Jstr,Jend, WORK)
# endif
!
!  Message passing: Send U (51) 
!
        call parallele_nbq(51)
!       call parallele_nbq(151)
!
!  ETA-Direction:
!
        do l_nbq = neqv_nh(1)+1,neqv_nh(6)  
          dum_s =             soundspeed_nbq**2  * rhs1_nbq(l_nbq)     &
                            - visc2_nbq_a(l_nbq) * rhsd2_nbq(l_nbq)   
          qdm_nbq_a(l_nbq,vnnew_nbq) = qdm_nbq_a(l_nbq,vnstp_nbq)      &
                                       + 2.*dtnbq*(                    &
                                              dum_s                    &
                                            + dqdmdt_nbq_a(l_nbq) )  
          rhssum_nbq_a(l_nbq) = rhssum_nbq_a(l_nbq)  +  dum_s    
        enddo 
!
!  V-momentum open boundary conditions
!
# ifdef OBC_NBQ
        call vnbq_bc_tile (Istr,Iend,Jstr,Jend, WORK)
# endif
!
!  Message passing: Send V (52) 
!
        call parallele_nbq(52)
!       call parallele_nbq(152)
!
!-------------------------------------------------------------------
!      Vertical Momentum equation: leapfrog time stepping
!         If explicit: (x,y,z) is dealt with here
!         If implicit: (x,y)   only
!-------------------------------------------------------------------
!
# ifndef NBQ_IMP
!
!  Z-Direction: Explicit
!
        do l_nbq = neqw_nh(1)+1,neqw_nh(6)
          dum_s = soundspeed_nbq**2  * rhs1_nbq(l_nbq)                 &
                - visc2_nbq_a(l_nbq) * rhsd2_nbq(l_nbq)
          qdm_nbq_a(l_nbq,vnnew_nbq) = qdm_nbq_a(l_nbq,vnstp_nbq)      &
                                          + 2.*dtnbq*(                 &
                                                 dum_s                 &
                                               + dqdmdt_nbq_a(l_nbq) )
          rhssum_nbq_a(l_nbq) = rhssum_nbq_a(l_nbq)  +  dum_s
        enddo 
# else
        call parallele_nbq(151)  ! u only 
        call parallele_nbq(152)  ! v only 

        call implicit_nbq (2)
# endif
!
!      Vertical momentum open boundary conditions
!
# ifdef OBC_NBQ
!       call wnbq_bc_tile (Istr,Iend,Jstr,Jend, WORK)
# endif
!
!-------------------------------------------------------------------
!      Message passing 
!-------------------------------------------------------------------
!
!  Send
        call parallele_nbq(53)   ! w only 
!       call parallele_nbq(153)   ! w only 

!  Receive
        call parallele_nbq(17)
!
!-------------------------------------------------------------------
!      Mass equation: leapfrog time stepping:
!-------------------------------------------------------------------
!
# ifndef NBQ_CONS0
        do l_nbq=1,neqcont_nh
          rhp_nbq_a(l_nbq,rnnew_nbq) = rhp_nbq_a(l_nbq,rnstp_nbq)      &
                             - div_nbq_a(l_nbq,dnrhs_nbq) * 2. * dtnbq 
        enddo
# else
        do l_nbq=1,neqcont_nh
          rhp_nbq_a(l_nbq,rnnew_nbq) = rhp_nbq_a(l_nbq,rnstp_nbq)      &
                             - div_nbq_a(l_nbq,dnrhs_nbq) * 2. * dtnbq &
                             + rhs1r_nbq(l_nbq) * 2. * dtnbq          
        enddo   
# endif
!
!-------------------------------------------------------------------
!      Density open boundary conditions
!-------------------------------------------------------------------
!
# ifdef OBC_NBQ
!        call rnbq_bc_tile (Istr,Iend,Jstr,Jend, WORK)
# endif
!
!-------------------------------------------------------------------
!       Mass equation: switch time indices (move forward)
!-------------------------------------------------------------------
!
        call density_nbq(7)
!       call density_nbq(17)
!
!*******************************************************************
!*******************************************************************
      enddo    ! NBQ loop

!*******************************************************************
!*******************************************************************
!
!
!-------------------------------------------------------------------
!......Message passing 
!-------------------------------------------------------------------
!
! Receive
!       call parallele_nbq(15)
# ifndef NBQ_IMP
        call parallele_nbq(151)       
        call parallele_nbq(152)       
        call parallele_nbq(153)  
# else       
        call parallele_nbq(153)  
# endif
!
!-------------------------------------------------------------------
!......Move forward: momentum
!-------------------------------------------------------------------
!
       call ru_nbq(7)
!
!-------------------------------------------------------------------
!......Computes surface kinematic condition
!-------------------------------------------------------------------
!
# ifdef NBQ_CONS4
       call cons_nbq(8)
# endif
!
!-------------------------------------------------------------------
!......Set NBQ/EXT coupling terms
!-------------------------------------------------------------------
!
      call ru_nbq(2)
# ifdef RVTK_DEBUG
!       call check_tab2d(rubar_nbq(:,:),'rubar_nbq step3d_nbq','u')
!       call check_tab2d(rvbar_nbq(:,:),'rvbar_nbq step3d_nbq','v')
!       call check_tab3d(rw_nbq_ext(:,:,0:N),'rw_nbq_ext step3d_nbq','r')
# endif      
      call density_nbq(2)
# ifdef RVTK_DEBUG
      call check_tab3d(rho_nbq_ext(:,:,1:N),'rho_nbq_ext step3d_nbq','r')
      call check_tab2d(rhobar_nbq(:,:,knew),'rhobar_nbq step3d_nbq','r')
# endif      
      end subroutine step3d_nbq

#else
      subroutine step3d_nbq_empty
      end subroutine step3d_nbq_empty
#endif
