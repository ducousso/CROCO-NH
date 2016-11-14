#include "cppdefs.h"
#ifdef NBQ
      subroutine initial_nh (icall)
!**********************************************************************
!
!                 NH and NBQ initialization
!
!**********************************************************************
!CXA MODULES TO ADD 

      use module_nh
      use module_nbq
# ifdef TRACETXT
      use module_tracetxt_out
# endif
      implicit none
# ifdef MPI      
      include 'mpif.h' !-------
# endif      
# include "param_F90.h"
# include "scalars_F90.h"
# include "work.h"
# include "grid.h"
# include "ocean2d.h"
# include "ocean3d.h"
# include "nbq.h"

      integer :: i,j,k,ierr
      integer :: icall

      if (icall.eq.1) then
!**********************************************************************
!
!                 rhp_nbq_a initializations (PART I)
!
!**********************************************************************

!----------------------------------------------------------------------
!........Semi-implicit scheme (0/1):
!----------------------------------------------------------------------
# ifdef NBQ_IMP
         ifl_imp_nbq = 1
         MPI_master_only write(6,*)
         MPI_master_only write(6,*) '--------------------------------'
         MPI_master_only write(6,*) ' NBQ: semi-implicit integration !'
         MPI_master_only write(6,*) '--------------------------------'
         MPI_master_only write(6,*)
# else
         ifl_imp_nbq = 0
         MPI_master_only write(6,*)
         MPI_master_only write(6,*) '---------------------------'
         MPI_master_only write(6,*) ' NBQ: explicit integration !'
         MPI_master_only write(6,*) '---------------------------'
         MPI_master_only write(6,*)
# endif

!
!----------------------------------------------------------------------
!  Initialize density perturbation and momentum arrays
!----------------------------------------------------------------------
!
        rhp_nbq_a(:,:) = 0.0
        qdm_nbq_a(:,:) = 0.0
        rhobar_nbq  = 1.

        vnnew_nbq = 2
        vnrhs_nbq = 1
        vnstp_nbq = 0

        rnnew_nbq = 2
        rnrhs_nbq = 1
        rnstp_nbq = 0

        dnrhs_nbq = 1
        dnstp_nbq = 0

!----------------------------------------------------------------------
!  Initialize parameters: should be done in a NH-namelist
!----------------------------------------------------------------------
!
        ifl_nbq  = 1        !CXA put elswhere or replace by cpp keys
        slip_nbq = 0

        iteration_nbq_max=ndtnbq
        soundspeed_nbq=csound_nbq !!! pseudoacoustic speed for tank
!       cw_int_nbq=soundspeed_nbq !!! ~ 2-10 sqrt(gH)_max
        cw_int_nbq=sqrt(9.81*4000.) !soundspeed_nbq !!! ~ 2-10 sqrt(gH)_max


!       MPI_master_only write(stdout,'3(A,I4/)')
!                       'NBQ: INITIALIZING ifl_nbq      =',ifl_nbq
!                       '     INITIALIZING slip_nbq     =',slip_nbq
!                       '     INITIALIZING ifl_imp_nbq  =',ifl_imp_nbq
!
!----------------------------------------------------------------------
!  Pre-numbering of grid points and matrices:
!----------------------------------------------------------------------
!
!....NH-Grid definition:
      call grid_def_nh

!... Numbering of pressure points:
      call nump_nh
!

!... Numbering of velocity points:
      call numuvw_nh
!
!----------------------------------------------------------------------
!... MPI Set-up 
!    Initializes NBQ exchange (ikl2l_xxx is needed)
!----------------------------------------------------------------------
!
# ifdef MPI 
      call MPI_nbq_Setup(Lmmpi,Mmmpi,N)
# endif
!
!----------------------------------------------------------------------
!... Initializes Grid-coef
!----------------------------------------------------------------------
!
      call grid_coef_nh
!
!----------------------------------------------------------------------
!... Initialize NBQ "implicit" scheme 
!----------------------------------------------------------------------
!
# ifdef NBQ_IMP
      if ( ifl_imp_nbq.eq.1 ) call implicit_nbq(0) 
# endif
         
!... Initialize momentum equations matrix (mom):
      call mat_mom_init_nh

!... Initialize density equation matrix (cont):
      call mat_cont_init_nh

!... Initialize implicit matrix
# ifdef NBQ_IMP
      if ( ifl_imp_nbq.eq.1 )  call implicit_nbq(-1)
# endif
!
!----------------------------------------------------------------------
!... ASCII output for NBQ-grid 
!----------------------------------------------------------------------
!
!# ifdef NBQ_OUT
      call output_nbq(1)
!# endif
!
!----------------------------------------------------------------------
!... Initialize matrix products and HIPS:
!----------------------------------------------------------------------
!
# ifndef NOHIPS
!CXA      call poisson_nh(0)
# endif
!
!----------------------------------------------------------------------
!... Set second viscosity coefficient:
!----------------------------------------------------------------------
!
         call viscous_nbq(0)
!
!----------------------------------------------------------------------
!... Initializes Acoustic waves:
!----------------------------------------------------------------------
!
!        call density_nbq(10)


       endif     ! icall == 1

       if (icall.eq.2) then
!**********************************************************************
!
!                 rhp_nbq_a initializations (PART II)
!
!**********************************************************************

!.........EOS to compute rho (ifnot already done):
          call rho_eos

!.........Initialize NBQ density field:
# ifdef NBQ_CONS7
          stop 'initial_nh !!!!!!!!'
!         do l_nbq=1,neqcont_nh
!            i = l2iq_nh(l_nbq)
!            j = l2jq_nh(l_nbq)
!            k = l2kq_nh(l_nbq)
!            rhp_nbq_a(l_nbq,0:2) = rho(i,j,k)*Hzr(i,j,k) 
!         enddo
# else
!         do l_nbq=1,neqcont_nh
!            i = l2iq_nh(l_nbq)
!            j = l2jq_nh(l_nbq)
!            k = l2kq_nh(l_nbq)
!            rhp_nbq_a(l_nbq,0:2) = rho(i,j,k)
!         enddo
# endif

!.........Initialize NBQ density field:
          do l_nbq=1,neqcont_nh
            i = l2iq_nh(l_nbq)
            j = l2jq_nh(l_nbq)
            k = l2kq_nh(l_nbq)
!           rhp_nbq_a(l_nbq,0:2) = rho(i,j,k)
            rhp_bq_a(l_nbq,0:2)  = rho(i,j,k)
            rho_nbq_ext(i,j,k)   = (rho0+rho(i,j,k))/rho0
            rho_nbq_avg1(i,j,k)  = (rho0+rho(i,j,k))/rho0
            rho_nbq_avg2(i,j,k)  = (rho0+rho(i,j,k))/rho0
          enddo

          rhobar_nbq     (:,:,:)=1.
          rhobar_nbq_avg1(:,:  )=1.

          do j=jstrq_nh-1,jendq_nh+1
          do i=istrq_nh-1,iendq_nh+1
             work2d(i,j)         = 0.
             rhobar_nbq(i,j,:)   = 0.
          enddo
          enddo

          do j=jstrq_nh-1,jendq_nh+1
          do i=istrq_nh-1,iendq_nh+1
          do k=1,N
             work2d(i,j)         = work2d(i,j)+Hzr(i,j,k)
             rhobar_nbq(i,j,:)   = rhobar_nbq(i,j,:)+     &
                                rho(i,j,k)*Hzr(i,j,k)
          enddo
          enddo
          enddo
!
!.......Rho0 added subsequently for added precision
 
        do j=jstrq_nh-1,jendq_nh+1
        do i=istrq_nh-1,iendq_nh+1
           rhobar_nbq(i,j,:)   = (rhobar_nbq(i,j,:)/work2d(i,j) + rho0)  &
                                 / rho0
           rhobar_nbq_avg1(i,j)= rhobar_nbq(i,j,1) 
        enddo
        enddo

!.......Some remaining initializations:
        rhssum_nbq_a(:)     = 0.d0
        div_nbq_a   (:,:)   = 0.d0
        qdm_v_ext   (:,:,:) = 0.d0  ! TO BE FINISHED

       endif 


      return
      end subroutine initial_nh
#else
      subroutine initial_nh_empty
      return
      end
#endif
