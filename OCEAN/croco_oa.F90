#include "cppdefs.h"
#ifdef ONLINE_ANALYSIS
!======================================================================
! Croco interface for Online Analysis (ONLINE_ANA)
!======================================================================
!
      subroutine croco_oa (icall)

!#ifdef NBQ
!      use module_nh 
!      use module_nbq
!#endif
      use module_interface_oa

      implicit none
# include "param_F90.h"
# include "scalars_F90.h"
# include "ocean2d.h"
# include "ocean3d.h"
# include "grid.h"
# include "nbq.h"

# include "def_bounds.h"

      integer :: icall
      integer :: i,j,k    

      character (len=200) :: dum1_c,dum2_c

      integer ::                       &
        maskr_c (GLOBAL_2D_ARRAY,1:N)  &
       ,masku_c (GLOBAL_2D_ARRAY,1:N)  &
       ,maskv_c (GLOBAL_2D_ARRAY,1:N)  &
       ,maskf_c (GLOBAL_2D_ARRAY,1:N)   

      real ::                          &
        hu_c(GLOBAL_2D_ARRAY)          &
       ,hv_c(GLOBAL_2D_ARRAY)          

# ifndef SPHERICAL
      real ::                          &
        xu_c (GLOBAL_2D_ARRAY)         &
       ,yu_c (GLOBAL_2D_ARRAY)         &
       ,xv_c (GLOBAL_2D_ARRAY)         &
       ,yv_c (GLOBAL_2D_ARRAY)     
# endif

      real :: u_test (GLOBAL_2D_ARRAY,1:N)

      integer  ::                      &
         istr_oa                       &
        ,jstr_oa                       &
        ,iend_oa                       &
        ,jend_oa                                                       

       istr_oa  = 1
       iend_oa  = LOCALLM
       jstr_oa  = 1
       jend_oa  = LOCALMM 


      if (icall==0) then
!**********************************************************************
! Initializations
!**********************************************************************

       dum1_c="NAMELIST_OANALYSIS"
       dum2_c="OUT_OANALYSIS"

# ifndef MASKING
       maskr_c = 1.
       masku_c = 1.
       maskv_c = 1.
       maskf_c = 1.
# else
       do i=istr_oa,iend_oa
       do j=jstr_oa,jend_oa
          maskr_c(i,j,1:N)  =maskr(i,j)
          maskf_c(i,j,1:N)  =maskr(i,j)
       enddo
       enddo

       do i=istr_oa,iend_oa
       do j=jstr_oa,jend_oa
          masku_c(i,j,1:N)=masku(i,j)
       enddo
       enddo

       do i=istr_oa,iend_oa
       do j=jstr_oa,jend_oa
          maskv_c(i,j,1:N)=maskv(i,j)
       enddo
       enddo
# endif

       do i=istr_oa,iend_oa
       do j=jstr_oa,jend_oa
          hu_c(i,j)=0.5*(h(i,j)-h(i-1,j))
       enddo
       enddo

       do i=istr_oa,iend_oa
       do j=jstr_oa,jend_oa
          hv_c(i,j)=0.5*(h(i,j)-h(i,j-1))
       enddo
       enddo

# ifndef SPHERICAL
       do i=istr_oa,iend_oa
       do j=jstr_oa,jend_oa
          xu_c(i,j)=0.5*(xr(i,j)-xr(i-1,j))
          yu_c(i,j)=0.5*(yr(i,j)-yr(i-1,j))
       enddo
       enddo

       do i=istr_oa,iend_oa
       do j=jstr_oa,jend_oa
          xv_c(i,j)=0.5*(xr(i,j)-xr(i,j-1))
          yv_c(i,j)=0.5*(yr(i,j)-yr(i,j-1))
       enddo
       enddo
# endif

    !    pointer_approach=.false.  & 

       call init_oa(                                 & 

       directory_in_oa =dum1_c                       &
      ,directory_out_oa=dum2_c                       &
      ,io_unit_oa=67                                 &
      ,iic_oa=iic                                    &
      ,kount0=ntstart                                &
      ,nt_max=ntimes                                 &   
      ,dti=dt                                        &

      ,imin=istr_oa, imax=iend_oa                    &
      ,jmin=jstr_oa, jmax=jend_oa                    &
      ,kmin=1,        kmax=N                         & 

# ifdef SPHERICAL
      ,lon_t=lonr(istr_oa:iend_oa,jstr_oa:jend_oa)   & 
# else
      ,lon_t=xr(istr_oa:iend_oa,jstr_oa:jend_oa)     & 
# endif
      ,lon_t_lbound=(/istr_oa,jstr_oa/)              &
      ,lon_t_ubound=(/iend_oa,jend_oa/)              &
# ifdef SPHERICAL
      ,lat_t=latr(istr_oa:iend_oa,jstr_oa:jend_oa)   & 
# else
      ,lat_t=yr(istr_oa:iend_oa,jstr_oa:jend_oa)     & 
# endif
      ,lat_t_lbound=(/istr_oa,jstr_oa/)              &
      ,lat_t_ubound=(/iend_oa,jend_oa/)              &
# ifdef SPHERICAL
      ,lon_u=lonu(istr_oa:iend_oa,jstr_oa:jend_oa) & 
# else
      ,lon_u=xu_c(istr_oa:iend_oa,jstr_oa:jend_oa) & 
# endif
      ,lon_u_lbound=(/istr_oa,jstr_oa/)              &
      ,lon_u_ubound=(/iend_oa,jend_oa/)              &
# ifdef SPHERICAL
      ,lat_u=latu(istr_oa:iend_oa,jstr_oa:jend_oa)   & 
# else
      ,lat_u=yu_c(istr_oa:iend_oa,jstr_oa:jend_oa)   & 
# endif
      ,lat_u_lbound=(/istr_oa,jstr_oa/)              &
      ,lat_u_ubound=(/iend_oa,jend_oa/)              &
# ifdef SPHERICAL
      ,lon_v=lonv(istr_oa:iend_oa,jstr_oa:jend_oa)   & 
# else
      ,lon_v=xv_c(istr_oa:iend_oa,jstr_oa:jend_oa)   & 
# endif
      ,lon_v_lbound=(/istr_oa,jstr_oa/)              &
      ,lon_v_ubound=(/iend_oa,jend_oa/)              &
# ifdef SPHERICAL
      ,lat_v=latv(istr_oa:iend_oa,jstr_oa:jend_oa)   &
# else
      ,lat_v=yv_c(istr_oa:iend_oa,jstr_oa:jend_oa)   &
# endif
      ,lat_v_lbound=(/istr_oa,jstr_oa/)              &
      ,lat_v_ubound=(/iend_oa,jend_oa/)              &
# ifdef SPHERICAL
      ,lon_f=lonr(istr_oa:iend_oa,jstr_oa:jend_oa)   &   ! Wrong grid !
# else
      ,lon_f=xr(istr_oa:iend_oa,jstr_oa:jend_oa)     &   ! Wrong grid !
# endif
      ,lon_f_lbound=(/istr_oa,jstr_oa/)              &
      ,lon_f_ubound=(/iend_oa,jend_oa/)              &
# ifdef SPHERICAL
      ,lat_f=latr(istr_oa:iend_oa,jstr_oa:jend_oa)   &   ! Wrong grid !
# else
      ,lat_f=yr(istr_oa:iend_oa,jstr_oa:jend_oa)     &   ! Wrong grid !
# endif
      ,lat_f_lbound=(/istr_oa,jstr_oa/)              &
      ,lat_f_ubound=(/iend_oa,jend_oa/)              &

      ,mask_t=maskr_c(istr_oa:iend_oa,jstr_oa:jend_oa,1:N)   &  
      ,mask_t_lbound=(/istr_oa,jstr_oa,1/)                   &
      ,mask_t_ubound=(/iend_oa,jend_oa,N/)                   &
      ,mask_f=maskf_c(istr_oa:iend_oa,jstr_oa:jend_oa,1:N)   & ! Wrong grid !
      ,mask_f_lbound=(/istr_oa,jstr_oa,1/)                   &
      ,mask_f_ubound=(/iend_oa,jend_oa,N+1/)                 &
      ,mask_u=masku_c(istr_oa:iend_oa,jstr_oa:jend_oa,1:N)   & 
      ,mask_u_lbound=(/istr_oa,jstr_oa,1/)                   &
      ,mask_u_ubound=(/iend_oa,jend_oa,N/)                   &
      ,mask_v=maskv_c(istr_oa:iend_oa,jstr_oa:jend_oa,1:N)   &
      ,mask_v_lbound=(/istr_oa,jstr_oa,1/)                   &
      ,mask_v_ubound=(/iend_oa,jend_oa,N/)                   &

      ,h_w=h(istr_oa:iend_oa,jstr_oa:jend_oa)                & 
      ,h_w_lbound=(/istr_oa,jstr_oa/)                        &
      ,h_w_ubound=(/iend_oa,jend_oa/)                        &
      ,h_u=hu_c(istr_oa:iend_oa,jstr_oa:jend_oa)             & 
      ,h_u_lbound=(/istr_oa,jstr_oa/)                        &
      ,h_u_ubound=(/iend_oa,jend_oa/)                        &
      ,h_v=hv_c(istr_oa:jstr_oa,jstr_oa:jend_oa)             & 
      ,h_v_lbound=(/istr_oa,jstr_oa/)                        &
      ,h_v_ubound=(/iend_oa,jend_oa/)                        &
      ,h_f=h(istr_oa:iend_oa,jstr_oa:jend_oa)                & ! Wrong grid !
      ,h_f_lbound=(/istr_oa,jstr_oa/)                        &
      ,h_f_ubound=(/iend_oa,jend_oa/)                        &

      ,rhp_t=rho(istr_oa:iend_oa,jstr_oa:jend_oa,1:N)        &
      ,rhp_t_lbound=(/istr_oa,jstr_oa,1/)                    &
      ,rhp_t_ubound=(/iend_oa,jend_oa,N/)                    &

      ,depth_t=z_r(istr_oa:iend_oa,jstr_oa:jend_oa,1:N)      & 
      ,depth_t_lbound=(/istr_oa,jstr_oa,1/)                  &
      ,depth_t_ubound=(/iend_oa,jend_oa,N/)              )

!#endif      

!     pointer_approach=.true.  

      call main_oa(                                                    &
                      ichoix=0                                         &
                     ,ivar_m=1                                         &
                     ,iic_oa=iic                                       &
                     ,dti=dt                                           &
                     ,nt_max=ntimes                                    &
                     ,imin=istr_oa, imax=iend_oa                       &
                     ,jmin=jstr_oa, jmax=jend_oa                       &
                     ,kmin=1,        kmax=N                            &

                     ,rhp_t=rho(istr_oa:iend_oa,jstr_oa:jend_oa,1:N)   &
                     ,rhp_t_lbound=(/istr_oa,jstr_oa,1/)               &
                     ,rhp_t_ubound=(/iend_oa,jend_oa,N/)               &

                     ,depth_t=z_r(istr_oa:iend_oa,jstr_oa:jend_oa,1:N) &
                     ,depth_t_lbound=(/istr_oa,jstr_oa,1/)             &
                     ,depth_t_ubound=(/iend_oa,jend_oa,N/)      )


      elseif (icall==1) then
!**********************************************************************
! At every (internal-mode) time step
!**********************************************************************

!      u_test=23*cos(2.*3.14159/500.*dt*real(iic)) !!-cos(2*3.14159/50.*dt*real(iic))
!      if (mynode.eq.0) write (6,*) iic,dt*real(iic),23*cos(2.*3.14159/500.*dt*real(iic)) &
!          ,2.*3.14159/500.*dt*real(iic)
      do i=istr_oa,iend_oa
      do j=jstr_oa,jend_oa
      do k=1,N
        !u_test(i,j,k)=u(i,j,k,nstp)-ubar(i,j,fast_indx_out)
         u_test(i,j,k)=real(i)
      enddo
      enddo
      enddo

      call main_oa(                                                    &
                      ichoix=0                                         &
                     ,ivar_m=1                                         &
                     ,iic_oa=iic                                       &
                     ,dti=dt                                           &
                     ,nt_max=ntimes                                    &
                     ,imin=istr_oa, imax=iend_oa                       &
                     ,jmin=jstr_oa, jmax=jend_oa                       &
                     ,kmin=1,        kmax=N                            &

                     ,rhp_t=rho(istr_oa:iend_oa,jstr_oa:jend_oa,1:N)   &
                     ,rhp_t_lbound=(/istr_oa,jstr_oa,1/)               &
                     ,rhp_t_ubound=(/iend_oa,jend_oa,N/)               &

                     ,depth_t=z_r(istr_oa:iend_oa,jstr_oa:jend_oa,1:N) &
                     ,depth_t_lbound=(/istr_oa,jstr_oa,1/)             &
                     ,depth_t_ubound=(/iend_oa,jend_oa,N/)      )

      endif

      end subroutine croco_oa
#else
      subroutine croco_oa_empty
      end subroutine croco_oa_empty
#endif
