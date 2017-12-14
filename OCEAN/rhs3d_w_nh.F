#include "cppdefs.h"
#if defined SOLVE3D && (defined NBQ || defined NHMG)

      subroutine rhs3d_w_nh(tile)
!====================================================================
! version 1  (13/01/2015)
!
! work to do : define cosa, sina, eomn in setup_grid.F
! global variables : wz
!
! For wz it is assumed that the control volume is (Hz(k+1)+Hz(k))/2
! and Hz(N)/2 for the top grid box
!
! rw is stored in A3d(1,4,trd) which must be reused in pre_step3d 
! and step3d_w1 
!
!====================================================================
      implicit none
      integer tile, trd, omp_get_thread_num
# include "param.h"
# include "private_scratch.h"
# include "compute_tile_bounds.h"
      trd=omp_get_thread_num()
      call rhs3d_w_tile (Istr,Iend,Jstr,Jend,  A3d(1,5,trd),
     &             A2d(1,1,trd), A2d(1,2,trd), A2d(1,3,trd),
# ifdef NHMG
     &      A2d(1,5,trd), A2d(1,6,trd), A2d(1,7,trd),A2d(1,8,trd),
# endif
     &             A2d(1,1,trd), A2d(1,2,trd), A2d(1,3,trd), 
     &             A2d(1,4,trd)
     &                  )
      return
      end
!====================================================================           
      subroutine rhs3d_w_tile (istr,iend,jstr,jend, rw,
     &                                        CF,FC,DC,
# ifdef NHMG
     &                   div_zx,div_zy,Udiv_zx,Vdiv_zy,
# endif
     &                              wrk1,wrk2, WFx,WFe)
   
      implicit none
      integer Istr,Iend,Jstr,Jend, i,j,k,kp
# ifdef MPI
     &                   ,imin,imax,jmin,jmax
# endif
# include "param.h"
      real rw(PRIVATE_2D_SCRATCH_ARRAY,0:N),
     &     CF(PRIVATE_1D_SCRATCH_ARRAY,0:N),  cff,cff1,cff2,
     &     FC(PRIVATE_1D_SCRATCH_ARRAY,0:N),  
     &     DC(PRIVATE_1D_SCRATCH_ARRAY,0:N),        
# ifdef NHMG
     &     div_zx(PRIVATE_1D_SCRATCH_ARRAY,1:N),  
     &     div_zy(PRIVATE_1D_SCRATCH_ARRAY,1:N),  
     &     Udiv_zx(PRIVATE_1D_SCRATCH_ARRAY,1:N),  
     &     Vdiv_zy(PRIVATE_1D_SCRATCH_ARRAY,1:N),  

# endif
     &   wrk1(PRIVATE_2D_SCRATCH_ARRAY), Huon_w,
     &   wrk2(PRIVATE_2D_SCRATCH_ARRAY), Hvom_w,
     &    WFx(PRIVATE_2D_SCRATCH_ARRAY), Omeg_r,
     &    WFe(PRIVATE_2D_SCRATCH_ARRAY)
       real gamma, epsil
      parameter (gamma=0.25)
      parameter (epsil=1.E-16)
# include "grid.h"
# include "ocean3d.h"
# include "coupling.h"
# include "forces.h"
# include "scalars.h"  
# include "nbq.h"
!
# include "compute_auxiliary_bounds.h"
!


!====================================================================      
!  Horizontal advection
!==================================================================== 
# define W_HADV
# if defined UV_ADV && defined W_HADV

#  define curv wrk1
!==
      do k=1,N                  !<-- outer loop          
!==
#  ifdef EW_PERIODIC
#   define I_EXT_RANGE Istr-1,Iend+2
#  else
#   ifdef MPI
          if (WEST_INTER) then
            imin=Istr-1
          else
            imin=max(Istr-1,1)
          endif
          if (EAST_INTER) then
            imax=Iend+2
          else
            imax=min(Iend+2,Lmmpi+1)
          endif
#    define I_EXT_RANGE imin,imax
#   else
#    define I_EXT_RANGE max(Istr-1,1),min(Iend+2,Lm+1)
#   endif
#  endif
#  ifdef NS_PERIODIC
#   define J_EXT_RANGE Jstr-1,Jend+2
#  else
#   ifdef MPI
          if (SOUTH_INTER) then
            jmin=Jstr-1
          else
            jmin=max(Jstr-1,1)
          endif
          if (NORTH_INTER) then
            jmax=Jend+2
          else
            jmax=min(Jend+2,Mmmpi+1)
          endif
#    define J_EXT_RANGE jmin,jmax
#   else
#    define J_EXT_RANGE max(Jstr-1,1),min(Jend+2,Mm+1)
#   endif
#  endif

!WFx
          do j=Jstr,Jend
            do i=I_EXT_RANGE
              WFx(i,j)=(Wz(i,j,k,nrhs)-Wz(i-1,j,k,nrhs))
#  ifdef MASKING
     &                                             *umask(i,j)
#  endif
            enddo
          enddo 

#  undef I_EXT_RANGE
#  ifndef EW_PERIODIC
          if (WESTERN_EDGE) then
            do j=Jstr,Jend
              WFx(0,j)=WFx(1,j)
            enddo
          endif
          if (EASTERN_EDGE) then
#   ifdef MPI
            do j=Jstr,Jend
              WFx(Lmmpi+2,j)=WFx(Lmmpi+1,j)
            enddo
#   else
             do j=Jstr,Jend
              WFx(Lm+2,j)=WFx(Lm+1,j)
            enddo
#   endif
          endif
#  endif
          
          do j=Jstr,Jend
            do i=Istr-1,Iend+1          
              curv(i,j)=WFx(i+1,j)-WFx(i,j)
            enddo
          enddo  

          kp   = min(k+1,N)    !<-- extrapolation at k=N    
          cff1 = 0.5          
          if(k.eq.N) cff1=0.25 !<-- at the surface the thickness of 
                               !<-- the control volume is Hz/2.           
          do j=Jstr,Jend
            do i=Istr,Iend+1
              HUon_w = cff1*(Huon(i,j,k)+Huon(i,j,kp))         
              if (Huon_w .gt. 0.) then
                cff=curv(i-1,j)
              else
                cff=curv(i,j)
              endif
              WFx(i,j)=0.5*( Wz(i,j,k,nrhs)+Wz(i-1,j,k,nrhs)
     &                           -0.333333333333*cff )*Huon_w
            enddo         
          enddo

!WFe
          do j=J_EXT_RANGE
            do i=Istr,Iend
              WFe(i,j)=(Wz(i,j,k,nrhs)-Wz(i,j-1,k,nrhs)) 
#  ifdef MASKING
     &                                         *vmask(i,j)
#  endif
            enddo
          enddo

#  undef J_EXT_RANGE
#  ifndef NS_PERIODIC
          if (SOUTHERN_EDGE) then
            do i=Istr,Iend
              WFe(i,0)=WFe(i,1)
            enddo
          endif
          if (NORTHERN_EDGE) then
#   ifdef MPI
            do i=Istr,Iend
              WFe(i,Mmmpi+2)=WFe(i,Mmmpi+1)
            enddo
#   else
            do i=Istr,Iend
              WFe(i,Mm+2)=WFe(i,Mm+1)
            enddo
#   endif
          endif
#  endif

          do j=Jstr-1,Jend+1
            do i=Istr,Iend
              curv(i,j)=WFe(i,j+1)-WFe(i,j)
            enddo
          enddo  

          kp   = min(k+1,N)    !<-- extrapolation at k=N    
          cff1 = 0.5          
          if(k.eq.N) cff1=0.25 !<-- at the surface the thickness of 
                               !<-- the control volume is Hz/2. 
          do j=Jstr,Jend+1
            do i=Istr,Iend
              HVom_w = cff1*(Hvom(i,j,k)+Hvom(i,j,kp))  !<-- Hvom on w-levels
              if (Hvom_w .gt. 0.) then
                cff=curv(i,j-1)
              else
                cff=curv(i,j)
              endif
              WFe(i,j)=0.5*( Wz(i,j,k,nrhs)+Wz(i,j-1,k,nrhs)
     &                          -0.333333333333*cff )*Hvom_w
            enddo
          enddo 
                    
          do j=Jstr,Jend
            do i=Istr,Iend
             rw(i,j,k)=rw(i,j,k)-WFx(i+1,j)+WFx(i,j)-WFe(i,j+1)+WFe(i,j)    
            enddo
          enddo
!================
!--> Comment: the computation of HVom_w and HUon_w assumes that rw must 
!--  be divided by 0.5*(Hz(i,j,k)+Hz(i,j,k+1)) in pre_step3d and step3d_w2 
!================
      enddo                    !<-- outer loop k
# endif /* W_HADV */

!====================================================================     
!== Vertical advection
!==================================================================== 
# define W_VADV
# if defined UV_ADV && defined W_VADV
       do j=Jstr,Jend !<-- outer loop 
!==
!====================================================================      
!  Vertical advection  [4th order compact scheme]
! Omega is interpolated using a fourth-order scheme 
!    W(k) = (-W(k-3/2)+9 W(k-1/2)+9 W(k+1/2)-W(k+3/2))/16
!====================================================================
#  define NEUMANN
#  ifdef UV_VADV_SPLINES
        do k=2,N-1
          do i=Istr,Iend
            kp   = min(k+1,N-1)
            DC(i,k)=0.5625*(HZR(i,j,k   )+HZR(i,j,k+1))
     &             -0.0625*(HZR(i,j,kp+1)+HZR(i,j,k-1))
          enddo
        enddo
        do i=Istr,Iend
          DC(i,1)=0.5*(HZR(i,j,1)+HZR(i,j,2))
          DC(i,N)=0.5*HZR(i,j,N)
#   if defined NEUMANN
          FC(i,0)=1.5*Wz(i,j,1,nrhs)
          CF(i,1)=0.5
#   elif defined LINEAR_CONTINUATION
          FC(i,0)=2.0*Wz(i,j,1,nrhs)
          CF(i,1)=1.
#   endif
        enddo
        do k=1,N-1,+1    ! <-- irreversible
          do i=Istr,Iend
            cff=1./(2.*DC(i,k)+DC(i,k+1)*(2.-CF(i,k)))
            CF(i,k+1)=cff*DC(i,k)
            FC(i,k)=cff*( 3.*( DC(i,k  )*Wz(i,j,k+1,nrhs)
     &                        +DC(i,k+1)*Wz(i,j,k  ,nrhs))
     &                              -DC(i,k+1)*FC(i,k-1))
            enddo
        enddo               !--> discard DC
        
        do i=Istr,Iend
#   if defined NEUMANN
          FC(i,N)=(3.*Wz(i,j,N,nrhs)-FC(i,N-1))/(2.-CF(i,N))
#   elif defined LINEAR_CONTINUATION
          FC(i,N)=(2.*Wz(i,j,N,nrhs)-FC(i,N-1))/(1.-CF(i,N))
#   endif
        enddo
               
        do k=N-1,0,-1       ! <-- irreversible
          do i=Istr,Iend
            FC(i,k)=FC(i,k)-CF(i,k+1)*FC(i,k+1)
          enddo
        enddo               !--> discard CF

        do k=1,N-1
          do i=Istr,Iend
            kp   = min(k+1,N-1)    !<-- extrapolation at k=N
            Omeg_r = 0.5625*(We(i  ,j,k)+We(i,j,k+1))
     &              -0.0625*(We(i,j,kp+1)+We(i,j,k-1))
            FC(i,k)=FC(i,k)*Omeg_r
          enddo
        enddo
        
        do i=Istr,Iend
          FC(i,N)=0.
          FC(i,0)=0.
        enddo
#  else
        do k=1,N-1
          do i=Istr,Iend
           Omeg_r  = 0.5*(We(i,j,k     ) + We(i,j,k+1     ))
           FC(i,k) = 0.5*(wz(i,j,k,nrhs) + wz(i,j,k+1,nrhs))*Omeg_r    
          enddo
        enddo

        do i=Istr,Iend
          Omeg_r  = 0.5*(We(i,j,0     ) + We(i,j,1     ))
          FC(i,0) = 0.5*(wz(i,j,0,nrhs) + wz(i,j,1,nrhs))*Omeg_r

          FC(i,N)=0.    
        enddo
#  endif
!
           do k=1,N
            do i=Istr,Iend
              rw(i,j,k)=rw(i,j,k)-FC(i,k)+FC(i,k-1)
#  ifdef MASKING              
              rw(i,j,k)=rw(i,j,k)*rmask(i,j)
#  endif              
            enddo
          enddo
!==
      enddo     !<-- outer loop j
!==
# endif /* UV_ADV && W_VADV */

# if defined NHMG && defined UV_ADV
!====================================================================      
!  Pseudo forces for the equation of vertical Flux (Molemaker et al., 2018)
!==================================================================== 

      do j=jstr,jend
        do k=1,N
          do i=Istr,Iend
            div_zx(i,k) = 0.5*Huon(i+1,j,k)*(dzdxi(i,j,k)+dzdxi(i+1,j,k)) 
     &                   -0.5*Huon(i  ,j,k)*(dzdxi(i,j,k)+dzdxi(i-1,j,k)) 
     &                   +0.5*Hvom(i,j+1,k)*(dzdxi(i,j,k)+dzdxi(i,j+1,k)) 
     &                   -0.5*Hvom(i,j  ,k)*(dzdxi(i,j,k)+dzdxi(i,j-1,k)) 
          div_zy(i,k) = 0.5*Huon(i+1,j,k)*(dzdeta(i,j,k)+dzdeta(i+1,j,k)) 
     &                 -0.5*Huon(i  ,j,k)*(dzdeta(i,j,k)+dzdeta(i-1,j,k)) 
     &                 +0.5*Hvom(i,j+1,k)*(dzdeta(i,j,k)+dzdeta(i,j+1,k)) 
     &                 -0.5*Hvom(i,j  ,k)*(dzdeta(i,j,k)+dzdeta(i,j-1,k)) 
          enddo
        enddo
        k=1
          do i=Istr,Iend
            div_zx(i,k) = div_zx(i,k)
     &                   +0.5*  We(i,j,k  )*(dzdxi(i,j,k)+dzdxi(i,j,k+1)) 
            div_zy(i,k) = div_zy(i,k)
     &                   +0.5*  We(i,j,k  )*(dzdeta(i,j,k)+dzdeta(i,j,k+1)) 
          enddo
        do k=2,N-1
          do i=Istr,Iend
            div_zx(i,k) = div_zx(i,k)
     &                 +0.5*  We(i,j,k  )*(dzdxi(i,j,k)+dzdxi(i,j,k+1)) 
     &                 -0.5*  We(i,j,k-1)*(dzdxi(i,j,k)+dzdxi(i,j,k-1)) 
            div_zy(i,k) = div_zy(i,k)
     &               +0.5*  We(i,j,k  )*(dzdeta(i,j,k)+dzdeta(i,j,k+1)) 
     &               -0.5*  We(i,j,k-1)*(dzdeta(i,j,k)+dzdeta(i,j,k-1)) 
          enddo
        enddo
        k=N
          do i=Istr,Iend
            div_zx(i,k) = div_zx(i,k) 
     &                 -0.5*  We(i,j,k-1)*(dzdxi(i,j,k)+dzdxi(i,j,k-1))
            div_zy(i,k) = div_zy(i,k) 
     &                 -0.5*  We(i,j,k-1)*(dzdeta(i,j,k)+dzdeta(i,j,k-1)) 
          enddo

        do k=1,N
          do i=Istr,Iend
            Udiv_zx(i,k) = 0.5*(u(i,j,k,nrhs) + u(i+1,j,k,nrhs))*div_zx(i,k) 
            Vdiv_zy(i,k) = 0.5*(v(i,j,k,nrhs) + v(i,j+1,k,nrhs))*div_zy(i,k) 
          enddo
        enddo

        do k=1,N-1
          do i=Istr,Iend
            rw(i,j,k) = rw(i,j,k) 
     &              - 0.5*(Udiv_zx(i,k) + Udiv_zx(i,k+1))
     &              - 0.5*(Vdiv_zy(i,k) + Vdiv_zy(i,k+1))
          enddo
        enddo
        k=N
          do i=Istr,Iend
            rw(i,j,k) = rw(i,j,k) - Udiv_zx(i,k) - Vdiv_zy(i,k) 
          enddo

      enddo

# endif

      return
      end  
#else
      subroutine rhs3d_w_empty
      end
#endif /* SOLVE3D && NBQ || NHMG */       
