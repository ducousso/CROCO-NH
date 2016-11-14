!----------------------------------------------------------------------
! Implicit treatment of vertical advection (if needed) during predictor 
! step. Vertical advection is handled via a first order upwind scheme
!
! With few additions to the scheme, vertical diffusion can also be added
! to this implicit greatment at the predictor step (VMIX_PREDICTOR key). 
! Historically, it is only done at the corrector step.
!----------------------------------------------------------------------
# define VMIX_PREDICTOR

#if defined TRIDIAG_TRA
          do i=istr,iend
#  ifdef VMIX_PREDICTOR
            FC(i,1)=cdt*Akt(i,j,1,iAkt)/( z_r(i,j,2)-z_r(i,j,1) )         
#  else
            FC(i,1)= 0.
#  endif
            WC(i,1)=DC(i,0)*Wi(i,j,1)
            cff=1./(Hz_half(i,j,1) +FC(i,1)+max(WC(i,1),0.))    !<- 1/b(1)
            CF(i,1)=cff*(           FC(i,1)-min(WC(i,1),0.))    !<- q(1) = c(1) / b(1)
            DC(i,1)=cff*t(i,j,1,nnew,itrc)                      !<- f(1) = f(1) / b(1)
          enddo

          do k=2,N-1,+1
            do i=istr,iend
#  ifdef VMIX_PREDICTOR
              FC(i,k)=cdt*Akt(i,j,k,iAkt)/( z_r(i,j,k+1)-z_r(i,j,k) )            
#  else
              FC(i,k)= 0.
#  endif
              WC(i,k)=DC(i,0)*Wi(i,j,k)
              cff=1./( Hz_half(i,j,k) +FC(i,k)+max(WC(i,k),0.)
     &                              +FC(i,k-1)-min(WC(i,k-1),0.)   
     &                   -CF(i,k-1)*(FC(i,k-1)+max(WC(i,k-1),0.))
     &                                                          )   !<- p = 1/(b(j)-a(j)*q(j-1))
              CF(i,k)=cff*(FC(i,k)-min(WC(i,k),0.))                 !<- c(j)*p

              DC(i,k)=cff*( t(i,j,k,nnew,itrc) +DC(i,k-1)*(         !<- f(j) = ( f(j) - a(j)*f(j-1) )*p 
     &                          FC(i,k-1)+max(WC(i,k-1),0.) ))      !<- DC(j) = cff*( DC(j-1)*a(j) )
            enddo
          enddo  !--> discard DC(:,0)

          do i=istr,iend
            t(i,j,N,nnew,itrc)=( t(i,j,N,nnew,itrc) +DC(i,N-1)*(       
     &                                FC(i,N-1)+max(WC(i,N-1),0.) )
     &               )/( Hz_half(i,j,N) +FC(i,N-1)-min(WC(i,N-1),0.)
     &                      -CF(i,N-1)*(FC(i,N-1)+max(WC(i,N-1),0.))
     &                                                            )
          enddo

          do k=N-1,1,-1
            do i=istr,iend
              t(i,j,k,nnew,itrc)=DC(i,k)+CF(i,k)*t(i,j,k+1,nnew,itrc)   
            enddo
          enddo
#elif defined TRIDIAG_U
         do i=istrU,iend
#  ifdef VMIX_PREDICTOR
            FC(i,1)=cdt*(Akv(i,j,1)+Akv(i-1,j,1))
     &                 /( z_r(i,j,2)+z_r(i-1,j,2)
     &                   -z_r(i,j,1)-z_r(i-1,j,1))         
#  else
            FC(i,1)= 0.
#  endif
            WC(i,1)=cdt*DC(i,0)*0.5*(Wi(i,j,1)+Wi(i-1,j,1))
            cff=1./(0.5*(Hz_half(i,j,1)+Hz_half(i-1,j,1)) 
     &                             +FC(i,1)+max(WC(i,1),0.))    !<- 1/b(1)
            CF(i,1)=cff*(           FC(i,1)-min(WC(i,1),0.))    !<- q(1) = c(1) / b(1)
            DC(i,1)=cff*u(i,j,1,nnew)                           !<- f(1) = f(1) / b(1)
          enddo

          do k=2,N-1,+1
            do i=istrU,iend
#  ifdef VMIX_PREDICTOR
              FC(i,k)=cdt*(Akv(i,j,k)+Akv(i-1,j,k))
     &                 /( z_r(i,j,k+1)+z_r(i-1,j,k+1)
     &                   -z_r(i,j,k  )-z_r(i-1,j,k  ))                          
#  else
              FC(i,k)= 0.
#  endif
              WC(i,k)=cdt*DC(i,0)*0.5*(Wi(i,j,k)+Wi(i-1,j,k))
              cff=1./( 0.5*(Hz_half(i,j,k)+Hz_half(i-1,j,k)) 
     &                                +FC(i,k)+max(WC(i,k),0.)
     &                              +FC(i,k-1)-min(WC(i,k-1),0.)   
     &                   -CF(i,k-1)*(FC(i,k-1)+max(WC(i,k-1),0.))
     &                                                          )   !<- p = 1/(b(j)-a(j)*q(j-1))
              CF(i,k)=cff*(FC(i,k)-min(WC(i,k),0.))                 !<- c(j)*p

              DC(i,k)=cff*( u(i,j,k,nnew) +DC(i,k-1)*(              !<- f(j) = ( f(j) - a(j)*f(j-1) )*p 
     &                          FC(i,k-1)+max(WC(i,k-1),0.) ))      !<- DC(j) = cff*( DC(j-1)*a(j) )
            enddo
          enddo  !--> discard DC(:,0)

          do i=istrU,iend
            u(i,j,N,nnew)=( u(i,j,N,nnew) +DC(i,N-1)*(         
     &                                FC(i,N-1)+max(WC(i,N-1),0.) )
     &               )/( 0.5*(Hz_half(i,j,N)+Hz_half(i-1,j,N)) 
     &                                 +FC(i,N-1)-min(WC(i,N-1),0.)
     &                      -CF(i,N-1)*(FC(i,N-1)+max(WC(i,N-1),0.))
     &                                                            )
          enddo

          do k=N-1,1,-1
            do i=istrU,iend
              u(i,j,k,nnew)=DC(i,k)+CF(i,k)*u(i,j,k+1,nnew)   
            enddo
          enddo        
#elif defined TRIDIAG_V
         do i=istr,iend
#  ifdef VMIX_PREDICTOR
            FC(i,1)=cdt*(Akv(i,j,1)+Akv(i,j-1,1))
     &                 /( z_r(i,j,2)+z_r(i,j-1,2)
     &                   -z_r(i,j,1)-z_r(i,j-1,1))         
#  else
            FC(i,1)= 0.
#  endif
            WC(i,1)=cdt*DC(i,0)*0.5*(Wi(i,j,1)+Wi(i,j-1,1))
            cff=1./(0.5*(Hz_half(i,j,1)+Hz_half(i,j-1,1)) 
     &                             +FC(i,1)+max(WC(i,1),0.))    !<- 1/b(1)
            CF(i,1)=cff*(           FC(i,1)-min(WC(i,1),0.))    !<- q(1) = c(1) / b(1)
            DC(i,1)=cff*v(i,j,1,nnew)                      !<- f(1) = f(1) / b(1)
          enddo

          do k=2,N-1,+1
            do i=istr,iend
#  ifdef VMIX_PREDICTOR
              FC(i,k)=cdt*(Akv(i,j,k)+Akv(i,j-1,k))
     &                 /( z_r(i,j,k+1)+z_r(i,j-1,k+1)
     &                   -z_r(i,j,k  )-z_r(i,j-1,k  ))                          
#  else
              FC(i,k)= 0.
#  endif
              WC(i,k)=cdt*DC(i,0)*0.5*(Wi(i,j,k)+Wi(i,j-1,k))
              cff=1./( 0.5*(Hz_half(i,j,k)+Hz_half(i,j-1,k)) 
     &                                +FC(i,k)+max(WC(i,k),0.)
     &                              +FC(i,k-1)-min(WC(i,k-1),0.)   
     &                   -CF(i,k-1)*(FC(i,k-1)+max(WC(i,k-1),0.))
     &                                                          )   !<- p = 1/(b(j)-a(j)*q(j-1))
              CF(i,k)=cff*(FC(i,k)-min(WC(i,k),0.))                 !<- c(j)*p
              DC(i,k)=cff*( v(i,j,k,nnew) +DC(i,k-1)*(              !<- f(j) = ( f(j) - a(j)*f(j-1) )*p 
     &                          FC(i,k-1)+max(WC(i,k-1),0.) ))      !<- DC(j) = cff*( DC(j-1)*a(j) )
            enddo
          enddo  !--> discard DC(:,0)

          do i=istr,iend
            v(i,j,N,nnew)=( v(i,j,N,nnew) +DC(i,N-1)*(        
     &                                FC(i,N-1)+max(WC(i,N-1),0.) )
     &               )/( 0.5*(Hz_half(i,j,N)+Hz_half(i,j-1,N)) 
     &                                 +FC(i,N-1)-min(WC(i,N-1),0.)
     &                      -CF(i,N-1)*(FC(i,N-1)+max(WC(i,N-1),0.))
     &                                                            )
          enddo

          do k=N-1,1,-1
            do i=istr,iend
              v(i,j,k,nnew)=DC(i,k)+CF(i,k)*v(i,j,k+1,nnew)   
            enddo
          enddo        
#endif
