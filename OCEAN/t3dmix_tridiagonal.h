! $Id: t3dmix_tridiagonal.h 1458 2014-02-03 15:01:25Z gcambon $
!
!======================================================================
! CROCO is a branch of ROMS developped at IRD and INRIA, in France
! The two other branches from UCLA (Shchepetkin et al)
! and Rutgers University (Arango et al) are under MIT/X style license.
! CROCO specific routines (nesting) are under CeCILL-C license.
!
! CROCO website : http://www.croco-ocean.org
!======================================================================
!
! This block is inside a j,itrc loop
!
!      do j=jstr,jend
!        do itrc=1,NT
!
! Perform implicit (backward Euler) time step for vertical diffusion,
!
!   dq(k)     1     [         q(k+1)-q(k)             q(k)-q(k-1) ]
!  ------ = ----- * [ Akt(k)* ----------- - Akt(k-1)* ----------- ]
!    dt     Hz(k)   [            dz(k)                   dz(k-1)  ]
!
! where q(k) represents tracer field t(:,:,k,:,itrc). Doing so
! implies solution of a tri-diagonal system
!
!     -FC(k-1)*q_new(k-1) +[Hz(k)+FC(k-1)+FC(k)]*q_new(k)
!                       -FC(k)*q_new(k+1) = Hz(k)*q_old(k)
!
!                dt*Akt(k)
! where FC(k) = ----------- is normalized diffusivity coefficient 
!                  dz(k)
!
! defined at W-points; q_new(k) is the new-time-step (unknown) tracer
! field; q_old(k) is old-time-step tracer (known).  As long as
! vertical diffusivity Akt(k) is nonnegative, the tri-diagonal matrix
! is diagonally dominant which guarantees stability of a Gaussian
! elimination procedure, (e.g., Richtmeyer annd  Morton, 1967).
! Top and bottom boundary conditions are assumed to be no-flux,
! effectively Akt(N)=Akt(0)=0, hence FC(N)=FC(1)=0. This leads to
! equations for top and bottom grid boxes; 
!
!   -FC(N-1)*q_new(N-1) +[Hz(N)+FC(N-1)]*q_new(N) = Hz(N)*q_old(N)
!
!          [Hz(1)+FC(1)]*q_new(1) -FC(1)*q_new(2) = Hz(1)*q_old(1)
!
! The FC(N)=FC(0)=0 boundary conditions does not mean that physical
! boundary conditions are no flux: the forcing fluxes have been
! applied explicitly above.  Instead, the no-flux condition should
! be interpreted as that the implicit step merely redistributes the
! tracer concentration throughout the water column. At this moment
! the content of array t(:,:,:,nnew,itrc) has meaning of Hz*tracer.
! After the implicit step it becomes just tracer. 
!
! The tridiagonal system to solve is for k=1,N :
!      A(k)T(k-1)+B(k)T(k)+C(k)T(k+1)=D(k)
! Here:
!      A(k)=-FC(k-1)
!      B(k)=Hz(k)+FC(k-1)+FC(k)
!      C(k)=-FC(k)
!      D(k)=Hz(k)*Told(k)    ( -dt*(CD(k)-CD(k-1))       if TS_MIX_IMP )
!                               CD(k)=Akz(k)*(Told(k+1)-Told(k)/dz(k)
!      FC(k)=dt*Akt(k)/dz(k) ( +dt*Akz(k)/dz(k)          if TS_MIX_IMP )
!      FC(0)=0  FC(N)=0
! The solution is:
!    1- first pass: setting modified coefficients
!       C''(1)=C(1)/B(1)   C''(k)=C(k)/[B(k)-C''(k-1)*A(k)] for k=2,N-1
!       D''(1)=D(1)/B(1)   D''(k)=[D(k)-D''(k-1)*A(k)/[B(k)-C''(k-1)*A(k)]
!    2- second pass: back-substitution
!       T(N)=D''(N)
!       T(k)=D''(k)-C''(k)*T(k+1) for k=N-1,1,-1
!

# ifdef SALINITY
          indx=min(itrc,isalt)
# else
          indx=min(itrc,itemp)
# endif
# ifdef DIAGNOSTICS_TS
          do k=1,N
            do i=Istr,Iend
               TVmix(i,j,k,itrc)=t(i,j,k,nnew,itrc)
            enddo
          enddo
# endif /* DIAGNOSTICS_TS */

!++
!++ Explicit vertical Laplacian
!++
# ifdef TS_MIX_IMP
          do i=istr,iend
            do k=1,N-1
              CD(i,k) = Akz(i,j,k)*(
     &           t(i,j,k+1,nstp,itrc)-t(i,j,k,nstp,itrc)
     &           )  / ( z_r(i,j,k+1)-z_r(i,j,k) )
            enddo 
            CD(i,0) = 0.
            CD(i,N) = 0.
          enddo 
# endif 

!++
!++ Implicit Part
!++
!
! First pass: 
! Compute the modified tridiagonal matrix coefficients for 
! the implicit vertical diffusion terms at future time step, 
! located at horizontal RHO-points and vertical W-points.
!
          do i=istr,iend
# ifdef TS_MIX_IMP
            FC(i,1)=dt*(Akt(i,j,1,indx)+Akz(i,j,1))
# else
            FC(i,1)=dt* Akt(i,j,1,indx) 
# endif
     &                               /( z_r(i,j,2)-z_r(i,j,1) )
# ifdef VADV_ADAPT_IMP            
            BC(i,1)=DC(i,0)*Wi(i,j,1)   
            cff=1./(Hz(i,j,1)      +FC(i,1)+max(BC(i,1),0.))    !<- 1/b(1)
            CF(i,1)=cff*(           FC(i,1)-min(BC(i,1),0.))    !<- q(1) = c(1) / b(1)            
# else
            cff=1./(Hz(i,j,1)+FC(i,1))
            CF(i,1)= cff*FC(i,1)
# endif            

# ifdef TS_MIX_IMP 
            DC(i,1)= cff*(t(i,j,1,nnew,itrc)-dt*(CD(i,1)-CD(i,0)))
# else
            DC(i,1)= cff* t(i,j,1,nnew,itrc)
# endif
          enddo
          
          
          
          
          do k=2,N-1,+1
            do i=istr,iend
# ifdef TS_MIX_IMP
              FC(i,k)=dt*(Akt(i,j,k,indx)+Akz(i,j,k))
# else 
              FC(i,k)=dt* Akt(i,j,k,indx) 
# endif
     &                              /( z_r(i,j,k+1)-z_r(i,j,k) )
     
# ifdef VADV_ADAPT_IMP     
              BC(i,k)=DC(i,0)*Wi(i,j,k)
              cff=1./(      Hz(i,j,k) +FC(i,k)+max(BC(i,k),0.)
     &                              +FC(i,k-1)-min(BC(i,k-1),0.)   
     &                   -CF(i,k-1)*(FC(i,k-1)+max(BC(i,k-1),0.))
     &                                                          )   !<- p = 1/(b(j)-a(j)*q(j-1))
              CF(i,k)=cff*(FC(i,k)-min(BC(i,k),0.))                 !<- c(j)*p
              DC(i,k)=cff*( t(i,j,k,nnew,itrc) +DC(i,k-1)*(         !<- f(j) = ( f(j) - a(j)*f(j-1) )*p 
     &                          FC(i,k-1)+max(BC(i,k-1),0.) )       !<- DC(j) = cff*( DC(j-1)*a(j) )
#else           
              cff=1./(Hz(i,j,k)+FC(i,k)+FC(i,k-1)*(1.-CF(i,k-1)))
              CF(i,k)=cff*FC(i,k)
              DC(i,k)=cff*(t(i,j,k,nnew,itrc)+FC(i,k-1)*DC(i,k-1)
#endif
# ifdef TS_MIX_IMP
     &                                    -dt*(CD(i,k)-CD(i,k-1))
# endif
     &                                                          )
            enddo
          enddo       
!
! Second pass: back-substitution
!
          do i=istr,iend
# ifdef VADV_ADAPT_IMP 
            t(i,j,N,nnew,itrc)=( t(i,j,N,nnew,itrc) 
#  ifdef TS_MIX_IMP
     &                           -dt*(CD(i,N)-CD(i,N-1))   
#  endif            
     &                                           +DC(i,N-1)*(        !<- f(j) = f(j) + 
     &                                FC(i,N-1)+max(BC(i,N-1),0.) )
     &               )/( Hz(i,j,N) +FC(i,N-1)-min(BC(i,N-1),0.)
     &                      -CF(i,N-1)*(FC(i,N-1)+max(BC(i,N-1),0.))
     &                                                            )
# else
             t(i,j,N,nnew,itrc)=( t(i,j,N,nnew,itrc)
#  ifdef TS_MIX_IMP
     &                           -dt*(CD(i,N)-CD(i,N-1))   
#  endif
     &                           +FC(i,N-1)*DC(i,N-1) )
     &                        /(Hz(i,j,N)+FC(i,N-1)*(1.-CF(i,N-1)))
# endif          
        enddo     
          
          
          
          
          do k=N-1,1,-1
            do i=istr,iend
              t(i,j,k,nnew,itrc)=DC(i,k)+CF(i,k)*t(i,j,k+1,nnew,itrc)
            enddo
          enddo           !--> discard FC,CF,DC

# ifdef DIAGNOSTICS_TS
          do k=1,N
            do i=Istr,Iend
              TVmix(i,j,k,itrc) = 
     &            -(TVmix(i,j,k,itrc)-t(i,j,k,nnew,itrc)*Hz(i,j,k))
     &                                        /(dt*pm(i,j)*pn(i,j))
#  ifdef MASKING
     &                                                 * rmask(i,j)
#  endif
            enddo
          enddo
# endif /* DIAGNOSTICS_TS */
!
! CONSTANT TRACERS
!
# ifdef CONST_TRACERS
          do k=1,N
            do i=istr,iend
              t(i,j,k,nnew,itrc)=t(i,j,k,nstp,itrc)
            enddo
          enddo
#  ifdef DIAGNOSTICS_TS
          do k=1,N
            do i=Istr,Iend
               TVmix(i,j,k,itrc)=0.0
#   ifdef MASKING
     &              * rmask(i,j)
#   endif
            enddo
          enddo
#  endif /* DIAGNOSTICS_TS */
# endif /* CONST_TRACERS */

