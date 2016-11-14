! $Id: gls_vert_advection.h 1160 2013-06-11 09:48:32Z gcambon $
!
!======================================================================
! CROCO is a branch of ROMS developped at IRD and INRIA, in France
! The two other branches from UCLA (Shchepetkin et al) 
! and Rutgers University (Arango et al) are under MIT/X style license.
! CROCO specific routines (nesting) are under CeCILL-C license.
! 
! CROCO website : http://www.croco-ocean.org/
!
!======================================================================
!
!======================================================================
!
!                  Compute vertical advection
!
!======================================================================
!
# define GLS_VADV_AKIMA
!
# ifdef GLS_VADV_SPLINES
!
! Compute vertical advective fluxes using parabolic splines: 
! FC=W*[spline-interpolated tracer]
!
! Firts construct parabolic splines:
! here CF is the set of vertical derivatives of the tracer field
! tmp(:,:,:,3) at the grid-box interfaces, W-points;
! FC is an auxiliary scratch variable.
!
          do i=Istr,Iend
            FC(i,0)=0.
            CF(i,0)=0.
          enddo
          do k=1,N-1,+1
            do i=Istr,Iend
              cff=1./(2.*Hz(i,j,k+1)+Hz(i,j,k)*(2.-FC(i,k-1)))
              FC(i,k)=cff*Hz(i,j,k+1)
              CF(i,k)=cff*(6.*(tmp(i,j,k+1,3)-tmp(i,j,k,3))
     &                                      -Hz(i,j,k)*CF(i,k-1))
            enddo
          enddo
          do i=Istr,Iend
            CF(i,N)=0.
          enddo
          do k=N-1,1,-1
            do i=Istr,Iend
              CF(i,k)=CF(i,k)-FC(i,k)*CF(i,k+1)
            enddo
          enddo               !--> discard FC
!
          cff=1./3.
          do k=1,N-1
            do i=Istr,Iend
              FC(i,k)=We(i,j,k)*( tmp(i,j,k,3)+cff*Hz(i,j,k)
     &                              *(CF(i,k)+0.5*CF(i,k-1)) )
            enddo
          enddo               !--> discard CF
          do i=Istr,Iend
            FC(i,N)=0.
            FC(i,0)=0.
            CF(i,0)=dt*pm(i,j)*pn(i,j)
          enddo
# elif defined GLS_VADV_AKIMA
!
! Compute vertical advective fluxes using 4th-order Akima scheme
!
          do k=1,N-1
            do i=istr,iend
              FC(i,k)=tmp(i,j,k+1,3)-tmp(i,j,k,3)
            enddo
          enddo
          do i=istr,iend
            FC(i,0)=FC(i,1)
            FC(i,N)=FC(i,N-1)
          enddo
          do k=1,N
            do i=istr,iend
              cff=2.*FC(i,k)*FC(i,k-1)
              if (cff.gt.epsil) then
                CF(i,k)=cff/(FC(i,k)+FC(i,k-1))
              else
                CF(i,k)=0.
              endif
            enddo
          enddo            !--> discard FC
          do k=1,N-1
            do i=istr,iend
              FC(i,k)=0.5*( tmp(i,j,k,3)+tmp(i,j,k+1,3)
     &                -0.333333333333*(CF(i,k+1)-CF(i,k)) )*We(i,j,k)
            enddo
          enddo            !--> discard CF
          do i=istr,iend
            FC(i,0)=0.
            FC(i,N)=0.
            CF(i,0)=dt*pm(i,j)*pn(i,j)
          enddo
# elif GLS_VADV_C2
!
! Compute vertical advective fluxes using 2th-order centered scheme
!
          do k=1,N-1
            do i=Istr,Iend
              FC(i,k)=0.5*We(i,j,k)*(tmp( i,j,k,3)
     &                              +tmp(i,j,k+1,3))
            enddo
          enddo
          do i=Istr,Iend
            FC(i, 0)=0.
            FC(i,N )=0.
            CF(i,0)=dt*pm(i,j)*pn(i,j)
          enddo
# else
!
! Compute vertical advective fluxes using 4th-order centered scheme
!
          do k=2,N-2
            do i=Istr,Iend
              FC(i,k)=We(i,j,k)*(0.58333333333333*( tmp(i,j,k,3)
     &                                             +tmp(i,j,k+1,3))
     &                          -0.08333333333333*( tmp(i,j,k-1,3)
     &                                             +tmp(i,j,k+2,3))
     &                                                              )
            enddo
          enddo
          do i=Istr,Iend
            FC(i, 0)=0.
            FC(i,  1)=We(i,j,  1)*( 0.5*tmp(i,j,  1,3 )
     &                         +0.58333333333333*tmp(i,j,  2,3)
     &                         -0.08333333333333*tmp(i,j,  3,3)
     &                                                           )
            FC(i,N-1)=We(i,j,N-1)*( 0.5*tmp(i,j,N  ,3)
     &                         +0.58333333333333*tmp(i,j,N-1,3)
     &                         -0.08333333333333*tmp(i,j,N-2,3)
     &                                                           )
            FC(i,N )=0.
            CF(i,0)=dt*pm(i,j)*pn(i,j)
          enddo
# endif

