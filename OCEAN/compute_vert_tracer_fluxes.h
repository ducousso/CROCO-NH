# ifdef TS_VADV_SPLINES
!
!----------------------------------------------------------
! Compute vertical advective fluxes using parabolic splines: 
! FC=W*[spline-interpolated tracer]
!----------------------------------------------------------
!
                              ! Construct parabolic splines: here
          do i=Istr,Iend      ! CF is the set of vertical derivatives
            FC(i,0)=0.        ! of the tracer field t(:,:,:,nadv,:),
            CF(i,0)=0.        ! FC is an auxiliary scratch variable. 
          enddo
          do k=1,N-1,+1
            do i=Istr,Iend
              cff    = 1./(2.*HZR(i,j,k+1)+HZR(i,j,k)*(2.-FC(i,k-1)))
              FC(i,k)= cff*HZR(i,j,k+1)
              CF(i,k)= cff*( 6.*( t(i,j,k+1,nadv,itrc)
     &                           -t(i,j,k  ,nadv,itrc) )-HZR(i,j,k)*CF(i,k-1)
     &                                                                      )
            enddo 
          enddo
          do i=Istr,Iend
            CF(i,N)=0.
          enddo
          do k=N-1,1,-1       !<-- irreversible
            do i=Istr,Iend
              CF(i,k)=CF(i,k)-FC(i,k)*CF(i,k+1)
            enddo
          enddo               !--> discard FC, keep CF

          cff=1./3.           ! Compute vertical advective fluxes 
          do k=1,N-1          ! FC=W*[spline-interpolated tracer]
            do i=Istr,Iend
              FC(i,k)=We(i,j,k)*( t(i,j,k,nadv,itrc)+cff*HZR(i,j,k)
     &                                  *(CF(i,k)+0.5*CF(i,k-1)) 
     &                                                            )
            enddo
          enddo               !--> discard CF
          do i=Istr,Iend
            FC(i,N)=0.
#  ifdef MOVING_BATHY
            FC(i,0)=We(i,j,0)*t(i,j,1,nadv,itrc)
#  else
            FC(i,0)=0.
#  endif
            CF(i,0)=dt*pm(i,j)*pn(i,j)
          enddo
# elif defined TS_VADV_AKIMA
!
!----------------------------------------------------------
! Compute vertical advective fluxes 
! using 4th-order Akima scheme
!----------------------------------------------------------
!
          do k=1,N-1
            do i=istr,iend
              FC(i,k)=t(i,j,k+1,nadv,itrc)-t(i,j,k,nadv,itrc)
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
              FC(i,k)=0.5*(   t(i,j,k  ,nadv,itrc)
     &                +       t(i,j,k+1,nadv,itrc)
     &                -0.333333333333*(CF(i,k+1)-CF(i,k)) 
     &                                                  )*We(i,j,k)
            enddo
          enddo            !--> discard CF
          do i=istr,iend
#  ifdef MOVING_BATHY
            FC(i,0)=We(i,j,0)*t(i,j,1,nadv,itrc)
#  else
            FC(i,0)=0.
#  endif
            FC(i,N)=0.
            CF(i,0)=dt*pm(i,j)*pn(i,j)
          enddo
# elif defined TS_VADV_WENO5
!
!----------------------------------------------------------
! Compute vertical advective fluxes 
! using 5th-order WENO scheme
!----------------------------------------------------------
!
          do k=3,N-3
            do i=Istr,Iend
              FC(i,k)=We(i,j,k)*
#  ifdef PREDICTOR
     &              flux6(
#  else
     &              flux5_weno( 
#  endif
     &             t(i,j,k-2,nadv,itrc), t(i,j,k-1,nadv,itrc), 
     &             t(i,j,k  ,nadv,itrc), t(i,j,k+1,nadv,itrc),
     &             t(i,j,k+2,nadv,itrc), t(i,j,k+3,nadv,itrc), We(i,j,k))
            enddo
          enddo
          do i=Istr,Iend
            FC(i,2)=We(i,j,2)*
#  ifdef PREDICTOR
     &               flux4(
#  else
     &               flux3_weno(
#  endif
     &           t(i,j,1,nadv,itrc), t(i,j,2,nadv,itrc), 
     &           t(i,j,3,nadv,itrc), t(i,j,4,nadv,itrc), We(i,j,2))
            FC(i,N-2)=We(i,j,N-2)*
#  ifdef PREDICTOR
     &               flux4(
#  else
     &               flux3_weno(
#  endif
     &           t(i,j,N-3,nadv,itrc), t(i,j,N-2,nadv,itrc), 
     &           t(i,j,N-1,nadv,itrc), t(i,j,N  ,nadv,itrc), We(i,j,N-2))

            FC(i,  1)=0.5*We(i,j,  1)*( t(i,j,1  ,nadv,itrc)
     &                               +  t(i,j,2  ,nadv,itrc))
            FC(i,N-1)=0.5*We(i,j,N-1)*( t(i,j,N-1,nadv,itrc)
     &                               +  t(i,j,N,  nadv,itrc))

#  ifdef MOVING_BATHY
            FC(i,0)=We(i,j,0)*t(i,j,1,nadv,itrc)
#  else
            FC(i,0)=0.
#  endif
            FC(i,N )=0.
            CF(i,0)=dt*pm(i,j)*pn(i,j)
          enddo
# elif defined TS_VADV_C2
!
!----------------------------------------------------------
! Compute vertical advective fluxes
! using 2th-order centered scheme
!----------------------------------------------------------
!
          do k=1,N-1
           do i=Istr,Iend
             FC(i,k)=0.5*We(i,j,k)*(t(i,j,k  ,nadv,itrc)
     &                           +  t(i,j,k+1,nadv,itrc))
           enddo
         enddo
         do i=Istr,Iend
#  ifdef MOVING_BATHY
            FC(i,0)=We(i,j,0)*t(i,j,1,nadv,itrc)
#  else
            FC(i,0)=0.
#  endif
           FC(i,N )=0.
           CF(i,0 )=dt*pm(i,j)*pn(i,j)
         enddo
# else
!
!----------------------------------------------------------
! Compute vertical advective fluxes
! using 4th-order centered scheme
!----------------------------------------------------------
!
          do k=2,N-2
            do i=Istr,Iend
              FC(i,k)=We(i,j,k)*(
     &                     0.58333333333333*( t(i,j,k  ,nadv,itrc)
     &                                       +t(i,j,k+1,nadv,itrc))
     &                    -0.08333333333333*( t(i,j,k-1,nadv,itrc)
     &                                       +t(i,j,k+2,nadv,itrc))
     &                                                            )
            enddo
          enddo
          do i=Istr,Iend
#  ifdef MOVING_BATHY
            FC(i,0)=We(i,j,0)*2.
     &              (0.58333333333333*t(i,j,1,nstp,itrc)-
     &               0.08333333333333*t(i,j,2,nstp,itrc))
#  else
            FC(i, 0)=0.0
#  endif
            FC(i,  1)=We(i,j,  1)*(     0.5*t(i,j,  1,nadv,itrc)
     &                       +0.58333333333333*t(i,j,  2,nadv,itrc)
     &                       -0.08333333333333*t(i,j,  3,nadv,itrc)
     &                                                            )
            FC(i,N-1)=We(i,j,N-1)*(     0.5*t(i,j,N  ,nadv,itrc)
     &                       +0.58333333333333*t(i,j,N-1,nadv,itrc)
     &                       -0.08333333333333*t(i,j,N-2,nadv,itrc)
     &                                                            )
            FC(i,N )=0.0
            CF(i,0 )=dt*pm(i,j)*pn(i,j)
          enddo
# endif
