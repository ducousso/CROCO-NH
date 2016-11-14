!
!===============================================================
!
! Compute 5th order horizontal advection
!
!===============================================================
!

#  ifdef MPI
          if (SOUTH_INTER) then
            jmin=1
          else
            jmin=3
          endif
          if (NORTH_INTER) then
            jmax=Mmmpi+1
          else
            jmax=Mmmpi-1
          endif
#  else
#   ifdef NS_PERIODIC
          jmin=1
          jmax=Mm+1
#   else
          jmin=3
          jmax=Mm-1
#   endif
#  endif
#  ifdef MPI
          if (WEST_INTER) then
            imin=1
          else
            imin=3
          endif
          if (EAST_INTER) then
            imax=Lmmpi+1
          else
            imax=Lmmpi-1
          endif
#  else
#   ifdef EW_PERIODIC
          imin=1
          imax=Lm+1
#   else
          imin=3
          imax=Lm-1
#   endif
#  endif

          DO j = Jstr,Jend+1  !j_loop_y_flux_5
                                                  !
            IF ( j.ge.jmin .and. j.le.jmax ) THEN ! use full stencil
                                                  !
              DO i = Istr,Iend
                vel = Hvom(i,j,k)
                flx5 = vel*FLUX5(
     &             t(i,j-3,k,nrhs,itrc), t(i,j-2,k,nrhs,itrc), 
     &             t(i,j-1,k,nrhs,itrc), t(i,j  ,k,nrhs,itrc),
     &             t(i,j+1,k,nrhs,itrc), t(i,j+2,k,nrhs,itrc),  vel )
#  ifdef MASKING 
                flx3 = vel*FLUX3(
     &             t(i,j-2,k,nrhs,itrc), t(i,j-1,k,nrhs,itrc),
     &             t(i,j  ,k,nrhs,itrc), t(i,j+1,k,nrhs,itrc),  vel ) 
                flx2 = vel*FLUX2(
     &             t(i,j-1,k,nrhs,itrc), t(i,j,k,nrhs,itrc), vel, cdif)
#   ifdef UP5_MASKING
                mask0=rmask(i,j-1)*rmask(i,j)
                mask2=rmask(i,j-2)*mask0*rmask(i,j+1)
                IF (vel.gt.0) THEN
                  mask1=rmask(i,j-2)*mask0
                  mask3=rmask(i,j-3)*mask2          
                ELSE
                  mask1=rmask(i,j+1)*mask0
                  mask3=rmask(i,j+2)*mask2
                ENDIF
                FE(i,j)=mask3*flx5+(1-mask3)*mask1*flx3+
     &                             (1-mask3)*(1-mask1)*mask0*flx2
#   else
                mask1=rmask(i,j-2)*rmask(i,j+1)
                mask2=rmask(i,j-3)*rmask(i,j+2)
                mask0=mask1*mask2
                FE(i,j)=mask0*flx5+(1-mask0)*mask1*flx3+
     &                         (1-mask0)*(1-mask1)*flx2
#   endif /* UP5_MASKING */
#  else
                FE(i,j)=flx5
#  endif /* MASKING */
              ENDDO
                                           !
            ELSE IF ( j.eq.jmin-2 ) THEN   ! 2nd order flux next to south
                                           ! boundary
              DO i = Istr,Iend
                vel = Hvom(i,j,k)
                FE(i,j) = vel*FLUX2(
     &             t(i,j-1,k,nrhs,itrc), t(i,j,k,nrhs,itrc), vel, cdif)
              ENDDO
                                                             !
            ELSE IF ( j.eq.jmin-1 .and. jmax.ge.jmin ) THEN  ! 3rd of 4th order flux 2 in
                                                             ! from south boundary
              DO i = Istr,Iend
                vel = Hvom(i,j,k)
                flx3 = vel*FLUX3(
     &             t(i,j-2,k,nrhs,itrc), t(i,j-1,k,nrhs,itrc),
     &             t(i,j  ,k,nrhs,itrc), t(i,j+1,k,nrhs,itrc),  vel )
#  ifdef MASKING
                flx2 = vel*FLUX2(
     &             t(i,j-1,k,nrhs,itrc), t(i,j,k,nrhs,itrc), vel, cdif)
                mask1=rmask(i,j-2)*rmask(i,j+1)
                FE(i,j)=mask1*flx3+(1-mask1)*flx2
#  else
                FE(i,j)=flx3
#  endif
              ENDDO
                                          !
            ELSE IF ( j.eq.jmax+2 ) THEN  ! 2nd order flux next to north
                                          ! boundary
              DO i = Istr,Iend
                vel = Hvom(i,j,k)
                FE(i,j) = vel*FLUX2(
     &             t(i,j-1,k,nrhs,itrc), t(i,j,k,nrhs,itrc), vel, cdif)
              ENDDO
                                          !
            ELSE IF ( j.eq.jmax+1 ) THEN  ! 3rd or 4th order flux 2 in from
                                          ! north boundary
              DO i = Istr,Iend
                vel = Hvom(i,j,k)
                flx3 = vel*FLUX3(
     &             t(i,j-2,k,nrhs,itrc), t(i,j-1,k,nrhs,itrc),
     &             t(i,j  ,k,nrhs,itrc), t(i,j+1,k,nrhs,itrc),  vel )
#  ifdef MASKING
                flx2 = vel*FLUX2(
     &             t(i,j-1,k,nrhs,itrc), t(i,j,k,nrhs,itrc), vel, cdif)
                mask1=rmask(i,j-2)*rmask(i,j+1)
                FE(i,j)=mask1*flx3+(1-mask1)*flx2
#  else
                FE(i,j)=flx3
#  endif
              ENDDO
            ENDIF
          ENDDO ! j_loop_y_flux_5

          DO i = Istr,Iend+1  !i_loop_x_flux_5
                                                  !
            IF ( i.ge.imin .and. i.le.imax ) THEN ! use full stencil
                                                  !
              DO j = Jstr,Jend
                vel = Huon(i,j,k)
                flx5 = vel*FLUX5(
     &             t(i-3,j,k,nrhs,itrc), t(i-2,j,k,nrhs,itrc),
     &             t(i-1,j,k,nrhs,itrc), t(i  ,j,k,nrhs,itrc),
     &             t(i+1,j,k,nrhs,itrc), t(i+2,j,k,nrhs,itrc),  vel )
#  ifdef MASKING
                flx3 = vel*FLUX3(
     &             t(i-2,j,k,nrhs,itrc), t(i-1,j,k,nrhs,itrc),
     &             t(i  ,j,k,nrhs,itrc), t(i+1,j,k,nrhs,itrc),  vel )
                flx2 = vel*FLUX2(
     &             t(i-1,j,k,nrhs,itrc), t(i,j,k,nrhs,itrc), vel, cdif)
#   ifdef UP5_MASKING
                mask0=rmask(i-1,j)*rmask(i,j)
                mask2=rmask(i-2,j)*mask0*rmask(i+1,j)
                IF (vel.gt.0) THEN
                  mask1=rmask(i-2,j)*mask0
                  mask3=rmask(i-3,j)*mask2          
                ELSE
                  mask1=rmask(i+1,j)*mask0
                  mask3=rmask(i+2,j)*mask2
                ENDIF
                FX(i,j)=mask3*flx5+(1-mask3)*mask1*flx3+
     &                             (1-mask3)*(1-mask1)*mask0*flx2
#   else
                mask1=rmask(i-2,j)*rmask(i+1,j)
                mask2=rmask(i-3,j)*rmask(i+2,j)
                mask0=mask1*mask2
                FX(i,j)=mask0*flx5+(1-mask0)*mask1*flx3+
     &                         (1-mask0)*(1-mask1)*flx2
#   endif /* UP5_MASKING */
#  else
                FX(i,j)=flx5
#  endif /* MASKING */
              ENDDO
                                           !
            ELSE IF ( i.eq.imin-2 ) THEN   ! 2nd order flux next to south
                                           ! boundary
              DO j = Jstr,Jend
                vel = Huon(i,j,k)
                FX(i,j) = vel*FLUX2(
     &             t(i-1,j,k,nrhs,itrc), t(i,j,k,nrhs,itrc), vel, cdif)
              ENDDO
                                                             !
            ELSE IF ( i.eq.imin-1 .and. imax.ge.imin ) THEN  ! 3rd of 4th order flux 2 in
                                                             ! from south boundary
              DO j = Jstr,Jend
                vel = Huon(i,j,k)
                flx3 = vel*FLUX3(
     &             t(i-2,j,k,nrhs,itrc), t(i-1,j,k,nrhs,itrc),
     &             t(i  ,j,k,nrhs,itrc), t(i+1,j,k,nrhs,itrc),  vel )
#  ifdef MASKING
                flx2 = vel*FLUX2(
     &             t(i-1,j,k,nrhs,itrc), t(i,j,k,nrhs,itrc), vel, cdif)
                mask1=rmask(i-2,j)*rmask(i+1,j)
                FX(i,j)=mask1*flx3+(1-mask1)*flx2
#  else
                FX(i,j)=flx3
#  endif
              ENDDO
                                          !
            ELSE IF ( i.eq.imax+2 ) THEN  ! 2nd order flux next to north
                                          ! boundary
              DO j = Jstr,Jend
                vel = Huon(i,j,k)
                FX(i,j) = vel*FLUX2(
     &             t(i-1,j,k,nrhs,itrc), t(i,j,k,nrhs,itrc), vel, cdif)
              ENDDO
                                          !
            ELSE IF ( i.eq.imax+1 ) THEN  ! 3rd or 4th order flux 2 in from
                                          ! north boundary
              DO j = Jstr,Jend
                vel = Huon(i,j,k)
                flx3 = vel*FLUX3(
     &             t(i-2,j,k,nrhs,itrc), t(i-1,j,k,nrhs,itrc),
     &             t(i  ,j,k,nrhs,itrc), t(i+1,j,k,nrhs,itrc),  vel )
#  ifdef MASKING
                flx2 = vel*FLUX2(
     &             t(i-1,j,k,nrhs,itrc), t(i,j,k,nrhs,itrc), vel, cdif)
                mask1=rmask(i-2,j)*rmask(i+1,j)
                FX(i,j)=mask1*flx3+(1-mask1)*flx2
#  else
                FX(i,j)=flx3
#  endif
              ENDDO
            ENDIF
          ENDDO ! i_loop_x_flux_5

