! $Id: FCT.h 1523 2014-04-14 16:35:35Z gcambon $
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
# ifdef TS_VADV_FCT
!
!======================================================================
! Compute vertical advective fluxes using FCT Zalezak (1979) scheme
!======================================================================
!
! Compute low-order (diffusive) and antidiffusive fluxes
!
#  define MINMAX_UPSTREAM
#  define WENO_SELECTION
!
! MINMAX_UPSTREAM: Max and min tracer values giving permissible bounds use a more 
! restrictive upstream stencil than in Zalesak (1979)
!
! WENO_SELECTION: an addition from Blossey & Durran (2008) to 
! selectively preserves monotonicity. Monotonicity preservation is applied 
! only where the scalar field is likely to contain discontinuities as indicated 
! by significant grid-cell-to-grid-cell variations in a smoothness measure 
! conceptually similar to that used in weighted essentially non-oscillatory (WENO) 
! methods. Strict positivity preservation is efficiently obtained through an 
! additional flux correction step.

#  define FH FC
#  define FL BC
#  define AF DC
          do k=1,N-1
            do i=Istr,Iend
              cff =pm(i,j)*pn(i,j)
              cff1=0.5*cff*W(i,j,k)
              cff2=0.5*sign(1.,cff1)
              FL(i,k)=cff1*( t(i,j,k  ,3,itrc)+t(i,j,k+1,3,itrc)
     &                -cff2*(t(i,j,k+1,3,itrc)-t(i,j,k  ,3,itrc)) )
              AF(i,k)=cff*FH(i,k)-FL(i,k)
            enddo
          enddo
          do i=Istr,Iend
            FL(i,0)=0.
            FL(i,N)=0.
          enddo
#  undef FH
!
! Monotone estimate of tracer field at nnew
!
          do k=1,N
            do i=Istr,Iend
              t(i,j,k,nnew,itrc)=(t(i,j,k,nnew,itrc)
     &                           -dt*(FL(i,k)-FL(i,k-1)))/Hz(i,j,k)
             enddo
          enddo
#  undef FL
!
! Flux Correction
!
#  define RPOS CF
#  define RNEG BC
#  define RNEG2 EC
#  define GAM GC
          do k=2,N-1
            do i=Istr,Iend
#  ifdef MINMAX_UPSTREAM
              if (W(i,j,k).gt.0) then
                tmax=max(t(i,j,k-1,3,itrc),t(i,j,k-1,nnew,itrc),
     &                   t(i,j,k  ,3,itrc),t(i,j,k  ,nnew,itrc))
                tmin=min(t(i,j,k-1,3,itrc),t(i,j,k-1,nnew,itrc),
     &                   t(i,j,k  ,3,itrc),t(i,j,k  ,nnew,itrc))
              else
                tmax=max(t(i,j,k+1,3,itrc),t(i,j,k+1,nnew,itrc),
     &                   t(i,j,k  ,3,itrc),t(i,j,k  ,nnew,itrc))
                tmin=min(t(i,j,k+1,3,itrc),t(i,j,k+1,nnew,itrc),
     &                   t(i,j,k  ,3,itrc),t(i,j,k  ,nnew,itrc))
              endif
#  else
              tmax=max(t(i,j,k-1,3,itrc),t(i,j,k-1,nnew,itrc),
     &                 t(i,j,k  ,3,itrc),t(i,j,k  ,nnew,itrc),
     &                 t(i,j,k+1,3,itrc),t(i,j,k+1,nnew,itrc))
              tmin=min(t(i,j,k-1,3,itrc),t(i,j,k-1,nnew,itrc),
     &                 t(i,j,k  ,3,itrc),t(i,j,k  ,nnew,itrc),
     &                 t(i,j,k+1,3,itrc),t(i,j,k+1,nnew,itrc))
#  endif

              Ppos=max(0.,AF(i,k-1))-min(0.,AF(i,k))
              Qpos=Hz(i,j,k)/dt*(tmax-t(i,j,k,nnew,itrc))
              RPOS(i,k)=max(0.,min(1.,Qpos/Ppos))

              Pneg=max(0.,AF(i,k))-min(0.,AF(i,k-1))
              Qneg=Hz(i,j,k)/dt*(t(i,j,k,nnew,itrc)-tmin)
              RNEG(i,k)=max(0.,min(1.,Qneg/Pneg))

#  ifdef WENO_SELECTION
              Qneg=Hz(i,j,k)/dt*t(i,j,k,nnew,itrc)
              RNEG2(i,k)=max(0.,min(1.,Qneg/Pneg))
              GAM(i,k)=(t(i,j,k  ,3,itrc)-t(i,j,k-1,3,itrc))*
     &                 (t(i,j,k  ,3,itrc)-t(i,j,k-1,3,itrc)) + 
     &                 (t(i,j,k+1,3,itrc)-t(i,j,k  ,3,itrc))*
     &                 (t(i,j,k+1,3,itrc)-t(i,j,k  ,3,itrc))
#  endif
            enddo
          enddo

          do i=Istr,Iend
            RPOS(i,1)=RPOS(i,2)
            RPOS(i,N)=RPOS(i,N-1)
            RNEG(i,1)=RNEG(i,2)
            RNEG(i,N)=RNEG(i,N-1)
#  ifdef WENO_SELECTION
            RNEG2(i,1)=RNEG2(i,2)
            RNEG2(i,N)=RNEG2(i,N-1)
            GAM(i,1)=GAM(i,2)
            GAM(i,N)=GAM(i,N-1)
#  endif
          enddo

          do k=1,N-1
            do i=Istr,Iend
              if (AF(i,k).ge.0.) then
                FC(i,k)=AF(i,k)*min(RPOS(i,k+1),RNEG(i,k))
              else
                FC(i,k)=AF(i,k)*min(RPOS(i,k  ),RNEG(i,k+1))
              endif
            enddo
          enddo

#  ifdef WENO_SELECTION
          lmdmax=20.
          do k=2,N-1
            do i=Istr,Iend
              cff=max(GAM(i,k-1),GAM(i,k),GAM(i,k+1))/
     &           (min(GAM(i,k-1),GAM(i,k),GAM(i,k+1)) + 1.e-10);
              if (cff.lt.lmdmax) then
                FC(i,k)=AF(i,k)*RNEG2(i,k)
              endif
            enddo
          enddo
#  endif

          do i=Istr,Iend
            FC(i,0)=0.
            FC(i,N)=0.
          enddo
          do k=1,N
            do i=Istr,Iend
              FC(i,k)=FC(i,k)*om_r(i,j)*on_r(i,j)
              t(i,j,k,nnew,itrc)=t(i,j,k,nnew,itrc)*Hz(i,j,k) ! m*degC
            enddo
          enddo

#  undef RPOS
#  undef RNEG
#  undef AF
#  undef RNEG2
#  undef GAM

# endif

