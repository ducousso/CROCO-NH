! $Id: gls_smooth.h 1160 2013-06-11 09:48:32Z gcambon $
!
!======================================================================
! CROCO is a branch of ROMS developped at IRD and INRIA, in France
! The two other branches from UCLA (Shchepetkin et al)
! and Rutgers University (Arango et al) are under MIT/X style license.
! CROCO specific routines (nesting) are under CeCILL-C license.
!
! CROCO website : http://www.croco-ocean.org/
!======================================================================
!
! PART OF KPP2005 (Shchepetkin et al. 2005)
!
#   ifndef EW_PERIODIC
      if (WESTERN_EDGE) then
        do j=J_EXT_RANGE
          hwrk(Istr-1,j,k)=hwrk(Istr,j,k)
        enddo
      endif
      if (EASTERN_EDGE) then
        do j=J_EXT_RANGE
          hwrk(Iend+1,j,k)=hwrk(Iend,j,k)
        enddo
      endif
#   endif
#   ifndef NS_PERIODIC
      if (SOUTHERN_EDGE) then
        do i=I_EXT_RANGE
          hwrk(i,Jstr-1,k)=hwrk(i,Jstr,k)
        enddo
      endif
      if (NORTHERN_EDGE) then
        do i=I_EXT_RANGE
          hwrk(i,Jend+1,k)=hwrk(i,Jend,k)
        enddo
      endif
#    ifndef EW_PERIODIC
      if (WESTERN_EDGE.and.SOUTHERN_EDGE) then
        hwrk(Istr-1,Jstr-1,k)=hwrk(Istr,Jstr,k)
      endif
      if (WESTERN_EDGE.and.NORTHERN_EDGE) then
        hwrk(Istr-1,Jend+1,k)=hwrk(Istr,Jend,k)
      endif
      if (EASTERN_EDGE.and.SOUTHERN_EDGE) then
        hwrk(Iend+1,Jstr-1,k)=hwrk(Iend,Jstr,k)
      endif
      if (EASTERN_EDGE.and.NORTHERN_EDGE) then
        hwrk(Iend+1,Jend+1,k)=hwrk(Iend,Jend,k)
      endif
#    endif
#   endif

      do j=Jstr,Jend+1
        do i=Istr,Iend+1
          wrk3(i,j)=0.25*(hwrk(i,j,k)  +hwrk(i-1,j,k)
     &                   +hwrk(i,j-1,k)+hwrk(i-1,j-1,k))
#   ifdef MASKING
     &                   *pmask2(i,j)
#   endif
        enddo
      enddo
      do j=Jstr,Jend
        do i=Istr,Iend
#   ifdef MASKING
          cff=0.25*(pmask2(i,j)   +pmask2(i+1,j)
     &             +pmask2(i,j+1) +pmask2(i+1,j+1))
#   else
          cff=1.
#   endif
          hwrk(i,j,k)=(1.-cff)*hwrk(i,j,k)+
     &              0.25*(wrk3(i,j)  +wrk3(i+1,j)
     &                   +wrk3(i,j+1)+wrk3(i+1,j+1))
#   ifdef MASKING
          hwrk(i,j,k)=hwrk(i,j,k)*rmask(i,j)
#   endif
        enddo
      enddo
