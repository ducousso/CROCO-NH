!
! $Id: MessPass3D.F 1143 2013-05-17 08:17:23Z serena $
!
#include "cppdefs.h"
#if defined MPI && defined SOLVE3D

# ifndef MP_3PTS
      subroutine MessPass3D_tile (Istr,Iend,Jstr,Jend, A, nmax)
# else
      subroutine MessPass3D_3pts_tile (Istr,Iend,Jstr,Jend, A, nmax)
# endif
!
!======================================================================
!
! This subroutine is designed for ROMS-MPI code. It exchanges domain
! boundary information, including 2 (or 3) ghost-cells in each
! direction.
!
! Ping Wang 9/15/99.
! Patrick Marchesiello 2012: generic number of ghost-cells Npts
!
!======================================================================
!
!     implicit none
# include "param.h"
# include "scalars.h"
# include "mpi_roms.h"
      include 'mpif.h'
!
! Nb of boundary points involved in communication
!
      integer Npts,ipts,jpts
# ifndef MP_3PTS
      parameter (Npts=2)
# else
      parameter (Npts=3)
# endif
      integer nmax
      real A(GLOBAL_2D_ARRAY,nmax)
CSDISTRIBUTE_RESHAPE A(BLOCK_PATTERN) BLOCK_CLAUSE
      integer Istr,Iend,Jstr,Jend, i,j,k, isize,jsize,ksize,
     &        req(8), status(MPI_STATUS_SIZE,8), ierr
      integer iter, mdii, mdjj
      integer sub_X,size_X, sub_E,size_E, size_Z

# if ! defined AGRIF
      parameter (size_Z=Npts*Npts*(N+1),
     &     sub_X=(Lm+NSUB_X-1)/NSUB_X,  size_X=Npts*(N+1)*(4+sub_X),
     &     sub_E=(Mm+NSUB_E-1)/NSUB_E,  size_E=Npts*(N+1)*(4+sub_E))

      real buf_snd4(size_Z),  ibuf_sndN(size_X),  buf_snd2(size_Z),
     &     buf_rev4(size_Z),  ibuf_revN(size_X),  buf_rev2(size_Z),
     &    jbuf_sndW(size_E),                      jbuf_sndE(size_E),
     &    jbuf_revW(size_E),                      jbuf_revE(size_E),
     &     buf_snd1(size_Z),  ibuf_sndS(size_X),  buf_snd3(size_Z),
     &     buf_rev1(size_Z),  ibuf_revS(size_X),  buf_rev3(size_Z)
# else
      real, dimension(:), allocatable ::
     &     buf_snd4,  ibuf_sndN,  buf_snd2,
     &     buf_rev4,  ibuf_revN,  buf_rev2,
     &    jbuf_sndW,             jbuf_sndE,
     &    jbuf_revW,             jbuf_revE,
     &     buf_snd1,  ibuf_sndS,  buf_snd3,
     &     buf_rev1,  ibuf_revS,  buf_rev3
# endif

c**
c      common /buffers_3D/
c     &     buf_snd4,     ibuf_sndN,     buf_snd2,
c     &     buf_rev4,     ibuf_revN,     buf_rev2,
c
c     &    jbuf_sndW,                    jbuf_sndE,
c     &    jbuf_revW,                    jbuf_revE,
c
c     &     buf_snd1,     ibuf_sndS,     buf_snd3,
c     &     buf_rev1,     ibuf_revS,     buf_rev3
c**
!
# include "compute_message_bounds.h"

# ifdef AGRIF
      size_Z=Npts*Npts*(N+1)
      sub_X=(Lm+NSUB_X-1)/NSUB_X
      size_X=Npts*(N+1)*(4+sub_X)
      sub_E=(Mm+NSUB_E-1)/NSUB_E
      size_E=Npts*(N+1)*(4+sub_E)

      Allocate(buf_snd4(size_Z),  ibuf_sndN(size_X),  buf_snd2(size_Z),
     &         buf_rev4(size_Z),  ibuf_revN(size_X),  buf_rev2(size_Z),
     &        jbuf_sndW(size_E),                     jbuf_sndE(size_E),
     &        jbuf_revW(size_E),                     jbuf_revE(size_E),
     &         buf_snd1(size_Z),  ibuf_sndS(size_X),  buf_snd3(size_Z),
     &         buf_rev1(size_Z),  ibuf_revS(size_X),  buf_rev3(size_Z))
# endif
!
      ksize=Npts*Npts*nmax                    ! message sizes for
      isize=Npts*ishft*nmax              ! corner messages and sides
      jsize=Npts*jshft*nmax              ! in XI and ETA directions 
!
! Prepare to receive and send: sides....
!
                            !  Message passing split into two stages
                            !  in order to optimize Send-Recv pairing
                            !  in such a way that if one subdomain
      do iter=0,1           !  sends message to, say, its WESTERN
        mdii=mod(ii+iter,2) !  neighbor, that neighbor is preparing
        mdjj=mod(jj+iter,2) !  to receive this message first (i.e.
                            !  message coming from its EASTERN side),
                            !  rather than send his WEST
                            !  bound message, similarly to the first
                            !  subdomain.

!
! Prepare to receive and send: sides....
        if (mdii.eq.0) then
          if ((WEST_INTER)) then
            do k=1,nmax
              do j=jmin,jmax
                do ipts=1,Npts
                  jbuf_sndW(k+nmax*(j-jmin+(ipts-1)*jshft))=A(ipts,j,k)
                enddo
              enddo
            enddo
            call MPI_Irecv (jbuf_revW, jsize, MPI_DOUBLE_PRECISION,
     &                         p_W, 2, MPI_COMM_WORLD, req(1), ierr)
            call MPI_Send  (jbuf_sndW, jsize, MPI_DOUBLE_PRECISION,
     &                         p_W, 1, MPI_COMM_WORLD,         ierr)
          endif
        else
          if (EAST_INTER) then
            do k=1,nmax
              do j=jmin,jmax
                do ipts=1,Npts
                  jbuf_sndE(k+nmax*(j-jmin+(ipts-1)*jshft))=
     &                                       A(Lmmpi-Npts+ipts,j,k)
                enddo
              enddo
            enddo
            call MPI_Irecv (jbuf_revE, jsize, MPI_DOUBLE_PRECISION,
     &                        p_E, 1, MPI_COMM_WORLD, req(2), ierr)
            call MPI_Send  (jbuf_sndE, jsize, MPI_DOUBLE_PRECISION,
     &                        p_E, 2, MPI_COMM_WORLD,         ierr)
          endif
        endif

        if (mdjj.eq.0) then
          if (SOUTH_INTER) then
            ibuf_snds = 0.
            do k=1,nmax
              do i=imin,imax
                do jpts=1,Npts
                  ibuf_sndS(k+nmax*(i-imin+(jpts-1)*ishft))=A(i,jpts,k)
                enddo
              enddo
            enddo
            call MPI_Irecv (ibuf_revS, isize, MPI_DOUBLE_PRECISION,
     &                         p_S, 4, MPI_COMM_WORLD, req(3), ierr)
            call MPI_Send  (ibuf_sndS, isize, MPI_DOUBLE_PRECISION,
     &                         p_S, 3, MPI_COMM_WORLD,         ierr)
          endif
        else
          if (NORTH_INTER) then
            ibuf_sndn = 0.
            do k=1,nmax
              do i=imin,imax
                do jpts=1,Npts
                  ibuf_sndN(k+nmax*(i-imin+(jpts-1)*ishft))=
     &                                         A(i,Mmmpi-Npts+jpts,k)
                enddo
              enddo
            enddo
            call MPI_Irecv (ibuf_revN, isize, MPI_DOUBLE_PRECISION,
     &                         p_N, 3, MPI_COMM_WORLD, req(4), ierr)
            call MPI_Send  (ibuf_sndN, isize, MPI_DOUBLE_PRECISION,
     &                         p_N, 4, MPI_COMM_WORLD,         ierr)
          endif
        endif
!
! ...corners:
!
        if (mdii.eq.0) then
          if (SOUTH_INTER .and. WEST_INTER) then
            do k=1,nmax
              do jpts=1,Npts
                do ipts=1,Npts
                  buf_snd1(k+nmax*(ipts-1+Npts*(jpts-1)))=A(ipts,jpts,k)
                enddo
              enddo
            enddo
            call MPI_Irecv (buf_rev1, ksize, MPI_DOUBLE_PRECISION, p_SW,
     &                                6, MPI_COMM_WORLD, req(5),ierr)
            call MPI_Send  (buf_snd1, ksize, MPI_DOUBLE_PRECISION, p_SW,
     &                                5, MPI_COMM_WORLD,        ierr)
          endif
        else
          if (NORTH_INTER .and. EAST_INTER) then
            do k=1,nmax
              do jpts=1,Npts
                do ipts=1,Npts
                  buf_snd2(k+nmax*(ipts-1+Npts*(jpts-1)))=
     &                            A(Lmmpi+ipts-Npts,Mmmpi+jpts-Npts,k)
                enddo
              enddo
            enddo
            call MPI_Irecv (buf_rev2, ksize, MPI_DOUBLE_PRECISION, p_NE,
     &                                  5, MPI_COMM_WORLD, req(6),ierr)
            call MPI_Send  (buf_snd2, ksize, MPI_DOUBLE_PRECISION, p_NE,
     &                                  6, MPI_COMM_WORLD,        ierr)
          endif
        endif

        if (mdii.eq.1) then
          if (SOUTH_INTER .and. EAST_INTER) then
            do k=1,nmax
              do jpts=1,Npts
                do ipts=1,Npts
                  buf_snd3(k+nmax*(ipts-1+Npts*(jpts-1)))=
     &                                A(Lmmpi+ipts-Npts,jpts,k)
                enddo
              enddo
            enddo
            call MPI_Irecv (buf_rev3, ksize, MPI_DOUBLE_PRECISION, p_SE,
     &                                8, MPI_COMM_WORLD, req(7),ierr)
            call MPI_Send  (buf_snd3, ksize, MPI_DOUBLE_PRECISION, p_SE,
     &                                7, MPI_COMM_WORLD,        ierr)
          endif
        else
          if (NORTH_INTER .and. WEST_INTER) then
            do k=1,nmax
              do jpts=1,Npts
                do ipts=1,Npts
                  buf_snd4(k+nmax*(ipts-1+Npts*(jpts-1)))=
     &                                A(ipts,Mmmpi+jpts-Npts,k)
                enddo
              enddo
            enddo
            call MPI_Irecv (buf_rev4, ksize, MPI_DOUBLE_PRECISION, p_NW,
     &                                7, MPI_COMM_WORLD, req(8),ierr)
            call MPI_Send  (buf_snd4, ksize, MPI_DOUBLE_PRECISION, p_NW,
     &                                8, MPI_COMM_WORLD,        ierr)
          endif
        endif
      enddo   !<-- iter

!
! Wait for completion of receive and fill ghost points: sides...
!
      if (WEST_INTER) then
        call MPI_Wait (req(1),status(1,1),ierr)
        do k=1,nmax
          do j=jmin,jmax
            do ipts=1,Npts
             A(ipts-Npts,j,k)=jbuf_revW(k+nmax*(j-jmin+(ipts-1)*jshft))
            enddo
          enddo
        enddo
      endif

      if (EAST_INTER) then
        call MPI_Wait (req(2),status(1,2),ierr)
        do k=1,nmax
          do j=jmin,jmax
            do ipts=1,Npts
              A(Lmmpi+ipts,j,k)=
     &                         jbuf_revE(k+nmax*(j-jmin+(ipts-1)*jshft))
            enddo
          enddo
        enddo
      endif

      if (SOUTH_INTER) then
        call MPI_Wait (req(3),status(1,3),ierr)
        do k=1,nmax
          do i=imin,imax
            do jpts=1,Npts
              A(i,jpts-Npts,k)=ibuf_revS(k+nmax*(i-imin+(jpts-1)*ishft))
            enddo
          enddo
        enddo
      endif

      if (NORTH_INTER) then
        call MPI_Wait (req(4),status(1,4),ierr)
        do k=1,nmax
          do i=imin,imax
            do jpts=1,Npts
              A(i,Mmmpi+jpts,k)=
     &                         ibuf_revN(k+nmax*(i-imin+(jpts-1)*ishft))
            enddo
          enddo
        enddo
      endif
!
! ...corners:
!
      if (SOUTH_INTER .and. WEST_INTER) then
        call MPI_Wait (req(5),status(1,5),ierr)
        do k=1,nmax
          do jpts=1,Npts
            do ipts=1,Npts
              A(ipts-Npts,jpts-Npts,k)=
     &                           buf_rev1(k+nmax*(ipts-1+Npts*(jpts-1)))
            enddo
          enddo
        enddo
      endif

      if (NORTH_INTER.and.EAST_INTER) then
        call MPI_Wait (req(6),status(1,6),ierr)
        do k=1,nmax
          do jpts=1,Npts
            do ipts=1,Npts
              A(Lmmpi+ipts,Mmmpi+jpts,k)=
     &                           buf_rev2(k+nmax*(ipts-1+Npts*(jpts-1)))
            enddo
          enddo
        enddo
      endif

      if (SOUTH_INTER .and. EAST_INTER) then
        call MPI_Wait (req(7),status(1,7),ierr)
        do k=1,nmax
          do jpts=1,Npts
            do ipts=1,Npts
              A(Lmmpi+ipts,jpts-Npts,k)=
     &                           buf_rev3(k+nmax*(ipts-1+Npts*(jpts-1)))
            enddo
          enddo
        enddo
      endif

      if (NORTH_INTER .and. WEST_INTER) then
        call MPI_Wait (req(8),status(1,8),ierr)
        do k=1,nmax
          do jpts=1,Npts
            do ipts=1,Npts
              A(ipts-Npts,Mmmpi+jpts,k)=
     &                           buf_rev4(k+nmax*(ipts-1+Npts*(jpts-1)))
            enddo
          enddo
        enddo
      endif

# if defined AGRIF
        DeAllocate(buf_snd4,  ibuf_sndN,  buf_snd2,
     &     buf_rev4,  ibuf_revN,  buf_rev2,
     &    jbuf_sndW,                      jbuf_sndE,
     &    jbuf_revW,                      jbuf_revE,
     &     buf_snd1,  ibuf_sndS,  buf_snd3,
     &     buf_rev1,  ibuf_revS,  buf_rev3)
# endif

      return
      end

# ifndef MP_3PTS
#  define MP_3PTS
#  include "MessPass3D.F"
#  undef MP_3PTS
# endif

#else
      subroutine MessPass3D_empty
      return
      end
#endif

