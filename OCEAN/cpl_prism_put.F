! $Id: cpl_prism_put.F 1274 2013-09-16 09:24:57Z gcambon $
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
      SUBROUTINE cpl_prism_put (sndtime)
!     !---------------------------------------------------------------------
!     !              ***  ROUTINE cpl_prism_put  ***
!     !
!     ! ** Purpose : - 
!     !           
!     !
!     !----------------------------------------------------------------------
#include "cppdefs.h"
#if defined OA_COUPLING || defined OW_COUPLING
# ifdef OA_MCT
      USE mod_prism             ! OASIS3 prism module
# else
      USE mod_prism_put_proto   ! OASIS3 prism module for snding
# endif
# ifdef AGRIF
      use Agrif_Util
# endif
      IMPLICIT NONE

#include "cppdefs.h"

# include "param.h"
# include "scalars.h"
# include "ocean3d.h"
# include "ocean2d.h"
# include "mpi_roms.h"
      
      INTEGER    :: sndtime, i, level
      DOUBLE PRECISION, DIMENSION(Lmmpi,Mmmpi)  :: snd_field 
      CHARACTER*1 clevel
      
# ifdef AGRIF
      level=Agrif_Fixed()
# else
      level=0
# endif
      write(clevel,'(i1.1)') level
!
      if (level==0) then
         MPI_master_only 
     & write(*,*) 'CPL-ROMS : put for parent at time', sndtime
      else		
         MPI_master_only 
     & write(*,*) 'CPL-ROMS : put for child:', level,
     &  'at time', sndtime
      endif
!      
      DO i = 1, ksnd
!-------------------------------------------------
         if (ssnd_clname(i)=='SRMSSTV'//clevel) then
            snd_field = dble(t(1:Lmmpi,1:Mmmpi,N,nnew,itemp) + 273.15 )         
            call prism_put_proto(ssnd_nid(i), sndtime, 
     &        snd_field, comp_ierror)
            if ( (comp_ierror == 4) .or.
     &           (comp_ierror == 5) .or.
     &           (comp_ierror == 6) .or.
     &           (comp_ierror == 8) .or.
     &           (comp_ierror == 9) ) then
           MPI_master_only write(*,*) 'CPL-ROMS : ',
     & ssnd_clname(i),' sent at time ',sndtime
            endif
         endif
!-------------------------------------------------
!        ! ssh : sea surface height (m)
         if (ssnd_clname(i)=='SRMSSHV'//clevel) then
            snd_field = dble(zeta(1:Lmmpi,1:Mmmpi,nnew))
            call prism_put_proto(ssnd_nid(i), sndtime,
     &        snd_field, comp_ierror)
            if ( (comp_ierror == 4) .or.
     &           (comp_ierror == 5) .or.
     &           (comp_ierror == 6) .or.
     &           (comp_ierror == 8) .or.
     &           (comp_ierror == 9) ) then
           MPI_master_only write(*,*) 'CPL-ROMS : ',
     & ssnd_clname(i),' sent at time ',sndtime
            endif
         endif
!-------------------------------------------------
!        ! uz : sea surface zonal currents (m.s-1)
         if (ssnd_clname(i)=='SRM_UZV'//clevel) then
            snd_field = dble(u(1:Lmmpi,1:Mmmpi,N,nnew))
            call prism_put_proto(ssnd_nid(i), sndtime,
     &        snd_field, comp_ierror)
            if ( (comp_ierror == 4) .or.
     &           (comp_ierror == 5) .or.
     &           (comp_ierror == 6) .or.
     &           (comp_ierror == 8) .or.
     &           (comp_ierror == 9) ) then
           MPI_master_only write(*,*) 'CPL-ROMS : ',
     & ssnd_clname(i),' sent at time ',sndtime
            endif
         endif
!-------------------------------------------------
!        ! vz : sea surface meridional currents (m.s-1)
         if (ssnd_clname(i)=='SRM_VZV'//clevel) then
            snd_field = dble(v(1:Lmmpi,1:Mmmpi,N,nnew))
            call prism_put_proto(ssnd_nid(i), sndtime,
     &        snd_field, comp_ierror)
            if ( (comp_ierror == 4) .or.
     &           (comp_ierror == 5) .or.
     &           (comp_ierror == 6) .or.
     &           (comp_ierror == 8) .or.
     &           (comp_ierror == 9) ) then
           MPI_master_only write(*,*) 'CPL-ROMS : ',
     & ssnd_clname(i),' sent at time ',sndtime
            endif
         endif
!-------------------------------------------------
      END DO
#else 
!---------------------------------------------------------------------------------------
!     Dummy subroutine                                                           NO Coupling
!---------------------------------------------------------------------------------------
#endif 
      END SUBROUTINE cpl_prism_put
      

      
