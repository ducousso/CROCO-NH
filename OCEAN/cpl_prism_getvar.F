! $Id: cpl_prism_getvar.F 1574 2014-07-01 15:13:38Z gcambon $
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
      SUBROUTINE cpl_prism_getvar(prcv_id, prcv_name, tab_t_tmp, rcvtime, iok)    
      
!     !---------------------------------------------------------------------
!     !              ***  ROUTINE cpl_prism_getvar  ***
!     !
!     ! ** Purpose : - 
!     !           
!     !
!     !----------------------------------------------------------------------
      
#include "cppdefs.h"
#if defined OA_COUPLING || defined OW_COUPLING
#ifdef OA_MCT
      USE mod_prism             ! OASIS3 prism module
#else 
      USE mod_prism_get_proto   ! OASIS3 prism module for receiving
#endif

#include "param.h"
#include "scalars.h"

      CHARACTER*64 :: prcv_name
      INTEGER :: prcv_id, rcvtime, comp_ierror, iok
      DOUBLE PRECISION, DIMENSION(Lmmpi,Mmmpi)  :: rcv_field 
      real,dimension(GLOBAL_2D_ARRAY) :: tab_t_tmp
!
!     1.0. Get rcv_field from OASIS
      iok = 0
!      print *,'name of received field = ',prcv_name
!      print *,'received time = ',rcvtime
      call prism_get_proto(prcv_id, rcvtime,
     &        rcv_field(1:Lmmpi,1:Mmmpi), comp_ierror)

!      print *,'comp_ierror=',comp_ierror
      if ( (comp_ierror == 3) .or. 
     &     (comp_ierror == 10) .or.  
     &     (comp_ierror == 11) .or. 
     &     (comp_ierror == 12) .or.  
     &     (comp_ierror == 13) ) then

         MPI_master_only write(*,*) 'CPL-ROMS : ',
     & prcv_name,' received at time ',rcvtime
!
         iok = comp_ierror    

!        1.a. Get the fields on the RHO grid
         tab_t_tmp(1:Lmmpi,1:Mmmpi) = real (rcv_field(1:Lmmpi,1:Mmmpi))
!     
!        1.b. Blend the borders of the fields
         if (WESTERN_EDGE) then
             tab_t_tmp(0,:)=2.*tab_t_tmp(1,:)-tab_t_tmp(2,:)
         endif
         if (EASTERN_EDGE) then
             tab_t_tmp(Lmmpi+1,:)=2.*tab_t_tmp(Lmmpi,:)-tab_t_tmp(Lmmpi-1,:)
         endif
         if (SOUTHERN_EDGE) then
              tab_t_tmp(:,0)=2.*tab_t_tmp(:,1)-tab_t_tmp(:,2)
         endif
         if (NORTHERN_EDGE) then
              tab_t_tmp(:,Mmmpi+1)=2.*tab_t_tmp(:,Mmmpi)-tab_t_tmp(:,Mmmpi-1)
         endif


         WRITE(333,*) '****************'
         WRITE(333,*) 'prism_get_proto: Incoming ', prcv_name
         WRITE(333,*) 'prism_get_proto: ivarid '  , prcv_id
         WRITE(333,*) 'prism_get_proto:   kstep', rcvtime
         WRITE(333,*) 'prism_get_proto:   info ', comp_ierror
         WRITE(333,*) '     - Minimum value is ', MINVAL(rcv_field)
         WRITE(333,*) '     - Maximum value is ', MAXVAL(rcv_field)
         WRITE(333,*) '     -     Sum value is ', SUM(rcv_field)
         WRITE(333,*) '****************'

         endif
!     

#else 
!---------------------------------------------------------------------------------------
!     Dummy subroutine                                                           NO Coupling
!---------------------------------------------------------------------------------------
#endif 
      END SUBROUTINE cpl_prism_getvar
      
