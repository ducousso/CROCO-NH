! $Id: cpl_prism_getlistvals.F 1507 2014-03-20 09:36:13Z serena $
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
      SUBROUTINE cpl_prism_getlistvals(nmax,prcv_clname,psnd_clname,
     &                                 krcvn,ksndn,oasis_runtime)    
      
!     !---------------------------------------------------------------------
!     !              ***  ROUTINE cpl_prism_getlistvals  ***
!     !
!     ! ** Purpose : - Read the namcouple file 
!     !           and detect the fields that will be send and received by WRF
!     !
!     !----------------------------------------------------------------------
      
#include "cppdefs.h"

#if defined OA_COUPLING || defined OW_COUPLING
#ifdef AGRIF
      use Agrif_Util
#endif
     
      CHARACTER*64,DIMENSION(nmax),INTENT(INOUT) :: prcv_clname ! Rcvd Coupling fields
      CHARACTER*64,DIMENSION(nmax),INTENT(INOUT) :: psnd_clname ! Sent Coupling fields
      INTEGER, INTENT(INOUT) :: krcvn,ksndn
      INTEGER, INTENT(INOUT) :: oasis_runtime ! Oasis total simulated time (secs)

!     Local variables
      INTEGER :: nulin,nfields,jf,nbtrans,fcode,fcoupling,it
      INTEGER :: level
      CHARACTER*80 clline
      CHARACTER*9 clword,clstring,clnfields,clruntime
      CHARACTER*64 SNDCODE,RCVCODE,cvarcode
      CHARACTER*1 clevel

#ifdef AGRIF
      level=Agrif_Fixed()
#else
      level=0
#endif
      write(clevel,'(i1.1)') level
!      print *,'getlistvals'
!
!     Initialisation
      krcvn = 0
      ksndn = 0
!     
!     1. Read OASIS-MCT namcouple file
      clstring = ' $STRINGS'
      clnfields= ' $NFIELDS'
      clruntime= ' $RUNTIME'
      nulin=78
      OPEN (file='namcouple', unit=nulin, 
     $     form='formatted', status='old', err=199)
!     
!     2. Get the total number of fields to exchange
 220  CONTINUE
      read(UNIT = nulin,FMT = 1001,END = 110)clword
      IF (clword .NE. clnfields) GOTO 220
 221  CONTINUE
      read(UNIT = nulin,FMT = 1002)clline
      IF (clline(1:1) .EQ. '#') GOTO 221
      read(clline,*)nfields
!     
!     3. Get the total simulated time for the actual run (seconds)
 230  CONTINUE
      read(UNIT = nulin,FMT = 1001,END = 110)clword
      IF (clword .NE. clruntime) GOTO 230
 231  CONTINUE
      read(UNIT = nulin,FMT = 1002)clline
      IF (clline(1:1) .EQ. '#') GOTO 231
      read(clline,*)oasis_runtime
!     
!     4. Get field informations
 240  CONTINUE
      read(UNIT = nulin,FMT = 1001,END = 110)clword
      IF (clword .NE. clstring) GOTO 240
!     
      do jf=1,nfields
 241    CONTINUE
        read(UNIT = nulin,FMT = 1002,END = 110)clline  
        IF (clline(1:1) .EQ. '#') GOTO 241  
        read(clline,*)SNDCODE,RCVCODE,fcode,fcoupling,nbtrans
!        
        RCVCODE=TRIM(ADJUSTL(RCVCODE)) 
        SNDCODE=TRIM(ADJUSTL(SNDCODE))              
!        
        if (nbtrans.gt.0) then
          do it=1,nbtrans+3
            read(UNIT = nulin,FMT = 1002,END = 110)clline
          enddo
        endif
!     
!     5. Test field codes
!------------------
!     Variables sent by ROMS
!-----------------
!     SST
!------------------
        cvarcode=TRIM(ADJUSTL('SRMSSTV'//clevel))
        if (SNDCODE.eq.cvarcode) then
          ksndn=ksndn+1
          psnd_clname(ksndn)=cvarcode
          goto 111
        endif
!------------------    
!     add OW_COUPLING
!------------------
!     SSH
!------------------
        cvarcode=TRIM(ADJUSTL('SRMSSHV'//clevel))
        if (SNDCODE.eq.cvarcode) then
          ksndn=ksndn+1
          psnd_clname(ksndn)=cvarcode
          goto 111
        endif
!-----------------
!     u
!------------------     
        cvarcode=TRIM(ADJUSTL('SRM_UZV'//clevel))
        if (SNDCODE.eq.cvarcode) then
          ksndn=ksndn+1
          psnd_clname(ksndn)=cvarcode
          goto 111
        endif
!-----------------
!     v
!------------------
        cvarcode=TRIM(ADJUSTL('SRM_VZV'//clevel))
        if (SNDCODE.eq.cvarcode) then
          ksndn=ksndn+1
          psnd_clname(ksndn)=cvarcode
          goto 111
        endif
!------------------
!     Variable received by ROMS
!------------------
!     Evaporation-Precipitation
!------------------ 
        cvarcode=TRIM(ADJUSTL('RRMEVPR'//clevel))
        if (RCVCODE.eq.cvarcode) then
          krcvn=krcvn+1
          prcv_clname(krcvn)=cvarcode
          goto 111
        endif
!------------------
!     Solar heat flux
!------------------     
        cvarcode=TRIM(ADJUSTL('RRMSRFL'//clevel))
        if (RCVCODE.eq.cvarcode) then
          krcvn=krcvn+1
          prcv_clname(krcvn)=cvarcode
          goto 111
        endif
!------------------
!     Total heat flux
!------------------     
        cvarcode=TRIM(ADJUSTL('RRMSTFL'//clevel))
        if (RCVCODE.eq.cvarcode) then
          krcvn=krcvn+1
          prcv_clname(krcvn)=cvarcode
          goto 111
        endif
!------------------
!     wind stress : x-component
!------------------     
        cvarcode=TRIM(ADJUSTL('RRMTAUX'//clevel))
        if (RCVCODE.eq.cvarcode) then
          krcvn=krcvn+1
          prcv_clname(krcvn)=cvarcode
          goto 111
        endif
!------------------
!     wind stress : y-component
!------------------      
        cvarcode=TRIM(ADJUSTL('RRMTAUY'//clevel))
        if (RCVCODE.eq.cvarcode) then
          krcvn=krcvn+1
          prcv_clname(krcvn)=cvarcode
          goto 111
        endif
!------------------
!     add OW_COUPLING
!------------------
!     mean wave period
!------------------
        cvarcode=TRIM(ADJUSTL('RRMT0M1'//clevel))
        if (RCVCODE.eq.cvarcode) then
          krcvn=krcvn+1
          prcv_clname(krcvn)=cvarcode
          goto 111
        endif
!------------------
!     wave height
!------------------     
        cvarcode=TRIM(ADJUSTL('RRM__HS'//clevel))
        if (RCVCODE.eq.cvarcode) then
          krcvn=krcvn+1
          prcv_clname(krcvn)=cvarcode
          goto 111
        endif
!------------------
!     cos of mean wave direction
!------------------     
        cvarcode=TRIM(ADJUSTL('RRMCDIR'//clevel))
        if (RCVCODE.eq.cvarcode) then
          krcvn=krcvn+1
          prcv_clname(krcvn)=cvarcode
          goto 111
        endif
!------------------
!     sin of mean wave direction
!------------------     
        cvarcode=TRIM(ADJUSTL('RRMSDIR'//clevel))
        if (RCVCODE.eq.cvarcode) then
          krcvn=krcvn+1
          prcv_clname(krcvn)=cvarcode
          goto 111
        endif
!------------------     
!   end add OW_COUPLING  
!------------------
 111    CONTINUE
        
      enddo	
      
      
 110  CONTINUE
      
      CLOSE(nulin)              !Close namcouple file
      
 199  CONTINUE
      
 1001 FORMAT(A9)
 1002 FORMAT(A80)


#else 
!---------------------------------------------------------------------------------------
!     Dummy subroutine                                                           NO Coupling
!---------------------------------------------------------------------------------------
#endif 
      END SUBROUTINE cpl_prism_getlistvals
      
