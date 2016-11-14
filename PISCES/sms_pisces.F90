#include "cppdefs.h"

MODULE sms_pisces   
   !!----------------------------------------------------------------------
   !!                     ***  sms_pisces.F90  ***  
   !! TOP :   PISCES Source Minus Sink variables
   !!----------------------------------------------------------------------
   !! History :   1.0  !  2000-02 (O. Aumont) original code
   !!             3.2  !  2009-04 (C. Ethe & NEMO team) style
   !!----------------------------------------------------------------------

#if defined key_pisces
   !!----------------------------------------------------------------------
   !!   'key_pisces'                                         PISCES model
   !!----------------------------------------------------------------------
   USE par_pisces
   USE ocean2pisces
   USE trc

   IMPLICIT NONE
   PUBLIC

#include "ocean2pisces.h90"

   !!*  Time variables
   INTEGER  ::   numnatp
   INTEGER  ::   nrdttrc           !: ???
   INTEGER  ::   niter1max, niter2max           !: ???
   INTEGER  ::   ndayflxtr         !: ???
   REAL(wp) ::   rfact , rfactr    !: ???
   REAL(wp) ::   rfact2, rfact2r   !: ???
   REAL(wp) ::   xstep             !: ???

   !!*  Biological parameters 
   REAL(wp) ::   part              !: ???
   REAL(wp) ::   rno3              !: ???
   REAL(wp) ::   o2ut              !: ???
   REAL(wp) ::   po4r              !: ???
   REAL(wp) ::   rdenit            !: ???
   REAL(wp) ::   o2nit             !: ???
   REAL(wp) ::   wsbio, wsbio2     !: ???
   REAL(wp) ::   xkmort            !: ???
   REAL(wp) ::   ferat3            !: ???

   !!* Damping 
   LOGICAL  ::   ln_pisdmp         !: relaxation or not of nutrients to a mean value
                                   !: when initialize from a restart file 
   LOGICAL  ::   ln_pisclo         !: Restoring or not of nutrients to initial value
                                   !: on close seas

   REAL(wp), DIMENSION(:), ALLOCATABLE ::   tra_ctl         !: previous trend values


   !!*  Biological fluxes for light
   INTEGER , ALLOCATABLE, SAVE, DIMENSION(:,:)   ::  neln  !: number of T-levels+ 1 in the euphotic layer
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:)   ::  heup  !: euphotic layer depth

   !!*  Biological fluxes for primary production
   REAL(wp), ALLOCATABLE, SAVE,   DIMENSION(:,:)  ::   xksi       !: ???
   REAL(wp), ALLOCATABLE, SAVE,   DIMENSION(:,:)  ::   xksimax    !: ???
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:,:)  ::   xnanono3   !: ???
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:,:)  ::   xdiatno3   !: ???
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:,:)  ::   xnanonh4   !: ???
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:,:)  ::   xdiatnh4   !: ???
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:,:)  ::   xlimphy    !: ???
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:,:)  ::   xlimdia    !: ???
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:,:)  ::   concdfe    !: ???
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:,:)  ::   concnfe    !: ???


   !!*  SMS for the organic matter
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   xfracal    !: ??
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   nitrfac    !: ??
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   xlimbac    !: ??
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   xdiss      !: ??
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   prodcal    !: Calcite production
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   grazing    !: Calcite production

   !!* Variable for chemistry of the CO2 cycle
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   akb3    !: ??
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   ak13    !: ??
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   ak23    !: ??
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   aksp    !: ??
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   akw3    !: 
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   borat   !: 
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   hi      !: 

   !!* Temperature dependancy of SMS terms
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   tgfunc    !: Temp.  dependancy of various biological rates
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   tgfunc2   !: Temp.  dependancy of mesozooplankton rates

   !!* Array used to indicate negative tracer values
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   xnegtr     !: ???

   !! * Shared module variables
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::        sio3eq, fekeq           !: chemistry of Fe and Si
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::        chemc                   !: Solubilities of O2 and CO2

#if defined key_kriest
   !!*  Kriest parameter for aggregation
   REAL(wp) ::   xkr_eta                            !: ???
   REAL(wp) ::   xkr_zeta                           !: ???
   REAL(wp) ::   xkr_massp                          !: ???
   REAL(wp) ::   xkr_mass_min, xkr_mass_max         !: ???
#endif

  REAL(wp), PARAMETER     ::  rtrn = 1.e-20
!  INTEGER :: jip = 15
! INTEGER :: jjp = 25
  INTEGER :: jip = 30
 INTEGER :: jjp = 25
  INTEGER :: jkp = KSURF


   !!* Substitution
#  include "ocean2pisces.h90"
   !!----------------------------------------------------------------------
   !! NEMO/TOP 3.2 , LOCEAN-IPSL (2009) 
   !! $Id: sms_pisces.F90 1830 2010-04-12 13:03:51Z cetlod $ 
   !! Software governed by the CeCILL licence (modipsl/doc/NEMO_CeCILL.txt)
   !!======================================================================   

CONTAINS

  INTEGER FUNCTION sms_pisces_alloc()
      !!----------------------------------------------------------------------
      !!        *** ROUTINE sms_pisces_alloc ***
      !!----------------------------------------------------------------------
      INTEGER ::   ierr(7)        ! Local variables
      !!----------------------------------------------------------------------
      ierr(:) = 0
      !*  Chemistry coef
      ALLOCATE( sio3eq(PRIV_3D_BIOARRAY), fekeq(PRIV_3D_BIOARRAY),  &
         &                           chemc(PRIV_2D_BIOARRAY,2), STAT=ierr(1) )
      !*  Biological fluxes for light
      ALLOCATE( neln(PRIV_2D_BIOARRAY), heup(PRIV_2D_BIOARRAY),  STAT=ierr(2) )
      !
      !*  Biological fluxes for primary production
      ALLOCATE( xksimax(PRIV_2D_BIOARRAY)     , xksi(PRIV_2D_BIOARRAY),       &
         &      xnanono3(PRIV_3D_BIOARRAY), xdiatno3(PRIV_3D_BIOARRAY),       &
         &      xnanonh4(PRIV_3D_BIOARRAY), xdiatnh4(PRIV_3D_BIOARRAY),       &
         &      xlimphy (PRIV_3D_BIOARRAY), xlimdia (PRIV_3D_BIOARRAY),       &
         &      concdfe (PRIV_3D_BIOARRAY), concnfe (PRIV_3D_BIOARRAY),  STAT=ierr(3) )
         !
      !*  SMS for the organic matter
      ALLOCATE( xfracal (PRIV_3D_BIOARRAY), nitrfac(PRIV_3D_BIOARRAY),       &
         &      prodcal(PRIV_3D_BIOARRAY) , grazing(PRIV_3D_BIOARRAY),       &
         &      xlimbac (PRIV_3D_BIOARRAY), xdiss  (PRIV_3D_BIOARRAY),   STAT=ierr(4) )
         !
      !* Variable for chemistry of the CO2 cycle
      ALLOCATE( akb3(PRIV_3D_BIOARRAY)    , ak13  (PRIV_3D_BIOARRAY) ,       &
         &      ak23(PRIV_3D_BIOARRAY)    , aksp  (PRIV_3D_BIOARRAY) ,       &
         &      akw3(PRIV_3D_BIOARRAY)    , borat (PRIV_3D_BIOARRAY) ,       &
         &      hi  (PRIV_3D_BIOARRAY)                          ,   STAT=ierr(5) )
         !
      !* Temperature dependancy of SMS terms
      ALLOCATE( tgfunc(PRIV_3D_BIOARRAY)  , tgfunc2(PRIV_3D_BIOARRAY) ,   STAT=ierr(6) )
         !
      !* Array used to indicate negative tracer values
      ALLOCATE( xnegtr(PRIV_3D_BIOARRAY)  ,                          STAT=ierr(7) )
      !
      sms_pisces_alloc = MAXVAL( ierr )
      !
      IF( sms_pisces_alloc /= 0 )   CALL ctl_warn('sms_pisces_alloc: failed to allocate arrays')
      !
   END FUNCTION sms_pisces_alloc


   SUBROUTINE tracer_stat( kt )
      !!----------------------------------------------------------------------
      !!                    ***  trc_rst_stat  ***
      !!
      !! ** purpose  :   Compute tracers statistics
      !!----------------------------------------------------------------------
      INTEGER, INTENT(in)  :: kt
      INTEGER  :: ji, jj, jk, jn
      REAL(wp) :: ztra, zmin, zmax, zmean, areatot
      REAL(wp), DIMENSION(PRIV_3D_BIOARRAY,jptra)  :: ptra
      REAL(wp), DIMENSION(PRIV_3D_BIOARRAY)        :: zmask, zvol
      !!----------------------------------------------------------------------

      IF( lwp ) THEN
         WRITE(numout,*) 
         WRITE(numout,*) ' TRACER STAT at time-step kt = ', kt
         WRITE(numout,*) 
      ENDIF
      !
      DO jn = 1, jptra
         DO jk = KRANGE
            DO jj = JRANGE
               DO ji = IRANGE
                  ptra(ji,jj,jk,jn) = trn(ji,jj,K,jn)
               ENDDO
            ENDDO
         ENDDO
      ENDDO
      areatot = 0.                                                           ! total volume of the ocean 
      DO jk = KRANGE
         DO jj = JRANGE
            DO ji = IRANGE          ! masked grid volume
               zvol(ji,jj,jk)  = cvol(ji,jj,K)
               zmask(ji,jj,jk) = tmask(ji,jj,K) * tmask_i(ji,jj) 
               areatot         = areatot + zvol(ji,jj,jk)
            ENDDO
        ENDDO
     ENDDO
     IF( lk_mpp )   CALL mpp_sum( areatot )     ! sum over the global domain  

     DO jn = 1, jptra
         ztra = 0.
         DO jk = KRANGE
            DO jj = JRANGE
               DO ji = IRANGE          ! masked grid volume
                  ztra  = ztra + ptra(ji,jj,jk,jn) * zvol(ji,jj,jk) 
               ENDDO
            ENDDO
         ENDDO
         zmin  = MINVAL( ptra(:,:,:,jn), mask= ( zmask(:,:,:) /= 0. ) ) 
         zmax  = MAXVAL( ptra(:,:,:,jn), mask= ( zmask(:,:,:) /= 0. ) ) 
         IF( lk_mpp ) THEN
            CALL mpp_sum( ztra )      ! min over the global domain
            CALL mpp_min( zmin )      ! min over the global domain
            CALL mpp_max( zmax )      ! max over the global domain
         END IF
         zmean  = ztra / areatot
         IF(lwp) WRITE(numout,9000) jn, TRIM( ctrcnm(jn) ), zmean, zmin, zmax
      END DO
      WRITE(numout,*) 
9000  FORMAT(' tracer nb :',i2,'    name :',a10,'    mean :',e18.10,'    min :',e18.10, '    max :',e18.10 )
      !
   END SUBROUTINE tracer_stat

   SUBROUTINE prt_ctl_trc( charout, ltra )

      CHARACTER (len=*),  INTENT(in)           :: charout   ! information about the tab3d array
      CHARACTER (len=*),  INTENT(in), OPTIONAL :: ltra    ! information about the tab3d array
      INTEGER :: ji, jj, jk, jn
      REAL(wp), DIMENSION(PRIV_3D_BIOARRAY,jptra)        :: ztab
      REAL(wp)  :: zsum,  zvctl  


      IF( PRESENT( ltra ) ) THEN
         DO jn = 1, jptra
            DO jk = KRANGE
               DO jj = JRANGE
                  DO ji = IRANGE
                     ztab(ji,jj,jk,jn) = tra(ji,jj,jk,jn) * tmask(ji,jj,K)
                  ENDDO
               ENDDO
            ENDDO
         ENDDO
      ELSE 
         DO jn = 1, jptra
            DO jk = KRANGE
               DO jj = JRANGE
                  DO ji = IRANGE
                     ztab(ji,jj,jk,jn) = trn(ji,jj,K,jn) * tmask(ji,jj,K)
                  ENDDO
               ENDDO
            ENDDO
         ENDDO
      ENDIF

      WRITE(numout,*) charout

      IF( PRESENT( ltra ) ) THEN
          DO jn = 1, jptra
             zvctl  = tra_ctl(jn)
             zsum   = SUM( ztab(:,:,:,jn) )
             IF( lk_mpp ) CALL mpp_sum( zsum )      ! min over the global domain
             WRITE(numout,FMT="(3x,a10,' : ',D23.16)") TRIM(ctrcnm(jn)), zsum-zvctl
             tra_ctl(jn) = zsum
          END DO
       ELSE
          DO jn = 1, jptra
             zvctl  = tra_ctl(jn)
             zsum   = SUM( ztab(:,:,:,jn) )
             IF( lk_mpp ) CALL mpp_sum( zsum )      ! min over the global domain
             WRITE(numout,FMT="(3x,a10,' : ',D23.16)") TRIM(ctrcnm(jn)), zsum
          END DO
      ENDIF

   END SUBROUTINE prt_ctl_trc      

   SUBROUTINE prt_ctl_trc_ini

      ALLOCATE( tra_ctl(jptra) )
      tra_ctl(:) = 0.e0           ! Initialization to zero

   END SUBROUTINE prt_ctl_trc_ini

  SUBROUTINE prt_ctl_trc_info( clinfo )
      !!----------------------------------------------------------------------
      !!                     ***  ROUTINE prt_ctl_trc_info  ***
      !!
      !! ** Purpose : - print information without any computation
      !!----------------------------------------------------------------------
      CHARACTER (len=*), INTENT(in) ::   clinfo      ! information to print
      !! 
      !
   END SUBROUTINE prt_ctl_trc_info



#else
   !!----------------------------------------------------------------------   
   !!  Empty module :                                     NO PISCES model
   !!----------------------------------------------------------------------
#endif
   
END MODULE sms_pisces    
