#include "cppdefs.h"

MODULE p4zsed
   !!======================================================================
   !!                         ***  MODULE p4sed  ***
   !! TOP :   PISCES Compute loss of organic matter in the sediments
   !!======================================================================
   !! History :   1.0  !  2004-03 (O. Aumont) Original code
   !!             2.0  !  2007-12  (C. Ethe, G. Madec)  F90
   !!----------------------------------------------------------------------
#if defined key_pisces
   !!----------------------------------------------------------------------
   !!   'key_pisces'                                       PISCES bio-model
   !!----------------------------------------------------------------------
   !!   p4z_sed        :  Compute loss of organic matter in the sediments
   !!   p4z_sbc        :  Read and interpolate time-varying nutrients fluxes
   !!   p4z_sed_init   :  Initialization of p4z_sed
   !!----------------------------------------------------------------------
   USE sms_pisces
   USE p4zint
   USE p4zopt
   USE p4zsink
   USE p4zrem
   USE p4zlim


   IMPLICIT NONE
   PRIVATE

   PUBLIC   p4z_sed   
   PUBLIC   p4z_sed_alloc   
   PUBLIC   p4z_sed_init   
   PUBLIC   p4z_sed_nam   

   !!* Substitution
#  include "ocean2pisces.h90"
#  include "top_substitute.h90"

   !! * Shared module variables
   LOGICAL, PUBLIC ::    &
     ln_dustfer  = .FALSE.      ,  &  !:
     ln_river    = .FALSE.      ,  &  !:
     ln_ndepo    = .FALSE.      ,  &  !:
     ln_sedinput = .FALSE.            !:

   REAL(wp), PUBLIC ::   &
     sedfeinput = 1.E-9   ,  &  !:
     dustsolub  = 0.014         !:

   !! * Module variables
!   INTEGER ::                   &
!     ryyss,                     &  !: number of seconds per year
!     rmtss                         !: number of seconds per month

   INTEGER ::                   &
      numdust,                  &  !: logical unit for surface fluxes data
      nflx1 , nflx2,            &  !: first and second record used
      nflx11, nflx12      ! ???
   REAL(wp), DIMENSION(:,:,:), PUBLIC, ALLOCATABLE, SAVE ::    &  !:
     dustmo                                !: 2 consecutive set of dust fields 
   REAL(wp), DIMENSION(:,:), ALLOCATABLE, SAVE   ::    &
     rivinp, cotdep, nitdep, dust, sidep
   REAL(wp), DIMENSION(:,:,:), ALLOCATABLE, SAVE  ::   &
     ironsed, nitrpot, irondep
   REAL(wp) :: sumdepsi, rivalkinput, rivpo4input, nitdepinput

   !!----------------------------------------------------------------------
   !! NEMO/TOP 2.0 , LOCEAN-IPSL (2007) 
   !! $Header:$ 
   !! Software governed by the CeCILL licence (modipsl/doc/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------

CONTAINS

   SUBROUTINE p4z_sed(kt, jnt)
      !!---------------------------------------------------------------------
      !!                     ***  ROUTINE p4z_sed  ***
      !!
      !! ** Purpose :   Compute loss of organic matter in the sediments. This
      !!              is by no way a sediment model. The loss is simply 
      !!              computed to balance the inout from rivers and dust
      !!
      !! ** Method  : - ???
      !!---------------------------------------------------------------------
      INTEGER, INTENT(in) ::   kt, jnt ! ocean time step
      INTEGER  ::   ji, jj, jk
!      INTEGER  ::   ikt
#if ! defined key_sed
      REAL(wp) ::   zsumsedsi, zsumsedpo4, zsumsedcal
#endif
      REAL(wp) ::   zconctmp , znitrpottot
      REAL(wp) ::   zlim, zconctmp2, zfact, zmsk
      REAL(wp) :: zrfact2
      CHARACTER (len=25) :: charout
      !!---------------------------------------------------------------------

      IF( (jnt == 1) .and. ( ln_dustfer ) )  CALL p4z_sbc( kt )

      ! Iron and Si deposition at the surface
      ! -------------------------------------
      IF( ln_dustfer ) THEN
         DO jj = JRANGE
            DO ji = IRANGE
               irondep(ji,jj,1) = ( dustsolub * dust(ji,jj) &
                  &     / ( 55.85 * rmtss ) + 3.e-10 / ryyss )   &
               &        * rfact2 / fse3t(ji,jj,KSURF)
               sidep  (ji,jj)   = 8.8 * 0.075 * dust(ji,jj) &
               &     * rfact2 / ( fse3t(ji,jj,KSURF) * 28.1 * rmtss )
               !
               tra(ji,jj,1,jpsil) = tra(ji,jj,1,jpsil) + sidep (ji,jj)
               tra(ji,jj,1,jpfer) = tra(ji,jj,1,jpfer) + irondep(ji,jj,1)
            END DO
          END DO

          ! Iron solubilization of particles in the water column
          ! ----------------------------------------------------
          DO jk = KRANGEL
             DO jj = JRANGE
                DO ji = IRANGE
                   irondep(ji,jj,jk) = dust(ji,jj) &
             &            / ( 10. * 55.85 * rmtss ) * rfact2 * 1.e-4
                   tra(ji,jj,jk,jpfer) = tra(ji,jj,jk,jpfer) + irondep(ji,jj,jk)
                END DO
             END DO
         END DO
      ENDIF

      ! Add the external input of nutrients, carbon and alkalinity
      ! ----------------------------------------------------------
      IF( ln_river ) THEN
         DO jj = JRANGE
            DO ji = IRANGE
               tra(ji,jj,1,jppo4) = tra(ji,jj,1,jppo4) + rivinp(ji,jj) * rfact2 
               tra(ji,jj,1,jpno3) = tra(ji,jj,1,jpno3) + rivinp(ji,jj) * rfact2
               tra(ji,jj,1,jpfer) = tra(ji,jj,1,jpfer) + rivinp(ji,jj) * 3.e-5 * rfact2
               tra(ji,jj,1,jpsil) = tra(ji,jj,1,jpsil) + cotdep(ji,jj)   * rfact2 / 6.
               tra(ji,jj,1,jpdic) = tra(ji,jj,1,jpdic) + rivinp(ji,jj) * 2.631 * rfact2
               tra(ji,jj,1,jptal) = tra(ji,jj,1,jptal) &
                  &                + ( cotdep(ji,jj) - rno3 * rivinp(ji,jj) )  * rfact2
            END DO
         END DO
      ENDIF

      IF( ln_ndepo ) THEN
         DO jj = JRANGE
            DO ji = IRANGE
               tra(ji,jj,1,jpno3) = tra(ji,jj,1,jpno3) + nitdep(ji,jj) * rfact2
               tra(ji,jj,1,jptal) = tra(ji,jj,1,jptal) - rno3 * nitdep(ji,jj) * rfact2
            END DO
         END DO
      ENDIF

      ! Add the external input of iron which is 3D distributed
      ! (dust, river and sediment mobilization)
      ! ------------------------------------------------------

      IF( ln_sedinput ) THEN     
         DO jk = KRANGE
            DO jj = JRANGE
               DO ji = IRANGE
                  tra(ji,jj,jk,jpfer) = tra(ji,jj,jk,jpfer) + ironsed(ji,jj,jk) * rfact2
              END DO
            END DO
         END DO
      ENDIF


#if ! defined key_sed
      ! Initialisation of variables used to compute Sinking Speed
      zsumsedsi  = 0.e0
      zsumsedpo4 = 0.e0
      zsumsedcal = 0.e0

      ! Loss of biogenic silicon, Caco3 organic carbon in the sediments. 
      ! First, the total loss is computed.
      ! The factor for calcite comes from the alkalinity effect
      ! -------------------------------------------------------------
      DO jj = JRANGE
         DO ji = IRANGE
            zfact = e1t(ji,jj) * e2t(ji,jj) / rday * tmask_i(ji,jj)
# if defined key_kriest
            zsumsedsi  = zsumsedsi  + zfact *  trn(ji,jj,KSED,jpdsi) * wscal (ji,jj,ikt)
            zsumsedpo4 = zsumsedpo4 + zfact *  trn(ji,jj,KSED,jppoc) * wsbio3(ji,jj,ikt)
# else
            zsumsedsi  = zsumsedsi  + zfact *  trn(ji,jj,KSED,jpdsi) * wsbio4(ji,jj,ikt)
            zsumsedpo4 = zsumsedpo4 + zfact *( trn(ji,jj,KSED,jpgoc) * wsbio4(ji,jj,ikt)   &
               &       + trn(ji,jj,KSED,jppoc) * wsbio3(ji,jj,ikt) )
# endif
            zsumsedcal = zsumsedcal + zfact *  trn(ji,jj,KSED,jpcal) * wscal (ji,jj,ikt) * 2.e0
         END DO
      END DO

      IF( lk_mpp ) THEN
         CALL mpp_sum( zsumsedsi  )   ! sums over the global domain
         CALL mpp_sum( zsumsedcal )   ! sums over the global domain
         CALL mpp_sum( zsumsedpo4 )   ! sums over the global domain
      ENDIF

#endif

      ! Then this loss is scaled at each bottom grid cell for
      ! equilibrating the total budget of silica in the ocean.
      ! Thus, the amount of silica lost in the sediments equal
      ! the supply at the surface (dust+rivers)
      ! ------------------------------------------------------

      DO jj = JRANGE
         DO ji = IRANGE
            zconctmp = trn(ji,jj,KSED,jpdsi) * xstep / fse3t(ji,jj,KSED)   &
# if ! defined key_kriest
     &             * wscal (ji,jj,ikt)
# else
     &             * wsbio4(ji,jj,ikt)
# endif
            tra(ji,jj,ikt,jpdsi) = tra(ji,jj,ikt,jpdsi) - zconctmp

#if ! defined key_sed
            tra(ji,jj,ikt,jpsil) = tra(ji,jj,ikt,jpsil) + zconctmp   &
             &      * 0.98
!             &      * ( 1.- ( sumdepsi + rivalkinput / ryyss / 6. ) / zsumsedsi )
#endif
         END DO
      END DO

      DO jj = JRANGE
         DO ji = IRANGE
            zconctmp = trn(ji,jj,KSED,jpcal) * wscal(ji,jj,ikt) * xstep / fse3t(ji,jj,KSED)
            tra(ji,jj,ikt,jpcal) = tra(ji,jj,ikt,jpcal) - zconctmp

#if ! defined key_sed
            tra(ji,jj,ikt,jptal) = tra(ji,jj,ikt,jptal) + zconctmp   &
               &   * 0.85 * 2.0
!               &   * ( 1.- ( rivalkinput / ryyss ) / zsumsedcal ) * 2.e0
            tra(ji,jj,ikt,jpdic) = tra(ji,jj,ikt,jpdic) + zconctmp   &
               &   * 0.85
!               &   * ( 1.- ( rivalkinput / ryyss ) / zsumsedcal )
#endif
         END DO
      END DO

      DO jj = JRANGE
         DO ji = IRANGE
            zfact = xstep / fse3t(ji,jj,KSED)
# if ! defined key_kriest
            zconctmp  = trn(ji,jj,KSED,jpgoc)
            zconctmp2 = trn(ji,jj,KSED,jppoc)
            tra(ji,jj,ikt,jpgoc) = tra(ji,jj,ikt,jpgoc) - zconctmp  * wsbio4(ji,jj,ikt) * zfact
            tra(ji,jj,ikt,jppoc) = tra(ji,jj,ikt,jppoc) - zconctmp2 * wsbio3(ji,jj,ikt) * zfact
#if ! defined key_sed
            tra(ji,jj,ikt,jpdoc) = tra(ji,jj,ikt,jpdoc)    &
            &      + ( zconctmp  * wsbio4(ji,jj,ikt) + zconctmp2 * wsbio3(ji,jj,ikt) ) * zfact   &
            &       * 0.92
!            &      * ( 1.- rivpo4input / (ryyss * zsumsedpo4 ) )
#endif
            tra(ji,jj,ikt,jpbfe) = tra(ji,jj,ikt,jpbfe) - trn(ji,jj,KSED,jpbfe) * wsbio4(ji,jj,ikt) * zfact
            tra(ji,jj,ikt,jpsfe) = tra(ji,jj,ikt,jpsfe) - trn(ji,jj,KSED,jpsfe) * wsbio3(ji,jj,ikt) * zfact

# else
            zconctmp  = trn(ji,jj,KSED,jpnum)
            zconctmp2 = trn(ji,jj,KSED,jppoc)
            tra(ji,jj,ikt,jpnum) = tra(ji,jj,ikt,jpnum) - zconctmp  * wsbio4(ji,jj,ikt) * zfact
            tra(ji,jj,ikt,jppoc) = tra(ji,jj,ikt,jppoc) - zconctmp2 * wsbio3(ji,jj,ikt) * zfact
#if ! defined key_sed
            tra(ji,jj,ikt,jpdoc) = tra(ji,jj,ikt,jpdoc)    &
            &      + ( zconctmp2 * wsbio3(ji,jj,ikt) ) * zfact &
            &       * 0.92
!            &      * ( 1.- rivpo4input / (ryyss * zsumsedpo4 ) )
#endif
            tra(ji,jj,ikt,jpsfe) = tra(ji,jj,ikt,jpsfe) - trn(ji,jj,KSED,jpsfe) * wsbio3(ji,jj,ikt) * zfact

# endif
         END DO
      END DO


      ! Potential nitrogen fixation dependant on temperature and iron
      ! -------------------------------------------------------------

      DO jk = KRANGE
         DO jj = JRANGE
            DO ji = IRANGE
               zlim = ( 1.- xnanono3(ji,jj,jk) - xnanonh4(ji,jj,jk) )
               IF( zlim <= 0.2 )   zlim = 0.01
               nitrpot(ji,jj,jk) = MAX( 0.e0, ( 0.6 * tgfunc(ji,jj,jk) - 2.15 ) / rday )   &
               &                  * zlim * rfact2 * trn(ji,jj,K,jpfer)   &
               &                  / ( conc3 + trn(ji,jj,K,jpfer) ) &
               &                  * ( 1.- EXP( -etot(ji,jj,jk) / 50.) )
            END DO
         END DO 
      END DO

      znitrpottot = 0.e0
      DO jk = KRANGE
         DO jj = JRANGE
            DO ji = IRANGE
               znitrpottot = znitrpottot + nitrpot(ji,jj,jk) * cvol(ji,jj,K)
            END DO
         END DO
      END DO

      IF( lk_mpp )   CALL mpp_sum( znitrpottot )  ! sum over the global domain

      ! Nitrogen change due to nitrogen fixation
      ! ----------------------------------------

      DO jk = KRANGE
         DO jj = JRANGE
            DO ji = IRANGE
               zfact = nitrpot(ji,jj,jk) * 1.e-7
               tra(ji,jj,jk,jpnh4) = tra(ji,jj,jk,jpnh4) + zfact
               tra(ji,jj,jk,jpoxy) = tra(ji,jj,jk,jpoxy) + zfact   * o2nit
               tra(ji,jj,jk,jppo4) = tra(ji,jj,jk,jppo4) + 30./ 46.* zfact
            END DO
         END DO
      END DO

#if defined key_trc_diaadd
        zrfact2 = 1.e+3 * rfact2r
        DO jj = JRANGE
           DO ji = IRANGE
              zmsk = zrfact2 * fse3t(ji,jj,KSURF) * tmask(ji,jj,KSURF)
              trc2d(ji,jj,jp_irondep)  = irondep(ji,jj,1) * zmsk                ! iron deposition      
              trc2d(ji,jj,jp_nfix   )  = nitrpot(ji,jj,1) * zmsk * 1.e-7        ! nitrogen fixation at surface
           END DO
        END DO

        DO jk = KRANGE
           DO jj = JRANGE
              DO ji = IRANGE
                 zmsk = zrfact2 * fse3t(ji,jj,K) * tmask(ji,jj,K)
                 trc3d(ji,jj,K,jp_nfixo2 ) = nitrpot(ji,jj,jk) * zmsk * 1.e-7 * o2nit  ! O2 production by Nfix
             END DO
           END DO
        ENDDO

# endif
      !
       IF(ln_ctl)   THEN  ! print mean trends (used for debugging)
         WRITE(charout, FMT="('sed ')")
         CALL prt_ctl_trc_info(charout)
         CALL prt_ctl_trc( charout, ltra='tra')
!         CALL prt_ctl_trc(tab4d=trn, mask=tmask, clinfo=ctrcnm)
       ENDIF

   END SUBROUTINE p4z_sed

   INTEGER FUNCTION p4z_sed_alloc()
      !!----------------------------------------------------------------------
      !!                     ***  ROUTINE p4z_sed_alloc  ***
      !!----------------------------------------------------------------------
      INTEGER ::   ierr(5)        ! Local variables

      ierr(:) = 0
      !
#ifdef NEMO
      ALLOCATE( dust(PRIV_2D_BIOARRAY), dustmo(PRIV_2D_BIOARRAY,2), STAT= ierr(1) )
#else
      ALLOCATE( dust(PRIV_2D_BIOARRAY), STAT= ierr(1) )
#endif
      ALLOCATE( irondep(PRIV_3D_BIOARRAY),sidep(PRIV_2D_BIOARRAY), STAT= ierr(2) )
      ALLOCATE( rivinp(PRIV_2D_BIOARRAY), cotdep(PRIV_2D_BIOARRAY), STAT=ierr(3) )
      ALLOCATE( nitdep(PRIV_2D_BIOARRAY), STAT=ierr(4) )
      ALLOCATE( nitrpot(PRIV_3D_BIOARRAY), ironsed(PRIV_3D_BIOARRAY), STAT=ierr(5) )
      !
      p4z_sed_alloc = MAXVAL( ierr )
      !
      IF( p4z_sed_alloc /= 0 )   CALL ctl_warn('p4z_sed_alloc: failed to allocate arrays')
      !
   END FUNCTION p4z_sed_alloc

# if defined NEMO

   SUBROUTINE p4z_sbc(kt)

      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE p4z_sbc  ***
      !!
      !! ** Purpose :   Read and interpolate the external sources of 
      !!                nutrients
      !!
      !! ** Method  :   Read the files and interpolate the appropriate variables
      !!
      !! ** input   :   external netcdf files
      !!
      !!----------------------------------------------------------------------
      !! * arguments
      INTEGER, INTENT( in  ) ::   kt   ! ocean time step
      !! * Local declarations
      INTEGER ::   &
         imois, imois2,       &  ! temporary integers
         i15  , iman             !    "          "
      REAL(wp) ::   &
         zxy                     !    "         "


      !!---------------------------------------------------------------------

      ! Initialization
      ! --------------

      i15 = nday / 16
      iman  = INT( raamo )
      imois = nmonth + i15 - 1
      IF( imois == 0 ) imois = iman
      imois2 = nmonth

      ! 1. first call kt=nit000
      ! -----------------------

      IF( kt == nit000 ) THEN
         ! initializations
         nflx1  = 0
         nflx11 = 0
         ! open the file
         IF(lwp) THEN
            WRITE(numout,*) ' '
            WRITE(numout,*) ' **** Routine p4z_sbc'
         ENDIF
         CALL iom_open ( 'dust.orca.nc', numdust )
      ENDIF


     ! Read monthly file
      ! ----------------

      IF( kt == nit000 .OR. imois /= nflx1 ) THEN

         ! Calendar computation

         ! nflx1 number of the first file record used in the simulation
         ! nflx2 number of the last  file record

         nflx1 = imois
         nflx2 = nflx1+1
         nflx1 = MOD( nflx1, iman )
         nflx2 = MOD( nflx2, iman )
         IF( nflx1 == 0 )   nflx1 = iman
         IF( nflx2 == 0 )   nflx2 = iman
         IF(lwp) WRITE(numout,*) 'first record file used nflx1 ',nflx1
         IF(lwp) WRITE(numout,*) 'last  record file used nflx2 ',nflx2

         ! Read monthly fluxes data

         ! humidity
         CALL iom_get ( numdust, jpdom_data, 'dust', dustmo(:,:,1), nflx1 )
         CALL iom_get ( numdust, jpdom_data, 'dust', dustmo(:,:,2), nflx2 )


      ENDIF

     ! 3. at every time step interpolation of fluxes
      ! ---------------------------------------------

      zxy = FLOAT( nday + 15 - 30 * i15 ) / 30
      dust(:,:) = ( (1.-zxy) * dustmo(:,:,1) + zxy * dustmo(:,:,2) )

      IF( kt == nitend ) CALL iom_close (numdust)

#else

   SUBROUTINE p4z_sbc(kt)

      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE p4z_sbc  ***
      !!
      !! ** Purpose :   Read and interpolate the external sources of 
      !!                nutrients
      !!
      !! ** Method  :   Read the files and interpolate the appropriate variables
      !!
      !! ** input   :   external netcdf files
      !!
      !!----------------------------------------------------------------------
      !! * arguments
      INTEGER, INTENT( in  ) ::   kt   ! ocean time step
      !
      INTEGER :: ji, jj, jk
      INTEGER, PARAMETER :: jpmois = 12
      INTEGER :: irec1, irec2, i15
      INTEGER :: nyear, nday, nmonth
      REAL    :: zpdtan, zpdtmo, zdemi, zt
      REAL    :: zxy, zjulian, zsec


      zpdtan = ryyss / rdt
      zpdtmo = zpdtan / float( jpmois )
      zdemi  = zpdtmo / 2.
      zt     = ( float( kt ) + zdemi) / zpdtmo
      

      !  recherche de l'indice des enregistrements
      !  du modele dynamique encadrant le pas de temps kt.
      !  --------------------------------------------------
      irec1 = zt - float(int ( zt ) )
      irec1 = int( zt )
      irec2 = irec1 + 1
      irec1 = MOD( irec1, jpmois )
      IF ( irec1 == 0 ) irec1 = jpmois
      irec2 = MOD( irec2, jpmois )
      IF ( irec2 == 0 ) irec2 = jpmois


      !
      ! Interpolation of dust deposition
      ! --------------------------------
      zjulian = FLOAT( nday_year )
      CALL ju2ymds( zjulian, nyear, nmonth, nday, zsec)

      i15 = nday_year / 16
      zxy = FLOAT( nday_year + 15 - 30 * i15 ) / 30
      DO jj = JRANGE
         DO ji = IRANGE
            dust(ji,jj) = ( 1. - zxy ) * dustmo(ji,jj,irec1)  &
            &                  + zxy   * dustmo(ji,jj,irec2)
        END DO
      END DO

#endif

   END SUBROUTINE p4z_sbc

   SUBROUTINE p4z_sed_nam

      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE p4z_sed_init  ***
      !!
      !! ** Purpose :   Initialization of the external sources of nutrients
      !!
      !! ** Method  :   Read the files and compute the budget
      !!      called at the first timestep (nittrc000)
      !!
      !! ** input   :   external netcdf files
      !!
      !!----------------------------------------------------------------------
      NAMELIST/nampissed/ ln_dustfer, ln_river, ln_ndepo, ln_sedinput, sedfeinput, dustsolub


      REWIND( numnatp )                     ! read numnatp
      READ  ( numnatp, nampissed )

      IF(lwp) THEN
         WRITE(numout,*) ' '
         WRITE(numout,*) ' Namelist : nampissed '
         WRITE(numout,*) ' ~~~~~~~~~~~~~~~~~ '
         WRITE(numout,*) '    Dust input from the atmosphere           ln_dustfer  = ', ln_dustfer
         WRITE(numout,*) '    River input of nutrients                 ln_river    = ', ln_river
         WRITE(numout,*) '    Atmospheric deposition of N              ln_ndepo    = ', ln_ndepo
         WRITE(numout,*) '    Fe input from sediments                  ln_sedinput = ', ln_sedinput
         WRITE(numout,*) '    Coastal release of Iron                  sedfeinput  =', sedfeinput
         WRITE(numout,*) '    Solubility of the dust                   dustsolub   =', dustsolub
      ENDIF


   END SUBROUTINE p4z_sed_nam

   SUBROUTINE p4z_sed_init

      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE p4z_sed_init  ***
      !!
      !! ** Purpose :   Initialization of the external sources of nutrients
      !!
      !! ** Method  :   Read the files and compute the budget
      !!      called at the first timestep (nittrc000)
      !!
      !! ** input   :   external netcdf files
      !!
      !!----------------------------------------------------------------------

      INTEGER ::   ji, jj, jk, jm
      INTEGER , PARAMETER ::   jpmois = 12, jpan = 1
      INTEGER :: numriv, numbath, numdep


      REAL(wp) ::   zcoef
      REAL(wp) ::   expide, denitide,zmaskt
      REAL(wp) , DIMENSION(PRIV_2D_BIOARRAY)     ::   riverdoc, river, ndepo
      REAL(wp) , DIMENSION(PRIV_3D_BIOARRAY) ::   cmask
      REAL(wp) , DIMENSION(PRIV_2D_BIOARRAY,jpmois)    ::   zdustmo

      DO jk = KRANGE
         DO jj = JRANGE
            DO ji = IRANGE
               irondep(ji,jj,jk) = 0.e0          
               ironsed(ji,jj,jk) = 0.e0          
               nitrpot(ji,jj,jk) = 0.e0          
            END DO
         END DO
     END DO
     DO jj = JRANGE
        DO ji = IRANGE
             sidep(ji,jj) = 0.e0          
        END DO
     END DO

      ! Dust input from the atmosphere
      ! ------------------------------
      IF( ln_dustfer ) THEN 
!         IF(lwp) WRITE(numout,*) '    Initialize dust input from atmosphere '
!         IF(lwp) WRITE(numout,*) '    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ '
!         CALL iom_open ( 'dust.orca.nc', numdust )
!         DO jm = 1, jpmois
!            CALL iom_get( numdust, jpdom_data, 'dust', zdustmo(:,:,jm), jm )
!         END DO
!         CALL iom_close( numdust )
         !
         ! total atmospheric supply of Si
         ! ------------------------------
         sumdepsi = 0.e0
!         DO jm = 1, jpmois
!            DO jj = JRANGE
!               DO ji = IRANGE
!                  sumdepsi = sumdepsi + zdustmo(ji,jj,jm) / (12.*rmtss) * 8.8        &
!                     &     * 0.075/28.1 * e1t(ji,jj) * e2t(ji,jj) * tmask(ji,jj,KSURF) * tmask_i(ji,jj)
!              END DO
!            END DO
!         END DO
!        IF( lk_mpp )  CALL mpp_sum( sumdepsi )  ! sum over the global domain
      ELSE
         sumdepsi = 0.e0
      ENDIF

      ! Nutrient input from rivers
      ! --------------------------
      IF( ln_river ) THEN
!        IF(lwp) WRITE(numout,*) '    Initialize the nutrient input by rivers from river.orca.nc file'
!         IF(lwp) WRITE(numout,*) '    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'
!         CALL iom_open ( 'river.orca.nc', numriv )
!         CALL iom_get  ( numriv, jpdom_data, 'riverdic', river   (:,:), jpan )
!         CALL iom_get  ( numriv, jpdom_data, 'riverdoc', riverdoc(:,:), jpan )
!         CALL iom_close( numriv )

        ! Number of seconds per year and per month
!      ryyss = nyear_len(1) * rday
!      rmtss = ryyss / raamo

         DO jj = JRANGE
            DO ji = IRANGE
               river   (ji,jj) = 0. 
               riverdoc(ji,jj) = 0.
            END DO
         END DO

         ! N/P and Si releases due to coastal rivers
         ! -----------------------------------------
         DO jj = JRANGE
            DO ji = IRANGE
               zcoef = ryyss * e1t(ji,jj) * e2t(ji,jj) * fse3t(ji,jj,KSURF) * tmask(ji,jj,KSURF) * tmask_i(ji,jj)
               cotdep(ji,jj) =  river(ji,jj)                  *1E9 / ( 12. * zcoef + rtrn )
               rivinp(ji,jj) = (river(ji,jj)+riverdoc(ji,jj)) *1E9 / ( 31.6* zcoef + rtrn )
            END DO
         END DO
         rivpo4input = 0.e0
         rivalkinput = 0.e0
!         DO jj = JRANGE
!            DO ji = IRANGE
!               zcoef = cvol(ji,jj,KSURF) * ryyss
!               rivpo4input = rivpo4input + rivinp(ji,jj) * zcoef
!               rivalkinput = rivalkinput + cotdep(ji,jj) * zcoef
!            END DO
!         END DO
!         IF( lk_mpp ) THEN
!            CALL mpp_sum( rivpo4input )  ! sum over the global domain
!            CALL mpp_sum( rivalkinput )  ! sum over the global domain
!         ENDIF
      ELSE
         rivpo4input = 0.e0
         rivalkinput = 0.e0
      ENDIF

      ! Nutrient input from dust
      ! ------------------------
      IF( ln_ndepo ) THEN
!        IF(lwp) WRITE(numout,*) '    Initialize the nutrient input by dust from ndeposition.orca.nc'
!        IF(lwp) WRITE(numout,*) '    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'
!         CALL iom_open ( 'ndeposition.orca.nc', numdep )
!         CALL iom_get  ( numdep, jpdom_data, 'ndep', ndepo(:,:), jpan )
!         CALL iom_close( numdep )
!      
         DO jj = JRANGE
            DO ji = IRANGE               
               ndepo(ji,jj) = 0.
            END DO
         END DO
         DO jj = JRANGE
            DO ji = IRANGE
               zcoef         = 14E6*ryyss*fse3t(ji,jj,KSURF) 
               nitdep(ji,jj) = 7.6 * ndepo(ji,jj) / ( zcoef + rtrn )
            END DO
         END DO
         nitdepinput = 0.e0
!         DO jj = JRANGE
!            DO ji = IRANGE
!               zcoef = cvol(ji,jj,KSURF) * ryyss
!               nitdepinput = nitdepinput + nitdep(ji,jj) * zcoef
!            END DO
!         END DO
!         IF( lk_mpp ) CALL mpp_sum( nitdepinput )  ! sum over the global domain
      ELSE
         nitdepinput = 0.e0
      ENDIF

      ! Coastal and island masks
      ! ------------------------
      IF( ln_sedinput ) THEN     
!         IF(lwp) WRITE(numout,*) '    Computation of an island mask to enhance coastal supply of iron'
!         IF(lwp) WRITE(numout,*) '    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'
!         IF(lwp) WRITE(numout,*) '       from bathy.orca.nc file '
!         CALL iom_open ( 'bathy.orca.nc', numbath )
!         CALL iom_get  ( numbath, jpdom_data, 'bathy', cmask(:,:,:), jpan )
!         CALL iom_close( numbath )
         !
         DO jj = JRANGE
            DO ji = IRANGE
               cmask(ji,jj,jpk) = 1
            ENDDO
         ENDDO
         DO jk = KRANGE-1
            DO jj = JRANGE-1
               DO ji = IRANGE-1
                  IF( tmask(ji,jj,K) /= 0. ) THEN
                     zmaskt = tmask(ji+1,jj,K) * tmask(ji-1,jj  ,K) * tmask(ji,jj+1,K)    &
                        &                      * tmask(ji  ,jj-1,K) * tmask(ji,jj  ,K)
                     IF( zmaskt == 0. )   cmask(ji,jj,K ) = 0.1
                  ENDIF
               END DO
            END DO
         END DO
         DO jk = KRANGE
            DO jj = JRANGE
               DO ji = IRANGE
                  expide   = MIN( 8.,( fsdept(ji,jj,K) / 500. )**(-1.5) )
                  denitide = -0.9543 + 0.7662 * LOG( expide ) - 0.235 * LOG( expide )**2
                  cmask(ji,jj,jk) = cmask(ji,jj,jk) * MIN( 1., EXP( denitide ) / 0.5 )
               END DO
            END DO
         END DO
      
        ! Coastal supply of iron
        ! -------------------------
        DO jk = KRANGE
           DO jj = JRANGE
              DO ji = IRANGE
                 ironsed(ji,jj,jk) = sedfeinput * cmask(ji,jj,jk) / ( fse3t(ji,jj,K) * rday )
              END DO
          END DO
         END DO
         !
      ENDIF

   END SUBROUTINE p4z_sed_init

   
      SUBROUTINE ju2ymds (julian,year,month,day,sec)
      !!----------------------------------------------------------------------
      !!                     ***  ROUTINE ymds2ju  ***
      !!
      !! ** Purpose :   send back the date corresponding to the given julian day
      !!
      !! ** Method  :   
      !!
      !! ** History :   R. Benshila, adaptation for CROCO 
      !!                IPSL       , original version  
      !!
      !!---------------------------------------------------------------------
      IMPLICIT NONE
  !
      REAL,INTENT(IN) :: julian
      INTEGER,INTENT(OUT) :: year,month,day
          REAL,INTENT(OUT)    :: sec
  !
      INTEGER :: julian_day
      REAL    :: julian_sec
      REAL,PARAMETER :: one_day = 86400.0
      !---------------------------------------------------------------------
      !! 
      julian_day = INT(julian)
      julian_sec = (julian-julian_day)*one_day
      !
      CALL ju2ymds_internal(julian_day,julian_sec,year,month,day,sec)
      ! 
     END SUBROUTINE ju2ymds


      SUBROUTINE ju2ymds_internal (julian_day,julian_sec,year, month,day,sec)
         !---------------------------------------------------------------------
          !- This subroutine computes from the julian day the year,
          !- month, day and seconds
      !-
          !- In 1968 in a letter to the editor of Communications of the ACM
          !- (CACM, volume 11, number 10, October 1968, p.657) Henry F. Fliegel
          !- and Thomas C. Van Flandern presented such an algorithm.
          !-
          !- See also :
          !http://www.magnet.ch/serendipity/hermetic/cal_stud/jdn.htm
          !-
          !- In the case of the Gregorian calendar we have chosen to use
          !- the Lilian day numbers. This is the day counter which starts
      !- on the 15th October 1582. This is the day at which Pope
          !- Gregory XIII introduced the Gregorian calendar.
          !- Compared to the true Julian calendar, which starts some 7980
          !- years ago, the Lilian days are smaler and are dealt with easily
      !- on 32 bit machines. With the true Julian days you can only the
          !- fraction of the day in the real part to a precision of a 1/4 of
          !- a day with 32 bits.
          !---------------------------------------------------------------------
          IMPLICIT NONE
      !
          INTEGER,INTENT(IN) :: julian_day
          REAL,INTENT(IN)    :: julian_sec
          INTEGER,INTENT(OUT) :: year,month,day
          REAL,INTENT(OUT)    :: sec
          !
          INTEGER :: l,n,i,jd,j,d,m,y,ml
          INTEGER :: add_day
          REAL :: eps_day
          REAL,PARAMETER :: one_day = 86400.0
!          REAL,PARAMETER :: one_year = 365.2425
          REAL,PARAMETER :: one_year = 365.
          INTEGER :: mon_len(12)=(/31,28,31,30,31,30,31,31,30,31,30,31/)
      !---------------------------------------------------------------------
      !  
      eps_day = SPACING(one_day)
          !
          jd = julian_day
          sec = julian_sec
          IF (sec > (one_day-eps_day)) THEN
            add_day = INT(sec/one_day)
            sec = sec-add_day*one_day
            jd = jd+add_day
          ENDIF
          IF (sec < -eps_day) THEN
             sec = sec+one_day
            jd = jd-1
         ENDIF  !
      IF ( (one_year > 365.0).AND.(one_year < 366.0) ) THEN
             !-- Gregorian
             jd = jd+2299160
             !
             l = jd+68569
             n = (4*l)/146097
             l = l-(146097*n+3)/4
             i = (4000*(l+1))/1461001
             l = l-(1461*i)/4+31
             j = (80*l)/2447
             d = l-(2447*j)/80
             l = j/11
             m = j+2-(12*l)
             y = 100*(n-49)+i+l
       ELSEIF (    (ABS(one_year-365.0) <= EPSILON(one_year)) &
     &  .OR.(ABS(one_year-366.0) <= EPSILON(one_year)) ) THEN
           !-- No leap or All leap
           y = jd/NINT(one_year)
           l = jd-y*NINT(one_year)
           m = 1
           ml = 0
         DO WHILE (ml+mon_len(m) <= l)
            ml = ml+mon_len(m)
            m = m+1
         ENDDO
!           d = l-ml+1
           d = l-ml
      ELSE
        !-- others
        ml = NINT(one_year/12.)
        y = jd/NINT(one_year)
        l = jd-y*NINT(one_year)
        m = (l/ml)+1
!        d = l-(m-1)*ml+1
        d = l-(m-1)*ml
      ENDIF
      !
      day = d
      month = m
      year = y
      !
    END SUBROUTINE ju2ymds_internal

#else
   !!======================================================================
   !!  Dummy module :                                   No PISCES bio-model
   !!======================================================================
CONTAINS
   SUBROUTINE p4z_sed                         ! Empty routine
   END SUBROUTINE p4z_sed
#endif 

   !!======================================================================
END MODULE  p4zsed
