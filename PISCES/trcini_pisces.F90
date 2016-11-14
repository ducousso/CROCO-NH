#include "cppdefs.h"

MODULE trcini_pisces
   !!======================================================================
   !!                         ***  MODULE trcini_pisces  ***
   !! TOP :   initialisation of the PISCES biochemical model
   !!======================================================================
   !! History :    -   !  1988-07  (E. Maier-Reiner) Original code
   !!              -   !  1999-10  (O. Aumont, C. Le Quere)
   !!              -   !  2002     (O. Aumont)  PISCES
   !!             1.0  !  2005-03  (O. Aumont, A. El Moussaoui) F90
   !!             2.0  !  2007-12  (C. Ethe, G. Madec) from trcini.pisces.h90
   !!----------------------------------------------------------------------
#if defined key_pisces
   !!----------------------------------------------------------------------
   !!   'key_pisces'                                       PISCES bio-model
   !!----------------------------------------------------------------------
   !! trc_ini_pisces   : PISCES biochemical model initialisation
   !!----------------------------------------------------------------------
   USE sms_pisces      ! Source Minus Sink variables
   USE trcsms_pisces   !
   USE p4zche          !  Chemical model
   USE p4zsink         !  vertical flux of particulate matter due to sinking
   USE p4zopt          !  optical model
   USE p4zrem          !  Remineralisation of organic matter
   USE p4zflx          !  Gas exchange
   USE p4zlim          !  Co-limitations of differents nutrients
   USE p4zprod         !  Growth rate of the 2 phyto groups
   USE p4zmicro        !  Sources and sinks of microzooplankton
   USE p4zmeso         !  Sources and sinks of mesozooplankton
   USE p4zmort         !  Mortality terms for phytoplankton
   USE p4zlys          !  Calcite saturation
   USE p4zsed          !  Sedimentation & burial

   !!* Substitution
#  include "ocean2pisces.h90"
#  include "top_substitute.h90"


   IMPLICIT NONE
   PRIVATE

   PUBLIC   trc_ini_pisces   ! called by trcini.F90 module
   PUBLIC   trc_nam_pisces   ! called by trcini.F90 module
   PUBLIC   trc_sbc_pisces   ! called by trcini.F90 module

   !! * Module variables
   REAL(wp) :: &
      sco2   =  2.312e-3         , &
      alka0  =  2.423e-3         , &
      oxyg0  =  177.6e-6         , &
      po4    =  2.174e-6         , &
      bioma0 =  1.000e-8         , &
      silic1 =  91.65e-6         , &
      no3    =  31.04e-6 * 7.6

   !!----------------------------------------------------------------------
   !! NEMO/TOP 2.0 , LOCEAN-IPSL (2007) 
   !! $Id: trcini_pisces.F90 1808 2010-03-11 09:17:56Z cetlod $ 
   !! Software governed by the CeCILL licence (modipsl/doc/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------

CONTAINS

    SUBROUTINE trc_ini_pisces

      !!----------------------------------------------------------------------
      !!                   ***  ROUTINE trc_ini_pisces ***
      !!
      !! ** Purpose :   Initialisation of the PISCES biochemical model
      !!----------------------------------------------------------------------
      INTEGER  ::  ji, jj, jk, jn, ierr
      REAL(wp) ::  zcaralk, zbicarb, zco3, zdic, zalk
      REAL(wp) ::  ztmas, ztmas1
      REAL(wp), DIMENSION(jptra)       :: trai
      REAL(wp)                         :: areatot



      IF(lwp) WRITE(numout,*)
      IF(lwp) WRITE(numout,*) ' trc_ini_pisces :   PISCES biochemical model initialisation'
      IF(lwp) WRITE(numout,*) ' ~~~~~~~~~~~~~~'



      ierr =         sms_pisces_alloc()          
      ierr = ierr +  trc_alloc()          ! Start of PISCES-related alloc routines...
      ierr = ierr +  p4z_sink_alloc()
      ierr = ierr +  p4z_opt_alloc()
      ierr = ierr +  p4z_prod_alloc()
      ierr = ierr +  p4z_rem_alloc()
      ierr = ierr +  p4z_flx_alloc()
      ierr = ierr +  p4z_sed_alloc()
      !
      IF( lk_mpp    )   CALL mpp_sum( ierr )
      IF( ierr /= 0 )   CALL ctl_stop( 'STOP in trc_ini_pisces : unable to allocate PISCES arrays' )

      IF( ln_ctl )  CALL prt_ctl_trc_ini

      !
      !                                            ! Time-step
      rfact   = rdt                                ! ---------
      rfactr  = 1. / rfact
      rfact2  = rfact / FLOAT( nrdttrc )
      rfact2r = 1. / rfact2
      xstep  = rfact2 / rday      ! Timestep duration for biology


      IF(lwp) WRITE(numout,*) 
      IF(lwp) WRITE(numout,*) '    Tracer  time step    rfact  = ', rfact, ' rdt = ', rdt
      IF(lwp) write(numout,*) '    Biology time step    rfact2 = ', rfact2
      IF(lwp) WRITE(numout,*) 



      ! Set biological ratios
      ! ---------------------
      rno3   =   16.   / 122.
      po4r   =   1.e0  / 122.
      o2nit  =  32.    / 122.
      rdenit =  97.6   /  16.
      o2ut   = 140.    / 122.

      CALL p4z_che        ! initialize the chemical constants

      ndayflxtr = nday_year      !  Initialize a counter for the computation of chemistry

      ! Initialization of tracer concentration in case of  no restart 
      !--------------------------------------------------------------
      ln_rsttr = ( nrrec /= 0 ) 
      !
      IF( .NOT. ln_rsttr ) THEN  
         DO jk = KRANGE
            DO jj = JRANGE
               DO ji = IRANGE
                  trn(ji,jj,K,jpdic) = sco2
                  trn(ji,jj,K,jpdoc) = bioma0
                  trn(ji,jj,K,jptal) = alka0
                  trn(ji,jj,K,jpoxy) = oxyg0
                  trn(ji,jj,K,jpcal) = bioma0
                  trn(ji,jj,K,jppo4) = po4 / po4r
                  trn(ji,jj,K,jppoc) = bioma0
#  if ! defined key_kriest
                  trn(ji,jj,K,jpgoc) = bioma0
                  trn(ji,jj,K,jpbfe) = bioma0 * 5.e-6
#  else
                  trn(ji,jj,K,jpnum) = bioma0 / ( 6. * xkr_massp )
#  endif
                  trn(ji,jj,K,jpsil) = silic1
                  trn(ji,jj,K,jpbsi) = bioma0 * 0.15
                  trn(ji,jj,K,jpdsi) = bioma0 * 5.e-6
                  trn(ji,jj,K,jpphy) = bioma0
                  trn(ji,jj,K,jpdia) = bioma0
                  trn(ji,jj,K,jpzoo) = bioma0
                  trn(ji,jj,K,jpmes) = bioma0
                  trn(ji,jj,K,jpfer) = 0.6E-9
                  trn(ji,jj,K,jpsfe) = bioma0 * 5.e-6
                  trn(ji,jj,K,jpdfe) = bioma0 * 5.e-6
                  trn(ji,jj,K,jpnfe) = bioma0 * 5.e-6
                  trn(ji,jj,K,jpnch) = bioma0 * 12. / 55.
                  trn(ji,jj,K,jpdch) = bioma0 * 12. / 55.
                  trn(ji,jj,K,jpno3) = no3
                  trn(ji,jj,K,jpnh4) = bioma0
               ENDDO
            ENDDO
         ENDDO

      ENDIF

      
      ! initialize the half saturation constant for silicate
      ! ----------------------------------------------------
      DO jj = JRANGE
         DO ji = IRANGE
            xksi(:,:)    = 2.e-6
            xksimax(:,:) = xksi(:,:)
         ENDDO
      ENDDO

      ! Initialization of chemical variables of the carbon cycle
      ! --------------------------------------------------------
      DO jk = KRANGE
         DO jj = JRANGE
            DO ji = IRANGE
               zdic    = trn(ji,jj,K,jpdic) * 1e-6 
               zalk    = trn(ji,jj,K,jptal) * 1e-6 
               ztmas   = tmask(ji,jj,K)
               ztmas1  = 1. - tmask(ji,jj,K)
               zcaralk = zalk - borat(ji,jj,jk) / (  1. + 1.E-8 / ( rtrn + akb3(ji,jj,jk) )  )
               zco3    = ( zcaralk - zdic ) * ztmas + 0.5e-3 * ztmas1
               zbicarb = ( 2. * zdic - zcaralk )
               hi(ji,jj,jk) = ( ak23(ji,jj,jk) * zbicarb / zco3 ) * ztmas + 1.e-9 * ztmas1
!               hi(ji,jj,jk) = 1.e-8
            END DO
         END DO
      END DO

      !  
      IF(lwp) THEN               ! control print
         WRITE(numout,*)
         WRITE(numout,*)
         WRITE(numout,*) '          *** Total number of passive tracer jptra = ', jptra
      ENDIF

      CALL tracer_stat( nit000 )

      IF(lwp) WRITE(numout,*) 'Initialization of PISCES tracers done'
      IF(lwp) WRITE(numout,*) ' '

      CALL p4z_sed_init       !  sedimentation 
      CALL p4z_opt_init       !  Optic: PAR in the water column
      CALL p4z_rem_init       !  remineralisation
      CALL p4z_flx_init       !  gas exchange 

      !
   END SUBROUTINE trc_ini_pisces


   SUBROUTINE trc_nam_pisces
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE trc_sms_pisces_init  ***
      !!
      !! ** Purpose :   Initialization of PH variable
      !!
      !!----------------------------------------------------------------------
      INTEGER  :: jn, ierr
      TYPE(PTRACER), DIMENSION(jptra) :: tracer


      NAMELIST/nampistrc/ tracer
      NAMELIST/nampisbio/ part, nrdttrc, wsbio, xkmort, ferat3, wsbio2, niter1max, niter2max
#if defined key_kriest
      NAMELIST/nampiskrp/ xkr_eta, xkr_zeta, xkr_ncontent, xkr_mass_min, xkr_mass_max
#endif

      IF(lwp) WRITE(numout,*)
      IF(lwp) WRITE(numout,*) ' trc_sms_pisces : read PISCES namelists'
      IF(lwp) WRITE(numout,*) ' ~~~~~~~~~~~~~~'


      !                               ! Open the namelist file
      !                               ! ----------------------
      CALL ctl_opn( numnatp, 'namelist_pisces', 'OLD', 'FORMATTED', 'SEQUENTIAL', -1, numout, .FALSE. )
      
      ALLOCATE( ctrcnm(jptra), ctrcnl(jptra), ctrcnu(jptra), STAT = ierr  )  
      IF( ierr /= 0 )   CALL ctl_warn('trc_alloc: failed to allocate arrays')

      IF(lwp) WRITE(numout,*) 'number of tracer : ', jptra
      DO jn = 1, jptra
         WRITE( ctrcnm(jn),'("TR_",I1)'           ) jn
         WRITE( ctrcnl(jn),'("TRACER NUMBER ",I1)') jn
         ctrcnu(jn) = 'mmole/m3'
      END DO

      REWIND( numnatp )                    
      READ  ( numnatp, nampistrc )

      DO jn = 1, jptra
         ctrcnm(jn) = tracer(jn)%clsname
         ctrcnl(jn) = tracer(jn)%cllname
         ctrcnu(jn) = tracer(jn)%clunit
      END DO


      IF(lwp) THEN                   ! control print
         DO jn = 1, jptra
            WRITE(numout,*) '   tracer nb             : ', jn 
            WRITE(numout,*) '   short name            : ', TRIM(ctrcnm(jn))
            WRITE(numout,*) '   long name             : ', TRIM(ctrcnl(jn))
            WRITE(numout,*) '   unit                  : ', TRIM(ctrcnu(jn))
            WRITE(numout,*) ' '
         END DO
      ENDIF

      REWIND( numnatp )                    
      READ  ( numnatp, nampisbio )

      IF(lwp) THEN                         ! control print
         WRITE(numout,*) ' Namelist : nampisbio'
         WRITE(numout,*) '    part of calcite not dissolved in guts     part      =', part
         WRITE(numout,*) '    frequence pour la biologie                nrdttrc   =', nrdttrc
         WRITE(numout,*) '    POC sinking speed                         wsbio     =', wsbio
         WRITE(numout,*) '    half saturation constant for mortality    xkmort    =', xkmort
         WRITE(numout,*) '    Fe/C in zooplankton                       ferat3    =', ferat3
         WRITE(numout,*) '    Big particles sinking speed               wsbio2    =', wsbio2
         WRITE(numout,*) '    Maximum number of iterations for POC      niter1max =', niter1max
         WRITE(numout,*) '    Maximum number of iterations for GOC      niter2max =', niter2max
      ENDIF

      CALL p4z_lim_nam       !  co-limitations by the various nutrients
      CALL p4z_prod_nam      !  phytoplankton growth rate over the global ocean.
      CALL p4z_rem_nam       !  remineralisation
      CALL p4z_mort_nam      !  phytoplankton mortality 
      CALL p4z_micro_nam     !  microzooplankton
      CALL p4z_meso_nam      !  mesozooplankton
      CALL p4z_lys_nam       !  calcite saturation
      CALL p4z_flx_nam       !  gas exchange 
      CALL p4z_sed_nam

#if defined key_kriest

      !                               ! nampiskrp : kriest parameters
      !                               ! -----------------------------
      xkr_eta      = 0.62        
      xkr_zeta     = 1.62        
      xkr_mass_min = 0.0002     
      xkr_mass_max = 1.      

      REWIND( numnatp )                     ! read natkriest
      READ  ( numnatp, nampiskrp )

      IF(lwp) THEN
         WRITE(numout,*)
         WRITE(numout,*) ' Namelist : nampiskrp'
         WRITE(numout,*) '    Sinking  exponent                        xkr_eta      = ', xkr_eta
         WRITE(numout,*) '    N content exponent                       xkr_zeta     = ', xkr_zeta
         WRITE(numout,*) '    Minimum mass for Aggregates              xkr_mass_min = ', xkr_mass_min
         WRITE(numout,*) '    Maximum mass for Aggregates              xkr_mass_max = ', xkr_mass_max
         WRITE(numout,*)
     ENDIF


     ! Computation of some variables
     xkr_massp = 5.7E-6 * 7.6 * xkr_mass_min**xkr_zeta

#endif


   END SUBROUTINE trc_nam_pisces


   SUBROUTINE trc_sbc_pisces 

# include "netcdf.inc"

      INTEGER :: ji, jj, irec
      INTEGER :: ncid, varid, dimid, ierr, &
     &           lstr, lenstr, nf_fread, nrec_dust
      REAL(wp) ::  dustmp(GLOBAL_2D_ARRAY,366)

#ifdef MPI
#define LOCALLM Lmmpi
#define LOCALMM Mmmpi
#else
#define LOCALLM Lm
#define LOCALMM Mm
#endif

      ALLOCATE( dustmo(GLOBAL_2D_ARRAY,12), STAT= ierr )

!
!    READ DUST INPUT FROM ATMOSPHERE
!    -------------------------------------
!
      IF( ln_dustfer ) THEN
        lstr=lenstr(bioname)
        ierr=nf_open (bioname(1:lstr), nf_nowrite, ncid)
        if (ierr .ne. nf_noerr) then
           write(stdout,4) bioname
        endif
        ierr=nf_inq_varid (ncid,"dust",varid)
        if (ierr .ne. nf_noerr) then
          write(stdout,5) "dust", bioname
        endif
        ierr=nf_inq_dimid(ncid,"dust_time",dimid)
        ierr=nf_inq_dimlen(ncid,dimid,nrec_dust)
!        write(*,*)'NREC_DUST=',nrec_dust
!        write(*,*)'-----------------------------'
        do irec=1,nrec_dust
          ierr=nf_fread(dustmp(START_2D_ARRAY,irec), ncid, varid, &
     &                                              irec, r2dvar)
          if (ierr .ne. nf_noerr) then
            write(stdout,6) "dust", irec 
          endif
        enddo
        ierr=nf_close(ncid)
        write(stdout,*) 
        write(stdout,'(6x,A,1x,I4)') &
#ifdef MPI
     &                   'TRCINI_PISCES -- Read dust deposition ', mynode
#else
     &                   'TRCINI_PISCES -- Read dust deposition ' 
#endif
  4     format(/,' TRCINI_PISCES - unable to open forcing netCDF ',1x,A)
  5     format(/,' TRCINI_PISCES - unable to find forcing variable: ',A, &
     &                               /,14x,'in forcing netCDF  ',A)
  6     format(/,' TRCINI_PISCES - error while reading variable: ',A,2x, &
     &                                           ' at TIME index = ',i4)

        DO irec = 1, nrec_dust
           DO jj = 1, LOCALMM
              DO ji = 1, LOCALLM
                 dustmo(ji,jj,irec) = dustmp(ji,jj,irec)
              ENDDO
           ENDDO
        ENDDO
      
      ENDIF

   END SUBROUTINE trc_sbc_pisces 


#else
   !!----------------------------------------------------------------------
   !!   Dummy module                            No PISCES biochemical model
   !!----------------------------------------------------------------------
CONTAINS
   SUBROUTINE trc_ini_pisces             ! Empty routine
   END SUBROUTINE trc_ini_pisces
#endif

   !!======================================================================
END MODULE trcini_pisces
