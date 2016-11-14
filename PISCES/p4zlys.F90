#include "cppdefs.h"

MODULE p4zlys
   !!======================================================================
   !!                         ***  MODULE p4zlys  ***
   !! TOP :   PISCES 
   !!======================================================================
   !! History :    -   !  1988-07  (E. MAIER-REIMER) Original code
   !!              -   !  1998     (O. Aumont) additions
   !!              -   !  1999     (C. Le Quere) modifications
   !!             1.0  !  2004     (O. Aumont) modifications
   !!             2.0  !  2007-12  (C. Ethe, G. Madec)  F90
   !!----------------------------------------------------------------------
#if defined key_pisces
   !!----------------------------------------------------------------------
   !!   'key_pisces'                                       PISCES bio-model
   !!----------------------------------------------------------------------
   !!   p4z_lys        :   Compute the CaCO3 dissolution 
   !!   p4z_lys_init   :   Read the namelist parameters
   !!----------------------------------------------------------------------
   USE sms_pisces

   IMPLICIT NONE
   PRIVATE

   PUBLIC   p4z_lys    ! called in p4zprg.F90
   PUBLIC   p4z_lys_nam    ! called in p4zprg.F90

#include "ocean2pisces.h90"

   !! * Shared module variables
   REAL(wp), PUBLIC ::   &
     kdca = 0.327e3   ,  &  !:
     nca  = 1.0             !:

   !! * Module variables
   REAL(wp) :: &
      calcon = 1.03E-2        ! mean calcite concentration [Ca2+] in sea water [mole/kg solution]

!   INTEGER ::  rmtss          !: number of seconds per month

   !!----------------------------------------------------------------------
   !! NEMO/TOP 2.0 , LOCEAN-IPSL (2007) 
   !! $Id: p4zlys.F90 1830 2010-04-12 13:03:51Z cetlod $ 
   !! Software governed by the CeCILL licence (modipsl/doc/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------

CONTAINS

   SUBROUTINE p4z_lys( kt, jnt )
      !!---------------------------------------------------------------------
      !!                     ***  ROUTINE p4z_lys  ***
      !!
      !! ** Purpose :   CALCULATES DEGREE OF CACO3 SATURATION IN THE WATER
      !!                COLUMN, DISSOLUTION/PRECIPITATION OF CACO3 AND LOSS
      !!                OF CACO3 TO THE CACO3 SEDIMENT POOL.
      !!
      !! ** Method  : - ???
      !!---------------------------------------------------------------------
      INTEGER, INTENT(in) ::   kt, jnt ! ocean time step
      INTEGER  ::   ji, jj, jk, jn
      REAL(wp) ::   zbot, zalk, zdic, zph, zremco3, zah2
      REAL(wp) ::   zdispot, zfact, zalka
      REAL(wp) ::   zomegaca, zexcess, zexcess0
      REAL(wp) ::   zrfact2
      REAL(wp), DIMENSION(PRIV_3D_BIOARRAY) ::   zco3
      REAL(wp), DIMENSION(PRIV_3D_BIOARRAY) :: zcaldiss, zmask
      CHARACTER (len=25) :: charout
      !!---------------------------------------------------------------------

         DO jk = KRANGE
            DO jj = JRANGE
               DO ji = IRANGE
                  zco3(ji,jj,jk) = 0.
                  zcaldiss(ji,jj,jk) = 0.
               ENDDO
            ENDDO
         ENDDO
      !     -------------------------------------------
      !     COMPUTE [CO3--] and [H+] CONCENTRATIONS
      !     -------------------------------------------
      
      DO jn = 1, 5                               !  BEGIN OF ITERATION
         !
!CDIR NOVERRCHK
         DO jk = KRANGE
!CDIR NOVERRCHK
            DO jj = JRANGE
!CDIR NOVERRCHK
               DO ji = IRANGE

                  ! SET DUMMY VARIABLE FOR TOTAL BORATE
                  zbot  = borat(ji,jj,jk)

                  ! SET DUMMY VARIABLE FOR TOTAL BORATE
                  zbot  = borat(ji,jj,jk)
                  zfact = rhop (ji,jj,K) / 1000. + rtrn

                  ! SET DUMMY VARIABLE FOR [H+]
                  zph   = hi(ji,jj,jk) * tmask(ji,jj,K) / zfact + ( 1.-tmask(ji,jj,K) ) * 1.e-9

                  ! SET DUMMY VARIABLE FOR [SUM(CO2)]GIVEN 
                  zdic  = trn(ji,jj,K,jpdic) / zfact
                  zalka = trn(ji,jj,K,jptal) / zfact

                  ! CALCULATE [ALK]([CO3--], [HCO3-])
                  zalk  = zalka - (  akw3(ji,jj,jk) / zph - zph   &
                     &             + zbot / (1.+ zph / akb3(ji,jj,jk) )  )

                  ! CALCULATE [H+] and [CO3--]
                  zah2 = SQRT( (zdic-zalk)*(zdic-zalk)+   &
                     &     4.*(zalk*ak23(ji,jj,jk)/ak13(ji,jj,jk))   &
                     &     *(2*zdic-zalk))

                  zah2=0.5*ak13(ji,jj,jk)/zalk*((zdic-zalk)+zah2)
                  zco3(ji,jj,jk) = zalk/(2.+zah2/ak23(ji,jj,jk))*zfact

                  hi(ji,jj,jk)  = zah2*zfact

               END DO
            END DO
         END DO
         !
      END DO 

      !     ---------------------------------------------------------
      !        CALCULATE DEGREE OF CACO3 SATURATION AND CORRESPONDING
      !        DISSOLOUTION AND PRECIPITATION OF CACO3 (BE AWARE OF
      !        MGCO3)
      !     ---------------------------------------------------------

      DO jk = KRANGE
         DO jj = JRANGE
            DO ji = IRANGE

               ! DEVIATION OF [CO3--] FROM SATURATION VALUE
               zomegaca = ( calcon * zco3(ji,jj,jk) ) / aksp(ji,jj,jk)

               ! SET DEGREE OF UNDER-/SUPERSATURATION
               zexcess0 = MAX( 0., ( 1.- zomegaca ) )
               zexcess  = zexcess0**nca

               ! AMOUNT CACO3 (12C) THAT RE-ENTERS SOLUTION
               !       (ACCORDING TO THIS FORMULATION ALSO SOME PARTICULATE
               !       CACO3 GETS DISSOLVED EVEN IN THE CASE OF OVERSATURATION)
              zdispot = kdca * zexcess * trn(ji,jj,K,jpcal)
# if defined key_off_degrad
              zdispot = zdispot * facvol(ji,jj,jk)
# endif

              !  CHANGE OF [CO3--] , [ALK], PARTICULATE [CACO3],
              !       AND [SUM(CO2)] DUE TO CACO3 DISSOLUTION/PRECIPITATION

              zcaldiss(ji,jj,jk) = zdispot * rfact2 / rmtss ! calcite dissolution
              zco3(ji,jj,jk)     = zco3(ji,jj,jk) + zcaldiss(ji,jj,jk) 
              !
              tra(ji,jj,jk,jptal) = tra(ji,jj,jk,jptal) + 2. * zcaldiss(ji,jj,jk) 
              tra(ji,jj,jk,jpcal) = tra(ji,jj,jk,jpcal) -      zcaldiss(ji,jj,jk) 
              tra(ji,jj,jk,jpdic) = tra(ji,jj,jk,jpdic) +      zcaldiss(ji,jj,jk) 
            END DO
         END DO
      END DO
 

# if defined key_trc_diaadd
      DO jk = KRANGE
         DO jj = JRANGE
            DO ji = IRANGE
               trc3d(ji,jj,K,jp_hi    )  = hi  (ji,jj,jk) * tmask(ji,jj,K)  ! PH
               trc3d(ji,jj,K,jp_co3   )  = zco3(ji,jj,jk) * tmask(ji,jj,K)  ! Ion carbonate
               trc3d(ji,jj,K,jp_co3sat)  = aksp(ji,jj,jk) / calcon * tmask(ji,jj,K)
            ENDDO
         ENDDO
      ENDDO
# endif
      !
       IF(ln_ctl)   THEN  ! print mean trends (used for debugging)
         WRITE(charout, FMT="('lys ')")
         CALL prt_ctl_trc_info(charout)
         CALL prt_ctl_trc( charout, ltra='tra')
!        CALL prt_ctl_trc(tab4d=tra, mask=mask, clinfo=ctrcnm)
       ENDIF

   END SUBROUTINE p4z_lys

   SUBROUTINE p4z_lys_nam

      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE p4z_lys_init  ***
      !!
      !! ** Purpose :   Initialization of CaCO3 dissolution parameters
      !!
      !! ** Method  :   Read the nampiscal namelist and check the parameters
      !!      called at the first timestep (nittrc000)
      !!
      !! ** input   :   Namelist nampiscal
      !!
      !!----------------------------------------------------------------------

      NAMELIST/nampiscal/ kdca, nca

      REWIND( numnatp )                     ! read numnatp
      READ  ( numnatp, nampiscal )

      IF(lwp) THEN                         ! control print
         WRITE(numout,*) ' '
         WRITE(numout,*) ' Namelist parameters for CaCO3 dissolution, nampiscal'
         WRITE(numout,*) ' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'
         WRITE(numout,*) '    diss. rate constant calcite (per month)   kdca      =', kdca
         WRITE(numout,*) '    order of reaction for calcite dissolution nca       =', nca
      ENDIF

      ! Number of seconds per month 
!      rmtss =  nyear_len(1) * rday / raamo

   END SUBROUTINE p4z_lys_nam

#else
   !!======================================================================
   !!  Dummy module :                                   No PISCES bio-model
   !!======================================================================
CONTAINS
   SUBROUTINE p4z_lys( kt )                   ! Empty routine
      INTEGER, INTENT( in ) ::   kt
      WRITE(*,*) 'p4z_lys: You should not have seen this print! error?', kt
   END SUBROUTINE p4z_lys
#endif 
   !!======================================================================
END MODULE  p4zlys
