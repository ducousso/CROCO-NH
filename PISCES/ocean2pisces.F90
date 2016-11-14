#include "cppdefs.h"


#ifdef PISCES
MODULE ocean2pisces

#ifdef XIOS
   USE xios
#endif


   IMPLICIT NONE
   PUBLIC


#include "param_F90.h" 
#include "grid.h"
#include "ocean3d.h"
#include "forces.h"
#include "mixing.h"
#include "diagnostics.h"
#include "scalars_F90.h"
#include "ncscrum_F90.h"
# if defined MPI
  include 'mpif.h'
# include "mpi_roms.h"
# endif
  

REAL, DIMENSION(GLOBAL_2D_ARRAY,N+1) :: fse3w     ! W-vertical scale factor
REAL, DIMENSION(GLOBAL_2D_ARRAY,N+1) :: fsdepw    ! W-depht 

  INTEGER Istrp,Iendp,Jstrp,Jendp
!$OMP threadprivate(Istrp,Iendp)
!$OMP threadprivate(Jstrp,Jendp)

  PUBLIC iom_open, iom_close, iom_get, iom_put

  INTERFACE iom_put
     MODULE PROCEDURE iom_p1d, iom_p2d, iom_p3d
  END INTERFACE
  INTERFACE iom_get
     MODULE PROCEDURE iom_g2d, iom_g3d
  END INTERFACE
  INTERFACE lbc_lnk
     MODULE PROCEDURE lbc_lnk_2d, lbc_lnk_3d
  END INTERFACE
  INTERFACE mpp_sum
     MODULE PROCEDURE mppsum_int, mppsum_real
  END INTERFACE
  INTERFACE mpp_max
     MODULE PROCEDURE mppmax_int, mppmax_real
  END INTERFACE
  INTERFACE mpp_min
     MODULE PROCEDURE mppmin_int, mppmin_real
  END INTERFACE


  LOGICAL :: lwp        
  LOGICAL :: ln_ctl     = .false.
  LOGICAL :: ln_qsr_bio = .false.   
  REAL    :: rn_abs     = 0.58
  REAL    :: rn_si0     = 0.35
  INTEGER :: numout    
  INTEGER :: jpdom_data = 1

CONTAINS

   SUBROUTINE ocean_2_pisces( Istr, Iend, Jstr, Jend )
   
      INTEGER :: Istr, Iend, Jstr, Jend
      INTEGER :: i, j, k


      Istrp = Istr
      Iendp = Iend
      Jstrp = Jstr
      Jendp = Jend


      DO k = 1, N+1
        DO j =  Jstr, Jend 
            DO i =  Istr, Iend 
               fsdepw(i,j,N+2-k) = -z_w(i,j,k-1)
            END DO
         END DO
      END DO


      DO k = 2, N
         DO j =  Jstr, Jend 
            DO i =  Istr, Iend 
               fse3w(i,j,k) = -z_r(i,j,N+1-k) + z_r(i,j,N+2-k)
           END DO
         END DO
      END DO

      DO j =  Jstr, Jend 
         DO i =  Istr, Iend 
            fse3w(i,j,1)   = -2 * z_r(i,j,N)
            fse3w(i,j,N+1) = 2 * ( -z_w(i,j,0) + z_r(i,j,1) )
         END DO
      END DO


   END SUBROUTINE ocean_2_pisces


   SUBROUTINE ctl_opn ( knum, cdfile, cdstat, cdform, cdacce, klengh, kout, ldwp, karea )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE ctl_opn  ***
      !!
      !! ** Purpose :   Open file and check if required file is available.
      !!
      !! ** Method  :   Fortan open
      !!
      !! History :
      !!        !  1995-12  (G. Madec)  Original code
      !!   8.5  !  2002-06  (G. Madec)  F90: Free form and module
      !!----------------------------------------------------------------------

      INTEGER          , INTENT(  out) ::   knum      ! logical unit to open
      CHARACTER(len=*) , INTENT(in   ) ::   cdfile    ! file name to open
      CHARACTER(len=*) , INTENT(in   ) ::   cdstat    ! disposition specifier
      CHARACTER(len=*) , INTENT(in   ) ::   cdform    ! formatting specifier
      CHARACTER(len=*) , INTENT(in   ) ::   cdacce    ! access specifier
      INTEGER          , INTENT(in   ) ::   klengh    ! record length
      INTEGER          , INTENT(in   ) ::   kout      ! number of logical units for write
      LOGICAL          , INTENT(in   ) ::   ldwp      ! boolean term for print
      INTEGER, OPTIONAL, INTENT(in   ) ::   karea     ! proc number
      !!
      CHARACTER(len=80) ::   clfile
      INTEGER           ::   iost

      ! adapt filename
      ! ----------------
      clfile = TRIM(cdfile)
      IF( PRESENT( karea ) ) THEN
         IF( karea > 1 )   WRITE(clfile, "(a,'_',i4.4)") TRIM(clfile), karea-1
      ENDIF
#if defined key_agrif
      IF( .NOT. Agrif_Root() )   clfile = TRIM(clfile)//'.'//TRIM(Agrif_CFixed())
      write(*,*) clfile
      knum=Agrif_Get_Unit()
#else
      knum=getunit()
#endif

      iost=0
      IF( cdacce(1:6) == 'DIRECT' )  THEN
         OPEN( UNIT=knum, FILE=clfile, FORM=cdform, ACCESS=cdacce, STATUS=cdstat, RECL=klengh, ERR=100, IOSTAT=iost )
      ELSE
         OPEN( UNIT=knum, FILE=clfile, FORM=cdform, ACCESS=cdacce, STATUS=cdstat             , ERR=100, IOSTAT=iost )
      ENDIF
      IF( iost == 0 ) THEN
         IF(ldwp) THEN
            WRITE(kout,*)
            WRITE(kout,*) '     file   : ', clfile,' open ok'
            WRITE(kout,*) '     unit   = ', knum
            WRITE(kout,*) '     status = ', cdstat
            WRITE(kout,*) '     form   = ', cdform
            WRITE(kout,*) '     access = ', cdacce
            WRITE(kout,*)
         ENDIF
      ENDIF
100   CONTINUE
      IF( iost /= 0 ) THEN
         IF(ldwp) THEN
            WRITE(kout,*)
            WRITE(kout,*) ' ===>>>> : bad opening file: ', clfile
            WRITE(kout,*) ' =======   ===  '
            WRITE(kout,*) '           unit   = ', knum
            WRITE(kout,*) '           status = ', cdstat
            WRITE(kout,*) '           form   = ', cdform
            WRITE(kout,*) '           access = ', cdacce
            WRITE(kout,*) '           iostat = ', iost
            WRITE(kout,*) '           we stop. verify the file '
            WRITE(kout,*)
         ENDIF
         STOP 'ctl_opn bad opening'
      ENDIF
      
   END SUBROUTINE ctl_opn


   FUNCTION getunit()
      !!----------------------------------------------------------------------
      !!                  ***  FUNCTION  getunit  ***
      !!
      !! ** Purpose :   return the index of an unused logical unit
      !!----------------------------------------------------------------------
      INTEGER :: getunit
      LOGICAL :: llopn 
      !!----------------------------------------------------------------------
      !
      getunit = 15   ! choose a unit that is big enough then it is not already used in NEMO
      llopn = .TRUE.
      DO WHILE( (getunit < 998) .AND. llopn )
         getunit = getunit + 1
         INQUIRE( unit = getunit, opened = llopn )
      END DO
      IF( (getunit == 999) .AND. llopn ) THEN
         CALL ctl_stop( 'getunit: All logical units until 999 are used...' )
         getunit = -1
      ENDIF
      !
   END FUNCTION getunit


   SUBROUTINE ctl_warn(clname)
      CHARACTER(len=*), INTENT(in) :: clname
      WRITE(numout,"(/,' ===>>> : W A R N I N G', /,'         ===============',/)") 
      IF(mynode .eq. 0 )  WRITE(numout,*) clname
   END SUBROUTINE

   SUBROUTINE ctl_stop(clname)
      CHARACTER(len=*), INTENT(in) :: clname
      WRITE(numout,"(/,' ===>>> : E R R O R',     /,'         ===========',/)") 
      IF(mynode .eq. 0 )  WRITE(numout,*) clname
      STOP
   END SUBROUTINE

   SUBROUTINE lbc_lnk_2d(ptab, cd_type, psgn)
      REAL, DIMENSION(:,:)  , INTENT(inout) ::   ptab     ! 3D array on which the boundary condition is applied
      CHARACTER(len=1)      , INTENT(in   ) ::   cd_type  ! define the nature of ptab array grid-points
      !                                                           ! = T , U , V , F , W points
      REAL                  , INTENT(in   ) ::   psgn     ! =-1 the sign change across the north fold boundary
   END SUBROUTINE lbc_lnk_2d

   SUBROUTINE lbc_lnk_3d(ptab, cd_type, psgn)
      REAL, DIMENSION(:,:,:), INTENT(inout) ::   ptab     ! 3D array on which the boundary condition is applied
      CHARACTER(len=1)      , INTENT(in   ) ::   cd_type  ! define the nature of ptab array grid-points
      !                                                           ! = T , U , V , F , W points
      REAL                  , INTENT(in   ) ::   psgn     ! =-1 the sign change across the north fold boundary
   END SUBROUTINE lbc_lnk_3d

   SUBROUTINE mppsum_int(ktab)
      INTEGER, INTENT(inout) :: ktab
      INTEGER :: ierror, localcomm     
      INTEGER :: iwork     

      localcomm=MPI_COMM_WORLD
      CALL mpi_allreduce( ktab, iwork, 1, mpi_integer, mpi_sum, localcomm, ierror)
      ktab = iwork
   END SUBROUTINE mppsum_int

   SUBROUTINE mppsum_real(ptab)
      REAL, INTENT(inout) :: ptab
      INTEGER :: ierror, localcomm     
      REAL    :: zwork     

      localcomm=MPI_COMM_WORLD
      CALL mpi_allreduce( ptab, zwork, 1, mpi_double_precision, mpi_sum, localcomm, ierror )
      ptab = zwork
   END SUBROUTINE mppsum_real

   SUBROUTINE mppmax_int(ktab)
      INTEGER, INTENT(inout) :: ktab
      INTEGER :: ierror, localcomm     
      INTEGER :: iwork     

      localcomm=MPI_COMM_WORLD
      CALL mpi_allreduce( ktab, iwork, 1, mpi_integer, mpi_max, localcomm, ierror)
      ktab = iwork
   END SUBROUTINE mppmax_int

   SUBROUTINE mppmax_real(ptab)
      REAL, INTENT(inout) :: ptab
      INTEGER :: ierror, localcomm     
      REAL    :: zwork     

      localcomm=MPI_COMM_WORLD
      CALL mpi_allreduce( ptab, zwork, 1, mpi_double_precision, mpi_max, localcomm, ierror )
      ptab = zwork
   END SUBROUTINE mppmax_real

    SUBROUTINE mppmin_int(ktab)
      INTEGER, INTENT(inout) :: ktab
      INTEGER :: ierror, localcomm     
      INTEGER :: iwork     

      localcomm=MPI_COMM_WORLD
      CALL mpi_allreduce( ktab, iwork, 1, mpi_integer, mpi_min, localcomm, ierror)
      ktab = iwork
   END SUBROUTINE mppmin_int

   SUBROUTINE mppmin_real(ptab)
      REAL, INTENT(inout) :: ptab
      INTEGER :: ierror, localcomm     
      REAL    :: zwork     

      localcomm=MPI_COMM_WORLD
      CALL mpi_allreduce( ptab, zwork, 1, mpi_double_precision, mpi_min, localcomm, ierror )
      ptab = zwork
   END SUBROUTINE mppmin_real


   !!----------------------------------------------------------------------
   !!                   INTERFACE iom
   !!----------------------------------------------------------------------

   SUBROUTINE iom_open( cdname, kiomid )
      CHARACTER(len=*), INTENT(in   )    ::   cdname   ! File name
      INTEGER         , INTENT(  out)    ::   kiomid   ! iom identifier of the opened file
   END SUBROUTINE iom_open

   SUBROUTINE iom_close( kiomid )
      INTEGER         , INTENT(in)       ::   kiomid   ! iom identifier of the opened file
   END SUBROUTINE iom_close

   SUBROUTINE iom_g2d( kiomid, kdom, cdvar, pvar, ktime )
      INTEGER         , INTENT(in   )                           ::   kiomid    ! Identifier of the file
      INTEGER         , INTENT(in   )                           ::   kdom      ! Type of domain to be read
      CHARACTER(len=*), INTENT(in   )                           ::   cdvar     ! Name of the variable
      REAL            , INTENT(  out), DIMENSION(:,:)           ::   pvar      ! read field
      INTEGER         , INTENT(in   )                , OPTIONAL ::   ktime     ! record number
   END SUBROUTINE iom_g2d

   SUBROUTINE iom_g3d( kiomid, kdom, cdvar, pvar, ktime )
      INTEGER         , INTENT(in   )                             ::   kiomid    ! Identifier of the file
      INTEGER         , INTENT(in   )                             ::   kdom      ! Type of domain to be read
      CHARACTER(len=*), INTENT(in   )                             ::   cdvar     ! Name of the variable
      REAL            , INTENT(  out), DIMENSION(:,:,:)           ::   pvar      ! read field
      INTEGER         , INTENT(in   )                  , OPTIONAL ::   ktime     ! record number
   END SUBROUTINE iom_g3d

   SUBROUTINE iom_p1d( cdname, pfield1d )
      CHARACTER(LEN=*)          , INTENT(in) ::   cdname
      REAL,     DIMENSION(:), INTENT(in) ::   pfield1d
#if defined key_iomput
      CALL xios_send_field( cdname, RESHAPE( (/pfield1d/), (/1,1,SIZE(pfield1d)/) ) )
#else
      IF( .FALSE. )   WRITE(numout,*) cdname, pfield1d   ! useless test to avoid compilation warnings
#endif
   END SUBROUTINE iom_p1d

   SUBROUTINE iom_p2d( cdname, pfield2d )
      CHARACTER(LEN=*)            , INTENT(in) ::   cdname
      REAL,     DIMENSION(:,:), INTENT(in) ::   pfield2d
#if defined key_iomput
      CALL xios_send_field(cdname, pfield2d)
#else
      IF( .FALSE. )   WRITE(numout,*) cdname, pfield2d   ! useless test to avoid compilation warnings
#endif
   END SUBROUTINE iom_p2d

   SUBROUTINE iom_p3d( cdname, pfield3d )
      CHARACTER(LEN=*)                , INTENT(in) ::   cdname
      REAL,       DIMENSION(:,:,:), INTENT(in) ::   pfield3d
#if defined key_iomput 
      CALL xios_send_field(cdname, pfield3d)
#else
      IF( .FALSE. )   WRITE(numout,*) cdname, pfield3d   ! useless test to avoid compilation warnings
#endif
   END SUBROUTINE iom_p3d


END MODULE ocean2pisces

#else
MODULE ocean2pisces_empty
END MODULE ocean2pisces_empty
#endif
