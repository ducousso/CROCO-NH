!     $Id:$
!     
!======================================================================
!     ROMS_AGRIF is a branch of ROMS developped at IRD and INRIA, in France
!     The two other branches from UCLA (Shchepetkin et al) 
!     and Rutgers University (Arango et al) are under MIT/X style license.
!     ROMS_AGRIF specific routines (nesting) are under CeCILL-C license.
!     
!     ROMS_AGRIF website : http://roms.mpl.ird.fr
!======================================================================
!     
#include "cppdefs.h"

#ifdef XIOS

      MODULE xios_module
      USE xios
      IMPLICIT NONE

      PUBLIC set_grid
      PUBLIC iom_set_domain_attr
      PUBLIC iom_set_axis_attr
      PUBLIC iom_set_field_attr
      PUBLIC iom_set_file_attr
      PUBLIC iom_get_file_attr
      PUBLIC set_scalar
      
      CONTAINS

      SUBROUTINE set_grid( cdgrd, plon, plat)
# include "param.h"
# include "ncscrum.h"
# include "scalars.h"
      !!----------------------------------------------------------------------
      !!                     ***  ROUTINE set_grid  ***
      !!
      !! ** Purpose :   set domain definition to XIOS library
      !!
      !! ** Method  :   
      !!
      !! ** History :   A. Ponte   , adaptation for CROCO 
      !!                S. Masson  , original version (NEMO)  
      !!
      !!----------------------------------------------------------------------
      CHARACTER(LEN=*)            , INTENT(in) ::   cdgrd
      REAL plon(GLOBAL_2D_ARRAY)
      REAL plat(GLOBAL_2D_ARRAY)
      !
      INTEGER vtype, horiz_type, vert_type
      INTEGER imin, imax, jmin, jmax, ni, nj
      INTEGER nx_glo, ny_glo 
      INTEGER istart, jstart
      INTEGER data_ni,data_nj 
      INTEGER :: ji
      !!----------------------------------------------------------------------
      !
      ! See ncscrum.h for r2dvar ...
      SELECT CASE (TRIM(cdgrd))
      CASE ("rho")
         vtype=r2dvar
         nx_glo=LLm+2
         ny_glo=MMm+2
      CASE ("b")
         vtype=r2dvar
         nx_glo=LLm+2
         ny_glo=MMm+2
      CASE ("u")
         vtype=u2dvar
         nx_glo=LLm+1
         ny_glo=MMm+2
      CASE ("v")
         vtype=v2dvar
         nx_glo=LLm+2
         ny_glo=MMm+1
      CASE ("w")
         vtype=r2dvar
         nx_glo=LLm+2
         ny_glo=MMm+2
      END SELECT

      ! From nf_fread.F
      vert_type=vtype/4         ! vertical and horizontal
      horiz_type=vtype-4*vert_type ! grid types, then calculate
      jmin=horiz_type/2         ! starting indices indices
      imin=horiz_type-2*jmin    ! in horizontal directions.
      !     
      istart=1
      jstart=1

# ifdef MPI
      IF (ii.gt.0) then
         istart=1-imin+iminmpi
         imin=1
      ENDIF
      IF (ii.eq.NP_XI-1) then
         imax=Lmmpi+1
      ELSE
         imax=Lmmpi
      ENDIF
      IF (jj.gt.0) then
         jstart=1-jmin+jminmpi
         jmin=1
      ENDIF
      IF (jj.eq.NP_ETA-1) then
         jmax=Mmmpi+1
      ELSE
         jmax=Mmmpi
      ENDIF
# else
      imin=1
      imax=Lm+1
      jmin=1
      jmax=Mm+1
# endif

      ! there is probably already a variable for ni and nj 
      ni=imax-imin+1
      nj=jmax-jmin+1

      ! from set_global_definitions.h
# ifdef THREE_GHOST_POINTS
#  ifdef MPI
      data_ni = Lm+3+padd_X+2+1
      data_nj = Mm+3+padd_E+2+1
#  endif
# else
#  ifdef MPI
      data_ni = Lm+2+padd_X+1+1
      data_nj = Mm+2+padd_E+1+1
#  endif
# endif

      ! notes iminmpi, jminmpi are computed in MPI_Setup.F
      CALL iom_set_domain_attr(TRIM(cdgrd),ni_glo=nx_glo, nj_glo=ny_glo,
     &     ibegin=istart, ni=ni,jbegin=jstart,nj=nj)
      ! to do: below should have be rewritten to match all configs (periodic, number of ghost points, ...)
      ! see set_global_definitions.h
      CALL iom_set_domain_attr(TRIM(cdgrd),data_dim=2,
     &     data_ibegin=START_1D_ARRAYXI-imin,
     &     data_ni=data_ni,
     &     data_jbegin=START_1D_ARRAYETA-jmin, 
     &     data_nj=data_nj)

      CALL iom_set_domain_attr(TRIM(cdgrd),
     &     lonvalue=RESHAPE(plon(imin:imax,jmin:jmax), (/ni*nj/)),
     &     latvalue=RESHAPE(plat(imin:imax,jmin:jmax), (/ni*nj/)))
      !      
      END SUBROUTINE set_grid


      SUBROUTINE iom_set_domain_attr( cdid, ni_glo, nj_glo, ibegin, jbegin,
     & ni, nj, zoom_ibegin, zoom_jbegin, zoom_ni, zoom_nj,
     & data_dim, data_ibegin, data_ni, data_jbegin, data_nj, 
     & lonvalue, latvalue, mask )
      !!----------------------------------------------------------------------
      !!                     ***  ROUTINE iom_set_domain_attr  ***
      !!
      !! ** Purpose :   set domain definition to XIOS library
      !!
      !! ** Method  :   
      !!
      !! ** History :   R. Benshila, adaptation for CROCO 
      !!                S. Masson  , original version (NEMO)  
      !!
      !!----------------------------------------------------------------------
      CHARACTER(LEN=*) , INTENT(in) :: cdid
      INTEGER , OPTIONAL, INTENT(in) :: ni_glo, nj_glo, ibegin, jbegin 
      INTEGER , OPTIONAL, INTENT(in) :: ni, nj
      INTEGER , OPTIONAL, INTENT(in) :: data_dim, data_ibegin, data_ni
      INTEGER , OPTIONAL, INTENT(in) :: data_jbegin, data_nj
      INTEGER , OPTIONAL, INTENT(in) :: zoom_ibegin, zoom_jbegin
      INTEGER , OPTIONAL, INTENT(in) :: zoom_ni, zoom_nj
      REAL , DIMENSION(:) , OPTIONAL, INTENT(in) :: lonvalue, latvalue
      LOGICAL, DIMENSION(:,:), OPTIONAL, INTENT(in) :: mask
      !!----------------------------------------------------------------------
      !
      IF ( xios_is_valid_domain(cdid) ) THEN
        CALL xios_set_domain_attr(cdid, ni_glo=ni_glo, nj_glo=nj_glo, 
     & ibegin=ibegin, jbegin=jbegin, ni=ni, nj=nj,
     & data_dim=data_dim, data_ibegin=data_ibegin, data_ni=data_ni, 
     & data_jbegin=data_jbegin, data_nj=data_nj, 
     & zoom_ibegin=zoom_ibegin, zoom_jbegin=zoom_jbegin, 
     & zoom_ni=zoom_ni, zoom_nj=zoom_nj,
     &    lonvalue=lonvalue, latvalue=latvalue,mask=mask )
      ENDIF
      !
      IF ( xios_is_valid_domaingroup(cdid) ) THEN
         CALL xios_set_domaingroup_attr( cdid,
     &  ni_glo=ni_glo, nj_glo=nj_glo, 
     &  ibegin=ibegin, jbegin=jbegin, ni=ni, nj=nj,
     &  data_dim=data_dim, data_ibegin=data_ibegin, data_ni=data_ni, 
     &  data_jbegin=data_jbegin, data_nj=data_nj ,
     &  zoom_ibegin=zoom_ibegin, zoom_jbegin=zoom_jbegin, 
     &  zoom_ni=zoom_ni, zoom_nj=zoom_nj,
     &  lonvalue=lonvalue, latvalue=latvalue,mask=mask )
      ENDIF
      !
      CALL xios_solve_inheritance()
      ! 
      END SUBROUTINE iom_set_domain_attr

      
      SUBROUTINE iom_set_axis_attr( cdid, paxis )
      !!----------------------------------------------------------------------
      !!                     ***  ROUTINE iom_set_axis_attr  ***
      !!
      !! ** Purpose :   set axis attribute for XIOS
      !!
      !! ** Method  :   
      !!
      !! ** History :   A. Ponte   , adaptation for CROCO 
      !!                S. Masson  , original version (NEMO)  
      !!
      !!----------------------------------------------------------------------
      CHARACTER(LEN=*)      , INTENT(in) ::   cdid
      REAL, DIMENSION(:), INTENT(in) ::   paxis
      !!----------------------------------------------------------------------
      !
      IF ( xios_is_valid_axis (cdid) ) then
       CALL xios_set_axis_attr(cdid, size=size(paxis),value=paxis )
      ENDIF
      !
      IF ( xios_is_valid_axisgroup(cdid) ) then
       CALL xios_set_axisgroup_attr(cdid, size=size(paxis),value=paxis )
      ENDIF
      !
      CALL xios_solve_inheritance()
      !
      END SUBROUTINE iom_set_axis_attr
 
      SUBROUTINE iom_set_field_attr( cdid, freq_op, freq_offset )
      !!----------------------------------------------------------------------
      !!                     ***  ROUTINE iom_set_field_attr  ***
      !!
      !! ** Purpose :   set output field attribute for XIOS
      !!
      !! ** Method  :   
      !!
      !! ** History :   A. Ponte   , adaptation for CROCO 
      !!                S. Masson  , original version (NEMO)  
      !!
      !!----------------------------------------------------------------------
      CHARACTER(LEN=*)          , INTENT(in) ::   cdid
      CHARACTER(LEN=*),OPTIONAL , INTENT(in) ::   freq_op
      CHARACTER(LEN=*),OPTIONAL , INTENT(in) ::   freq_offset
      !!----------------------------------------------------------------------
      !
      IF ( xios_is_valid_field     (cdid) ) 
     &   CALL xios_set_field_attr   
     &  ( cdid, freq_op=freq_op, freq_offset=freq_offset )
      !
      IF ( xios_is_valid_fieldgroup(cdid) )   
     & CALL xios_set_fieldgroup_attr( 
     &     cdid, freq_op=freq_op, freq_offset=freq_offset )
      !
      CALL xios_solve_inheritance()
      !
      END SUBROUTINE iom_set_field_attr


      SUBROUTINE iom_set_file_attr( cdid, name, name_suffix )
      !!----------------------------------------------------------------------
      !!                     ***  ROUTINE iom_set_file_attr  ***
      !!
      !! ** Purpose :   set file attribute for XIOS
      !!
      !! ** Method  :   
      !!
      !! ** History :   A. Ponte   , adaptation for CROCO 
      !!                S. Masson  , original version (NEMO)  
      !!
      !!----------------------------------------------------------------------
      CHARACTER(LEN=*)          , INTENT(in) ::   cdid
      CHARACTER(LEN=*),OPTIONAL , INTENT(in) ::   name, name_suffix
      !!----------------------------------------------------------------------
      !
      IF ( xios_is_valid_file     (cdid) )   CALL xios_set_file_attr  
     &    ( cdid, name=name, name_suffix=name_suffix )
      !
      IF ( xios_is_valid_filegroup(cdid) )   CALL xios_set_filegroup_attr
     & ( cdid, name=name, name_suffix=name_suffix )
      !
      CALL xios_solve_inheritance()
      !
      END SUBROUTINE iom_set_file_attr

      
      SUBROUTINE iom_get_file_attr( cdid, name, name_suffix, output_freq )
# include "param.h"
# include "scalars.h"
# include "ncscrum.h"
      !!----------------------------------------------------------------------
      !!                     ***  ROUTINE iom_get_file_attr  ***
      !!
      !! ** Purpose :   get attribute from xml file
      !!
      !! ** Method  :   
      !!
      !! ** History :   R. Benshila, adaptation for CROCO 
      !!                S. Masson  , original version (NEMO)  
      !!
      !!----------------------------------------------------------------------
      CHARACTER(LEN=*)          , INTENT(in ) :: 
     &   cdid
      CHARACTER(LEN=*),OPTIONAL , INTENT(out) :: 
     &   name, name_suffix, output_freq
      !
      LOGICAL                                 :: 
     &   llexist1,llexist2,llexist3
      !---------------------------------------------------------------------
      !
      IF( PRESENT( name        ) )   name = ''          ! default values
      IF( PRESENT( name_suffix ) )   name_suffix = ''
      IF( PRESENT( output_freq ) )   output_freq = ''
      IF ( xios_is_valid_file     (cdid) ) THEN
         CALL xios_solve_inheritance()
         CALL xios_is_defined_file_attr   
     &      ( cdid, name = llexist1, name_suffix = llexist2,
     &        output_freq = llexist3)
         IF(llexist1)   CALL xios_get_file_attr   
     &      ( cdid, name = name )
         IF(llexist2)   CALL xios_get_file_attr   
     &      ( cdid, name_suffix = name_suffix )
         IF(llexist3)   CALL xios_get_file_attr   
     &      ( cdid, output_freq = output_freq )
      ENDIF

      IF ( xios_is_valid_filegroup(cdid) ) THEN
         CALL xios_solve_inheritance()
         CALL xios_is_defined_filegroup_attr( cdid, name = llexist1, 
     &    name_suffix = llexist2, output_freq = llexist3)
         IF(llexist1)   CALL xios_get_filegroup_attr( cdid, 
     &    name = name )
         IF(llexist2)   CALL xios_get_filegroup_attr( cdid, 
     &    name_suffix = name_suffix )
         IF(llexist3)   CALL xios_get_filegroup_attr( cdid, 
     &    output_freq = output_freq )
      ENDIF
      !
      END SUBROUTINE iom_get_file_attr

      
      SUBROUTINE set_scalar
# include "param.h"
# include "scalars.h"
      !!----------------------------------------------------------------------
      !!                     ***  ROUTINE set_scalar  ***
      !!
      !! ** Purpose :   fake axis for salar output
      !!
      !! ** Method  :   
      !!
      !! ** History :   R. Benshila, adaptation for CROCO 
      !!                S. Masson  , original version (NEMO)  
      !!
      !!----------------------------------------------------------------------
      REAL, DIMENSION(1) ::   zz = 1.
      !!----------------------------------------------------------------------
      ! 
      CALL iom_set_domain_attr('scalarpoint', ni_glo=NNODES, nj_glo=1, 
     &  ibegin=mynode+1, jbegin=1, ni=1, nj=1)
      CALL iom_set_domain_attr('scalarpoint', data_dim=2,data_ibegin = 1, 
     &                         data_ni = 1, data_jbegin = 1, data_nj = 1)
      zz=REAL(mynode+1)
      CALL iom_set_domain_attr('scalarpoint', lonvalue=zz, 
     &  latvalue=zz)
      !
      END SUBROUTINE set_scalar

      
      SUBROUTINE set_xmlatt
# include "param.h"
      !!----------------------------------------------------------------------
      !!                     ***  ROUTINE set_xmlatt  ***
      !!
      !! ** Purpose :   automatic definitions of some of the xml attributs...
      !!
      !! ** Method  :   
      !!
      !! ** History :   R. Benshila, adaptation for CROCO 
      !!                S. Masson  , original version (NEMO)  
      !!
      !!----------------------------------------------------------------------
      CHARACTER(len=1),DIMENSION( 3) ::   clgrd                    ! suffix name
      CHARACTER(len=256)             ::   clsuff                   ! suffix name
      CHARACTER(len=1)               ::   cl1                      ! 1 character
      CHARACTER(len=2)               ::   cl2                      ! 2 characters
      CHARACTER(len=3)               ::   cl3                      ! 3 characters
      INTEGER                        ::   ji, jg                   ! loop counters
      INTEGER                        ::   ix, iy                   ! i-,j- index
      REAL       ,DIMENSION(11) ::   zlontao                  ! longitudes of tao    moorings
      REAL      ,DIMENSION( 7) ::   zlattao                  ! latitudes  of tao    moorings
      REAL        ,DIMENSION( 4) ::   zlonrama                 ! longitudes of rama   moorings
      REAL       ,DIMENSION(11) ::   zlatrama                 ! latitudes  of rama   moorings
      REAL        ,DIMENSION( 3) ::   zlonpira                 ! longitudes of pirata moorings
      REAL     ,DIMENSION( 9) ::   zlatpira                 ! latitudes  of pirata moorings
      !!----------------------------------------------------------------------
      !
      ! frequency of the call of iom_put (attribut: freq_op)
      WRITE(cl1,'(i1)') 1 
      CALL iom_set_field_attr('field_definition', 
     & freq_op = cl1//'ts', freq_offset='0ts')

      ! output file names (attribut: name)
      DO ji = 1, 9
         WRITE(cl1,'(i1)') ji
         CALL iom_update_file_name('file'//cl1)
      END DO
      DO ji = 1, 99
         WRITE(cl2,'(i2.2)') ji
         CALL iom_update_file_name('file'//cl2)
      END DO
      DO ji = 1, 999
         WRITE(cl3,'(i3.3)') ji
         CALL iom_update_file_name('file'//cl3)
      END DO

      ! Zooms...
      clgrd = (/ 'T', 'U', 'W' /)
      DO jg = 1, SIZE(clgrd)     ! grid type
         cl1 = clgrd(jg)
         ! Equatorial section (attributs: jbegin, ni, name_suffix)
         CALL dom_ngb( 0., 0., ix, iy, cl1 )
         CALL iom_set_domain_attr ('Eq'//cl1, 
     &    zoom_jbegin=iy, zoom_ni=LLm0)
         CALL iom_get_file_attr   ('Eq'//cl1, 
     &    name_suffix = clsuff             )
         CALL iom_set_file_attr   ('Eq'//cl1, 
     &    name_suffix = TRIM(clsuff)//'_Eq')
         CALL iom_update_file_name('Eq'//cl1)
      END DO
      ! TAO moorings (attributs: ibegin, jbegin, name_suffix)
      zlontao = (/ 137.0, 147.0, 156.0, 165.0, -180.0, -170.0,
     &  -155.0, -140.0, -125.0, -110.0, -95.0 /)
      zlattao = (/  -8.0,  -5.0,  -2.0,   0.0,    2.0,    5.0,
     &     8.0 /)
      CALL set_mooring( zlontao, zlattao )
      ! RAMA moorings (attributs: ibegin, jbegin, name_suffix)
      zlonrama = (/  55.0,  67.0, 80.5, 90.0 /)
      zlatrama = (/ -16.0, -12.0, -8.0, -4.0, -1.5, 0.0, 1.5,
     &  4.0, 8.0, 12.0, 15.0 /)
      CALL set_mooring( zlonrama, zlatrama )
      ! PIRATA moorings (attributs: ibegin, jbegin, name_suffix)
      zlonpira = (/ -38.0, -23.0, -10.0 /)
      zlatpira = (/ -19.0, -14.0,  -8.0, 0.0, 4.0, 8.0, 12.0, 15.0, 20.0 /)
      CALL set_mooring( zlonpira, zlatpira )
      !
      END SUBROUTINE set_xmlatt


      SUBROUTINE set_mooring( plon, plat)
      !!----------------------------------------------------------------------
      !!                     ***  ROUTINE set_mooring  ***
      !!
      !! ** Purpose :   automatic definitions of moorings xml attributs...
      !!
      !! ** Method  :   provide a list of coordinates
      !!
      !! ** History :   R. Benshila, adaptation for CROCO 
      !!                S. Masson  , original version (NEMO)  
      !!
      !!----------------------------------------------------------------------
      REAL, DIMENSION(:), INTENT(in) ::  plon, plat           ! longitudes/latitudes oft the mooring
      !
      CHARACTER(len=1),DIMENSION(1) ::   clgrd = (/ 'T' /)        ! suffix name
      CHARACTER(len=256)            ::   clname                   ! file name
      CHARACTER(len=256)            ::   clsuff                   ! suffix name
      CHARACTER(len=1)              ::   cl1                      ! 1 character
      CHARACTER(len=6)              ::   clon,clat                ! name of longitude, latitude
      INTEGER                       ::   ji, jj, jg               ! loop counters
      INTEGER                       ::   ix, iy                   ! i-,j- index
      REAL                          ::   zlon, zlat
      !!----------------------------------------------------------------------
      !
      DO jg = 1, SIZE(clgrd)
         cl1 = clgrd(jg)
         DO ji = 1, SIZE(plon)
            DO jj = 1, SIZE(plat)
               zlon = plon(ji)
               zlat = plat(jj)
               ! modifications for RAMA moorings
               IF( zlon ==  67. .AND. zlat ==  15. )   zlon =  65.
               IF( zlon ==  90. .AND. zlat <=  -4. )   zlon =  95.
               IF( zlon ==  95. .AND. zlat ==  -4. )   zlat =  -5.
               ! modifications for PIRATA moorings
               IF( zlon == -38. .AND. zlat == -19. )   zlon = -34.
               IF( zlon == -38. .AND. zlat == -14. )   zlon = -32.
               IF( zlon == -38. .AND. zlat ==  -8. )   zlon = -30.
               IF( zlon == -38. .AND. zlat ==   0. )   zlon = -35.
               IF( zlon == -23. .AND. zlat ==  20. )   zlat =  21.
               IF( zlon == -10. .AND. zlat == -14. )   zlat = -10.
               IF( zlon == -10. .AND. zlat ==  -8. )   zlat =  -6.
               IF( zlon == -10. .AND. zlat ==   4. ) THEN  
                   zlon = 0.   ;   zlat = 0.   
               ENDIF
               CALL dom_ngb( zlon, zlat, ix, iy, cl1 )
               IF( zlon >= 0. ) THEN  
                  IF( zlon == REAL(NINT(zlon)) ) THEN   
                  WRITE(clon, '(i3,  a)') NINT( zlon), 'e'
                  ELSE                                      
                  WRITE(clon, '(f5.1,a)')       zlon , 'e'
                  ENDIF
               ELSE             
                  IF( zlon == REAL(NINT(zlon)) ) THEN  
                  WRITE(clon, '(i3,  a)') NINT(-zlon), 'w'
                  ELSE                                     
                  WRITE(clon, '(f5.1,a)')      -zlon , 'w'
                  ENDIF
               ENDIF
               IF( zlat >= 0. ) THEN  
                  IF( zlat == REAL(NINT(zlat)) ) THEN  
                  WRITE(clat, '(i2,  a)') NINT( zlat), 'n'
                  ELSE                                      
                  WRITE(clat, '(f4.1,a)')       zlat , 'n'
                  ENDIF
               ELSE             
                  IF( zlat == REAL(NINT(zlat)) ) THEN   
                  WRITE(clat, '(i2,  a)') NINT(-zlat), 's'
                  ELSE                                      
                  WRITE(clat, '(f4.1,a)')      -zlat , 's'
                  ENDIF
               ENDIF
               clname = TRIM(ADJUSTL(clat))//TRIM(ADJUSTL(clon))
               CALL iom_set_domain_attr 
     &           (TRIM(clname)//cl1, zoom_ibegin= ix, zoom_jbegin= iy)
               CALL iom_get_file_attr   
     &           (TRIM(clname)//cl1, name_suffix = clsuff)
               CALL iom_set_file_attr   
     &           (TRIM(clname)//cl1, name_suffix = TRIM(clsuff)
     &            //'_'//TRIM(clname))
               CALL iom_update_file_name
     &           (TRIM(clname)//cl1)
            END DO
         END DO
      END DO
      ! 
      END SUBROUTINE set_mooring


      SUBROUTINE dom_ngb( plon, plat, kii, kjj, cdgrid )
# include "param.h"
# include "grid.h"
# ifdef MPI
#      include "mpi_roms.h"
       include 'mpif.h'
# endif
      !!----------------------------------------------------------------------
      !!                    ***  ROUTINE dom_ngb  ***
      !!
      !! ** Purpose :   find the closest grid point from a given lon/lat position
      !!
      !! ** Method  :   look for minimum distance in cylindrical projection 
      !!                -> not good if located at too high latitude...
      !!
      !!** History  : R. Benshila, adaptation for CROCO 
      !!              S. Masson  , original version (NEMO)  
      !!
      !----------------------------------------------------------------------
      REAL            , INTENT(in   ) ::   plon, plat   ! longitude,latitude of the point
      INTEGER         , INTENT(  out) ::   kii, kjj     ! i-,j-index of the closes grid point
      CHARACTER(len=1), INTENT(in   ) ::   cdgrid       ! grid name 'T', 'U', 'V', 'W'
      !
      INTEGER , DIMENSION(2) ::   iloc
      REAL                   ::   zlon, zmini
      REAL,DIMENSION(GLOBAL_2D_ARRAY) ::  zglam, zgphi, 
     &                                    zmask, zdist
      REAL, DIMENSION(2,1) ::   zain,zaout
      INTEGER :: ierror
      !!--------------------------------------------------------------------
      !
      zmask(:,:) = 0.
# ifdef SPHERICAL
      SELECT CASE( cdgrid )
      CASE( 'U' ) 
      zglam(:,:) = lonu(:,:) 
      zgphi(:,:) = latu(:,:)
#  ifdef MASKING
      zmask(:,:) = umask(:,:)
#  endif
      CASE( 'V' ) 
      zglam(:,:) = lonv(:,:) 
      zgphi(:,:) = latv(:,:) 
#  ifdef MASKING
      zmask(:,:) = vmask(:,:)
#  endif
      CASE DEFAULT 
      zglam(:,:) = lonr(:,:) 
      zgphi(:,:) = latr(:,:) ; 
#  ifdef MASKING
      zmask(:,:) = rmask(:,:)
#  endif
      END SELECT
# else
      SELECT CASE( cdgrid )
      CASE( 'U' ) 
      zglam(:,:) = yr(:,:) 
      zgphi(:,:) = xp(:,:)
#  ifdef MASKING
      zmask(:,:) = umask(:,:)
#  endif
      CASE( 'V' ) 
      zglam(:,:) = yp(:,:) 
      zgphi(:,:) = xr(:,:) 
# ifdef MASKING
      zmask(:,:) = vmask(:,:)
# endif
      CASE DEFAULT 
      zglam(:,:) = yr(:,:) 
      zgphi(:,:) = xr(:,:) ; 
#  ifdef MASKING
      zmask(:,:) = rmask(:,:)
#  endif
      END SELECT
# endif

# ifdef SPHERICAL
      zlon       = MOD( plon       + 720., 360. )                                     ! plon between    0 and 360
      zglam(:,:) = MOD( zglam(:,:) + 720., 360. )                                     ! glam between    0 and 360
      IF( zlon > 270. )   zlon = zlon - 360.                                          ! zlon between  -90 and 270
      IF( zlon <  90. )   WHERE( zglam(:,:) > 180. ) zglam(:,:) = zglam(:,:) - 360.   ! glam between -180 and 180
# endif
      zglam(:,:) = zglam(:,:) - zlon
      zgphi(:,:) = zgphi(:,:) - plat
      zdist(:,:) = zglam(:,:) * zglam(:,:) + zgphi(:,:) * zgphi(:,:)
      
# ifdef MPI
      zmini = MINVAL( zdist(:,:) , mask= zmask == 1.e0 )
      iloc = MINLOC( zdist(:,:) , mask= zmask == 1.e0 )
      !
      kii = iloc(1) + iminmpi - 1
      kjj = iloc(2) + jminmpi - 1
      !
      zain(1,:)=zmini
      zain(2,:)=kii+10000.*kjj
      !
      CALL MPI_ALLREDUCE( zain,zaout, 1, MPI_2DOUBLE_PRECISION,
     &                    MPI_MINLOC,MPI_COMM_WORLD,ierror)
      !
      zmini = zaout(1,1)
      kii = INT(zaout(2,1)/10000.)
      kjj = INT(zaout(2,1) - 10000.*kjj )
# else
         iloc(:) = MINLOC( zdist(:,:), mask = zmask(:,:) == 1.e0 )
         kii = iloc(1) - 1
         kjj = iloc(2) - 1
# endif
      !
      END SUBROUTINE dom_ngb


      SUBROUTINE iom_update_file_name( cdid )
# include "param.h"
# include "scalars.h"
# include "ncscrum.h"
      !!----------------------------------------------------------------------
      !!                    ***  ROUTINE iom_update_file_name  ***
      !!
      !! ** Purpose :   update output file name from model information
      !!
      !! ** Method  :   request xml attribute and modify it
      !!
      !! ** History :   R. Benshila, adaptation for CROCO 
      !!                S. Masson  , original version (NEMO)  
      !!
      !!---------------------------------------------------------------------
      CHARACTER(LEN=*)          , INTENT(in) ::   cdid
      !
      CHARACTER(LEN=256) ::   clname
      CHARACTER(LEN=20)  ::   clfreq
      CHARACTER(LEN=20)  ::   cldate
      INTEGER            ::   idx
      INTEGER            ::   jn
      INTEGER            ::   itrlen
      INTEGER            ::   iyear, imonth, iday, isec
      REAL               ::   zsec, jday0
      LOGICAL            ::   llexist
      !!----------------------------------------------------------------------
      !
# ifdef START_DATE
      ! start_date with format dd-mm-yyyy
       READ(start_date(1:2),fmt='(i2)') iday
       READ(start_date(4:5),fmt='(i2)') imonth
       READ(start_date(7:10),fmt='(i4)') iyear
       CALL ymds2ju( iyear, imonth,iday, 0., jday0 )  
# else
       CALL ymds2ju( 1, 1, 1, 0., jday0 )  
# endif
      DO jn = 1,2

         IF( jn == 1 )   CALL iom_get_file_attr( cdid, name 
     &           = clname, output_freq = clfreq )
         IF( jn == 2 )   CALL iom_get_file_attr( cdid, name_suffix = clname )

         IF ( TRIM(clname) /= '' ) THEN 

            idx = INDEX(clname,'@expname@') + INDEX(clname,'@EXPNAME@')
            DO WHILE ( idx /= 0 ) 
               clname = clname(1:idx-1)//TRIM(replace_blank(TRIM(title),"_"))
     &          //clname(idx+9:LEN_TRIM(clname))
               idx = INDEX(clname,'@expname@') + INDEX(clname,'@EXPNAME@')
            END DO

            idx = INDEX(clname,'@freq@') + INDEX(clname,'@FREQ@')
            DO WHILE ( idx /= 0 ) 
               IF ( TRIM(clfreq) /= '' ) THEN
                  itrlen = LEN_TRIM(clfreq)
                  IF ( clfreq(itrlen-1:itrlen) == 'mo' ) THEN
                  clfreq = clfreq(1:itrlen-1)
                  ENDIF
                  clname = clname(1:idx-1)//TRIM(clfreq)
     &                    //clname(idx+6:LEN_TRIM(clname))
               ENDIF
               idx = INDEX(clname,'@freq@') + INDEX(clname,'@FREQ@')
            END DO

            idx = INDEX(clname,'@startdate@') + INDEX(clname,'@STARTDATE@')
            DO WHILE ( idx /= 0 ) 
               cldate = iom_sdate( tdays+jday0  )
               clname = clname(1:idx-1)//TRIM(cldate)//
     &          clname(idx+11:LEN_TRIM(clname))
               idx = INDEX(clname,'@startdate@') + INDEX(clname,'@STARTDATE@')
            END DO

            idx = INDEX(clname,'@startdatefull@') 
     &       + INDEX(clname,'@STARTDATEFULL@')
            DO WHILE ( idx /= 0 ) 
               cldate = iom_sdate(tdays+jday0 , ldfull = .TRUE. )
               clname = clname(1:idx-1)//TRIM(cldate)//
     &          clname(idx+15:LEN_TRIM(clname))
               idx = INDEX(clname,'@startdatefull@')
     &           + INDEX(clname,'@STARTDATEFULL@')
            END DO

            idx = INDEX(clname,'@enddate@') + INDEX(clname,'@ENDDATE@')
            DO WHILE ( idx /= 0 ) 
               cldate = iom_sdate( tdays+jday0 + dt
     &          / day2sec * REAL( ntimes - ntstart ), ld24 = .TRUE. )
               clname = clname(1:idx-1)//TRIM(cldate)
     &          //clname(idx+9:LEN_TRIM(clname))
               idx = INDEX(clname,'@enddate@') + INDEX(clname,'@ENDDATE@')
            END DO

            idx = INDEX(clname,'@enddatefull@')
     &        + INDEX(clname,'@ENDDATEFULL@')
            DO WHILE ( idx /= 0 ) 
               cldate = iom_sdate( tdays+jday0 + dt  
     &          /day2sec * REAL( ntimes - ntstart ), ld24 = .TRUE., 
     &           ldfull = .TRUE. )
               clname = clname(1:idx-1)//TRIM(cldate)
     &          //clname(idx+13:LEN_TRIM(clname))
               idx = INDEX(clname,'@enddatefull@')
     &           + INDEX(clname,'@ENDDATEFULL@')
            END DO

            IF( jn == 1 )   CALL iom_set_file_attr( cdid, name
     &               = clname )
            IF( jn == 2 )   CALL iom_set_file_attr( cdid, name_suffix
     &               = clname )

         ENDIF

      END DO
      !
      END SUBROUTINE iom_update_file_name


      FUNCTION iom_sdate( pjday, ld24, ldfull )
      !!----------------------------------------------------------------------
      !!                     ***  ROUTINE iom_sdate  ***
      !!
      !! ** Purpose :   send back the date corresponding to the given julian day
      !!
      !! ** Method  :   
      !!
      !! ** History :   R. Benshila, adaptation for CROCO 
      !!                S. Masson  , original version (NEMO)  
      !!
      !!---------------------------------------------------------------------
      REAL    , INTENT(in   )           ::   pjday         ! julian day
      LOGICAL , INTENT(in   ), OPTIONAL ::   ld24          ! true to force 24:00 instead of 00:00
      LOGICAL , INTENT(in   ), OPTIONAL ::   ldfull        ! true to get the compleate date: yyyymmdd_hh:mm:ss
      !
      CHARACTER(LEN=20) ::   iom_sdate
      CHARACTER(LEN=50) ::   clfmt                         !  format used to write the date
      INTEGER           ::   iyear, imonth, iday, ihour, iminute, isec
      REAL              ::   zsec
      LOGICAL           ::   ll24, llfull
      !!---------------------------------------------------------------------
      !
      IF( PRESENT(ld24) ) THEN   ;   ll24 = ld24
      ELSE                       ;   ll24 = .FALSE.
      ENDIF

      IF( PRESENT(ldfull) ) THEN   ;   llfull = ldfull
      ELSE                         ;   llfull = .FALSE.
      ENDIF

      CALL ju2ymds( pjday, iyear, imonth, iday, zsec )
      isec = NINT(zsec)

      IF( ll24 .AND. isec == 0 ) THEN   ! 00:00 of the next day -> move to 24:00 of the current day
         CALL ju2ymds( pjday - 1., iyear, imonth, iday, zsec )
         isec = 86400
      ENDIF

      IF( iyear < 10000 ) THEN  
      clfmt = "i4.4,2i2.2"                ! format used to write the date
      ELSE                   
      WRITE(clfmt, "('i',i1,',2i2.2')") INT(LOG10(REAL(iyear))) + 1
      ENDIF

      IF( llfull ) THEN
         clfmt = TRIM(clfmt)//",'_',i2.2,':',i2.2,':',i2.2"
         ihour   = isec / 3600
         isec    = MOD(isec, 3600)
         iminute = isec / 60
         isec    = MOD(isec, 60)
         WRITE(iom_sdate, '('//TRIM(clfmt)//')') 
     &    iyear, imonth, iday, ihour, iminute, isec    ! date of the end of run
      ELSE
         WRITE(iom_sdate, '('//TRIM(clfmt)//')')
     &     iyear, imonth, iday                          ! date of the end of run
      ENDIF

      END FUNCTION iom_sdate

 
      SUBROUTINE ymds2ju (year,month,day,sec,julian)
      !!----------------------------------------------------------------------
      !!                     ***  ROUTINE ymds2ju  ***
      !!
      !! ** Purpose :   send back the julian day corresponding to the given date
      !!
      !! ** Method  :   
      !!
      !! ** History :   R. Benshila, adaptation for CROCO 
      !!                IPSL       , original version  
      !!
      !!---------------------------------------------------------------------
      IMPLICIT NONE
      !
      INTEGER,INTENT(IN) :: year,month,day
      REAL,INTENT(IN)    :: sec
      REAL,INTENT(OUT) :: julian
      !
      INTEGER :: julian_day
      REAL    :: julian_sec
      REAL,PARAMETER :: one_day = 86400.0    
      !---------------------------------------------------------------------
      !
      CALL ymds2ju_internal (year,month,day,sec,julian_day,julian_sec)
      julian = julian_day+julian_sec/one_day
      !
      END SUBROUTINE ymds2ju

      SUBROUTINE ymds2ju_internal (year,month,day,sec,julian_day,julian_sec)
      !---------------------------------------------------------------------
      !- Converts year, month, day and seconds into a julian day
      !-
      !- In 1968 in a letter to the editor of Communications of the ACM
      !- (CACM, volume 11, number 10, October 1968, p.657) Henry F. Fliegel
      !- and Thomas C. Van Flandern presented such an algorithm.
      !-
      !- See also : http://www.magnet.ch/serendipity/hermetic/cal_stud/jdn.htm
      !-
      !- In the case of the Gregorian calendar we have chosen to use
      !- the Lilian day numbers. This is the day counter which starts
      !- on the 15th October 1582.
      !- This is the day at which Pope Gregory XIII introduced the
      !- Gregorian calendar.
      !- Compared to the true Julian calendar, which starts some
      !- 7980 years ago, the Lilian days are smaler and are dealt with
      !- easily on 32 bit machines. With the true Julian days you can only
      !- the fraction of the day in the real part to a precision of
      !- a 1/4 of a day with 32 bits.
      !---------------------------------------------------------------------
      IMPLICIT NONE
      !
      INTEGER,INTENT(IN) :: year,month,day
      REAL,INTENT(IN)    :: sec
      INTEGER,INTENT(OUT) :: julian_day
      REAL,INTENT(OUT)    :: julian_sec
      !
      REAL,PARAMETER :: one_day = 86400.0    
      REAL,PARAMETER :: one_year = 365.2425
      INTEGER :: mon_len(12)=(/31,28,31,30,3
     &     1,30,31,31,30,31,30,31/)
      INTEGER :: jd,m,y,d,ml
      !---------------------------------------------------------------------
      !
      m = month
      y = year
      d = day
      !
      ! We deduce the calendar from the length of the year as it
      ! is faster than an INDEX on the calendar variable.
      !
      IF ( (one_year > 365.0).AND.(one_year < 366.0) ) THEN
      !-- "Gregorian"
       jd = (1461*(y+4800+INT((m-14)/12)))/4 
     &      +(367*(m-2-12*(INT((m-14)/12))))/12 
     &      -(3*((y+4900+INT((m-14)/12))/100))/4 
     &      +d-32075
       jd = jd-2299160
      ELSE IF (    (ABS(one_year-365.0) <= EPSILON(one_year))  
     &  .OR.(ABS(one_year-366.0) <= EPSILON(one_year)) ) THEN
      !-- "No leap" or "All leap"
      ml = SUM(mon_len(1:m-1))
      jd = y*NINT(one_year)+ml+(d-1)
      ELSE
      !-- Calendar with regular month
       ml = NINT(one_year/12.)
       jd = y*NINT(one_year)+(m-1)*ml+(d-1)
      ENDIF
      !
      julian_day = jd
      julian_sec = sec
      !
      END SUBROUTINE ymds2ju_internal

     
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
	  ! 
	  julian_day = INT(julian)
	  julian_sec = (julian-julian_day)*one_day
      !
      CALL ju2ymds_internal(julian_day,julian_sec,year,month,day,sec)
      !
	  END SUBROUTINE ju2ymds


      SUBROUTINE ju2ymds_internal (julian_day,julian_sec,year,
     &  month,day,sec)
	  !---------------------------------------------------------------------
	  !- This subroutine computes from the julian day the year,
	  !- month, day and seconds
      !-
	  !- In 1968 in a letter to the editor of Communications of the ACM
	  !- (CACM, volume 11, number 10, October 1968, p.657) Henry F. Fliegel
	  !- and Thomas C. Van Flandern presented such an algorithm.
	  !-
	  !- See also : http://www.magnet.ch/serendipity/hermetic/cal_stud/jdn.htm
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
      REAL,PARAMETER :: one_year = 365.2425
      INTEGER :: mon_len(12)=(/31,28,31,30,3
     &     1,30,31,31,30,31,30,31/)
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
	  ENDIF
	  !
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
	  ELSE IF (    (ABS(one_year-365.0) <= EPSILON(one_year)) 
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
		 d = l-ml+1
      ELSE
	     !-- others
		 ml = NINT(one_year/12.)
		 y = jd/NINT(one_year)
		 l = jd-y*NINT(one_year)
		 m = (l/ml)+1
		 d = l-(m-1)*ml+1
      ENDIF
	  !
	  day = d
      month = m
      year = y
      !
	  END SUBROUTINE ju2ymds_internal

   
      FUNCTION replace_blank(string,rep) RESULT(outs)
      !!----------------------------------------------------------------------
      !!                     ***  FUNCTION replace_blank  ***
      !!
      !! ** Purpose :   replace blank strin by a given pattern
      !!
      !! ** Method  :   
      !!
      !! ** History :   R. Benshila
      !!
      !!---------------------------------------------------------------------
      !
      CHARACTER(len=*) :: string, rep
      CHARACTER(len=100) :: outs
      !!---------------------------------------------------------------------
      INTEGER :: stringLen 
      INTEGER :: last, actual
      !!---------------------------------------------------------------------
      ! 
      outs=string
      stringLen = LEN(string)
      last = 1
      actual = 1

      DO WHILE (actual < stringLen)
        IF (string(last:last) == ' ') THEN
            actual = actual + 1
            outs(last:last) = '_'
            last=actual
        ELSE
            last = last + 1
            IF (actual < last) actual = last
        ENDIF
      END DO
      !
      END FUNCTION replace_blank

		    
      END MODULE xios_module

#include "cppdefs.h"
       SUBROUTINE init_xios(tile)

      IMPLICIT NONE
# include "param.h"
      INTEGER tile
# include "compute_tile_bounds.h"
      
      CALL init_xios_tile (Istr,Iend,Jstr,Jend)
      !
      END SUBROUTINE init_xios

      SUBROUTINE init_xios_tile(Istr,Iend,Jstr,Jend)
      !!----------------------------------------------------------------------
      !!                     ***  ROUTINE iom_sdate  ***
      !!
      !! ** Purpose :   defintion of grid for output
      !!
      !! ** Method  :   
      !!
      !! ** History :   R. Benshila, imrpovement for CROCO 
      !!                A. Ponte   , adaption for ROMS  
      !!                S. Masson  , original version (NEMO)  
      !!
      !!---------------------------------------------------------------------
      !
      USE xios     ! XIOS library
      USE xios_module
      !    
      IMPLICIT NONE
      !
# include "param.h"
# include "grid.h"
# include "ocean3d.h"
# include "scalars.h"
# include "ncscrum.h"
# include "mpi_roms.h"
      !
      INTEGER Istr,Iend,Jstr,Jend
      !
      INTEGER tile,ji
      INTEGER ilocal_comm
      TYPE(xios_time) :: dtime  = xios_time(0, 0, 0, 0, 0, 0)
      TYPE(xios_context) :: ctx_hdl
      TYPE(xios_field) :: field_hdl
      TYPE(xios_fieldgroup) :: fieldgroup_hdl
      TYPE(xios_file) :: file_hdl
      LOGICAL ok
      !
      CHARACTER(len=19) :: cldate
      CHARACTER(len=10) :: clname
      CHARACTER(len=10) :: cdname
      INTEGER iday,imonth,iyear
      !!---------------------------------------------------------------------
      !  
# include "compute_auxiliary_bounds.h"
      !
# ifdef EW_PERIODIC
#  define IU_RANGE Istr,Iend
#  define IV_RANGE Istr,Iend
# else
#  define IU_RANGE Istr,IendR
#  define IV_RANGE IstrR,IendR
# endif

# ifdef NS_PERIODIC
#  define JU_RANGE Jstr,Jend
#  define JV_RANGE Jstr,Jend
# else
#  define JU_RANGE JstrR,JendR
#  define JV_RANGE Jstr,JendR
# endif
      !
      cdname='roms'
      clname = cdname
# ifdef AGRIF
      IF( TRIM(Agrif_CFixed()) /= '0' )
     & clname =TRIM(cdname)//"."//TRIM(Agrif_CFixed())
# endif
# if defined MPI
      CALL xios_context_initialize(TRIM(clname), MPI_COMM_WORLD)
# else
      CALL xios_context_initialize(TRIM(clname), 0)
# endif
      CALL iom_swap( cdname )

      ! calendar parameters
      CALL xios_set_context_attr(TRIM(clname), calendar_type= 
     &      "Gregorian")

# ifdef START_DATE
      ! start_date with format dd-mm-yyyy
      READ(start_date(1:2),fmt='(i2)') iday
      READ(start_date(4:5),fmt='(i2)') imonth
      READ(start_date(7:10),fmt='(i4)') iyear
# else
      iday=1
      imonth=1       
      iyear=1
# endif
      WRITE(cldate,"(i4.4,'-',i2.2,'-',i2.2,' 00:00:00')") 
     & iyear,imonth,iday
      CALL xios_set_context_attr(TRIM(clname), 
     &        start_date=cldate )

      ! horizontal grid definition
      CALL set_scalar

# ifdef SPHERICAL
      CALL set_grid( "rho", lonr, latr )
      CALL set_grid( "b", lonr, latr )
      CALL set_grid( "u", lonu, latu )
      CALL set_grid( "v", lonv, latv )
      CALL set_grid( "w", lonr, latr )
# else
      CALL set_grid( "rho", xr, yr )
      CALL set_grid( "b", xr, yr )
      CALL set_grid( "u", xp, yr )
      CALL set_grid( "v", xr, yp )
      CALL set_grid( "w", xr, yr )
# endif       
 
      ! vertical grid definition
      CALL iom_set_axis_attr( "s_rho", sc_r )
      CALL iom_set_axis_attr( "s_w", sc_w )
# ifdef SEDIMENT
      CALL iom_set_axis_attr( "s_b",(/ (REAL(ji), ji=1,NLAY) /))
# else
      ! fake axis to avoid chaning xml file 
      CALL iom_set_axis_attr( "s_b",(/ (REAL(ji), ji=1,1) /))
# endif
      
      ! time axis (for ROMS tools)

      ! automatic definitions of some of the xml attributs
      CALL set_xmlatt

      ! end file definition
      dtime%second = dt
      CALL xios_set_timestep(dtime)

      ! end definition phase
      CALL xios_close_context_definition()
      !
      CALL xios_update_calendar(0)
      END SUBROUTINE init_xios_tile
      
      
      SUBROUTINE iom_swap( cdname )
      !!---------------------------------------------------------------------
      !!                   ***  SUBROUTINE  iom_swap  ***
      !!
      !! ** Purpose :  swap context between different agrif grid for xmlio_server
      !!---------------------------------------------------------------------
      USE xios
      !
      CHARACTER(len=*), INTENT(in) :: cdname
      TYPE(xios_context) :: roms_hdl
# ifdef AGRIF
      IF( TRIM(Agrif_CFixed()) == '0' ) THEN
        CALL xios_get_handle(TRIM(cdname),roms_hdl)
      ELSE
        CALL xios_get_handle(TRIM(cdname)//"."//TRIM(Agrif_CFixed()),roms_hdl)
      ENDIF
# else
      CALL xios_get_handle(TRIM(cdname),roms_hdl)

# endif
      !
      CALL xios_set_current_context(roms_hdl)
      !
      END SUBROUTINE iom_swap


      SUBROUTINE iom_context_finalize( cdname )
      !!---------------------------------------------------------------------
      !!                   ***  SUBROUTINE  iom_context_finalize  ***
      !!
      !! ** Purpose :  finalization of output definition
      !!---------------------------------------------------------------------
      USE xios
      !
      CHARACTER(LEN=*), INTENT(in) :: cdname
      TYPE(xios_context) :: roms_hdl
      !
      IF( xios_is_valid_context(cdname) ) THEN
         CALL iom_swap( cdname )   ! swap to cdname context
         CALL xios_context_finalize() ! finalize the context
         IF( cdname /= "roms" ) CALL iom_swap( "roms" )   ! return back to roms context
      ENDIF
      !
      END SUBROUTINE iom_context_finalize

#endif /* XIOS */





