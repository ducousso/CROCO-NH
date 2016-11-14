! $Id: etalon_data.h 784 2012-03-15 11:45:05Z gcambon $
!======================================================================
! ROMS_AGRIF is a branch of ROMS developped at IRD and INRIA, in France
! The two other branches from UCLA (Shchepetkin et al) 
! and Rutgers University (Arango et al) are under MIT/X style license.
! ROMS_AGRIF specific routines (nesting) are under CeCILL-C license.
! 
! ROMS_AGRIF website : http://roms.mpl.ird.fr
!======================================================================
!
!===================================================================
#if defined AGRIF && !defined AGRIF_2WAY
      if (Agrif_Root()) then
        check_point(1)=1
        etalon_line(1)=
     &    '4.933231491E-05 2.6729110E+01 2.6729159E+01 4.3513589E+15'
        check_point(2)=100
        etalon_line(2)=
     &    '1.047386115E-03 2.6751426E+01 2.6752474E+01 4.3513425E+15'
        check_point(3)=200
        etalon_line(3)=
     &    '1.082244925E-03 2.6781480E+01 2.6782562E+01 4.3513694E+15'
        check_point(4)=400
        etalon_line(4)=
     &    '1.365199027E-03 2.6815489E+01 2.6816854E+01 4.3514026E+15'
        check_point(5)=600
        etalon_line(5)=
     &    '2.186481330E-03 2.6887946E+01 2.6890133E+01 4.3514522E+15'
        check_point(6)=720
        etalon_line(6)=
     &    '2.723826168E-03 2.6932442E+01 2.6935165E+01 4.3514815E+15'
        else
!-------------------------------------------------------------------
        check_point(1)=1
        etalon_line(1)=
     &    '2.359677007E-05 2.0769407E+01 2.0769431E+01 4.5655748E+14'
        check_point(2)=300
        etalon_line(2)=
     &    '9.109429940E-04 2.0755151E+01 2.0756062E+01 4.5653911E+14'
        check_point(3)=600
        etalon_line(3)=
     &    '9.313027129E-04 2.0750639E+01 2.0751571E+01 4.5654491E+14'
        check_point(4)=1200
        etalon_line(4)=
     &    '1.353043403E-03 2.0743701E+01 2.0745054E+01 4.5655004E+14'
        check_point(5)=1800
        etalon_line(5)=
     &    '1.862819959E-03 2.0715330E+01 2.0717193E+01 4.5655567E+14'
        check_point(6)=2160
        etalon_line(6)=
     &    '2.521406081E-03 2.0693429E+01 2.0695950E+01 4.5656143E+14'
        endif
!====================================================================
#elif defined AGRIF && defined AGRIF_2WAY
      if (Agrif_Root()) then
        check_point(1)=1
        etalon_line(1)=
     &    '4.844591562E-05 2.6735577E+01 2.6735626E+01 4.3496646E+15'
        check_point(2)=100
        etalon_line(2)=
     &    '1.031913720E-03 2.6757359E+01 2.6758391E+01 4.3496488E+15'
        check_point(3)=200
        etalon_line(3)=
     &    '1.062522681E-03 2.6786104E+01 2.6787167E+01 4.3496743E+15'
        check_point(4)=400
        etalon_line(4)=
     &    '1.310750312E-03 2.6823041E+01 2.6824352E+01 4.3497107E+15'
        check_point(5)=600
        etalon_line(5)=
     &    '2.205242188E-03 2.6899377E+01 2.6901582E+01 4.3497654E+15'
        check_point(6)=720
        etalon_line(6)=
     &    '2.846020342E-03 2.6941020E+01 2.6943866E+01 4.3497918E+15'
        else
!-------------------------------------------------------------------
        check_point(1)=1
        etalon_line(1)=
     &    '2.369560524E-05 2.0769292E+01 2.0769316E+01 4.5655746E+14'
        check_point(2)=300
        etalon_line(2)=
     &    '1.224654905E-03 2.0765802E+01 2.0767026E+01 4.5653922E+14'
        check_point(3)=600
        etalon_line(3)=
     &    '1.168878748E-03 2.0762968E+01 2.0764137E+01 4.5654420E+14'
        check_point(4)=1200
        etalon_line(4)=
     &    '1.380687073E-03 2.0739928E+01 2.0741308E+01 4.5654985E+14'
        check_point(5)=1800
        etalon_line(5)=
     &    '1.902295322E-03 2.0741347E+01 2.0743249E+01 4.5655944E+14'
        check_point(6)=2160
        etalon_line(6)=
     &    '3.027461293E-03 2.0732648E+01 2.0735676E+01 4.5656314E+14'
        endif
!===================================================================
#else 
        check_point(1)=1
        etalon_line(1)=
     &    '4.933231491E-05 2.6729110E+01 2.6729159E+01 4.3513589E+15'
        check_point(2)=100
        etalon_line(2)=
     &    '1.047386115E-03 2.6751426E+01 2.6752474E+01 4.3513425E+15'
        check_point(3)=200
        etalon_line(3)=
     &    '1.082244925E-03 2.6781480E+01 2.6782562E+01 4.3513694E+15'
        check_point(4)=400
        etalon_line(4)=
     &    '1.365199027E-03 2.6815489E+01 2.6816854E+01 4.3514026E+15'
        check_point(5)=600
        etalon_line(5)=
     &    '2.186481330E-03 2.6887946E+01 2.6890133E+01 4.3514522E+15'
        check_point(6)=720
        etalon_line(6)=
     &    '2.723826168E-03 2.6932442E+01 2.6935165E+01 4.3514815E+15'
#endif
