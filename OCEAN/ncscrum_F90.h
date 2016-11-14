! $Id: ncscrum.h 1588 2014-08-04 16:26:01Z marchesiello $
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
! This is include file "ncscrum.h".
! ==== == ======= ==== ============
!

      integer r2dvar, u2dvar, v2dvar, p2dvar, r3dvar, &
     &                u3dvar, v3dvar, p3dvar, w3dvar, b3dvar
      parameter (r2dvar=0, u2dvar=1, v2dvar=2, p2dvar=3, &
     & r3dvar=4, u3dvar=5, v3dvar=6, p3dvar=7, w3dvar=8,b3dvar=12) 


      character*80 date_str, title, start_date 
      character*80 ininame,  grdname,  hisname &
     &         ,   rstname,  frcname,  bulkname,  usrname &
#ifdef AVERAGES
     &                                ,   avgname &
#endif
#ifdef DIAGNOSTICS_TS 
     &                                ,  dianame &
# ifdef AVERAGES
     &                                ,  dianame_avg &
# endif
#endif
#ifdef DIAGNOSTICS_UV
     &                                ,  dianameM &
# ifdef AVERAGES
     &                                ,  dianameM_avg &
# endif
#endif
#ifdef DIAGNOSTICS_BIO
     &                                ,  dianamebio &
# ifdef AVERAGES
     &                                ,  dianamebio_avg &
# endif
#endif
#if (defined TCLIMATOLOGY  && !defined ANA_TCLIMA)\
 || (defined ZCLIMATOLOGY  && !defined ANA_SSH)\
 || (defined M2CLIMATOLOGY && !defined ANA_M2CLIMA)\
 || (defined M3CLIMATOLOGY && !defined ANA_M3CLIMA)
     &                                ,   clmname &
#endif
#ifdef FRC_BRY 
     &                                ,   bry_file &
#endif
#if defined WKB_WWAVE && !defined ANA_BRY_WKB
     &                                ,   brywkb_file &
#endif
#ifdef ASSIMILATION
     &                      ,  aparnam,   assname &
#endif
#ifdef BIOLOGY
     &                                ,   bioname &
#endif
#ifdef SEDIMENT
     &                                ,   sedname &
#endif
     &         ,   qbarname, tsrcname

#ifdef SOLVE3D
      character*75  vname(20, 500)
#else
      character*75  vname(20, 90)
#endif

      common /cncscrum/       date_str,   title,  start_date &
     &         ,   ininame,  grdname, hisname &
     &         ,   rstname,  frcname, bulkname,  usrname &
     &         ,   qbarname, tsrcname &
#ifdef AVERAGES
     &                                ,  avgname &
#endif
#if defined DIAGNOSTICS_TS
     &                                ,  dianame &
# ifdef AVERAGES
     &                                ,  dianame_avg &
# endif
#endif
#if defined DIAGNOSTICS_UV
     &                                ,  dianameM &
# ifdef AVERAGES
     &                                ,  dianameM_avg &
# endif
#endif
#ifdef DIAGNOSTICS_BIO
     &                                ,  dianamebio &
# ifdef AVERAGES
     &                                ,  dianamebio_avg &
# endif
#endif
#if (defined TCLIMATOLOGY  && !defined ANA_TCLIMA)\
 || (defined ZCLIMATOLOGY  && !defined ANA_SSH)\
 || (defined M2CLIMATOLOGY && !defined ANA_M2CLIMA)\
 || (defined M3CLIMATOLOGY && !defined ANA_M3CLIMA)
     &                                ,   clmname &
#endif
#ifdef FRC_BRY
     &                                ,   bry_file &
#endif
#if defined WKB_WWAVE && !defined ANA_BRY_WKB
     &                                ,   brywkb_file &
#endif
#ifdef ASSIMILATION
     &                      ,  aparnam,   assname &
#endif
#ifdef SEDIMENT
     &                      ,  sedname &
#endif
#ifdef BIOLOGY
     &                      ,  bioname &
#endif
     &                      ,  vname

