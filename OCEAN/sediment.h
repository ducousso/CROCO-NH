! $Id: sediment.h 1458 2014-02-03 15:01:25Z gcambon $
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
#ifdef SEDIMENT
/*
** Include file "sediment.h".
************************************* Meinte Blaas, John C. Warner ***
** Copyright (c) 2002/2004 Rutgers, UCLA                            **
************************************************* Hernan G. Arango ***
**                                                                  **
**  Bthk(NLAY)     User-defined initial bed layer thickness (m)     **
**  Bpor(NLAY)     User-defined initial porosity of bed layer       **
**  Bfr(NLAY,NST)  User-defined initial vol.fraction of layer, class *
**                                                                  **
**  Hrip      User-defined initial ripple height from file          **
**  Lrip      User-defined initial ripple length from file          **
**                                                                  **
**  bed_thick Sediment bed layer thickness (m)                      **
**  bed_poros Sediment bed layer porosity (void/total bed vol.)     **
**  bed_frac  Volume fraction of size class in bed layer            **
**  bed_age   Mass of sediment per layer                            **
**  bed_mass  Mass of sediment per layer                            **
**  bot_thick Active layer thickness                                **
**                                                                  **
**  settling_flux   depostion flux (kg/m2)                          **
**  ero_flux        erosion flux   (kg/m2)                          **
**  bedldu          bedload flux in xsi direction (kg/m2)           **                                                 
**  bedldv          bedload flux in eta direction (kg/m2)           **
**                                                                  **
**  morph_fac  Morphodynamic factor (non dimensionnal)              **
**                                                                  **
** Parameters for sediment model:                                   **
**                                                                  **
**  Csed     Sediment concentration (mg/l), used during analytical  **
**             initialization.                                      **
**  Erate    Surface erosion rate (kg/m2/s).                        **
**  Sd       Sediment grain diameter per size class (m).            **
**  Srho     Sediment grain density (kg/m3).                        **
**  Wsed     Particle settling velocity (m/s).                      **
**  tau_ce   Kinematic critical shear for erosion (m2/s2).          **
**  tau_cd   Kinematic critical shear for deposition (m2/s2).       **
**                                                                  **
** Sediment tracers identification indices:                         **
**                                                                  **
**  idsed    Cohesive and noncohesive sediment indices.             **
**  idmud    Cohesive sediment indices.                             **
**  isand    Noncohesive sediment indices.                          **
**                                                                  **
**  Stitle   Name of sediment.in inputfile                          **
**********************************************************************
*/  
      real Csed(NST), Erate(NST), Sd(NST), Srho(NST),
     &     Wsed(NST), tau_ce(NST), tau_cd(NST)
      common /ssediment/
     &        Csed, Erate, Sd,
     &        Srho, Wsed, tau_ce, tau_cd

      real Bthk(NLAY), Bpor(NLAY)
      common /sediment_bedthk/ Bthk, Bpor
             
      real Bfr(NLAY,NST)
      common /sediment_bedfrc/ Bfr      
      
      real Hrip, Lrip
      common /sediment_bedrip/ Hrip, Lrip
      
      real bed_thick(GLOBAL_2D_ARRAY,NLAY),
     &     bed_poros(GLOBAL_2D_ARRAY,NLAY),
     &     bed_age  (GLOBAL_2D_ARRAY,NLAY),
     &     bed_mass (GLOBAL_2D_ARRAY,NLAY,2,NST),
     &     worksed_bed(GLOBAL_2D_ARRAY,NLAY)
      common /sediment_bed/ bed_thick, bed_poros,
     &                      bed_age  , bed_mass,
     &                      worksed_bed

      real bed_frac(GLOBAL_2D_ARRAY,NLAY,NST),
     &     worksed_frac(GLOBAL_2D_ARRAY,NLAY)
      common /sediment_frac/ bed_frac, worksed_frac     

      real bot_thick(GLOBAL_2D_ARRAY)
      common /bot_thick/ bot_thick 

      real bedload_coeff, morph_fac
      common /bed_coeff/ bedload_coeff, morph_fac 
  
# ifdef SUSPLOAD
      real settling_flux(GLOBAL_2D_ARRAY,NST)
      real ero_flux(GLOBAL_2D_ARRAY,NST)
      common /sed_settling/ settling_flux,ero_flux  
# endif
# ifdef BEDLOAD
      real bedldu(GLOBAL_2D_ARRAY,NST)
      real bedldv(GLOBAL_2D_ARRAY,NST)
      common /sed_bedload/ bedldu, bedldv  
# endif
# ifdef MOVING_BATHY
      real bed_thick_tot(GLOBAL_2D_ARRAY,2)
      common /sed_morph/ bed_thick_tot
# endif
  
# ifdef AVERAGES
      real bed_frac_avg(GLOBAL_2D_ARRAY,NLAY,NST)
      common /sediment_frac_avg/ bed_frac_avg
# endif

      character*80 Stitle
      common /charseds/ Stitle

#endif /* SEDIMENT */

