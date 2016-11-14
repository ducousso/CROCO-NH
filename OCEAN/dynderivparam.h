! $Id: dynderivparam.h 1458 2014-02-03 15:01:25Z gcambon $
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
!
! Domain subdivision parameters:
! ====== =========== ===========

#ifdef AGRIF

# ifdef MPI
      Lm=(LLm+NP_XI-1)/NP_XI; Mm=(MMm+NP_ETA-1)/NP_ETA
# else
      Lm=LLm; Mm=MMm
# endif
!
! Derived dimension parameters.
!
      padd_X=(Lm+2)/2-(Lm+1)/2; padd_E=(Mm+2)/2-(Mm+1)/2
!********************************************************************
!            Horizontal array dimensions in netCDF files.
! xi_rho     WARNING!!! In MPI code in the case of PARALLEL_FILES 
! xi_u       _and_ NON-Periodicity in either XI- or ETA-direction,
! eta_rho    these depend on corresonding MPI-node indices ii,jj
! eta_v      and therefore become live variables, which are placed
!            into common block below rather than defined here as
!            parameters. 
!
! Note (P. Marchesiello): 
!   the remark above is now extended to periodic conditions, i.e., 
!   if PARALLEL_FILES is defined, netCDF files array dimensions are 
!   always set in MPI-Setup and depend on MPI-nodes. After rejoining 
!   the parallel files (ncjoin), the resulting global netCDF file has
!   the same dimension as it would have if PARALLEL_FILES was undefined.
!
# if defined MPI && defined PARALLEL_FILES
!# ifdef EW_PERIODIC
!      xi_rho=Lm;     xi_u=Lm
!# endif
!# ifdef NS_PERIODIC
!      eta_rho=Mm;    eta_v=Mm
!# endif
# else
      xi_rho=LLm+2;  xi_u=xi_rho-1
      eta_rho=MMm+2; eta_v=eta_rho-1
# endif

#endif  
! <- key AGRIF

!********************************************************************
#if defined AGRIF || defined AUTOTILING
# undef ALLOW_SINGLE_BLOCK_MODE
# ifdef  ALLOW_SINGLE_BLOCK_MODE
      size_XI=6+Lm; size_ETA=6+Mm
# else
      size_XI=7+(Lm+NSUB_X-1)/NSUB_X
      size_ETA=7+(Mm+NSUB_E-1)/NSUB_E
# endif
      sse=size_ETA/Np; ssz=Np/size_ETA
      se=sse/(sse+ssz);   sz=1-se
      N1dXI = size_XI
      N1dETA = size_ETA
      N2d=size_XI*(se*size_ETA+sz*Np)
      N3d=size_XI*size_ETA*Np
#endif

#if defined AGRIF && defined AUTOTILING
      MAX_NSUB_X=Lm/6
      MAX_NSUB_E=Mm/6 
#endif

#if !defined MPI
      iminmpi = 1
      jminmpi = 1
      Lmmpi = Lm
      Mmmpi = Mm
#endif

