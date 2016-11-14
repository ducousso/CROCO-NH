! $Id: set_global_definitions.h 1618 2014-12-18 14:39:51Z rblod $
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
/* 
======================================================================
 This is "global_definitions.h": It contains a set of predetermined
 macro definitions which are inserted into the individual files by
 C-preprocessor. General user is strongly discouraged from attempts
 to modify anything below this line.
======================================================================
*/

/*
  Define standard dimensions for the model arrays (vertical
 dimensions are inserted explicitly in the code, when needed). The
 periodic and nonperiodic versions may be different by the number of
 ghost points on each edge (2 or 1 respectively). This distinction
 is present only in the purely SHARED MEMORY code. In the case of
 message passing, when array dimensions correspond to a portion of
 the physical domain (as opposite to the whole domain), so two ghost
 zones are always provided on the each side. These data for these
 two ghost zones is then exchanged by message passing. 
*/
#if defined TS_HADV_UP5 || defined TS_HADV_C6 \
    || defined TS_HADV_WENO5 || defined BIO_HADV_WENO5
# define THREE_GHOST_POINTS
# define THREE_GHOST_POINTS_TS
# undef  THREE_GHOST_POINTS_UV
#endif

#ifdef THREE_GHOST_POINTS
# ifdef MPI
#  define GLOBAL_2D_ARRAY -2:Lm+3+padd_X,-2:Mm+3+padd_E
#  define GLOBAL_1D_ARRAYXI -2:Lm+3+padd_X
#  define GLOBAL_1D_ARRAYETA -2:Mm+3+padd_E
#  define START_2D_ARRAY -2,-2
#  define START_1D_ARRAYXI -2
#  define START_1D_ARRAYETA -2
# else
#  ifdef EW_PERIODIC
#   define GLOBAL_1D_ARRAYXI -2:Lm+3+padd_X
#   define START_1D_ARRAYXI -2
#   ifdef NS_PERIODIC
#    define GLOBAL_2D_ARRAY -2:Lm+3+padd_X,-2:Mm+3+padd_E
#    define GLOBAL_1D_ARRAYETA -2:Mm+3+padd_E
#    define START_2D_ARRAY -2,-2
#    define START_1D_ARRAYETA -2
#   else
#    define GLOBAL_2D_ARRAY -2:Lm+3+padd_X,0:Mm+1+padd_E
#    ifdef NBQ
#     define GLOBAL_2D_ARRAY_EXT_NBQ -2:Lm+3+padd_X,-1:Mm+2+padd_E
#    endif
#    define GLOBAL_1D_ARRAYETA 0:Mm+1+padd_E
#    define START_2D_ARRAY -2,0
#    define START_1D_ARRAYETA 0
#   endif
#  else
#   define GLOBAL_1D_ARRAYXI 0:Lm+1+padd_X
#   define START_1D_ARRAYXI 0
#   ifdef NS_PERIODIC
#    define GLOBAL_2D_ARRAY 0:Lm+1+padd_X,-2:Mm+3+padd_E
#    ifdef NBQ
#     define GLOBAL_2D_ARRAY_EXT_NBQ -1:Lm+2+padd_X,-2:Mm+3+padd_E
#    endif
#    define GLOBAL_1D_ARRAYETA -2:Mm+3+padd_E
#    define START_2D_ARRAY 0,-2
#    define START_1D_ARRAYETA -2
#   else
#    define GLOBAL_2D_ARRAY 0:Lm+1+padd_X,0:Mm+1+padd_E
#    ifdef NBQ
#     define GLOBAL_2D_ARRAY_EXT_NBQ -1:Lm+2+padd_X,-1:Mm+2+padd_E
#    endif
#    define GLOBAL_1D_ARRAYETA 0:Mm+1+padd_E
#    define START_2D_ARRAY 0,0
#    define START_1D_ARRAYETA 0
#   endif
#  endif
# endif
#else
# ifdef MPI
#  define GLOBAL_2D_ARRAY -1:Lm+2+padd_X,-1:Mm+2+padd_E
#  define GLOBAL_1D_ARRAYXI -1:Lm+2+padd_X
#  define GLOBAL_1D_ARRAYETA -1:Mm+2+padd_E
#  define START_2D_ARRAY -1,-1
#  define START_1D_ARRAYXI -1
#  define START_1D_ARRAYETA -1
# else
#  ifdef EW_PERIODIC
#   define GLOBAL_1D_ARRAYXI -1:Lm+2+padd_X
#   define START_1D_ARRAYXI -1
#   ifdef NS_PERIODIC
#    define GLOBAL_2D_ARRAY -1:Lm+2+padd_X,-1:Mm+2+padd_E
#    define GLOBAL_1D_ARRAYETA -1:Mm+2+padd_E
#    define START_2D_ARRAY -1,-1
#    define START_1D_ARRAYETA -1
#   else
#    define GLOBAL_2D_ARRAY -1:Lm+2+padd_X,0:Mm+1+padd_E
#    ifdef NBQ
#     define GLOBAL_2D_ARRAY_EXT_NBQ -1:Lm+2+padd_X,-1:Mm+2+padd_E
#    endif
#    define GLOBAL_1D_ARRAYETA 0:Mm+1+padd_E
#    define START_2D_ARRAY -1,0
#    define START_1D_ARRAYETA 0
#   endif
#  else
#   define GLOBAL_1D_ARRAYXI 0:Lm+1+padd_X
#   define START_1D_ARRAYXI 0
#   ifdef NS_PERIODIC
#    define GLOBAL_2D_ARRAY 0:Lm+1+padd_X,-1:Mm+2+padd_E
#    ifdef NBQ
#     define GLOBAL_2D_ARRAY_EXT_NBQ -1:Lm+2+padd_X,-1:Mm+2+padd_E
#    endif
#    define GLOBAL_1D_ARRAYETA -1:Mm+2+padd_E
#    define START_2D_ARRAY 0,-1
#    define START_1D_ARRAYETA -1
#   else
#    define GLOBAL_2D_ARRAY 0:Lm+1+padd_X,0:Mm+1+padd_E
#    ifdef NBQ
#     define GLOBAL_2D_ARRAY_EXT_NBQ -1:Lm+2+padd_X,-1:Mm+2+padd_E
#    endif
#    define GLOBAL_1D_ARRAYETA 0:Mm+1+padd_E
#    define START_2D_ARRAY 0,0
#    define START_1D_ARRAYETA 0
#   endif
#  endif
# endif
#endif

#define PRIVATE_1D_SCRATCH_ARRAY Istr-2:Iend+2
#define PRIVATE_2D_SCRATCH_ARRAY Istr-2:Iend+2,Jstr-2:Jend+2
#define PRIVATE_1DXI_SCRATCH_ARRAY Istr-2:Iend+2
#define PRIVATE_1DETA_SCRATCH_ARRAY Jstr-2:Jend+2

/*
  The following definitions contain fortran logical expressions
 equivalent to the question: ''Am I the thread working on subdomain
 [tile] which is adjacent to the WESTERN [EASTERN/SOUTHERN/NORTHERN]
 edge of the model domain?'' These logical expressions are used to
 control loop bounds over a subdomain [tile], so that boundary points
 are included, if needed and if the subdomain is adjacent to the
 boundary. They are also used to decide which thread is updating the
 segment of the boundary [bcs2d,bcs3d] to avoid mutual overlap. In
 the case when there is only one subdomain all four of these logical
 expressions have value .TRUE.

   Message passing and shared memory versions.
*/
#ifdef MPI
# define WESTERN_EDGE .not.WEST_INTER
# define EASTERN_EDGE .not.EAST_INTER
# define SOUTHERN_EDGE .not.SOUTH_INTER
# define NORTHERN_EDGE .not.NORTH_INTER
#else
# define WESTERN_EDGE istr.eq.1
# define EASTERN_EDGE iend.eq.Lm
# define SOUTHERN_EDGE jstr.eq.1
# define NORTHERN_EDGE jend.eq.Mm
#endif

/*
  Sometimes it is needed to include MPI-node number into printed
 message. To do it conditionally (MPI code only) add MYID (without
 preceeding comma) into the end of the message to be printed.
*/
#ifdef MPI
# define MYID ,' mynode =', mynode
#else
# define MYID !
#endif

/*
  Sometimes an operation needs to be restricted to one MPI process,
 the master process. Typically this occurs when it is desirable to
 avoid redundant write of the same message by all MPI processes into
 stdout. The following switch serves this purpose:
*/
#ifdef MPI
# define MPI_master_only if (mynode.eq.0)
#else
# define MPI_master_only
#endif

/*
  Similarly, if operation needed to be done by one thread only, e.g.
 copy a redundantly computed private scalar into shared scalar, or
 write an error message in situation when the error condition is
 discovered redundantly by every thread (and guaranteed to be the
 same for all) and only one needs to complain. The following flag is
 used to restrict the operation only to thread which is working on
 south-western tile. This switch is the same for MPI/nonMPI code.
*/
#define ZEROTH_TILE Istr+Jstr.eq.2

/*
  Occasinally a subroutine designed to process a tile may be called
 to process the whole domain. If it is necessary to distinguish
 whether it is being called for the whole domain (SINGLE_TILE_MODE)
 or a tile. This switch is the same for MPI/nonMPI code.
*/
#ifdef MPI
# undef AUTOTILING
# define SINGLE_TILE_MODE  Iend-Istr+Jend-Jstr.eq.Lmmpi+Mmmpi-2
#else
# define SINGLE_TILE_MODE  Iend-Istr+Jend-Jstr.eq.Lm+Mm-2
#endif

/*
  Define time indices logic
*/
#define FIRST_TIME_STEP iic.eq.ntstart
#ifdef SOLVE3D
# define FIRST_2D_STEP iif.eq.1
# define NOT_LAST_2D_STEP iif.lt.nfast+1
#else
# define FIRST_2D_STEP iic.eq.ntstart
# define NOT_LAST_2D_STEP iic.lt.ntimes+2
#endif

/* Switch ON/OFF double precision for real type variables (since this
 is mostly controlled by mpc and/or compuler options, this CPP-switch
 affects only on the correct choice of netCDF functions, see below)
 and the use QUAD precision for global summation variables, which is
 always desirable, but some compilers do not support it.
*/
#define DBLEPREC

#if defined DBLEPREC && !defined Linux && !defined PGI && !defined __IFC
# define QUAD 16
# define QuadZero 0.Q0
/* # define QuadZero 0.0_16 */
!! Gc remove because incompatibe with AGRIF
!#elif defined DBLEPREC && defined Ifort
!/* Ifort supports QUAD precision */
!# define QUAD 16
!# define QuadZero 0.Q0
!! Gc remove because incompatibe with AGRIF
#else
# define QUAD 8
# define QuadZero 0.D0
#endif

/*
  The following definitions are machine dependent macros, compiler
 directives, etc. A proper set of definitions is activated by a
 proper choice C-preprocessor flag, i.e. -DSGI for an SGI computer
 or -DCRAY for a Cray shared memory architecture (Y-MP, C-90, J-90).
 Definitions for other shared memory platforms may be appended here.
*/
#if defined sgi || defined SGI
# define CVECTOR CDIR$ IVDEP
# define CSDOACROSS C$DOACROSS
# define CAND C$&
# define ENTER_CRITICAL_REGION SPACE call mp_setlock()
# define EXIT_CRITICAL_REGION  SPACE call mp_unsetlock()
# define CSDISTRIBUTE_RESHAPE !! c$distribute 
/* # define CSDISTRIBUTE_RESHAPE !! c$distribute_reshape */
# define BLOCK_PATTERN block,block
# define BLOCK_CLAUSE !! onto(2,*)
#elif defined cray || defined CRAY
# ifdef  DBLEPREC
#  undef  DBLEPREC
# endif
# define CVECTOR CDIR$ IVDEP
# define CSDOACROSS CMIC$ DO ALL
# define SHARE SHARED
# define LOCAL PRIVATE
# define CAND CMIC$&
# define ENTER_CRITICAL_REGION CMIC$ GUARD
# define EXIT_CRITICAL_REGION CMIC$ END GUARD
#endif

/*
   Put grid variables in output files     
*/
#define PUT_GRID_INTO_RESTART
#define PUT_GRID_INTO_HISTORY
#define PUT_GRID_INTO_AVERAGES

/*
  Choice of double/single precision for real type variables
 and associated intrinsic functions.
*/
#ifdef DBLEPREC
!-# define float dfloat
!-# define FLoaT dfloat
!-# define FLOAT dfloat
!-# define sqrt dsqrt
!-# define SQRT dsqrt
!-# define exp dexp
!-# define EXP dexp
!-# define dtanh dtanh
!-# define TANH dtanh
# define NF_FTYPE NF_DOUBLE
# define nf_get_att_FTYPE nf_get_att_double
# define nf_put_att_FTYPE nf_put_att_double
# define nf_get_var1_FTYPE nf_get_var1_double
# define nf_put_var1_FTYPE nf_put_var1_double
# define nf_get_vara_FTYPE nf_get_vara_double
# define nf_put_vara_FTYPE nf_put_vara_double
# define nf_put_var_FTYPE nf_put_var_double
# define nf_put_att_FTYPE nf_put_att_double
#else
# define NF_FTYPE NF_REAL
# define nf_get_att_FTYPE nf_get_att_real
# define nf_put_att_FTYPE nf_put_att_real
# define nf_get_var1_FTYPE nf_get_var1_real
# define nf_put_var1_FTYPE nf_put_var1_real
# define nf_get_vara_FTYPE nf_get_vara_real
# define nf_put_vara_FTYPE nf_put_vara_real
# define nf_put_var_FTYPE nf_put_var_real
# define nf_put_att_FTYPE nf_put_att_real
#endif

/*
 Choice of double/single precision for netCDF output.
*/
#ifdef OUT_DOUBLE
# define NF_FOUT NF_DOUBLE
#else
# define NF_FOUT NF_REAL
#endif

/*
 Decide which time step of fast variables zeta, ubar, vbar goes
 to output.
*/ 
#ifdef SOLVE3D
# define fast_indx_out knew
#else
# define fast_indx_out kstp
#endif


