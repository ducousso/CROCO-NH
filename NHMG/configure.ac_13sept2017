########################################################################
## Process this file with autoconf to produce a configure script.
##
## If you change this script, submit these commands after change(s) :
## aclocal ; automake ; autoconf     
########################################################################
## Non-Hydrodynamic Multi-Grids library
## (https://github.com/ngrigri/NHMG)
##
## OriginalAuthor@mail of this configure.ac : Nicolas.Grima@univ-brest.fr
## Date                                     : January 2013
########################################################################
## How to use it:
##--------------
## aclocal
## autoconf
## automake
## autoreconf
##
## To clean files after ./configure:
## make mostlyclean
## make clean
## make distclean
## (make maintainer-clean)
########################################################################
#
########################################################################
# Initialization calls
########################################################################
## Revision stamp from configure.ac into configure.
AC_REVISION($Revision: 0.0.0 $)
#
## Ensure that a recent enough version of Autotconf is being used.
AC_PREREQ([2.59])
#
## Initialize autoconf with name of project, version, bug report 
## and support email address.
AC_INIT([NHMG],[0.1.8],[nmolem@atmos.ucla.edu, Guillaume.Roullet@univ-brest.fr, Nicolas.Ducousso@univ-brest.fr, Nicolas.Grima@univ-brest.fr ],[NHMG],[https://github.com/ngrigri/NHMG])
#
#
## Set the directory that contains support scripts such as install-sh
## and config.guess
AC_CONFIG_AUX_DIR(./confdb)
#
AM_INIT_AUTOMAKE
#
## Compute the canonical host-system type variable "host".
AC_CANONICAL_HOST
#
# Keep libtool macros in an m4 directory.
AC_CONFIG_MACRO_DIR([m4])
#
# Set up libtool.
AC_MSG_NOTICE([setting up libtool])
LT_PREREQ([2.2])
LT_INIT
#
#
#########################################################################
# Checks for typedefs, structures, and compiler characteristics.
#########################################################################
AC_PROG_INSTALL

#
## Configue argument definition.
## AC_ARG_ENABLE(..,..,etc)
#
## OPTIMIZATION LEVELS
##
AC_ARG_ENABLE(
    optimization,
    [  --enable-optimization=level - Control optimization level.
                             The following levels are supported.
       debug     - debugging compiler options will be selected.
       normal    - soft optimization (default).
       aggressive - aggressive optimization (YOU HAVE TO VERIFY YOUR RESULTS!).],
    ,
    enable_optimization=normal
             )

## Specific Intel processor Sandy Bridge architecture -xAVX compiler option
#
AC_ARG_ENABLE(
    avx,
    [  --enable-avx - Turn on profiling compiler options.],
    enable_avx=yes,
    enable_avx=no
             )

#
## PROFILING (if it is available)
##
AC_ARG_ENABLE(
    profiling,
    [  --enable-profiling - Turn on profiling compiler options.],
    enable_prof=yes,
    enable_prof=no
             )

#
## Intel trace analyser (if it is available)
##
AC_ARG_ENABLE(
    traceanalyser,
    [  --enable-traceanalyser - Intel traceanalyser (in mpi mode only !).],
    enable_traceanalyser=yes,
    enable_traceanalyser=no
             )

#
## MPI libraries
##
AC_ARG_ENABLE(
    mpt,
    [  --enable-mpi_mpt - MPT library and includes (caparmor!).],
    enable_mpi_mpt=yes,
    enable_mpi_mpt=no
             )

AC_ARG_ENABLE(
    intel_mpi,
    [  --enable-intel_mpi - Intel MPI library and includes.],
    enable_intel_mpi=yes,
    enable_intel_mpi=no
             )

AC_ARG_ENABLE(
    openmpi,
    [  --enable-openmpi - OpenMPI library and includes.],
    enable_openmpi=yes,
    enable_openmpi=no
             )

AC_ARG_ENABLE(
    mpich,
    [  --enable-mpich - MPICH library and includes.],
    enable_mpich=yes,
    enable_mpich=no
             )

#
## OpenMP
##
AC_ARG_ENABLE(
    openmp,
    [  --enable-openmp - OpenMP parallelisation.],
    enable_openmp=yes,
    enable_openmp=no
             )

#
## Netcdf if it is need for Netcdf4
##
AC_ARG_ENABLE(
    netcdf,
    [  --enable-netcdf - Netcdf library and includes.],
    enable_netcdf=yes,
    enable_netcdf=no
             )

#
## Hdf5 if it is need for Netcdf4
##
AC_ARG_ENABLE(
    hdf5,
    [  --enable-hdf5 - HDF5 for Netcdf 4.],
    enable_hdf5=yes,
    enable_hdf5=no
             )

#
## IDRIS ada specific LDFLAGS options !?!
##
AC_ARG_ENABLE(
    ada,
    [  --enable-ada - Netcdf libraries are explicitly added in LDFLAGS options.],
    enable_ada=yes,
    enable_ada=no
             )

AC_ARG_VAR(MPI_LIB,Location of MPI library (compile-time))
AC_ARG_VAR(MPI_INC,Location of MPI module (compile-time))

##################################
## Check for a Fortran compiler ##
##################################
AC_LANG(Fortran)
#------------------------------------------------------------------------
# list of Fortran compilers extracted from the NetCDF configure.in file.
#------------------------------------------------------------------------
# ifort is the Intel f90/95/2003 compiler.
# gfortran is the official GNU fortran compiler from gcc suite.
#........................................................................
# f95 is a generic name for Fortran 95 compiler (not tested).
# f90 is a generic name for Fortran compilers. 
# Host must be tested to choose right options.
#    - SGI (Irix OS)
#    - HP-Compaq Alpha/Server (Tru64 OS)
#    - NEC-SX (SUPER-UX OS)
# pgf90 are the Portland Group F77 and F90 compilers.
# xlf90/xlf95 are IBM (AIX) F90/F95 compilers.
# lf95 is the Lahey-Fujitsu compiler.
# fort is the Compaq Fortran 90 (now 95) compiler for Linux/Alpha.
# pathf90 is the Pathscale Fortran 90 compiler.
# g95 is the GNU Fortran compiler.
#------------------------------------------------------------------------
AC_PROG_FC(ifort gfortran f95 fort xlf90 pgf90 epcf90 pathf90 f90 xlf95 lf95 g95)
##AC_PROG_FC
if test -n "$FC" ; then 
  case "$FC" in
##    GENERIC FORTRAN COMPILER (SGI-IRIX, HP-TRUE64, NEC-SX )
    f90)
      case "$host" in
        *-sgi-irix*)
           case "$enable_optimization" in
	         debug)
               AC_MSG_NOTICE([  -> DEBUGGING OPTIONS are SELECTED])
               FCFLAGS="-g -O0 -C -fullwarn -DEBUG:trap_uninitialized=ON:subscript_check=ON"
               LDFLAGS="-g"
             ;;
             aggressive)
               AC_MSG_NOTICE([  -> AGGRESSIVE COMPILER OPTIONS are SELECTED])
    	       FCFLAGS="-g3 -O3 -ipa -listing"
               LDFLAGS="-g3"
             ;;
             normal|*)
               AC_MSG_NOTICE([  -> NORMAL MODE])
	           FCFLAGS="-g3 -O2 -listing"
              LDFLAGS="-g3"
	         ;;
           esac
	       if test "$enable_prof" = "yes" ; then
              AC_MSG_WARN([!!! NO PROFILING COMPILER OPTIONS ON IRIX SYSTEM !!!])
              AC_MSG_WARN([!!!        PLEASE READ SPEEDSHOP MANUAL          !!!])
           fi
        ;;
        alpha*-dec-osf*)
          case "$enable_optimization" in
	        debug)
              AC_MSG_NOTICE([  -> DEBUGGING OPTIONS are SELECTED])
              FCFLAGS="-V -ladebug -g -O0 -C -check overflow -check underflow -warn nouninitialized -warn argument_checking"
              LDFLAGS="-ladebug -g"
            ;;
            aggressive)
              AC_MSG_NOTICE([  -> AGGRESSIVE COMPILER OPTIONS are SELECTED])
    	      FCFLAGS="-V -g3 -fast -math_library fast"
              LDFLAGS="-g3 -fast -math_library fast"
            ;;
            normal|*)
              AC_MSG_NOTICE([  -> NORMAL MODE])
	          FCFLAGS="-V -g3 -O"
              LDFLAGS=""
	        ;;
          esac
	      if test "$enable_prof" = "yes" ; then
            AC_MSG_NOTICE([  -> PROFILING is SELECTED (gprof)])
            FCFLAGS="-pg $FCFLAGS"
            LDFLAGS="-pg $LDFLAGS"
          fi
        ;;
        *nec*superux*)
          case "$enable_optimization" in
	        debug)
              AC_MSG_NOTICE([  -> DEBUGGING OPTIONS are SELECTED])
              FCFLAGS='-C debug -eR -eP -R1 -R5 -Wf"-L nostdout" -Wf"-L source mrgmsg" -Wf"-L summary" -Wf"-init stack=nan" -Wf"-init heap=nan" -Wl"-f nan" Wf"-msg d" -Wf"-msg o"'
              LDFLAGS="-C debug"
            ;;
            aggressive)
              AC_MSG_NOTICE([  -> AGGRESSIVE COMPILER OPTIONS are SELECTED])
    	      FCFLAGS='-C hopt -R1 -R5 -Wf"-L nostdout"  -Wf"-L summary" -Wf"-pvctl fullmsg" -Wf"-O infomsg"'
              LDFLAGS="-C hopt"
            ;;
            normal|*)
              AC_MSG_NOTICE([  -> NORMAL MODE])
	          FCFLAGS='-R1 -R5 -Wf"-L nostdout"  -Wf"-L summary" -Wf"-pvctl fullmsg" -Wf"-O infomsg"'
              LDFLAGS=""
	        ;;
          esac
	      if test "$enable_prof" = "yes" ; then
            AC_MSG_NOTICE([  -> PROFILING is SELECTED (gprof)])
            FCFLAGS="-ftrace $FCFLAGS"
            LDFLAGS="-ftrace $LDFLAGS"
          fi
	      if test "$enable_openmp" = "yes" ; then
            AC_MSG_NOTICE([  -> OpenMP PARALLELISATION IS SELECTED])
            FCFLAGS="-Popenmp $FCFLAGS"
            LDFLAGS="-Popenmp $LDFLAGS"
          fi
	    ;;
        *solaris*)
          case "$enable_optimization" in
	        debug)
              AC_MSG_NOTICE([  -> DEBUGGING OPTIONS are SELECTED])
              FCFLAGS="-g -C -stackvar -w4"
              LDFLAGS="-g "
            ;;
            aggressive)
              AC_MSG_NOTICE([  -> AGGRESSIVE COMPILER OPTIONS are SELECTED])
    	      FCFLAGS="-O5 -fast"
              LDFLAGS=""
            ;;
            normal|*)
              AC_MSG_NOTICE([  -> NORMAL MODE])
	         FCFLAGS="-O2"
              LDFLAGS=""
	        ;;
          esac
	      if test "$enable_prof" = "yes" ; then
            AC_MSG_NOTICE([  -> PROFILING is SELECTED (prof)])
            FCFLAGS="-p $FCFLAGS"
            LDFLAGS="-p $LDFLAGS"
          fi
        ;;
        *)
          AC_MSG_WARN([NHMG MSG: !!! HOST and/or SYSTEM is UNKNOWN : $host !!!])
        ;;
        esac
      ;;
##    INTEL FORTRAN COMPILER on LINUX OPERATING SYSTEM
      ifort)
	    case "$enable_optimization" in
	      debug)
            AC_MSG_NOTICE([ifort -> DEBUGGING OPTIONS are SELECTED])
	        CFLAGS="-g -O0"
            FCFLAGS="-g -O0 -check all -traceback -warn all -warn unused -debug variable_locations"
	        LDFLAGS="-g -O0 -check all -traceback -auto"
          ;;
          aggressive)
            AC_MSG_NOTICE([ifort -> AGGRESSIVE COMPILER OPTIONS are SELECTED])
	        CFLAGS=" -g -O3 -ipo"
    	   	FCFLAGS="-g -O3 -ipo"
            LDFLAGS="-g -O3 -ipo"
          ;;
          normal|*)
            AC_MSG_NOTICE([ifort -> NORMAL MODE])
	        CFLAGS="-g -O2"
	        FCFLAGS="-g -O2"
	      ;;
	    esac
        if test "$enable_avx" = "yes" ; then
              FCFLAGS="$FCFLAGS -xAVX"
              LDFLAGS="$LDFLAGS -xAVX"
        else
              FCFLAGS="$FCFLAGS -xHost"
              LDFLAGS="$LDFLAGS -xHost"
        fi
	    if test "$enable_prof" = "yes" ; then
          AC_MSG_NOTICE([      -> PROFILING is SELECTED (gprof and intel report)])
	      CFLAGS="-pg $CFLAGS"
##          FCFLAGS="-pg -opt-report 3 -opt-report-file=$<.log -opt-report-phase=all -profile-functions -profile-loops=all -profile-loops-report=2 $FCFLAGS"
            FCFLAGS="-pg -qopt-report=5 -qopt-report-phase=all -qopt-report-file=$<.log $FCFLAGS"
          LDFLAGS="-pg $LDFLAGS"
        fi
     	if test "$enable_openmp" = "yes" ; then
          AC_MSG_NOTICE([      -> OpenMP PARALLELISATION IS SELECTED])
	      CFLAGS="-openmp $CFLAGS"
          FCFLAGS="-openmp $FCFLAGS"
          LDFLAGS="-openmp $LDFLAGS"
        fi
      ;;
##    IBM FORTRAN COMPILER on AIX OPERATING SYSTEM
      xlf90|xlf95)
	    case "$enable_optimization" in
	      debug)
            FCFLAGS="-qsuffix=cpp=F90 -qsuffix=f=f90 -qfree=f90 -g -qnooptimize -C -qinitauto=7FBFFFFF -qflttrap=overflow:underflow:zerodivide:invalid:enable -qfloat=nans -qsigtrap -qextchk"
          ;;
          aggressive)
            FCFLAGS="-qsuffix=cpp=F90 -qsuffix=f=f90 -qfree=f90 -O5 -qipa=level=2 -qessl -qhot=vector -qunroll"
            LDFLAGS="-qessl"
          ;;
          normal|*)
            FCFLAGS="-qsuffix=cpp=F90 -qsuffix=f=f90 -qfree=f90 -O3 -qstrict"
	      ;;
	    esac
	    if test "$enable_prof" = "yes" ; then
          AC_MSG_NOTICE([  -> PROFILING is SELECTED (gprof)])
          FCFLAGS="-pg $FCFLAGS"
          LDFLAGS="-pg $LDFLAGS"
        fi
	    if test "$enable_openmp" = "yes" ; then
          AC_MSG_NOTICE([  -> OpenMP PARALLELISATION IS SELECTED])
          FCFLAGS="-qnosave -qsmp=omp $FCFLAGS"
          LDFLAGS="-qnosave -qsmp=omp $LDFLAGS"
        fi
      ;;
##    PORTLAND GROUP FORTRAN COMPILER
      pgf90)
	    FCFLAGS="-g"
      ;;
##    GENERIC Fortran 95 compiler (not tested)
      f95)
    	FCFLAGS="-g"
      ;;
##    HP_COMPAQ ALPHASERVER FORTRAN COMPILER (LINUX OPERATING SYSTEM)
      fort)
	    FCFLAGS="-g"
      ;;
##    Lahey-Fujitsu compiler
      lf95)
	    FCFLAGS="-g"
      ;;
##    PATHSCALE FORTRAN COMPILER (AMD-OPTERON) (Not Tested)
      pathf90)
	    FCFLAGS="-g"
      ;;
##    GNU FORTRAN 90/95 COMPILER (Tested on Intel-PC and Mac OS X)
      g95)
	    case "$enable_optimization" in
	      debug)
            AC_MSG_NOTICE([  -> DEBUGGING OPTIONS are SELECTED])
            FCFLAGS="-g -O0 -fno-second-underscore -Wall -Wunset-vars -Wunused-vars -fbounds-check "
	        LDFLAGS="-g -O0 -fno-second-underscore"
          ;;
          aggressive)
            AC_MSG_NOTICE([  -> AGGRESSIVE COMPILER OPTIONS are SELECTED])
    	    FCFLAGS="-g -O3 -fno-second-underscore"
            LDFLAGS="-g -O3 -fno-second-underscore"
          ;;
          normal|*)
            AC_MSG_NOTICE([  -> NORMAL MODE])
	        FCFLAGS="-g -O -fno-second-underscore"
            LDFLAGS="-g -O -fno-second-underscore"
	      ;;
	    esac
	    if test "$enable_prof" = "yes" ; then
          AC_MSG_NOTICE([  -> PROFILING is SELECTED (gprof)])
          FCFLAGS="-pg $FCFLAGS"
          LDFLAGS="-pg $LDFLAGS"
        fi
      ;;
##    OFFICIAL GCC GNU FORTRAN 90/95 COMPILER (Tested on Intel-PC with Linux/Ubuntu)
      gfortran)
	    case "$enable_optimization" in
	      debug)
            AC_MSG_NOTICE([  -> DEBUGGING OPTIONS are SELECTED])
	        CFLAGS="-g -O0 -Wall -Wextra -pedantic -fimplicit-none -fbounds-check -fbacktrace -ffpe-trap=invalid,zero,overflow,underflow -ftrapv -fno-automatic"
            FCFLAGS="-g -O0 -frecord-marker=4 -Wall -Wextra -pedantic -fimplicit-none -fbounds-check -fbacktrace -ffpe-trap=invalid,zero,overflow,underflow -ftrapv -fno-automatic"
	        LDFLAGS="-g -O0 -frecord-marker=4 -Wall -Wextra -pedantic -fimplicit-none -fbounds-check -fbacktrace -ffpe-trap=invalid,zero,overflow,underflow -ftrapv -fno-automatic"
          ;;
          aggressive)
            AC_MSG_NOTICE([  -> AGGRESSIVE COMPILER OPTIONS are SELECTED])
	        CFLAGS="-g -O3"
    	    FCFLAGS="-g -O3 -frecord-marker=4"
            LDFLAGS="-g -O3 -frecord-marker=4"
          ;;
          normal|*)
            AC_MSG_NOTICE([  -> NORMAL MODE])
	        CFLAGS="-g -O2"
	        FCFLAGS="-g -O2 -frecord-marker=4"
            LDFLAGS="-g -O2 -frecord-marker=4"
	      ;;
	    esac
        if test "$enable_avx" = "yes" ; then
              FCFLAGS="$FCFLAGS -mavx"
              LDFLAGS="$LDFLAGS -mavx"
        fi
	    if test "$enable_prof" = "yes" ; then
          AC_MSG_NOTICE([  -> PROFILING is SELECTED (gprof)])
	      CFLAGS="-pg $CFLAGS"
          FCFLAGS="-pg $FCFLAGS"
          LDFLAGS="-pg $LDFLAGS"
        fi
     	if test "$enable_openmp" = "yes" ; then
          AC_MSG_NOTICE([      -> OpenMP PARALLELISATION IS SELECTED])
	      CFLAGS="-fopenmp $CFLAGS"
          FCFLAGS="-fopenmp $FCFLAGS"
          LDFLAGS="-fopenmp $LDFLAGS"
        fi

      ;;
  esac  
  #AC_MSG_NOTICE([$FC options:])
  #AC_MSG_NOTICE([    - FCFLAGS     =  ${FCFLAGS}])
  #AC_MSG_NOTICE([    - FCFLAGS_f90 = ${FCFLAGS_f90}])
  #AC_FC_SRCEXT(f90)
  #AC_FC_FREEFORM()
  F77=$FC
else
  AC_MSG_ERROR([No Fortran 90 compiler available on this machine.
               Please use F90FLAGS to specify it or
               update your environnement variable PATH or
               install a Fortran 90 compiler.])
fi

#########
## MPI ##
#########
if test "$enable_mpi_mpt" = "yes" ; then
  AC_MSG_NOTICE([       -> MPT PARALLELISATION IS SELECTED (caparmor)])
  AC_MSG_NOTICE([       -> mpif90]) 
  FC=mpif90
  CFLAGS="$CFLAGS"
  FCFLAGS="$FCFLAGS -I${MPI_INC}"
  TESTLIBS="$TESTLIBS -I${MPI_INC} -L${MPI_LIB} -lmpi"
fi

if test "$enable_intel_mpi" = "yes" ; then
  AC_MSG_NOTICE([       -> Intel MPI PARALLELISATION IS SELECTED])
  AC_MSG_NOTICE([       -> mpiifort]) 
  FC=mpiifort
  if test "$enable_traceanalyser" = "yes" ; then
     FC="$FC -trace"
     AC_MSG_NOTICE([       -> mpiifort -trace (traceanalyser)])
  fi
  CFLAGS="$CFLAGS"
  FCFLAGS="$FCFLAGS -I${MPI_INC}"
  TESTLIBS="$TESTLIBS -I${MPI_INC} -L${MPI_LIB} -lmpi"
fi

if test "$enable_openmpi" = "yes" ; then
  AC_MSG_NOTICE([      -> OpenMPI PARALLELISATION IS SELECTED])
  FC=mpif90
  CFLAGS="$CFLAGS"
  FCFLAGS="$FCFLAGS -I${MPI_INC}"
  TESTLIBS="$TESTLIBS -I${MPI_INC} -L${MPI_LIB} -lmpi"
fi

if test "$enable_mpich" = "yes" ; then
  AC_MSG_NOTICE([      -> MPI PARALLELISATION IS SELECTED])
  FC=mpif90
  CFLAGS="$CFLAGS"
  FCFLAGS="$FCFLAGS -I${MPI_INC}"
  TESTLIBS="$TESTLIBS  -I${MPI_INC} -L${MPI_LIB} -lmpich -lmpichf90"
fi


########################################################
## Test if fortran compiler works on a simple program ##
########################################################
AC_MSG_CHECKING([for Fortran compiler can handle Fortran-90 code])
cat <<EOF >conftest.f90
  program conftest
    IMPLICIT NONE 
    REAL(kind=8), PARAMETER :: A = 3.141592654
  end program conftest
EOF
doit='$FC ${FCFLAGS} ${FCFLAGS_f90} conftest.f90 >&5'
if AC_TRY_EVAL(doit); then
  nc_ans=yes
else
  nc_ans=no
fi
rm -f conftest.f90 a.out \$\<.log
AC_MSG_RESULT([$nc_ans])
if test "x$nc_ans" = xno; then
  AC_MSG_ERROR([A simple Fortran 90 test doesn't work.])
fi

#
#########################################################################
# Checks for library functions. (From NCO configure.in)
#########################################################################
if test "$enable_netcdf" = "yes" ; then
# Begin Mandatory Link Test with libnetcdf.a
# Make precious variables for ./configure --help
  AC_ARG_VAR(NETCDF_INC,Location of NetCDF module (compile-time))
  AC_ARG_VAR(NETCDF_LIB,Location of NetCDF library (compile-time))
# Add user-specified netCDF locations, if any, else add /usr/local/ 
# locations if they exist
################
## NETCDF_LIB ##
################
  if test "${NETCDF_LIB}"; then
      if test -d "${NETCDF_LIB}"; then
        if test -f "${NETCDF_LIB}/libnetcdf.a"; then
          if test -f "${NETCDF_LIB}/libnetcdff.a"; then
            TESTLIBS="${TESTLIBS} -L${NETCDF_LIB} -lnetcdff -lnetcdf"
            if test "$enable_ada" = "yes" ; then
              LDFLAGS="${LDFLAGS} -L${NETCDF_LIB} -lnetcdff -lnetcdf"
            fi
          else
  	        TESTLIBS="${TESTLIBS} -L${NETCDF_LIB} -lnetcdf"
            if test "$enable_ada" = "yes" ; then
              LDFLAGS="${LDFLAGS} -L${NETCDF_LIB} -lnetcdf"
            fi
          fi
        else
          AC_MSG_ERROR([Can't find libnetcdf.a in ${NETCDF_LIB}])
        fi
      else
        AC_MSG_ERROR([${NETCDF_LIB} location does not exist!])
      fi
  else
      case "$host" in
        *apple-darwin*)
          NETCDF_LIB="/sw/lib/netcdf-gfortran/lib";;
        *)
          NETCDF_LIB="/usr/lib"
      esac
      if test -d "${NETCDF_LIB}"; then
        if test -f "${NETCDF_LIB}/libnetcdf.a"; then
          if test -f "${NETCDF_LIB}/libnetcdff.a"; then
            TESTLIBS="${TESTLIBS} -L${NETCDF_LIB} -lnetcdff -lnetcdf"
          else
  	        TESTLIBS="${TESTLIBS} -L${NETCDF_LIB} -lnetcdf"
          fi
        else
          AC_MSG_ERROR([Can't find libnetcdf.a in ${NETCDF_LIB}])
        fi
      else
        AC_MSG_ERROR([${NETCDF_LIB} location does not exist!])
      fi
  fi

################
## NETCDF_INC ##
################
  if test "${NETCDF_INC}"; then
      if test -d "${NETCDF_INC}"; then
        if test -f "${NETCDF_INC}/netcdf.mod"; then
  	FCFLAGS="${FCFLAGS} -I${NETCDF_INC}"
        else
          AC_MSG_ERROR([Can't find netcdf.mod in ${NETCDF_INC}])
        fi
      else
  	AC_MSG_ERROR([{NETCDF_INC} location does not exist!])
      fi
  else
      case "$host" in
        *apple-darwin*)
          NETCDF_INC="/sw/lib/netcdf-gfortran/include";;
        *) 
	  NETCDF_INC="/usr/include"
      esac
      if test -d "${NETCDF_INC}"; then
        if test -f "${NETCDF_INC}/netcdf.mod"; then
       	  FCFLAGS="${FCFLAGS} -I${NETCDF_INC}"
        else
          AC_MSG_ERROR([Can't find netcdf.mod in ${NETCDF_INC}])
        fi
      else
  	    AC_MSG_ERROR([{NETCDF_INC} location does not exist!])
      fi
  fi

##############
## messages ##
##############
  AC_MSG_NOTICE([  -> NetCDF library: ${NETCDF_LIB}])
  AC_MSG_NOTICE([  -> NetCDF module : ${NETCDF_INC}])

###############
## HDF5  LIB ##
###############
  if test "$enable_hdf5" = "yes" ; then
    if test "${HDF5_LIB}"; then
      if test -d "${HDF5_LIB}"; then
         TESTLIBS="${TESTLIBS} -L${HDF5_LIB}"
        if test -f "${HDF5_LIB}/libhdf5_hl.a"; then
   	  TESTLIBS="${TESTLIBS} -lhdf5_hl"
        else
          AC_MSG_ERROR([Can't find libhdf5_hl.a in ${HDF5_LIB}])
        fi
        if test -f "${HDF5_LIB}/libhdf5_fortran.a"; then
          TESTLIBS="${TESTLIBS} -lhdf5_fortran"
        fi
        if test -f "${HDF5_LIB}/libhdf5.a"; then
          TESTLIBS="${TESTLIBS} -lhdf5"
        else
          AC_MSG_ERROR([Can't find libhdf5.a in ${HDF5_LIB}])
        fi
        if test -f "${HDF5_LIB}/libz.a"; then
          TESTLIBS="${TESTLIBS} -lz"
        fi
      else
        AC_MSG_ERROR([${HDF5_LIB} location does not exist!])
      fi
    else
      HDF5_LIB="/usr/lib"
      if test -d "${HDF5_LIB}"; then
         TESTLIBS="${TESTLIBS} -L${HDF5_LIB}"
         if test -f "${HDF5_LIB}/libhdf5_hl.a"; then
           TESTLIBS="${TESTLIBS} -lhdf5_hl"
         else
           AC_MSG_ERROR([Can't find libhdf5_hl.a in ${HDF5_LIB}])
         fi
         if test -f "${HDF5_LIB}/libhdf5_fortran.a"; then
           TESTLIBS="${TESTLIBS} -lhdf5_fortran"
         fi
         if test -f "${HDF5_LIB}/libhdf5.a"; then
           TESTLIBS="${TESTLIBS} -lhdf5"
         else
           AC_MSG_ERROR([Can't find libhdf5.a in ${HDF5_LIB}])
         fi
         if test -f "${HDF5_LIB}/libz.a"; then
          TESTLIBS="${TESTLIBS} -lz"
         fi
      else
        AC_MSG_ERROR([${HDF5_LIB} location does not exist!])
      fi
    fi
    AC_MSG_NOTICE([  -> HDF5 library for Netcdf 4 : ${HDF5_LIB}])
  fi

##################
## HDF5 INCLUDE ##
##################
  if test "$enable_hdf5" = "yes" ; then
    if test "${HDF5_INC}"; then
      if test -d "${HDF5_INC}"; then
         FCFLAGS="${FCFLAGS} -I${HDF5_INC}"
      else
        AC_MSG_ERROR([${HDF5_INC} location does not exist!])
      fi
    else
      HDF5_INC="/usr/include"
      if test -d "${HDF5_INC}"; then
         FCFLAGS="${FCFLAGS} -I${HDF5_INC}"
      else
        AC_MSG_ERROR([${HDF5_INC} location does not exist!])
      fi
    fi
    AC_MSG_NOTICE([  -> HDF5 include for Netcdf 4 : ${HDF5_INC}])
  fi

######################################################
## Test a simple fortran 90 code linked with NetCDF ##
######################################################
AC_MSG_CHECKING([for Fortran-90 program linked with NetCDF library])
cat <<EOF >conftest.f90
    program conftest
      USE netcdf
    END program conftest
EOF
  echo ""
  echo "$FC ${FCFLAGS} ${FCFLAGS_f90} conftest.f90 ${TESTLIBS}"
  doit='$FC ${FCFLAGS} ${FCFLAGS_f90} conftest.f90 ${TESTLIBS}'
  if AC_TRY_EVAL(doit); then
    nc_ans=yes
  else
    nc_ans=no
  fi
  rm -f conftest.f90 a.out
  AC_MSG_RESULT([$nc_ans])
  if test "x$nc_ans" = xno; then
    AC_MSG_NOTICE([ ])
    AC_MSG_ERROR([A simple Fortran 90 program linked with Netcdf library doesn't work. Please verify that the NetCDF library was compiled with the Fortran compiler $FC.])
  fi
fi



#########################################################################
# Export DATA
#########################################################################
AC_SUBST(CPPFLAGS,[$CPPFLAGS])
AC_SUBST(F77, [$F77])
AC_SUBST(FC,[$FC])
AC_SUBST(FCFLAGS,[$FCFLAGS])
AC_SUBST(FCFLAGS_f90,[$FCFLAGS_f90])
AC_SUBST(TESTLIBS,[$TESTLIBS])

#########################################################################
# Checks for system services
#########################################################################
AC_MSG_NOTICE([generating makefiles])
AC_CONFIG_FILES([Makefile
                 src/Makefile
                 tests/Makefile
                 doc/Makefile
                 examples/Makefile
                 examples/matlab/Makefile ])

AC_OUTPUT()

# Echo build parameters for debugging
echo ""
echo "Configuration Parameters:"
echo "------------------------"
echo "NHMG's version........ ${VERSION}"
echo "prefix............... ${prefix}"
echo "FC................... ${FC}"
echo "FCFLAGS.............. ${FCFLAGS}"
echo "FCFLAGS_f90.......... ${FCFLAGS_f90}"
echo "LDFLAGS.............. ${LDFLAGS}"
echo "LIBS................. ${LIBS}"
echo "ENABLE_OPTIMIZATION.. ${enable_optimization}"
echo "ENABLE_mpi_mpt....... ${enable_mpi_mpt}"
echo "ENABLE_intel_mpi..... ${enable_intel_mpi}"
echo "ENABLE_open_mpi...... ${enable_openmpi}"
echo "ENABLE_mpich......... ${enable_mpich}"
echo "      MPI_LIB........ ${MPI_LIB}"
echo "      MPI_INC........ ${MPI_INC}"
echo "ENABLE_NETCDF........ ${enable_netcdf}"
if test "$enable_netcdf" = "yes" ; then
  echo "  -NETCDF_LIB........ ${NETCDF_LIB}"
  echo "  -NETCDF_INC........ ${NETCDF_INC}"
fi
echo "ENABLE_PROFILING..... ${enable_prof}"
echo "ENABLE_TRACEANALYSER. ${enable_traceanalyser}"
echo "host................. ${host}"
echo ""
date

# Inform users of the very useful configure.eg
echo ""
echo "*******************************************************************"
echo "*              NHMG's configuration is completed                *" 
echo "* You are now ready to run 'make clean', 'make' and 'make install'*"
echo "*******************************************************************"
