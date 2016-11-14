#!/bin/bash
####################################################
#               COMPILATION JOB                    #
####################################################
#
# set source, compilation and run directories
#
SOURCE=$HOME/croco/OCEAN
SCRDIR=./Compile
RUNDIR=`pwd`
ROOT_DIR=$SOURCE/..
echo "============================"
#
# determine operating system
#
OS=`uname`
echo "OPERATING SYSTEM IS: $OS"

#
# compiler options
#
if [[ $OS == Linux ]] ; then

	LINUX_FC=ifort
#
#	set 32 or 64 Bits executable
#
	ARCH=`uname -m`
	echo "PROCESSOR IS: $ARCH"
	if [[ $ARCH == x86_64 ]] ; then
		BITS=SIXTYFOUR
	else
    	BITS=THIRTYTWO
	fi

elif [[ $OS == Darwin ]] ; then

        DARWIN_FC=gfortran

else
  	BITS=THIRTYTWO
fi

#
# set NETCDF directories
#
if [[ $OS == Linux ]] ; then
  if [[ $LINUX_FC == g77 ]] ; then
    if [[ $BITS == THIRTYTWO ]] ; then
      NETCDFLIB="-L../../netcdf_g77 -lnetcdf"
      NETCDFINC="-I../../netcdf_g77"
    fi
  elif [[ $LINUX_FC == ifort || $LINUX_FC == gfortran ]] ; then
    if [[ $BITS == THIRTYTWO ]] ; then
      NETCDFLIB="-L../../netcdf_ifc -lnetcdf"
      NETCDFINC="-I../../netcdf_ifc"
    else
      NETCDFLIB="-L../../netcdf_x86_64 -lnetcdf"
      NETCDFINC="-I../../netcdf_x86_64"
    fi
  fi
fi
#
# If needed set your own NETCDF directories
#-----------------------------------------------------------
# Use : 
#-lnetcdf           : version netcdf-3.6.3                --
#-lnetcdff -lnetcdf : version netcdf-4.1.2                --
#-lnetcdff          : version netcdf-fortran-4.2-gfortran --
#-----------------------------------------------------------
#
#NETCDFLIB="-L/usr/local/lib -lnetcdf"
#NETCDFINC="-I/usr/local/include"
NETCDFLIB=$(nf-config --flibs)
NETCDFINC=-I$(nf-config --includedir)

#
# set MPI directories if needed
#
MPIF90="/usr/local/bin/mpif90"
MPILIB="-L/usr/local/lib -lmpi"
MPIINC="-I/usr/local/include"

echo 'NETCDFLIB='$NETCDFLIB
echo 'NETCDFINC='$NETCDFINC
echo ' '
#
# set OASIS-MCT (or OASIS3) directories if needed
#
PRISM_ROOT_DIR=../../oasis3-mct/compile_oa3-mct

#
# set XIOS directory if needed
#
XIOS_ROOT_DIR=$HOME/XIOS
#
# END OF USER'S MODIFICATIONS
####################################################
#
# Use GNU Make command
#
MAKE=make
#
# clean scratch area
#
rm -rf $SCRDIR
mkdir $SCRDIR
#
# AGRIF sources directory
#
AGRIF_SRC=${ROOT_DIR}/AGRIF
#
# copy SOURCE code
#
/bin/cp -f ${SOURCE}/*.F90 $SCRDIR
[ ! -z "$(ls ${SOURCE}/*.h90 2>/dev/null)" ] && /bin/cp -f ${SOURCE}/*.h90 $SCRDIR
/bin/cp -f ${SOURCE}/*.F   $SCRDIR
/bin/cp -f ${SOURCE}/*.h   $SCRDIR
/bin/cp -f ${SOURCE}/Make* $SCRDIR
/bin/cp -f ${SOURCE}/testkeys.F $SCRDIR
/bin/cp -f ${SOURCE}/jobcomp $SCRDIR
/bin/cp -f ${SOURCE}/amr.in $SCRDIR
/bin/cp -RLf ${AGRIF_SRC} $SCRDIR
/bin/cp -f ${ROOT_DIR}/XIOS/*.F $SCRDIR
/bin/cp -f ${ROOT_DIR}/PISCES/* $SCRDIR
/bin/cp -f ${ROOT_DIR}/PISCES/kRGB61* $RUNDIR
if [[ -e "namelist_pisces" ]] ; then
	echo "  file namelist_pisces exists in Run directory"
else
	/bin/cp -f ${ROOTDIR}/PISCES/namelist_pisces* $RUNDIR
	echo "  file namelist_pisces copied from source directory"
fi
#
# overwrite with local files
#
[ ! -z "$(ls *.F90 2>/dev/null)" ] && /bin/cp -f *.F90 $SCRDIR
[ ! -z "$(ls *.F 2>/dev/null)" ] && /bin/cp -f *.F $SCRDIR
[ ! -z "$(ls *.h 2>/dev/null)" ] && /bin/cp -f *.h $SCRDIR
[ ! -z "$(ls Make* 2>/dev/null)" ] && /bin/cp -f Make* $SCRDIR
#
#
# CVTK  files  DEBUG CPP KEYS
#
[ -f  cppdefs_dev_bak1.h ] && /bin/cp -f cppdefs_dev_bak1.h ${SCRDIR}/cppdefs_dev.h
[ -f  param_bak1.h ] && /bin/cp -f param_bak1.h ${SCRDIR}/param.h
[ -f  cppdefs_bak1.h ] && /bin/cp -f cppdefs_bak1.h ${SCRDIR}/cppdefs.h
#
# Change directory
#
cd $SCRDIR
#
# generates LDFLAGS1 according to users notifications
#
LDFLAGS1="$NETCDFLIB"
CPPFLAGS1="$NETCDFINC -IROMSFILES/AGRIF_INC"
#
# Set compilation options
#
if [[ $OS == Linux ]] ; then           # ===== LINUX =====
	if [[ $LINUX_FC == ifort || $LINUX_FC == ifc ]] ; then
		CPP1="cpp -traditional -DLinux -DIfort"
		CFT1=ifort
                FFLAGS1="-O0 -g -72 -fno-alias -i4 -r8 -fp-model precise"
                #FFLAGS1="-O0 -g -72 -fno-alias -i4 -r8 -mcmodel=large -shared-intel -fp-model precise"
                #FFLAGS1="-O1 -72 -fno-alias -i4 -r8 -fp-model precise"
		LDFLAGS1="$LDFLAGS1"
	elif [[ $LINUX_FC == gfortran ]] ; then
		CPP1="cpp -traditional -DLinux"
		CFT1=gfortran
#		 FFLAGS1="-O1 -fdefault-real-8 -fdefault-double-8 -mcmodel=medium"
	        FFLAGS1="-O0 -g -fdefault-real-8 -fdefault-double-8 -fbacktrace \
 			-fbounds-check -finit-real=nan -finit-integer=8888"
		LDFLAGS1="$LDFLAGS1"
	fi
elif [[ $OS == Darwin ]] ; then        # ===== DARWIN =====
	CPP1="cpp -traditional -DLinux"
	if [[ $DARWIN_FC == gfortran ]] ; then  
        	CFT1="gfortran"
        	FFLAGS1="-O4 -fdefault-real-8 -fdefault-double-8"
#	 	FFLAGS1="-O0 -g -fdefault-real-8 -fdefault-double-8 -fbacktrace \
#			-fbounds-check -finit-real=nan -finit-integer=8888"
	else
		CFT1="ifort"
		FFLAGS1="-O2 -r8 -i4 -g -72"
#		FFLAGS1="-O0 -g -traceback -debug all -r8 -i4 -g -72"
	fi
elif [[ $OS == AIX ]] ; then           # ===== IBM =====
	CPP1="/lib/cpp"
	CFT1="xlf95 -I$HOME/include/"
	if  [[ $BITS == THIRTYTWO ]] ; then
		MAKEAGRIF="Makefile.ibm"
		FFLAGS1="-qfixed -O5 -qstrict -qalias=noaryovrlp -qhot -qrealsize=8 \
			-qintsize=4 -qarch=auto -qtune=auto -qcache=auto -bmaxdata:0x80000000"
#		FFLAGS1="-g -qfixed -O2 -qstrict -qalias=noaryovrlp -qrealsize=8 \
#			-qintsize=4 -qarch=auto -qtune=auto -qcache=auto -bmaxdata:0x80000000"
	else
		MAKEAGRIF="Makefile.ibm.64"
		FFLAGS1="-q64 -qwarn64 -qfixed -qrealsize=8 -qintsize=8 -qhot \
			-qalias=noaryovrlp -qthreaded -O3 -qarch=pwr4 -qtune=pwr4 -qunroll=yes"
	fi
elif [[ $OS == OSF1 ]] ; then          # ===== COMPAQ =====
	CPP1="/lib/cpp"
	CFT1="f95"
	FFLAGS1="-fast -i4 -r8"
elif [[ $OS == IRIX64 ]] ; then        # ===== SGI =====
	CPP1="/usr/freeware/bin/cpp -traditional"
	CFT1="f90"
	FFLAGS1="-O2"
elif [[ $OS == SunOS ]] ; then         # ===== SUN ===== :  tested on SunFire 880 (SPARC III)
	GREP="/usr/xpg4/bin/grep"      #                         and Sun Ultra-60 (SPARC II)
	CPP1=/lib/cpp
	CFT1="f95"
	if [[ $BITS == THIRTYTWO ]] ; then
		MAKEAGRIF="Makefile.sun"
		FFLAGS1="-O5 -xtarget=native -xprefetch -xtypemap=real:64,double:128 -xlibmopt"
	else
		MAKEAGRIF="Makefile.sun.64"
		FFLAGS1="-O5 -xtarget=native64 -xprefetch -xtypemap=real:64,double:128 -xlibmopt "
	fi
else
	echo "Unknown Operating System"
	exit
fi
#
# determine if AGRIF compilation is required
#
unset COMPILEAGRIF
echo "Checking COMPILEAGRIF..."
if $($CPP1 testkeys.F | grep -i -q agrifisdefined) ; then
	echo " => AGRIF activated"
	COMPILEAGRIF=TRUE
	FFLAGS1="$FFLAGS1 -IAGRIF"
	LDFLAGS1="-LAGRIF -lagrif $LDFLAGS1"
# we use the AGRIF Makedefs.generic definition
	/bin/cp -f Makedefs.generic.AGRIF Makedefs.generic
fi
#
# determine if MPI compilation is required
#
unset COMPILEMPI
echo "Checking COMPILEMPI..."
if $($CPP1 testkeys.F | grep -i -q mpiisdefined) ; then
	echo " => MPI activated"
	COMPILEMPI=TRUE
	LDFLAGS1="$LDFLAGS1 $MPILIB"
	CPPFLAGS1="$CPPFLAGS1 $MPIINC"
	FFLAGS1="$FFLAGS1 $MPIINC"
	CFT1="$MPIF90"
fi
#
# determine if NBQ related solvers (BLAS/LAPACK) are required
#
unset COMPILENBQ
echo "Checking COMPILENBQ..."
if $($CPP1 testkeys.F | grep -i -q nbqisdefined) ; then
	echo " => NBQ activated"
	COMPILENBQ=TRUE
	LDFLAGS1="-lblas -llapack $LDFLAGS1"
	FFLAGS1="$FFLAGS1 -ffree-line-length-none"
fi
#
# determine if XIOS compilation is required
#
unset COMPILEXIOS
echo "Checking COMPILEXIOS..."
if $($CPP1 testkeys.F | grep -i -q xiosisdefined) ; then
        echo " => XIOS activated"
        COMPILEXIOS=TRUE
        LDFLAGS1="$LDFLAGS1 $XIOS_ROOT_DIR/lib/libxios.a  -lstdc++ -lnetcdff"
        CPPFLAGS1="$CPPFLAGS1 -I$XIOS_ROOT_DIR/inc"
        FFLAGS1="$FFLAGS1 -I$XIOS_ROOT_DIR/inc"
        ln -s $XIOS_ROOT_DIR/bin/xios_server.exe $RUNDIR/.
        $CPP1 -P -traditional -imacros cppdefs.h  ../field_def.xml_full ../field_def.xml
fi
#
# determine if OASIS compilation is required
#
unset COMPILEOASIS
echo "Checking COMPILEOASIS..."
if $($CPP1 testkeys.F | grep -i -q oacplisdefined) ; then
    echo " => OASIS activated"
    CHAN=MPI1
    if $($CPP1 testkeys.F | grep -i -q oacpl_mctisdefined) ; then
	echo " => OASIS-MCT activated"
	LIBPSMILE="${PRISM_ROOT_DIR}/lib/libpsmile.${CHAN}.a \
		${PRISM_ROOT_DIR}/lib/libmct.a  \
		${PRISM_ROOT_DIR}/lib/libmpeu.a \
		${PRISM_ROOT_DIR}/lib/libscrip.a"
	PSMILE_INCDIR="-I${PRISM_ROOT_DIR}/build/lib/psmile.${CHAN} \
		-I${PRISM_ROOT_DIR}/build/lib/mct"
    elif $($CPP1 testkeys.F | grep -i -q oacpl_oa3isdefined) ; then
	echo " => OASIS3 activated"
	LIBPSMILE="${PRISM_ROOT_DIR}/lib/libanaisg.a \
		${PRISM_ROOT_DIR}/lib/libanaism.a \
		${PRISM_ROOT_DIR}/lib/libclim.${CHAN}.a \
		${PRISM_ROOT_DIR}/lib/libpsmile.${CHAN}.a \
		${PRISM_ROOT_DIR}/lib/libfscint.a  \
		${PRISM_ROOT_DIR}/lib/libmpp_io.a \
		${PRISM_ROOT_DIR}/lib/libscrip.a"
	PSMILE_INCDIR="-I${PRISM_ROOT_DIR}/build/lib/psmile.${CHAN} \
		-I${PRISM_ROOT_DIR}/build/lib/clim.${CHAN} \
		-I${PRISM_ROOT_DIR}/build/lib/mpp_io"
    fi
    COMPILEOASIS=TRUE
    LDFLAGS1="$LDFLAGS1 $LIBPSMILE"
    CPPFLAGS1="$CPPFLAGS1 ${PSMILE_INCDIR}"
    FFLAGS1="$FFLAGS1 ${PSMILE_INCDIR}"
fi
#
# rewrite Makedefs according to previous flags
# with MPI flags if needed
#
rm -f Makedefs
echo 's?$(FFLAGS1)?'$FFLAGS1'?g' > flags.tmp
echo 's?$(LDFLAGS1)?'$LDFLAGS1'?g' >> flags.tmp
echo 's?$(CPP1)?'$CPP1'?g' >> flags.tmp
echo 's?$(CFT1)?'$CFT1'?g' >> flags.tmp
echo 's?$(CPPFLAGS1)?'$CPPFLAGS1'?g' >> flags.tmp
sed -f flags.tmp Makedefs.generic > Makedefs
rm -f flags.tmp

#
# clean scratch
#
$MAKE clobber

#
# compile the precompiling program
#
$MAKE mpc

#
# prepare and compile the library
#
if [[ $COMPILEAGRIF ]] ; then
#
# compile the AGRIF librairy
#
	if [[ $COMPILEMPI ]] ; then
		$MAKE -C AGRIF FC="$CFT1" CPP="$CPP1" CPPFLAGS="-DAGRIF_MPI $MPIINC" FFLAGS="$FFLAGS1"
	else
		$MAKE -C AGRIF FC="$CFT1" CPP="$CPP1" FFLAGS="$FFLAGS1"
	fi
	if [[ $OS == Darwin ]] ; then          # DARWIN
# run RANLIB on Darwin system
		ranlib AGRIF/libagrif.a
	fi
#
	$CPP1 amr.in | grep -v -e ! -e '#' -e % -e '*' > amr.scrum
	mkdir ROMSFILES
	mv AGRIF/conv ROMSFILES/.
	mv amr.scrum ROMSFILES/.
	cd ROMSFILES
	mkdir AGRIF_MODELFILES
	mkdir AGRIF_INC
	cd ..
	for i in *.h *.h90 ; do
		echo $i
		cat cppdefs.h $i | cpp -P | grep -v -e ! -e '#' -e % -e '*' > ROMSFILES/$i
	done
	mv -f ROMSFILES/private_scratch_AMR.h ROMSFILES/private_scratch.h
fi

#
# determine if OPENMP compilation is needed
#
unset COMPILEOMP
if $($CPP1 testkeys.F | grep -i -q openmp) ; then
	COMPILEOMP=TRUE
	if [[ $OS == Linux ]] ; then
		if [[ $LINUX_FC == gfortran ]] ; then
			FFLAGS1="$FFLAGS1 -fopenmp"
		elif [[ $LINUX_FC == ifort || $LINUX_FC == ifc ]] ; then
			FFLAGS1="$FFLAGS1 -openmp"
		else
			FFLAGS1="$FFLAGS1 -openmp"
		fi
	elif [[ $OS == Darwin ]] ; then 
		if [[ $DARWIN_FC == gfortran ]] ; then 
			FFLAGS1="$FFLAGS1 -fopenmp"
    		else
			FFLAGS1="$FFLAGS1 -openmp"
		fi
	elif [[ $OS == AIX ]] ; then
		FFLAGS1="$FFLAGS1 -qsmp=omp"
		CFT1="xlf95_r"
	elif [[ $OS == OSF1   ]] ; then
		FFLAGS1="$FFLAGS1 -omp"
	elif [[ $OS == IRIX64 ]] ; then
		FFLAGS1="$FFLAGS1 -mp"
	elif [[ $OS == SunOS  ]] ; then 
		FFLAGS1="$FFLAGS1 -openmp"
	fi
fi

#
# rewrite Makedefs according to previous flags
# with openmp flags if needed
#
rm -f Makedefs
echo 's?$(FFLAGS1)?'$FFLAGS1'?g' > flags.tmp
echo 's?$(LDFLAGS1)?'$LDFLAGS1'?g' >> flags.tmp
echo 's?$(CPP1)?'$CPP1'?g' >> flags.tmp
echo 's?$(CFT1)?'$CFT1'?g' >> flags.tmp
echo 's?$(CPPFLAGS1)?'$CPPFLAGS1'?g' >> flags.tmp
sed -f flags.tmp Makedefs.generic > Makedefs
rm -f flags.tmp

#
# compile croco
#
$MAKE 
mv croco $RUNDIR
#
# Compile ncjoin and partit
#
#$MAKE ncjoin
#$MAKE partit
#mv partit ncjoin $RUNDIR
