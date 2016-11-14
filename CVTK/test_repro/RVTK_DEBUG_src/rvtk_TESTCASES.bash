#!/bin/bash
#
#======================================================================
# ROMS_AGRIF is a branch of ROMS developped at IRD and INRIA, in France
# The two other branches from UCLA (Shchepetkin et al) 
# and Rutgers University (Arango et al) are under MIT/X style license.
# ROMS_AGRIF specific routines (nesting) are under CeCILL-C license.
# 
# ROMS_AGRIF website : http://www.croco-ocean.org
#======================================================================
#
#---------------------------------------------------------------------
# Script to Run CVTK DEBUG procedure managing parallelization type 
# AND AGRIF nesting type (No nesting, Nesting 1-way, Nesting 2-ways) : 
# VORTEX and REGIONAL
#--------------------------------------------------------------------
echo "==========================="
echo "MPIRUN COMMAND: "$MPIRUN
echo "==========================="
echo "Remove *.exe* *.log* "
[ ! -z "$(ls *.exe* 2>/dev/null)" ] && /bin/rm *.exe*
[ ! -z "$(ls *.log* 2>/dev/null)" ] &&/bin/rm *.log*
echo "Remove the CHECKFILE"
[ -f check_file ] && /bin/rm check_file
echo " "
#
# Lists
#
LIST_EXAMPLE='ACOUS BASIN CANYON_A CANYON_B EQUATOR GRAV_ADJ IGW INNERSHELF INTERNAL JET KH_INST OVERFLOW RIP S2DV SEAMOUNT SHELFRONT SHOREFACE SOLITON SWASH TANK THACKER UPWELLING'
LIST_EXAMPLE='ACOUS BASIN CANYON_A CANYON_B EQUATOR GRAV_ADJ IGW INNERSHELF INTERNAL JET OVERFLOW RIP SEAMOUNT SHELFRONT SHOREFACE SOLITON SWASH TANK THACKER UPWELLING'
LIST_EXAMPLE='SOLITON'
LIST_KEY='MPI OPENMP REGIONAL ETALON_CHECK'
LIST_WORDS='ETALON difference: ABNORMAL ERROR BUGBIN'
# 1x4 4x1 2x2 1X8 and 8X1 additional tests
ADDTEST='ON'
#
# Type of parallelization
#
COMPOMP='ON'
COMPMPI='OFF'
echo ' '
#
echo 'OpenMP testing: '$COMPOMP
echo 'MPI    testing: '$COMPMPI

#
# Get code source from jobcomp file
#
sed -n -e '/SOURCE=/p' jobcomp_rvtk.bash > tmp1
sed -n '$p' tmp1 > tmp2
SOURCE=$(sed -n -e '/SOURCE=/ s/.*\= *//p' tmp2)
rm -f tmp1 tmp2
echo 'Sources code: '$SOURCE
echo ' '

#
# Get updated files
#
/bin/cp ${SOURCE}/cppdefs_dev.h cppdefs_dev_bak1.h 
/bin/cp ${SOURCE}/cppdefs.h cppdefs_bak1.h
/bin/cp ${SOURCE}/param.h param_bak0.h
#
# Replace with local files if any ### PAT
#
#[ -f cppdefs_dev.h ] && /bin/cp cppdefs_dev.h cppdefs_dev_bak1.h 
#[ -f cppdefs.h ] && /bin/cp cppdefs.h cppdefs_bak1.h
#[ -f param.h ] && /bin/cp param.h param_bak0.h
#
# Title
#
echo TESTS OF $LIST_EXAMPLE
#
# Undef all examples
#
for EXAMPLE in $LIST_EXAMPLE ; do
    echo undef $EXAMPLE
    sed 's/'define\ $EXAMPLE'/'undef\ $EXAMPLE'/'  cppdefs_bak1.h > cppdefs_bak2.h
    /bin/mv cppdefs_bak2.h cppdefs_bak1.h
done
#
# Undef AGRIF, MPI, OPENMP etc..
#
echo " "
echo "UNDEF THE KEYS IN LIST_KEYS"
for KEY in  $LIST_KEY ; do
    echo undef $KEY
    sed 's/'define\ $KEY'/'undef\ $KEY'/'  cppdefs_bak1.h > cppdefs_bak2.h
    /bin/mv cppdefs_bak2.h cppdefs_bak1.h
done
#
# Define DEBUG keys
#
echo " "
echo "============================================"
echo "TEST CVTK_DEBUG [cppdefs_dev.h] "
echo "============================================"
sed 's/'undef\ \ \*RVTK_DEBUG'/'define\ RVTK_DEBUG'/' < cppdefs_dev_bak1.h > cppdefs_dev_bak2.h
/bin/mv cppdefs_dev_bak2.h cppdefs_dev_bak1.h
echo " "
echo "Debut de la boucle sur les cas tests"
for EXAMPLE in $LIST_EXAMPLE ; do
    echo "CAS TEST # "$EXAMPLE
    #
    # Serial runs
    #
    echo "======================"
    echo SERIAL TESTS
    echo "Remove the CHECKFILE"
     [ -f check_file ] && /bin/rm check_file
    echo " "
    
    echo "Undef NPP > 1"
    sed 's/'NSUB_X=1,\ \ \*NSUB_E=NPP'/'NSUB_X=1,\ NSUB_E=1'/' < param_bak0.h > param_bak1.h
    sed 's/'NPP=1'/'NPP=1'/' < param_bak1.h > param_bak2.h
    /bin/mv param_bak2.h param_bak1.h
    echo " "
    
    echo "undef MPI"
    sed 's/'define\ \ \*MPI'/'undef\ MPI'/' < cppdefs_bak1.h > cppdefs_bak2.h
    /bin/mv cppdefs_bak2.h cppdefs_bak1.h
    echo " "
    
    echo "undef OPENMP"
    sed 's/'define\ \ \*OPENMP'/'undef\ OPENMP'/' < cppdefs_bak1.h > cppdefs_bak2.h
    /bin/mv cppdefs_bak2.h cppdefs_bak1.h
    echo " "
    
    /bin/cp param_bak0.h param_bak1.h
    echo COMPILE SERIAL $EXAMPLE
    sed 's/'undef\ \ \*$EXAMPLE'/'define\ $EXAMPLE'/' < cppdefs_bak1.h > cppdefs_bak2.h
    /bin/mv cppdefs_bak2.h cppdefs_bak1.h
    date
    time ./jobcomp_rvtk.bash > jobcomp_serial_${EXAMPLE}.log
    date
    sed 's/'define\ $EXAMPLE'/'undef\ $EXAMPLE'/' < cppdefs_bak1.h > cppdefs_bak2.h
    /bin/mv cppdefs_bak2.h cppdefs_bak1.h
    /bin/mv croco croco_serial_${EXAMPLE}.exe
    echo RUN SERIAL $EXAMPLE
    time ./croco_serial_${EXAMPLE}.exe > serial_${EXAMPLE}.log
    date
    echo TEST SERIAL $EXAMPLE
    for WORD in  $LIST_WORDS ; do
	 grep --binary-files=text $WORD serial_${EXAMPLE}.log
    done
    [ ! -z "$(ls *.nc 2>/dev/null)" ] && /bin/rm *.nc

    if  [ ${COMPOMP} == 'ON' ] ; then
	##############################################################################
	# OpenMP
	#############################################################################
	echo 
	echo define OPENMP
	sed 's/'undef\ \ \*OPENMP'/'define\ OPENMP'/' < cppdefs_bak1.h > cppdefs_bak2.h
	/bin/mv cppdefs_bak2.h cppdefs_bak1.h

	#---------------------------------------------------------------------------------
	echo "======================"
	echo OPEN-MP 1X2 NPP=2 TESTS
	echo
	/bin/rm param_bak1.h
	sed 's/'NSUB_X=1,\ \ \*NSUB_E=NPP'/'NSUB_X=1,\ NSUB_E=NPP'/' < param_bak0.h > param_bak1.h
	sed 's/'NPP=4'/'NPP=2'/' < param_bak1.h > param_bak2.h
	/bin/mv param_bak2.h param_bak1.h
	export OMP_NUM_THREADS=2
	#
	
	echo "--------------------------"
	if [[ "${EXAMPLE}" == 'RIP' || "${EXAMPLE}" == 'JET' || "${EXAMPLE}" == 'GRAV_ADJ'  || "${EXAMPLE}" == 'INNERSHELF' || "${EXAMPLE}" == 'INTERNAL'  || "${EXAMPLE}" == 'ACOUS' || "${EXAMPLE}" == 'KH_INST' || "${EXAMPLE}" == 'IGW' || "${EXAMPLE}" == 'S2DV' || "${EXAMPLE}" == 'SWASH' || "${EXAMPLE}" == 'SHOREFACE' || "${EXAMPLE}" == 'THACKER' ||  "${EXAMPLE}" == 'TANK' ]] ; then
	    
	else 
	    echo COMPILE OPENMP 1X2 $EXAMPLE
	    sed 's/'undef\ $EXAMPLE'/'define\ $EXAMPLE'/' < cppdefs_bak1.h > cppdefs_bak2.h
	    /bin/mv cppdefs_bak2.h cppdefs_bak1.h
	    date
	    time ./jobcomp_rvtk.bash > jobcomp_openmp_${EXAMPLE}.log
	    date
	    sed 's/'define\ $EXAMPLE'/'undef\ $EXAMPLE'/' < cppdefs_bak1.h > cppdefs_bak2.h
	    /bin/mv cppdefs_bak2.h cppdefs_bak1.h
	    /bin/mv croco croco_omp1X2_${EXAMPLE}.exe
	    echo RUN OPENMP 1X2 $EXAMPLE
	    time ./croco_omp1X2_${EXAMPLE}.exe > openmp1X2_${EXAMPLE}.log
	    date
	    echo TEST OPENMP 1X2  $EXAMPLE
	    for WORD in $LIST_WORDS ; do
		grep --binary-files=text $WORD openmp1X2_${EXAMPLE}.log
	    done
	fi
	[ ! -z "$(ls *.nc 2>/dev/null)" ] && /bin/rm *.nc

	#------------------------------------------------------------------------------
	echo "======================"
	echo OPEN-MP 2X1 NPP=2 TESTS
	echo
	/bin/rm param_bak1.h
	sed 's/'NSUB_X=1,\ \ \*NSUB_E=NPP'/'NSUB_X=NPP,\ NSUB_E=1'/' < param_bak0.h > param_bak1.h
	sed 's/'NPP=4'/'NPP=2'/' < param_bak1.h > param_bak2.h
	/bin/mv param_bak2.h param_bak1.h
	export OMP_NUM_THREADS=2
	#
	if [[ "${EXAMPLE}" == 'RIP' || "${EXAMPLE}" == 'JET' || "${EXAMPLE}" == 'OVERFLOW'  || "${EXAMPLE}" == 'SHELFRONT' ]] ; then
	    echo 'SKIP THIS TEST CASE ' $EXAMPLE
	else

	    echo COMPILE OPENMP 2X1 $EXAMPLE
	    sed 's/'undef\ $EXAMPLE'/'define\ $EXAMPLE'/' < cppdefs_bak1.h > cppdefs_bak2.h
	    /bin/mv cppdefs_bak2.h cppdefs_bak1.h
	    date
	    time ./jobcomp_rvtk.bash > jobcomp_openmp_${EXAMPLE}.log
	    date
	    sed 's/'define\ $EXAMPLE'/'undef\ $EXAMPLE'/' < cppdefs_bak1.h > cppdefs_bak2.h
	    /bin/mv cppdefs_bak2.h cppdefs_bak1.h
	    /bin/mv croco croco_omp2X1_${EXAMPLE}.exe
	    echo RUN OPENMP 2X1 $EXAMPLE
	    time ./croco_omp2X1_${EXAMPLE}.exe > openmp2X1_${EXAMPLE}.log
	    date
	    echo TEST OPENMP 2X1 $EXAMPLE
	    for WORD in  $LIST_WORDS ; do
		grep --binary-files=text $WORD openmp2X1_${EXAMPLE}.log
	    done
	fi
	[ ! -z "$(ls *.nc 2>/dev/null)" ] && /bin/rm *.nc

	#------------------------------------------------------------------------------
	if [ $ADDTEST == 'ON' ]; then
	    echo "======================"
	    echo OPEN-MP 2X2 NPP=2 TESTS
	    echo
	    /bin/rm param_bak1.h

	    sed 's/'NSUB_X=1,\ \ \*NSUB_E=NPP'/'NSUB_X=2,\ NSUB_E=2'/' < param_bak0.h > param_bak1.h
	    sed 's/'NPP=4'/'NPP=2'/'  param_bak1.h > param_bak2.h
	    /bin/mv param_bak2.h param_bak1.h
	    export OMP_NUM_THREADS=2
	    #
	    echo "--------------------------"
	    if [[ "${EXAMPLE}" == 'RIP' || "${EXAMPLE}" == 'JET' || "${EXAMPLE}" == 'GRAV_ADJ'  || "${EXAMPLE}" == 'INNERSHELF' || "${EXAMPLE}" == 'INTERNAL' || "${EXAMPLE}" == 'SHELFRONT' || "${EXAMPLE}" == 'OVERFLOW' || "${EXAMPLE}" == 'ACOUS' || "${EXAMPLE}" == 'KH_INST' || "${EXAMPLE}" == 'IGW' || "${EXAMPLE}" == 'S2DV' || "${EXAMPLE}" == 'SWASH' || "${EXAMPLE}" == 'SHOREFACE' || "${EXAMPLE}" == 'THACKER' || "${EXAMPLE}" == 'TANK' ]] ; then
		echo 'SKIP THIS TEST CASE ' $EXAMPLE
	    else
		
		echo COMPILE OPENMP 2X2 $EXAMPLE
		sed 's/'undef\ $EXAMPLE'/'define\ $EXAMPLE'/' < cppdefs_bak1.h > cppdefs_bak2.h
		/bin/mv cppdefs_bak2.h cppdefs_bak1.h
		date
		time ./jobcomp_rvtk.bash > jobcomp_openmp_${EXAMPLE}.log
		date
		sed 's/'define\ $EXAMPLE'/'undef\ $EXAMPLE'/' < cppdefs_bak1.h > cppdefs_bak2.h
		/bin/mv cppdefs_bak2.h cppdefs_bak1.h
		/bin/mv croco croco_omp2X2_${EXAMPLE}.exe
		echo RUN OPENMP 2X2 $EXAMPLE
		time ./croco_omp2X2_${EXAMPLE}.exe > openmp2X2_${EXAMPLE}.log
		date
		echo TEST OPENMP 2X2 $EXAMPLE
		for WORD in  $LIST_WORDS ; do
		    grep --binary-files=text $WORD openmp2X2_${EXAMPLE}.log
		done
		
	    fi
	    [ ! -z "$(ls *.nc 2>/dev/null)" ] && /bin/rm *.nc
	    #----------------------------------------------------------------------------------
	    echo "======================"
	    echo OPEN-MP 1X4 NPP=4 TESTS
	    echo
	    /bin/rm param_bak1.h
	    
	    sed 's/'NSUB_X=1,\ \ \*NSUB_E=NPP'/'NSUB_X=1,\ NSUB_E=4'/' < param_bak0.h > param_bak1.h
	    sed 's/'NPP=4'/'NPP=4'/'  param_bak1.h > param_bak2.h
	    /bin/mv param_bak2.h param_bak1.h
	    export OMP_NUM_THREADS=4
	    #
	    echo "--------------------------"
	    if [[ "${EXAMPLE}" == 'RIP' || "${EXAMPLE}" == 'JET' || "${EXAMPLE}" == 'GRAV_ADJ'  || "${EXAMPLE}" == 'INNERSHELF' || "${EXAMPLE}" == 'SHOREFACE' || "${EXAMPLE}" == 'INTERNAL' || "${EXAMPLE}" == 'ACOUS' || "${EXAMPLE}" == 'KH_INST' || "${EXAMPLE}" == 'IGW' || "${EXAMPLE}" == 'S2DV' || "${EXAMPLE}" == 'SWASH' || "${EXAMPLE}" == 'SHOREFACE' || "${EXAMPLE}" == 'THACKER' || "${EXAMPLE}" == 'TANK' ]] ; then
		echo 'SKIP THIS TEST CASE ' $EXAMPLE
	    else

		echo COMPILE OPENMP 1X4 $EXAMPLE
		sed 's/'undef\ $EXAMPLE'/'define\ $EXAMPLE'/' < cppdefs_bak1.h > cppdefs_bak2.h
		/bin/mv cppdefs_bak2.h cppdefs_bak1.h
		date
		time ./jobcomp_rvtk.bash > jobcomp_openmp_${EXAMPLE}.log
		date
		sed 's/'define\ $EXAMPLE'/'undef\ $EXAMPLE'/' < cppdefs_bak1.h > cppdefs_bak2.h
		/bin/mv cppdefs_bak2.h cppdefs_bak1.h
		/bin/mv croco croco_omp1X4_${EXAMPLE}.exe
		echo RUN OPENMP 1X4 $EXAMPLE
		time ./croco_omp1X4_${EXAMPLE}.exe > openmp1X4_${EXAMPLE}.log
		date
		echo TEST OPENMP 1X4 $EXAMPLE
		for WORD in $LIST_WORDS ; do
		    grep --binary-files=text $WORD openmp1X4_${EXAMPLE}.log
		done
	    fi
	    [ ! -z "$(ls *.nc 2>/dev/null)" ] && /bin/rm *.nc
	    #----------------------------------------------------------------------------------
	    echo "======================"
	    echo OPEN-MP 4X1 NPP=4 TESTS
	    echo
	    /bin/rm param_bak1.h
	    
	    sed 's/'NSUB_X=1,\ \ \*NSUB_E=NPP'/'NSUB_X=4,\ NSUB_E=1'/' < param_bak0.h > param_bak1.h
	    sed 's/'NPP=4'/'NPP=4'/' < param_bak1.h > param_bak2.h
	    /bin/mv param_bak2.h param_bak1.h
	    export OMP_NUM_THREADS=4
	    #
	    echo "--------------------------"
	    if [[ "${EXAMPLE}" == 'RIP' || "${EXAMPLE}" == 'JET' ||  "${EXAMPLE}" == 'OVERFLOW'  || "${EXAMPLE}" == 'SHELFRONT' ]] ; then
		echo 'SKIP THIS TEST CASE ' $EXAMPLE
	    else
		
		echo COMPILE OPENMP 4X1 $EXAMPLE
		sed 's/'undef\ $EXAMPLE'/'define\ $EXAMPLE'/' < cppdefs_bak1.h > cppdefs_bak2.h
		/bin/mv cppdefs_bak2.h cppdefs_bak1.h
		date
		time ./jobcomp_rvtk.bash > jobcomp_openmp_${EXAMPLE}.log
		date
		sed 's/'define\ $EXAMPLE'/'undef\ $EXAMPLE'/' < cppdefs_bak1.h > cppdefs_bak2.h
		/bin/mv cppdefs_bak2.h cppdefs_bak1.h
		/bin/mv croco croco_omp4X1_${EXAMPLE}.exe
		echo RUN OPENMP 4X1 $EXAMPLE
		time ./croco_omp4X1_${EXAMPLE}.exe > openmp4X1_${EXAMPLE}.log
		date
		echo TEST OPENMP 4X1 $EXAMPLE
		for WORD in  $LIST_WORDS ; do
		    grep --binary-files=text $WORD openmp4X1_${EXAMPLE}.log
		done
	    fi
	    [ ! -z "$(ls *.nc 2>/dev/null)" ] && /bin/rm *.nc
	    #----------------------------------------------------------------------------------
	    echo "======================"
	    echo OPEN-MP 2X4 NPP=8 TESTS
	    echo
	    /bin/rm param_bak1.h

	    sed 's/'NSUB_X=1,\ \ \*NSUB_E=NPP'/'NSUB_X=2,\ NSUB_E=4'/' < param_bak0.h > param_bak1.h
	    sed 's/'NPP=4'/'NPP=8'/' < param_bak1.h > param_bak2.h
	    /bin/mv param_bak2.h param_bak1.h
	    export OMP_NUM_THREADS=8
	    #
	    echo "--------------------------"
	    if [[ "${EXAMPLE} == 'RIP'" || "${EXAMPLE}" == 'JET' || "${EXAMPLE}" == 'GRAV_ADJ'  || "${EXAMPLE}" == 'INNERSHELF' || "${EXAMPLE}" == 'OVERFLOW'  || "${EXAMPLE}" == 'SHELFRONT' || "${EXAMPLE}" == 'SHOREFACE' || "${EXAMPLE}" == 'INTERNAL' || "${EXAMPLE}" == 'ACOUS' || "${EXAMPLE}" == 'KH_INST' || "${EXAMPLE}" == 'IGW' || "${EXAMPLE}" == 'S2DV' || "${EXAMPLE}" == 'SWASH' || "${EXAMPLE}" == 'SHOREFACE' || "${EXAMPLE}" == 'THACKER' || "${EXAMPLE}" == 'TANK' ]] ; then
		echo 'SKIP THIS TEST CASE ' $EXAMPLE
	    else

		echo COMPILE OPENMP 2X4 $EXAMPLE
		sed 's/'undef\ $EXAMPLE'/'define\ $EXAMPLE'/' < cppdefs_bak1.h > cppdefs_bak2.h
		/bin/mv cppdefs_bak2.h cppdefs_bak1.h
		date
		time ./jobcomp_rvtk.bash > jobcomp_openmp_${EXAMPLE}.log
		date
		sed 's/'define\ $EXAMPLE'/'undef\ $EXAMPLE'/' < cppdefs_bak1.h > cppdefs_bak2.h
		/bin/mv cppdefs_bak2.h cppdefs_bak1.h
		/bin/mv croco croco_omp2X4_${EXAMPLE}.exe
		echo RUN OPENMP 2X4 $EXAMPLE
		time ./croco_omp2X4_${EXAMPLE}.exe > openmp2X4_${EXAMPLE}.log
		date
		echo TEST OPENMP 2X4 $EXAMPLE
		for WORD in  $LIST_WORDS ; do
		    grep --binary-files=text $WORD openmp2X4_${EXAMPLE}.log
		done
	    fi
	    [ ! -z "$(ls *.nc 2>/dev/null)" ] && /bin/rm *.nc
	    #----------------------------------------------------------------------------------
	    echo "======================"
	    echo OPEN-MP 4X2 NPP=2 TESTS
	    echo
	    /bin/rm param_bak1.h

	    sed 's/'NSUB_X=1,\ \ \*NSUB_E=NPP'/'NSUB_X=4,\ NSUB_E=2'/' < param_bak0.h > param_bak1.h
	    sed 's/'NPP=4'/'NPP=8'/' < param_bak1.h > param_bak2.h
	    /bin/mv param_bak2.h param_bak1.h
	    export OMP_NUM_THREADS=8
	    #
	    echo "--------------------------"
	    if [[ "${EXAMPLE}" == 'RIP' || "${EXAMPLE}" == 'JET' || "${EXAMPLE}" == 'GRAV_ADJ'  || "${EXAMPLE}" == 'INNERSHELF' || "${EXAMPLE}" == 'OVERFLOW'  || "${EXAMPLE}" == 'SHELFRONT' || "${EXAMPLE}" == 'SHOREFACE' || "${EXAMPLE}" == 'INTERNAL' || "${EXAMPLE}" == 'ACOUS' || "${EXAMPLE}" == 'KH_INST' || "${EXAMPLE}" == 'IGW' || "${EXAMPLE}" == 'S2DV' || "${EXAMPLE}" == 'SWASH' || "${EXAMPLE}" == 'SHOREFACE' || "${EXAMPLE}" == 'THACKER' || "${EXAMPLE}" == 'TANK' ]] ; then
		echo 'SKIP THIS TEST CASE ' $EXAMPLE
	    else

		echo COMPILE OPENMP 4X2 $EXAMPLE
		sed 's/'undef\ $EXAMPLE'/'define\ $EXAMPLE'/' < cppdefs_bak1.h > cppdefs_bak2.h
		/bin/mv cppdefs_bak2.h cppdefs_bak1.h
		date
		time ./jobcomp_rvtk.bash > jobcomp_openmp_${EXAMPLE}.log
		date
		sed 's/'define\ $EXAMPLE'/'undef\ $EXAMPLE'/' < cppdefs_bak1.h > cppdefs_bak2.h
		/bin/mv cppdefs_bak2.h cppdefs_bak1.h
		/bin/mv croco croco_omp4X2_${EXAMPLE}.exe
		echo RUN OPENMP 4X2 $EXAMPLE
		time ./croco_omp4X2_${EXAMPLE}.exe > openmp4X2_${EXAMPLE}.log
		date
		echo TEST OPENMP 4X2 $EXAMPLE
		for WORD in  $LIST_WORDS ; do
		    grep --binary-files=text $WORD openmp4X2_${EXAMPLE}.log
		done
	    fi
	    [ ! -z "$(ls *.nc 2>/dev/null)" ] && /bin/rm *.nc

	    #----------------------------------------------------------------------------------

	    echo "======================"
	    echo OPEN-MP 1X8 NPP=8 TESTS
	    echo
	    /bin/rm param_bak1.h

	    sed 's/'NSUB_X=1,\ \ \*NSUB_E=NPP'/'NSUB_X=1,\ NSUB_E=8'/' < param_bak0.h > param_bak1.h
	    sed 's/'NPP=4'/'NPP=8'/' < param_bak1.h > param_bak2.h
	    /bin/mv param_bak2.h param_bak1.h
	    export OMP_NUM_THREADS=8
	    #
	    echo "--------------------------"
	    if [[ "${EXAMPLE}" == 'RIP' || "${EXAMPLE}" == 'JET' || "${EXAMPLE}" == 'GRAV_ADJ'  || "${EXAMPLE}" == 'INNERSHELF' || "${EXAMPLE}" == 'SHOREFACE' || "${EXAMPLE}" == 'INTERNAL' || "${EXAMPLE}" == 'ACOUS' || "${EXAMPLE}" == 'KH_INST' || "${EXAMPLE}" == 'IGW' || "${EXAMPLE}" == 'S2DV' || "${EXAMPLE}" == 'SWASH' || "${EXAMPLE}" == 'THACKER' || "${EXAMPLE}" == 'TANK' ]] ; then
		echo 'SKIP THIS TEST CASE ' $EXAMPLE
	    else

		echo COMPILE OPENMP 1X8 $EXAMPLE
		sed 's/'undef\ $EXAMPLE'/'define\ $EXAMPLE'/' < cppdefs_bak1.h > cppdefs_bak2.h
		/bin/mv cppdefs_bak2.h cppdefs_bak1.h
		date
		time ./jobcomp_rvtk.bash > jobcomp_openmp_${EXAMPLE}.log
		date
		sed 's/'define\ $EXAMPLE'/'undef\ $EXAMPLE'/' < cppdefs_bak1.h > cppdefs_bak2.h
		/bin/mv cppdefs_bak2.h cppdefs_bak1.h
		/bin/mv croco croco_omp1X8_${EXAMPLE}.exe
		echo RUN OPENMP 1X8 $EXAMPLE
		time ./croco_omp1X8_${EXAMPLE}.exe > openmp1X8_${EXAMPLE}.log
		date
		echo TEST OPENMP 1X8 $EXAMPLE
		for WORD in  $LIST_WORDS ; do
		    grep --binary-files=text $WORD openmp1X8_${EXAMPLE}.log
		done
	    fi
	    [ ! -z "$(ls *.nc 2>/dev/null)" ] && /bin/rm *.nc
	    #----------------------------------------------------------------------------------
	    echo "======================"
	    echo OPEN-MP 8X1 NPP=8 TESTS
	    echo
	    /bin/rm param_bak1.h

	    sed 's/'NSUB_X=1,\ \ \*NSUB_E=NPP'/'NSUB_X=8,\ NSUB_E=1'/' < param_bak0.h > param_bak1.h
	    sed 's/'NPP=4'/'NPP=8'/' < param_bak1.h > param_bak2.h
	    /bin/mv param_bak2.h param_bak1.h
	    export OMP_NUM_THREADS=8
	    #

	    echo "--------------------------"
	    if [[ "${EXAMPLE}" == 'ACOUS' || "${EXAMPLE}" == 'RIP' || "${EXAMPLE}" == 'JET' || "${EXAMPLE}" == 'OVERFLOW'  || "${EXAMPLE}" == 'SHELFRONT' ]] ; then
		echo 'SKIP THIS TEST CASE ' $EXAMPLE
	    else

		echo COMPILE OPENMP 8X1 $EXAMPLE
		sed 's/'undef\ $EXAMPLE'/'define\ $EXAMPLE'/' < cppdefs_bak1.h > cppdefs_bak2.h
		/bin/mv cppdefs_bak2.h cppdefs_bak1.h
		date
		time ./jobcomp_rvtk.bash > jobcomp_openmp_${EXAMPLE}.log
		date
		sed 's/'define\ $EXAMPLE'/'undef\ $EXAMPLE'/' < cppdefs_bak1.h > cppdefs_bak2.h
		/bin/mv cppdefs_bak2.h cppdefs_bak1.h
		/bin/mv croco croco_omp8X1_${EXAMPLE}.exe
		echo RUN OPENMP 8X1 $EXAMPLE
		time ./croco_omp8X1_${EXAMPLE}.exe > openmp8X1_${EXAMPLE}.log
		date
		echo TEST OPENMP 8X1 $EXAMPLE
		for WORD in  $LIST_WORDS ; do
		    grep --binary-files=text $WORD openmp8X1_${EXAMPLE}.log
		done
	    fi
	    [ ! -z "$(ls *.nc 2>/dev/null)" ] && /bin/rm *.nc
	    
	    #-----------------------------------------------------------------------------------------------
	fi #-> $ADDTEST == 'ON'
    fi  #COMPOMP

    #
    # Undef OPENMP
    #
    sed 's/'define\ OPENMP'/'undef\ OPENMP'/' < cppdefs_bak1.h > cppdefs_bak2.h
    /bin/mv cppdefs_bak2.h cppdefs_bak1.h


    ###############################################################################
    if [ ${COMPMPI} == 'ON' ] ; then
	###############################################################################
	# MPI
	#
	echo '------------------------------'
	echo '---MPI TESTING MPI TESTING----'
	echo '------------------------------'
	echo 
	echo define MPI
	sed 's/'undef\ \ \*MPI'/'define\ MPI'/' < cppdefs_bak1.h > cppdefs_bak2.h
	/bin/mv cppdefs_bak2.h cppdefs_bak1.h
	echo
	#
	#----------------------------------------------------------------
	echo "============="
	echo MPI 1X2 TESTS
	echo
	/bin/rm param_bak1.h
	/bin/cp param_bak0.h param_bak1.h

	sed 's/'NP_XI=1,\ \ \*NP_ETA=4'/'NP_XI=1,\ NP_ETA=2'/' < param_bak0.h > param_bak1.h
	#

	echo '----------------'
	 if [[ "${EXAMPLE}" == 'GRAV_ADJ'  || "${EXAMPLE}" == 'INNERSHELF' || "${EXAMPLE}" == 'INTERNAL' || "${EXAMPLE}" == 'ACOUS' || "${EXAMPLE}" == 'KH_INST' || "${EXAMPLE}"== 'IGW' || "${EXAMPLE}" == 'S2DV' || "${EXAMPLE}" == 'SWASH' || "${EXAMPLE}" == 'SHOREFACE' || "${EXAMPLE}" == 'THACKER' || "${EXAMPLE}" == 'TANK' ]] ; then
	    echo 'SKIP THIS TEST CASE ' $EXAMPLE

	else 
	    echo COMPILE MPI 1X2 $EXAMPLE
	    sed 's/'undef\ $EXAMPLE'/'define\ $EXAMPLE'/' < cppdefs_bak1.h > cppdefs_bak2.h
	    /bin/mv cppdefs_bak2.h cppdefs_bak1.h
	    date
	    time ./jobcomp_rvtk.bash > jobcomp_mpi_${EXAMPLE}.log
	    date
	    sed 's/'define\ $EXAMPLE'/'undef\ $EXAMPLE'/' < cppdefs_bak1.h > cppdefs_bak2.h
	    /bin/mv cppdefs_bak2.h cppdefs_bak1.h
	    /bin/mv croco croco_mpi1X2_${EXAMPLE}.exe
	    echo RUN MPI 1X2 $EXAMPLE
	    time $MPIRUN -np 2 ./croco_mpi1X2_${EXAMPLE}.exe > mpi1X2_${EXAMPLE}.log
	    date
	    echo TEST MPI 1X2 $EXAMPLE
	    for WORD in  $LIST_WORDS  ; do
		grep --binary-files=text $WORD mpi1X2_${EXAMPLE}.log
	    done
	fi
	[ ! -z "$(ls *.nc 2>/dev/null)" ] && /bin/rm *.nc
	#----------------------------------------------------------------
	echo "============="
	echo MPI 2X1 TESTS
	echo
	/bin/rm param_bak1.h
	/bin/cp param_bak0.h param_bak1.h

	sed 's/'NP_XI=1,\ \ \*NP_ETA=4'/'NP_XI=2,\ NP_ETA=1'/' < param_bak0.h > param_bak1.h
	#

	echo '----------------'
	if [[ "${EXAMPLE}" == 'OVERFLOW'  || "${EXAMPLE}" == 'SHELFRONT' ]] ; then
	    echo 'SKIP THIS TEST CASE ' $EXAMPLE
	else 
	    echo COMPILE MPI 2X1 $EXAMPLE
	    sed 's/'undef\ $EXAMPLE'/'define\ $EXAMPLE'/' < cppdefs_bak1.h > cppdefs_bak2.h
	    /bin/mv cppdefs_bak2.h cppdefs_bak1.h
	    date
	    time ./jobcomp_rvtk.bash > jobcomp_mpi_${EXAMPLE}.log
	    date
	    sed 's/'define\ $EXAMPLE'/'undef\ $EXAMPLE'/' < cppdefs_bak1.h > cppdefs_bak2.h
	    /bin/mv cppdefs_bak2.h cppdefs_bak1.h
	    /bin/mv croco ./croco_mpi2X1_${EXAMPLE}.exe
	    echo RUN MPI 2X1 $EXAMPLE
	    time $MPIRUN -np 2 ./croco_mpi2X1_${EXAMPLE}.exe > mpi2X1_${EXAMPLE}.log
	    date
	    echo TEST MPI 2X1 $EXAMPLE

	    for WORD in  $LIST_WORDS ; do
		grep --binary-files=text $WORD mpi2X1_${EXAMPLE}.log
	    done
	fi

	[ ! -z "$(ls *.nc 2>/dev/null)" ] && /bin/rm *.nc
	#----------------------------------------------------------------
	if [ $ADDTEST == 'ON' ] ; then
	    echo "============="
	    echo MPI 2X2 TESTS
	    echo
	    /bin/rm param_bak1.h
	    /bin/cp param_bak0.h param_bak1.h

	    sed 's/'NP_XI=1,\ \ \*NP_ETA=4'/'NP_XI=2,\ NP_ETA=2'/' < param_bak0.h > param_bak1.h

	    echo '----------------'
	     if [[ "${EXAMPLE}" == 'GRAV_ADJ'  || "${EXAMPLE}" == 'INNERSHELF' || "${EXAMPLE}" == 'INTERNAL' || "${EXAMPLE}" == 'SHELFRONT'|| "${EXAMPLE}" == 'OVERFLOW' || "${EXAMPLE}" == 'ACOUS' || "${EXAMPLE}" == 'KH_INST' || "${EXAMPLE}" == 'IGW' || "${EXAMPLE}" == 'S2DV' || "${EXAMPLE}" == 'SWASH' || "${EXAMPLE}" == 'SHOREFACE' || "${EXAMPLE}" == 'THACKER' || "${EXAMPLE}" == 'TANK' ]] ; then
		echo 'SKIP THIS TEST CASE ' $EXAMPLE
	    else

		echo COMPILE MPI 2X2 $EXAMPLE
		sed 's/'undef\ $EXAMPLE'/'define\ $EXAMPLE'/' < cppdefs_bak1.h > cppdefs_bak2.h
		/bin/mv cppdefs_bak2.h cppdefs_bak1.h
		date
		time ./jobcomp_rvtk.bash > jobcomp_mpi_${EXAMPLE}.log
		date
		sed 's/'define\ $EXAMPLE'/'undef\ $EXAMPLE'/' < cppdefs_bak1.h > cppdefs_bak2.h
		/bin/mv cppdefs_bak2.h cppdefs_bak1.h
		/bin/mv croco croco_mpi2X2_${EXAMPLE}.exe
		echo RUN MPI 2X2 $EXAMPLE
		time $MPIRUN -np 4 ./croco_mpi2X2_${EXAMPLE}.exe > mpi2X2_${EXAMPLE}.log
		date
		echo TEST MPI 2X2 $EXAMPLE
		for WORD in  $LIST_WORDS ; do
		    grep --binary-files=text $WORD mpi2X2_${EXAMPLE}.log
		done
	    fi

	    [ ! -z "$(ls *.nc 2>/dev/null)" ] && /bin/rm *.nc
	    #----------------------------------------------------------------
	    echo "============="
	    echo MPI 1X4 TESTS
	    echo
	    /bin/rm param_bak1.h
	    /bin/cp param_bak0.h param_bak1.h

	    sed 's/'NP_XI=1,\ \ \*NP_ETA=4'/'NP_XI=1,\ NP_ETA=4'/' < param_bak0.h > param_bak1.h
	    #

	    echo '----------------'
	    if [[ "${EXAMPLE}" == 'GRAV_ADJ'  || "${EXAMPLE}" == 'INNERSHELF' || "${EXAMPLE}" == 'INTERNAL' || "${EXAMPLE}" == 'ACOUS' || "${EXAMPLE}" == 'KH_INST' || "${EXAMPLE}" == 'IGW' || "${EXAMPLE}" == 'S2DV' || "${EXAMPLE}" == 'SWASH' || "${EXAMPLE}" == 'SHOREFACE' || "${EXAMPLE}" == 'THACKER' || "${EXAMPLE}" == 'TANK' ]] ; then
		echo 'SKIP THIS TEST CASE ' $EXAMPLE
	    else

		echo COMPILE MPI 1X4 $EXAMPLE
		sed 's/'undef\ $EXAMPLE'/'define\ $EXAMPLE'/' < cppdefs_bak1.h > cppdefs_bak2.h
		/bin/mv cppdefs_bak2.h cppdefs_bak1.h
		date
		time ./jobcomp_rvtk.bash > jobcomp_mpi_${EXAMPLE}.log
		date
		sed 's/'define\ $EXAMPLE'/'undef\ $EXAMPLE'/' < cppdefs_bak1.h > cppdefs_bak2.h
		/bin/mv cppdefs_bak2.h cppdefs_bak1.h
		/bin/mv croco croco_mpi1X4_${EXAMPLE}.exe
		echo RUN MPI 1X4 $EXAMPLE
		time $MPIRUN -np 4 ./croco_mpi1X4_${EXAMPLE}.exe > mpi1X4_${EXAMPLE}.log
		date
		echo TEST MPI 1X4 $EXAMPLE
		for WORD in  $LIST_WORDS ; do
		    grep --binary-files=text $WORD mpi1X4_${EXAMPLE}.log
		done
	    fi
	    [ ! -z "$(ls *.nc 2>/dev/null)" ] && /bin/rm *.nc
	    #----------------------------------------------------------------
	    echo "============="
	    echo MPI 4X1 TESTS
	    echo
	    /bin/rm param_bak1.h
	    /bin/cp param_bak0.h param_bak1.h

	    sed 's/'NP_XI=1,\ \ \*NP_ETA=4'/'NP_XI=4,\ NP_ETA=1'/' < param_bak0.h > param_bak1.h
	    #

	    echo '----------------'
	    if [[  "${EXAMPLE}" == 'JET' || "${EXAMPLE}" == 'OVERFLOW'  || "${EXAMPLE}" == 'SHELFRONT' ]] ; then
		echo 'SKIP THIS TEST CASE ' $EXAMPLE
	    else
		echo COMPILE MPI 4X1 $EXAMPLE
		sed 's/'undef\ $EXAMPLE'/'define\ $EXAMPLE'/' < cppdefs_bak1.h > cppdefs_bak2.h
		/bin/mv cppdefs_bak2.h cppdefs_bak1.h
		date
		time ./jobcomp_rvtk.bash > jobcomp_mpi_${EXAMPLE}.log
		date
		sed 's/'define\ $EXAMPLE'/'undef\ $EXAMPLE'/' < cppdefs_bak1.h > cppdefs_bak2.h
		/bin/mv cppdefs_bak2.h cppdefs_bak1.h
		/bin/mv croco croco_mpi4X1_${EXAMPLE}.exe
		echo RUN MPI 4X1 $EXAMPLE
		time $MPIRUN -np 4 ./croco_mpi4X1_${EXAMPLE}.exe > mpi4X1_${EXAMPLE}.log
		date
		echo TEST MPI 4X1 $EXAMPLE

		for WORD in  $LIST_WORDS  ; do
		    grep --binary-files=text $WORD mpi4X1_${EXAMPLE}.log
		done
	    fi
	    [ ! -z "$(ls *.nc 2>/dev/null)" ] && /bin/rm *.nc
	    #----------------------------------------------------------------
	    echo '=============='
	    echo MPI 2X4 TESTS
	    echo
	    /bin/rm param_bak1.h
	    /bin/cp param_bak0.h param_bak1.h

	    sed 's/'NP_XI=1,\ \ \*NP_ETA=4'/'NP_XI=2,\ NP_ETA=4'/' < param_bak0.h > param_bak1.h
	    echo '----------------'
	    if [[ "${EXAMPLE}" == 'GRAV_ADJ'  || "${EXAMPLE}" == 'INNERSHELF' || "${EXAMPLE}" == 'OVERFLOW'  || "${EXAMPLE}" == 'SHELFRONT' || "${EXAMPLE}" == 'INTERNAL' || "${EXAMPLE}" == 'ACOUS' || "${EXAMPLE}" == 'KH_INST' || "${EXAMPLE}" == 'IGW' || "${EXAMPLE}" == 'S2DV' || "${EXAMPLE}" == 'SWASH' || "${EXAMPLE}" == 'SHOREFACE' || "${EXAMPLE}" == 'THACKER' || "${EXAMPLE}" == 'TANK' ]] ; then
		echo 'SKIP THIS TEST CASE ' $EXAMPLE
	    else

		echo COMPILE MPI 2X4 $EXAMPLE
		sed 's/'undef\ $EXAMPLE'/'define\ $EXAMPLE'/' < cppdefs_bak1.h > cppdefs_bak2.h
		/bin/mv cppdefs_bak2.h cppdefs_bak1.h
		date
		time ./jobcomp_rvtk.bash > jobcomp_mpi_${EXAMPLE}.log
		date
		sed 's/'define\ $EXAMPLE'/'undef\ $EXAMPLE'/' < cppdefs_bak1.h > cppdefs_bak2.h
		/bin/mv cppdefs_bak2.h cppdefs_bak1.h
		/bin/mv croco croco_mpi2X4_${EXAMPLE}.exe
		echo RUN MPI 2X4 $EXAMPLE
		time $MPIRUN -np 8 ./croco_mpi2X4_${EXAMPLE}.exe > mpi2X4_${EXAMPLE}.log
		date
		echo TEST MPI 2X4 $EXAMPLE
		for WORD in  $LIST_WORDS  ; do
		    grep --binary-files=text $WORD mpi2X4_${EXAMPLE}.log
		done
	    fi
	    [ ! -z "$(ls *.nc 2>/dev/null)" ] && /bin/rm *.nc
	    #----------------------------------------------------------------
	    echo '=============='
	    echo MPI 4X2 TESTS
	    echo
	    /bin/rm param_bak1.h
	    /bin/cp param_bak0.h param_bak1.h

	    sed 's/'NP_XI=1,\ \ \*NP_ETA=4'/'NP_XI=4,\ NP_ETA=2'/' < param_bak0.h > param_bak1.h
	    echo '----------------'
	    if [[  "${EXAMPLE}" == 'JET' || "${EXAMPLE}" == 'GRAV_ADJ'  || "${EXAMPLE}" == 'INNERSHELF' || "${EXAMPLE}" == 'OVERFLOW'  || "${EXAMPLE}" == 'SHELFRONT' || "${EXAMPLE}" == 'INTERNAL' || "${EXAMPLE}" == 'ACOUS' || "${EXAMPLE}" == 'KH_INST' || "${EXAMPLE}" == 'IGW' || "${EXAMPLE}" == 'S2DV' || "${EXAMPLE}" == 'SWASH' || "${EXAMPLE}" == 'SHOREFACE' || "${EXAMPLE}" == 'THACKER' || "${EXAMPLE}" == 'TANK' ]] ; then
		echo 'SKIP THIS TEST CASE ' $EXAMPLE
	    else

		echo COMPILE MPI 4X2 $EXAMPLE
		sed 's/'undef\ $EXAMPLE'/'define\ $EXAMPLE'/' < cppdefs_bak1.h > cppdefs_bak2.h
		/bin/mv cppdefs_bak2.h cppdefs_bak1.h
		date
		time ./jobcomp_rvtk.bash > jobcomp_mpi_${EXAMPLE}.log
		date
		sed 's/'define\ $EXAMPLE'/'undef\ $EXAMPLE'/' < cppdefs_bak1.h > cppdefs_bak2.h
		/bin/mv cppdefs_bak2.h cppdefs_bak1.h
		/bin/mv croco croco_mpi4X2_${EXAMPLE}.exe
		echo RUN MPI 4X2 $EXAMPLE
		time $MPIRUN -np 8 ./croco_mpi4X2_${EXAMPLE}.exe > mpi4X2_${EXAMPLE}.log
		date
		echo TEST MPI 4X2 $EXAMPLE

		for WORD in  $LIST_WORDS  ; do
		    grep --binary-files=text $WORD mpi4X2_${EXAMPLE}.log
		done
	    fi
	    [ ! -z "$(ls *.nc 2>/dev/null)" ] && /bin/rm *.nc
	    #----------------------------------------------------------------

	    echo '=============='
	    echo MPI 1X8 TESTS
	    echo
	    /bin/rm param_bak1.h
	    /bin/cp param_bak0.h param_bak1.h

	    sed 's/'NP_XI=1,\ \ \*NP_ETA=4'/'NP_XI=1,\ NP_ETA=8'/' < param_bak0.h > param_bak1.h
	    #


	    echo '----------------'
	    if [[ "${EXAMPLE}" == 'GRAV_ADJ'  || "${EXAMPLE}" == 'INNERSHELF'  || "${EXAMPLE}" == 'INTERNAL' || "${EXAMPLE}" == 'ACOUS' || "${EXAMPLE}" == 'KH_INST' || "${EXAMPLE}" == 'IGW' || "${EXAMPLE}" == 'S2DV' || "${EXAMPLE}" == 'SWASH' || "${EXAMPLE}" == 'SHOREFACE' || "${EXAMPLE}" == 'THACKER' || "${EXAMPLE}" == 'TANK' ]] ; then
		echo 'SKIP THIS TEST CASE ' $EXAMPLE
	    else

		echo COMPILE MPI 1X8 $EXAMPLE
		sed 's/'undef\ $EXAMPLE'/'define\ $EXAMPLE'/' < cppdefs_bak1.h > cppdefs_bak2.h
		/bin/mv cppdefs_bak2.h cppdefs_bak1.h
		date
		time ./jobcomp_rvtk.bash > jobcomp_mpi_${EXAMPLE}.log
		date
		sed 's/'define\ $EXAMPLE'/'undef\ $EXAMPLE'/' < cppdefs_bak1.h > cppdefs_bak2.h
		/bin/mv cppdefs_bak2.h cppdefs_bak1.h
		/bin/mv croco croco_mpi1X8_${EXAMPLE}.exe
		echo RUN MPI 1X8 $EXAMPLE
		time $MPIRUN -np 8 ./croco_mpi1X8_${EXAMPLE}.exe > mpi1X8_${EXAMPLE}.log
		date
		echo TEST MPI 1X8 $EXAMPLE

		for WORD in  $LIST_WORDS ; do
		    grep --binary-files=text $WORD mpi1X8_${EXAMPLE}.log
		done
	    fi
	    [ ! -z "$(ls *.nc 2>/dev/null)" ] && /bin/rm *.nc
	    #----------------------------------------------------------------
	    #----------------------------------------------------------------
	    echo '=============='
	    echo MPI 8X1 TESTS
	    echo
	    /bin/rm param_bak1.h
	    /bin/cp param_bak0.h param_bak1.h

	    sed 's/'NP_XI=1,\ \ \*NP_ETA=4'/'NP_XI=8,\ NP_ETA=1'/' < param_bak0.h > param_bak1.h
	    echo '----------------'
	    if [[  "${EXAMPLE}" == 'ACOUS' || "${EXAMPLE}" == 'JET' || "${EXAMPLE}" == 'OVERFLOW'  || "${EXAMPLE}" == 'SHELFRONT' || "${EXAMPLE}" == 'SHOREFACE' || "${EXAMPLE}" == 'JET' || "${EXAMPLE}" == 'ACOUS' ]] ; then
		echo 'SKIP THIS TEST CASE ' $EXAMPLE
	    else

		echo COMPILE MPI 8X1 $EXAMPLE
		sed 's/'undef\ $EXAMPLE'/'define\ $EXAMPLE'/' < cppdefs_bak1.h > cppdefs_bak2.h
		/bin/mv cppdefs_bak2.h cppdefs_bak1.h
		date
		time ./jobcomp_rvtk.bash > jobcomp_mpi_${EXAMPLE}.log
		date
		sed 's/'define\ $EXAMPLE'/'undef\ $EXAMPLE'/' < cppdefs_bak1.h > cppdefs_bak2.h
		/bin/mv cppdefs_bak2.h cppdefs_bak1.h
		/bin/mv croco croco_mpi8X1_${EXAMPLE}.exe
		echo RUN MPI 8X1 $EXAMPLE
		time $MPIRUN -np 8 ./croco_mpi8X1_${EXAMPLE}.exe > mpi8X1_${EXAMPLE}.log
		date
		echo TEST MPI 8X1 $EXAMPLE
		for WORD in  $LIST_WORDS ; do
		    grep --binary-files=text $WORD mpi8X1_${EXAMPLE}.log
		done
	    fi
	    [ ! -z "$(ls *.nc 2>/dev/null)" ] && /bin/rm *.nc
	    echo "Fin Cas Test "$EXAMPLE
	    #----------------------------------------------------------------

	fi  #-> ( $ADDTEST == 'ON' ) then     

    fi  # COMPMPI  

done   #boucle sur les cas tests

#----------------------------------------------------------------
#----------------------------------------------------------------
# Cleaning
echo " "
echo "End of the CVTK PROCEDURE"
echo "CLEANING"
#/bin/rm cppdefs_bak1.h param_bak0.h param_bak1.h
