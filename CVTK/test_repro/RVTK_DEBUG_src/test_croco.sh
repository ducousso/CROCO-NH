#!/bin/bash

#======================================================================
# Global declaration
#======================================================================
#do_gitupdate=off

echo
echo '============================================== '
echo
echo '         TEST CROCO WITH DEBUG RVTK            '
echo
echo '============================================== '

#
# TEST = TESTCASES, REGIONAL or VORTEX
#
TEST=$1
echo
if [ $# -eq 0 ]; then
    echo "No configuration argument supplied: take TESTCASES"
    TEST=TESTCASES
else
    echo "Configuration argument supplied: $TEST"
fi

#
# Get code source from jobcomp file
#
sed -n -e '/SOURCE=/p' jobcomp_rvtk.bash > tmp1
sed -n '$p' tmp1 > tmp2
eval "SOURCE_CROCO=`sed -n -e '/SOURCE=/ s/.*\= *//p' tmp2`"
rm -f tmp1 tmp2
echo
echo 'SOURCE_CROCO='$SOURCE_CROCO

#
# Get compilator
#
OS=`uname`
echo
echo " OPERATING SYSTEM IS: $OS"
if [[ $OS == Linux ]] ; then
        export compilo=`sed -n -e '/LINUX_FC=/ s/.*\= *//p' jobcomp_rvtk.bash`
elif [[ $OS == Darwin ]] ; then
        export compilo=`sed -n -e '/DARWIN_FC=/ s/.*\= *//p' jobcomp_rvtk.bash`
fi

#
# MAKE CONSISTENT CHOICES
#
#if [[ $compilo == ifort ]]; then
# echo
# echo "Compilator is: "$compilo
# echo
# export PATH=/usr/local/bin:/opt/intel/Compiler/11.1/058/bin:$PATH
# export LD_LIBRARY_PATH=/usr/local/lib
#else
# echo
# echo "Compilator is: "$compilo
#echo
# export PATH=/usr/local/bin:/opt/local/bin:/opt/local/sbin:$PATH
# export LD_LIBRARY_PATH=/usr/local/lib
#fi

export MPIRUN=`which mpirun`
export RVTK_DIR=`pwd`
echo 'TESTING processed in RVTK_DIR='$RVTK_DIR
echo

############################################################################
#  Update the source
#-------------------
echo 'PATH='$PATH
echo
echo 'LD_LIBRARY_PATH='$LD_LIBRARY_PATH
echo
echo 'MPIRUN='$MPIRUN

## GIT code update
##-----------------
#if [[ $do_gitupdate == on ]]; then
#    echo
#    echo "PROCESS GIT UPDATE"
#    cd  $SOURCE_CROCO/..
#    /usr/bin/git pull 
#    cd -
#    echo "GIT UPDATE DONE"
#else
#    echo
#    echo "NO GIT UPDATE"
#fi

# Get revision of sources
#-------------------------
#./gitinfo.sh $SOURCE_CROCO > gitinfos
numrev0=`sed -n '/revision/{n;p;}' gitinfos`
numrev=`echo $numrev0 | tr -d [:blank:]`
echo
echo Rev$numrev

# Run RVTK
#----------
echo "Run rvtk"
cd $RVTK_DIR
echo "Remove all Recap log files "
/bin/rm -f Recap_${TEST}*

SCRIPT_RVTK=rvtk_${TEST}.bash

echo 
echo "Run $SCRIPT_RVTK"
#./$SCRIPT_RVTK > Recap_${TEST}.git${numrev} 2>&1
./$SCRIPT_RVTK > Recap_${TEST}.git${numrev}
# Clean output and locate BUGBIN
#--------------------------------
today=`date +%Y%m%d`
mv Recap_${TEST}.git${numrev} Recap_${TEST}_${today}.git${numrev}
echo "extract_results_croco.pl"
./extract_results_croco.pl "$TEST"
head -n 24 Recap_${TEST}_${today}.git${numrev} > header.txt
cat Results_${TEST}_${today}.git${numrev} >> header.txt
mv header.txt Results_${TEST}_${today}.git${numrev}
mv Results_${TEST}_${today}.git${numrev} ./Log

#mv *.log ./Log

# Copy the file on the legos network
#-----------------------------------
#scp "./Log/Results_${TEST}_${today}" cambon@torres.legos.obs-mip.fr:/home/olvac/cambon/Rvtk_Debug_CR

cd $RVTK_DIR



