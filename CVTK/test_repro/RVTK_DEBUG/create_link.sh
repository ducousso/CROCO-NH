#!/bin/bash
echo '============================================================='
echo 'Create the link between RVTK_DEBUG/ dir . and RVTK_DEBUG_src/'
echo '  '

dir_home=$PWD/../RVTK_DEBUG_src

mkdir TEST_CASES

echo '==============='
echo 'Process input file'
ln -sf CROCO_FILES_VHR CROCO_FILES
ln -sf ${dir_home}/VHR .
ln -sf ${dir_home}/JET .
ln -sf ${dir_home}/VORTEX .
ln -sf ${dir_home}/SHOREFACE .

echo '==============='
echo 'Process scripts'
ln -sf ${dir_home}/gitinfo.sh .
ln -sf ${dir_home}/test_* .
ln -sf ${dir_home}/extract_* .
ln -sf ${dir_home}/rvtk* .
ln -sf ${dir_home}/Log .
cp -Rf ${dir_home}/git_process.bash .
cp -Rf ${dir_home}/jobcomp_rvtk.bash .
echo '==============='
echo 'Process namelist files'

#--
ln -sf ${dir_home}/VHR/AGRIF_FixedGrids.in.REGIONAL.VHR AGRIF_FixedGrids.in.REGIONAL
ln -sf ${dir_home}/VORTEX/AGRIF_FixedGrids.in.VORTEX .

ln -sf VHR/croco.in.VHR.1 croco.in.1
ln -sf VHR/croco.in.VHR croco.in 
#--
cd TEST_CASES
ln -sf ${dir_home}/ANA/croco.in* .
#--
ln -sf ${dir_home}/JET/croco.in* .
ln -sf ${dir_home}/JET/jet*.nc .
#--
ln -sf ${dir_home}/VORTEX/croco.in* .
ln -sf ${dir_home}/VORTEX/vortex_*.nc* .
#--
ln -sf ${dir_home}/SHOREFACE/croco.in* .
ln -sf ${dir_home}/SHOREFACE/shoreface*.nc .

cd ../

#--
echo '==============='
echo 'Process slurm files eventually needed'
cp -sf ${dir_home}/*.pbs .
cp -sf ${dir_home}/*.slurm .

cp -Rf jobcomp_rvtk.bash.BACK jobcomp_rvtk.bash
cp -Rf git_process.bash.BACK git_process.bash

echo '  '
echo 'Well done: Finish linking'
echo '============================================================='
