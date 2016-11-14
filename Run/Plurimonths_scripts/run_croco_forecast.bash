#!/bin/bash
#
################### USER'S DEFINED PARAMETERS ##########################
#
#=======================================================================
#  Define environment variables
#=======================================================================
#
# Environment for Crontab
#
OS=`uname`
echo "OPERATING SYSTEM IS: $OS"

if [ -f /etc/bashrc ]; then
  . /etc/bashrc
fi
if [[ $OS == Linux ]] ; then
  source ~/.bashrc
elif [[ $OS == Darwin ]] ; then
  source ~/.bash_profile
else
  echo "unkbown operating system" 1>&2
fi
#
export TOOLSDIR=$HOME/croco_tools/Forecast_tools
export RUNDIR=${HOME}/croco_tools/Run
export MATLAB=/usr/local/bin/matlab
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/opt/loaddap-3.5.2/lib

unalias cp
unalias mv
export CP=/bin/cp
export MV=/bin/mv
export LN=/bin/ln
export DATESTR=`date +%Y-%m-%d`
#
#=======================================================================
#  Define options
#=======================================================================
#
echo Start forecast 
date
#
# Get forcing Files from DODS SERVER and process them for CROCO
# PRE_PROCESS=1 ==> do the work (0 otherwise)
#
export PRE_PROCESS=1
#
# Perform Iterations for convergence of ROMS and OGCM
# ITERATION=1 ==> do several hindcasts using nudging
#
export ITERATION=0
#
# Restart from previous forecast
#
export RESTART=0
#
# Clean results of previous forecasts
#
export CLEANING=0
#
# ONERUN process: ONERUN=1: make one run for hindcast/forecast
#                 ONERUN=0: make two runs for hindcast and forecast
#
export ONERUN=1
#
#
#=======================================================================
#  Define directories, files and running parameters
#=======================================================================
#
export SCRATCHDIR=${RUNDIR}/SCRATCH
export INPUTDIR=${RUNDIR}
export MSSDIR=${RUNDIR}/CROCO_FILES
export MSSOUT=${RUNDIR}/FORECAST
#
export MODEL=croco
export CODFILE=croco
export EXEC="mpirun -np 2"
#
export GRDFILE=${MODEL}_grd.nc
export INIFILE=${MODEL}_ini.nc
export RSTFILE=${MODEL}_rst.nc
export AVGFILE=${MODEL}_avg.nc
export HISFILE=${MODEL}_his.nc
export BLKFILE=${MODEL}_blk_GFS_0.nc
export FRCFILE=${MODEL}_frc_GFS_0.nc
export BRYFILE=${MODEL}_bry_mercator_0.nc
export CLMFILE=${MODEL}_clm_mercator_0.nc
export INIFILE0=${MODEL}_ini_mercator_0.nc
#
################  END USER'S DEFINED PARAMETERS ##########################
#
# Go the Input directory 
# 
cd $INPUTDIR
#
# Create working directories if needed
#
if [[ ! -e $SCRATCHDIR ]]; then
    mkdir $SCRATCHDIR
else
    echo "$SCRATCHDIR already exists" 1>&2
fi
if [[ ! -e $MSSOUT ]]; then
    mkdir $MSSOUT
else
    echo "$MSSOUT already exists" 1>&2
fi
#
# Clean I/O files from previous forecast
#
if [ $CLEANING = 1 ] ; then
  rm -f $MSSDIR/${MODEL}_blk_* $MSSDIR/${MODEL}_frc_* $MSSDIR/${MODEL}_bry_*
  rm -f $MSSDIR/${MODEL}_clm_* $MSSDIR/${MODEL}_ini_*
  rm -f $MSSOUT/${MODEL}_his_* $MSSOUT/${MODEL}_avg_* $MSSOUT/${MODEL}_rst_*
fi
#
#=======================================================================
# PRODUCE INITIAL CONDITIONS AND FORCING
#=======================================================================
# Compute lateral boundaries from MERCATOR
#     and surface forcing from GFS
#
if [ $PRE_PROCESS = 1 ] ; then
  echo "Processing boundary and forcing files"
  $MATLAB  -batch -nodisplay < make_forecast.m > matlab_forecast.out
fi
#
# Copy files in SCRATCH dir
#
echo "Getting $BLKFILE from $MSSDIR"
$CP -f  $MSSDIR/$BLKFILE $SCRATCHDIR
echo "Getting $FRCFILE from $MSSDIR"
$CP -f  $MSSDIR/$FRCFILE $SCRATCHDIR
echo "Getting $BRYFILE from $MSSDIR"
$CP -f  $MSSDIR/$BRYFILE $SCRATCHDIR
echo "Getting $CLMFILE from $MSSDIR"
$CP -f  $MSSDIR/$CLMFILE $SCRATCHDIR
#
# Initial file:
#  if RESTART, get initial conditions 
#  from the previous hindcast run
#  else use global data.
#
if [ $RESTART = 1 ] ; then
  echo "Getting $INIFILE from $MSSOUT"
  $CP -f  $MSSOUT/$INIFILE $SCRATCHDIR
else
  echo "Getting $INIFILE from $MSSDIR"
  $CP -f  $MSSDIR/$INIFILE0 $SCRATCHDIR/$INIFILE
fi
#
# Get static files
#
echo "Getting $CODFILE from $INPUTDIR"
$CP -f $INPUTDIR/$CODFILE $SCRATCHDIR
chmod u+x $CODFILE
echo "Getting ${GRDFILE} from $MSSDIR"
$CP -f $MSSDIR/${GRDFILE} $SCRATCHDIR
echo "Getting ${MODEL}_hindcast.in from $INPUTDIR"
$CP -f $INPUTDIR/${MODEL}_hindcast.in $SCRATCHDIR
echo "Getting ${MODEL}_forecast.in from $INPUTDIR"
$CP -f $INPUTDIR/${MODEL}_forecast.in $SCRATCHDIR
echo "Getting ${MODEL}_stations.in from $INPUTDIR"
$CP -f $INPUTDIR/${MODEL}_stations.in $SCRATCHDIR
#
#  Change directory
#
cd $SCRATCHDIR
#
#=======================================================================
#  HINDCAST/FORECAST RUN
#=======================================================================
#
if [ $ONERUN = 1 ] ; then

  echo Hindcast/Forecast run
  date
  $EXEC $CODFILE ${MODEL}_forecast.in > ${MODEL}_forecast_${DATESTR}.out
  date
  #
  # Store the initial file for the next forecast
  #
  $CP -f $SCRATCHDIR/$RSTFILE ${MSSOUT}/$INIFILE
  #
  # Store the output forecast files
  #
  $CP $SCRATCHDIR/$HISFILE ${MSSOUT}/${MODEL}_his_forecast_${DATESTR}.nc
  $CP $SCRATCHDIR/$AVGFILE ${MSSOUT}/${MODEL}_avg_forecast_${DATESTR}.nc
  #
else
  #
  #=======================================================================
  #  HINDCAST RUN
  #=======================================================================
  #
  echo Hindcast run  
  # Don't forget to modify NRPFRST and NRREC 
  # in croco_hindcast.in and croco_forecast.in
  date
  $EXEC $CODFILE ${MODEL}_hindcast.in > ${MODEL}_hindcast_${DATESTR}.out
  #
  if [ $ITERATION = 1 ] ; then
    echo 'ITERATION ITERATION'
    $LN -sf $TOOLSDIR/iteration.m iteration.m
    $LN -sf $RUNDIR/start.m start.m
    $LN -sf $RUNDIR/crocotools_param.m crocotools_param.m
    $MATLAB  -nodisplay < iteration.m > iteration.out
    rm -f iteration.m start.m crocotools_param.m
  fi
  #
  date
  #
  # Get the initial file for the forecast run
  #
  $CP -f $SCRATCHDIR/$RSTFILE ${SCRATCHDIR}/$INIFILE
  #
  # Store the initial file for the next hindcast run
  #
  $CP -f $SCRATCHDIR/$RSTFILE ${MSSOUT}/$INIFILE
  #
  # Store the output hindcast files
  #
  $MV $SCRATCHDIR/$HISFILE ${MSSOUT}/${MODEL}_his_hindcast_${DATESTR}.nc
  $MV $SCRATCHDIR/$AVGFILE ${MSSOUT}/${MODEL}_avg_hindcast_${DATESTR}.nc
  $MV $SCRATCHDIR/$RSTFILE ${MSSOUT}/${MODEL}_rst_hindcast_${DATESTR}.nc
  #
  #=======================================================================
  #  FORECAST RUN
  #=======================================================================
  #
  echo Forecast run 
  date
  $EXEC $CODFILE ${MODEL}_forecast.in > ${MODEL}_forecast_${DATESTR}.out
  date
  #
  # Store the output forecast files
  #
  $CP $SCRATCHDIR/$HISFILE ${MSSOUT}/${MODEL}_his_forecast_${DATESTR}.nc
  $CP $SCRATCHDIR/$AVGFILE ${MSSOUT}/${MODEL}_avg_forecast_${DATESTR}.nc
  #
fi
#
#=======================================================================
#  PLOT RESULTS
#=======================================================================
#
cd $INPUTDIR

$LN -sf $TOOLSDIR/plot_forecast_croco.m plot_forecast_croco.m
$LN -sf $TOOLSDIR/plot_quick_forecast.m plot_quick_forecast.m

# Production plot
#
$MATLAB  -batch -nodisplay < plot_forecast_croco.m >  plot_forecast_croco.out

# Quick plot
#
#$MATLAB  -batch -nodisplay < plot_quick_forecast.m >  plot_quick_forecast.out

rm -f plot_forecast_croco.m plot_quick_forecast.m

echo Forecast finished
date



