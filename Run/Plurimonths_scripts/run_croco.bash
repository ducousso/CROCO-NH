#!/bin/bash
########################################################
#  Define files and run parameters
########################################################
#
# Name used for the input files. For example croco_grd.nc
MODEL=croco

# Scratch directory where the model is run
SCRATCHDIR=`pwd`/SCRATCH

# Input directory where the croco_inter.in input file is located
INPUTDIR=`pwd`

# AGRIF input file which defines the position of child grids
AGRIF_FILE=AGRIF_FixedGrids.in

# Directory where the croco input NetCDF files (croco_grd.nc, ...) are stored
MSSDIR=`pwd`/CROCO_FILES

# Directory where the croco output and restart NetCDF files (croco_his.nc, ...) are stored
MSSOUT=`pwd`/CROCO_FILES

# CROCO executable
CODFILE=croco

# number of processors for MPI run
NBPROCS=8

# command for running the mode : ./ for sequential job, mpirun -np NBPROCS for mpi run
RUNCMD='./'
#RUNCMD="mpirun -np $NBPROCS "

#
#  Define environment variables for OPENMP
#
OMP_SCHEDULE=static
OMP_NUM_THREADS=2
OMP_DYNAMIC=false
OMP_NESTED=false
KMP_LIBRARY=throughput
KMP_STACKSIZE=2m
KMP_DUPLICATE_LIB_OK=TRUE

# Model time step [seconds]
#
DT=3600
#
# Number of days per month
#
NDAYS=30
#
# number total of grid levels
#
NLEVEL=1
#
#  Time Schedule  -  TIME_SCHED=0 --> yearly files
#                    TIME_SCHED=1 --> monthly files
#
TIME_SCHED=1
#
NY_START=1
NY_END=10
NM_START=1
NM_END=12
#
#unalias cp
#unalias mv
#limit coredumpsize unlimited
CP=/bin/cp
MV=/bin/mv
#
########################################################
#  END USER CHANGE
########################################################
#
if [[ $TIME_SCHED == 0 ]]; then
  NM_START=1
  NM_END=1
fi
#
# coarse netcdf file names
#
GRDFILE=${MODEL}_grd.nc
FORFILE=${MODEL}_frc.nc
BLKFILE=${MODEL}_blk.nc
BRYFILE=${MODEL}_bry.nc
CLMFILE=${MODEL}_clm.nc
INIFILE=${MODEL}_ini.nc
#
#  Restart file - RSTFLAG=0 --> No Restart
#		  RSTFLAG=1 --> Restart
#
if [[ $NY_START == 1 && $NM_START == 1 ]]; then
  RSTFLAG=0
else
  RSTFLAG=1
fi
#
if [[ $RSTFLAG != 0 ]]; then
  NY=$NY_START
  NM=$NM_START
  if [[ $TIME_SCHED == 0 ]]; then
     NY=$((NY - 1))
    TIME=Y${NY}
  else
    NM=$((NM - 1))
    if [[ $NM == 0 ]]; then
      NM=12 
      NY=$((NY - 1))
    fi
    TIME=Y${NY}M${NM}
  fi
  RSTFILE=${MODEL}_rst_${TIME}.nc
fi
#
# Get the code
#
cd $SCRATCHDIR
echo "Getting $CODFILE from $INPUTDIR"
$CP -f $INPUTDIR/$CODFILE $SCRATCHDIR
chmod u+x $CODFILE
if [[ $NLEVEL -gt 1 ]]; then
   echo "Getting $AGRIF_FILE from $INPUTDIR"
   $CP -f $INPUTDIR/$AGRIF_FILE $SCRATCHDIR
fi
#
# Get the netcdf files
#
LEVEL=0
echo "Getting ${BRYFILE} from $MSSDIR"
$CP -f $MSSDIR/${BRYFILE} $SCRATCHDIR
while [[ $LEVEL != $NLEVEL ]]; do
  if [[ ${LEVEL} == 0 ]]; then
    ENDF=
  else
    ENDF=.${LEVEL}
  fi
  echo "Getting ${GRDFILE}${ENDF} from $MSSDIR"
  $CP -f $MSSDIR/${GRDFILE}${ENDF} $SCRATCHDIR
  echo "Getting ${FORFILE}${ENDF} from $MSSDIR"
  $CP -f $MSSDIR/${FORFILE}${ENDF} $SCRATCHDIR
  echo "Getting ${BLKFILE}${ENDF} from $MSSDIR"
  $CP -f $MSSDIR/${BLKFILE}${ENDF} $SCRATCHDIR
  echo "Getting ${CLMFILE}${ENDF} from $MSSDIR"
  $CP -f $MSSDIR/${CLMFILE}${ENDF} $SCRATCHDIR
  if [[ $RSTFLAG == 0 ]]; then
    echo "Getting ${INIFILE}${ENDF} from $MSSDIR"
    $CP -f $MSSDIR/${INIFILE}${ENDF} $SCRATCHDIR
  else
    echo "Getting ${RSTFILE}${ENDF} from $MSSOUT"
    $CP -f $MSSOUT/${RSTFILE}${ENDF} $SCRATCHDIR
    $CP -f ${RSTFILE}${ENDF} ${MODEL}_ini.nc${ENDF}
  fi
  echo "Getting ${MODEL}_inter.in${ENDF} from $INPUTDIR"
  $CP -f $INPUTDIR/${MODEL}_inter.in${ENDF} ${MODEL}_inter.in${ENDF}

  LEVEL=$((LEVEL + 1))
done
#
# Put the number of time steps in the .in files
#
NUMTIMES=0
NUMTIMES=$((NDAYS * 24 * 3600))
NUMTIMES=$((NUMTIMES / DT))
echo "Writing in ${MODEL}_inter.in"
LEVEL=0
while [[ $LEVEL != $NLEVEL ]]; do
  if [[ ${LEVEL} == 0 ]]; then
    ENDF=''
  else
    ENDF=.${LEVEL}
    NUMTIMES=$((3 * NUMTIMES))
  fi
  echo "USING NUMTIMES = $NUMTIMES"
  sed 's/NUMTIMES/'$NUMTIMES'/' < ${MODEL}_inter.in${ENDF} > ${MODEL}.in${ENDF}
  LEVEL=$((LEVEL + 1))
done
#
###########################################################
#  Compute
###########################################################
#
NY_END=$((NY_END + 1))
NM_END=$((NM_END + 1))
NY=$NY_START
while [[ $NY != $NY_END ]]; do
  if [[ $NY == $NY_START ]]; then
    NM=$NM_START
  else 
    NM=1
  fi
  MY_YEAR=$NY
  MY_YEAR=$((MY_YEAR + 1))
  if [[ $MY_YEAR == $NY_END ]]; then
    MONTH_END=$NM_END
  else 
    MONTH_END=13
  fi
  if [[ $TIME_SCHED == 0 ]]; then
    MONTH_END=2
  fi
  while [[ $NM != $MONTH_END ]]; do
    if [[ $TIME_SCHED == 0 ]]; then
      TIME=Y${NY}
      echo "Computing YEAR $NY"
    else
      TIME=Y${NY}M${NM}
      echo "Computing YEAR $NY MONTH $NM"
    fi
#
#  COMPUTE
#
    date
    ${RUNCMD}$CODFILE  ${MODEL}.in > ${MODEL}_${TIME}.out
    date
#
# Test if the month has finised properly
    status=`tail -2 ${MODEL}_${TIME}.out | grep DONE | wc -l`
    if [[ $status != 1 ]]; then
        echo
        echo "Month ${TIME} did not work"
        echo
        exit 1
    fi
#
#  Archive
#
    LEVEL=0
    while [[ $LEVEL != $NLEVEL ]]; do
      if [[ ${LEVEL} == 0 ]]; then
        ENDF=
      else
        ENDF=.${LEVEL}
      fi
      $CP -f ${MODEL}_rst.nc${ENDF} ${INIFILE}${ENDF}
      $MV -f ${MODEL}_his.nc${ENDF} ${MSSOUT}${MODEL}_his_${TIME}.nc${ENDF}
      $MV -f ${MODEL}_rst.nc${ENDF} ${MSSOUT}${MODEL}_rst_${TIME}.nc${ENDF}
      $MV -f ${MODEL}_avg.nc${ENDF} ${MSSOUT}${MODEL}_avg_${TIME}.nc${ENDF}

      LEVEL=$((LEVEL + 1))
    done
    NM=$((NM + 1))
  done
  NY=$((NY + 1))
done
#
#############################################################












