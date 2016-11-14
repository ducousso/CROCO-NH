#!/bin/bash
#SBATCH -J RVTK_REG
#SBATCH -N 1 -n 10 --ntasks-per-node=10
#SBATCH --ntasks-per-core=1
#SBATCH --mem=10000
#SBATCH --mail-type=ALL
#SBATCH --mail-user=gildas.cambon@ird.fr

#----------------------------------------------------------
# Commande on EOS
#================
export OMP_NUM_THREADS=1

##If use of mpiexec
#-----------------
#ExpandNodeList -r -p 1  "$SLURM_NODELIST" > mpd.hosts
#ExpandNodeList -r -p $SLURM_NTASKS_PER_NODE "$SLURM_NODELIST" > nodes
#mpdboot -v -n $SLURM_NNODES -f mpd.hosts -r ssh

##If use srun
##-----------
#export I_MPI_PMI_LIBRARY=/usr/lib64/libpmi.so


#Chargement des modules nÃ©cessaires Ã  l'exÃ©cution du script (et uniquement ces modules)
#--------------------------------------------------------------------------------------

module purge
module load intel/14.0.2.144 intelmpi/4.1.3.049 netcdf/4.3.2 nco/4.4.4 git/2.3.0 
module list

#==
./test_croco.sh REGIONAL
#==

##If use of mpiexec
#mpdallexit
#rm mpd.hosts nodes                                                                                                                                                                                                                                                                                                                                                                
