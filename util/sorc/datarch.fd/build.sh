set -ax
module load envvar/1.0
module load PrgEnv-intel/8.1.0
module load craype/2.7.8
module load intel/19.1.3.304
module load cray-mpich/8.1.4

export VERROOT=${VERROOT:-${PACKAGEROOT}/sfcship.${sfcship_ver}/versions}
. ${VERROOT}/build.ver


module load w3nco/${w3nco_ver:?}
module load bufr/${bufr_ver:?}
module load w3emc/${w3emc_ver:?}
module load bacio/${bacio_ver:?}
#module load graphics/2.0.0

module list
make
