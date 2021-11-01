set -ax

export VERROOT=${VERROOT:-${PACKAGEROOT}/sfcship.${sfcship_ver}/versions}
. ${VERROOT}/build.ver


module load w3nco/${w3nco_ver:?}
module load bufr/${bufr_ver:?}
module load w3emc/${w3emc_ver:?}
module load bacio/${bacio_ver:?}
#module load graphics/2.0.0

module list
make
