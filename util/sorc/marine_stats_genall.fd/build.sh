set -ax

export VERROOT=${VERROOT:-${PACKAGEROOT}/sfcship.${sfcship_ver}/versions}
. ${VERROOT}/build.ver

module load w3nco/${w3nco_ver:?}

module list
make
