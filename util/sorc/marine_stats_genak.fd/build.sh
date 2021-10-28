set -ax
export VERROOT=/lfs/h2/emc/obsproc/noscrub/Steve.Stegall/githubwkspc/sfcship/versions
. ${VERROOT}/build.ver

module load w3nco/${w3nco_ver:?}

module list
make
