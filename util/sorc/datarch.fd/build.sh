set -ax
module load envvar/1.0
module load PrgEnv-intel/8.1.0
module load craype/2.7.8
module load intel/19.1.3.304
module load cray-mpich/8.1.4
#module load graphics/2.0.0
module load bufr/11.4.0
module load w3nco/2.4.1
module load bacio/2.4.1
module load w3emc/2.7.3
module list
make
