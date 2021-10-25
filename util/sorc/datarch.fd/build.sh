set -ax
module load ips/18.0.1.163  impi/18.0.1
#module load ips/18.0.1.163  smpi/10.1.1.0
module load graphics/2.0.0
module load bufr/11.3.0
module load w3nco/2.0.6
module load bacio/2.0.2
module use /gpfs/dell1/nco/ops/nwprod/modulefiles/mpi_prod/ips/18.0.1/impi/18.0.1/            
#module use /gpfs/dell1/nco/ops/nwprod/modulefiles/mpi_prod/ips/18.0.1/impi/10.1.1.0/            
module load w3emc/2.3.0
module list
make
