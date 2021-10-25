##################################################################
# Name: marine_stats_getrep_iss.sh   Author:
# Abstract:
#
# HISTORY:  NOV. 2019- migrate to phase3
##################################################################

set -x
INFILE=$1
echo "In marine_stats_getrep_iss.sh INFILE =   " $INFILE
grep " KS0" $INFILE > iss_ship1
grep " KS1" $INFILE >> iss_ship1
grep " KS2" $INFILE >> iss_ship1
grep " KS3" $INFILE >> iss_ship1
grep " KS4" $INFILE >> iss_ship1
grep " KS5" $INFILE >> iss_ship1
grep " KS6" $INFILE >> iss_ship1
grep " KS7" $INFILE >> iss_ship1
grep " KS8" $INFILE >> iss_ship1 
grep " KS9" $INFILE >> iss_ship1
grep "ELWX5" $INFILE >> iss_ship1
cat iss_ship1 > iss.ships
