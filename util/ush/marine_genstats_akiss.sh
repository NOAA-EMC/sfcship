#!/bin/bash
####################################################################
# Name: marine_genstats_akiss.sh   Author:
# Abstract:
#
# HISTORY:  NOV. 2019- migrate to phase3
####################################################################

set -x
#
REGION=$1
if [ "$REGION" = AK ] 
then
  $USHhrly/marine_stats_getrep_ak.sh $DATA/archive.sort
  cp $DATA/ak.ships $DATA/ships1
  EXEC2=$EXEChrly/marine_stats_genak
  prog=ak
  wc -l $DATA/ships1 | awk '{printf "%8s %s\n", $1, $2}' > $DATA/numrep.$prog
elif [ "$REGION" = ISS ]
then
  $USHhrly/marine_stats_getrep_iss.sh $DATA/archive.sort
  cp $DATA/iss.ships $DATA/ships1
  EXEC2=$EXEChrly/marine_stats_geniss
  prog=iss
  wc -l $DATA/ships1 | awk '{printf "%8s %s\n", $1, $2}' > $DATA/numrep.$prog
fi

#
export pgm=$EXEC2
prep_step
startmsg
#
#input files
export FORT11="$DATA/ships1"   
export FORT12="$DATA/numrep.$prog"   
#
#output files
export FORT40="$DATA/slp.stats.$prog"
export FORT41="$DATA/air.stats.$prog"
export FORT42="$DATA/wdi.stats.$prog"
export FORT43="$DATA/wsp.stats.$prog"
export FORT44="$DATA/sst.stats.$prog"
#
#run compiled fortran
#
$EXEC2 > $DATA/out.$prog
#
#cat output files together
cat $DATA/slp.stats.$prog $DATA/air.stats.$prog > $DATA/tmp1                       
cat $DATA/wdi.stats.$prog $DATA/wsp.stats.$prog > $DATA/tmp2
cat $DATA/sst.stats.$prog > $DATA/tmp3       

cat $DATA/tmp1 $DATA/tmp2 $DATA/tmp3 > $DATA/$prog.stats

if [ "$REGION" = AK ] 
then
export pgm=newstats
prep_step
startmsg
#
#input files
export FORTTS="unit_vars=yes"     # Allow overriding default names.

export FORT11="${VOSarch_IN:-${COMROOT?}/${NET}/${envir}/VOS}/ship_names"
export FORT12="$prog.stats"
export FORT20="new$prog.stats"
$EXEChrly/marine_stats_newstats

fi
#
exit
#

