#!/bin/bash
set +x
# Program Name:  EXRW2
# Author(s)/Contact(s):  Steven Lilly
# Abstract:  Dump marine data for OPC to quality control
# History Log:
#     10/2019: First implementation of this script on Phase3
#
# Usage:
#   Parameters: None
#   Input Files:
#     shpall - group 001.001
#     dbuoy - group 001.002
#     mbuoy - group 001.003
#     lcman - group 001.004
#     tideg - group 001.005
#
#   Output Files:
#     sfcship.${job}.${cyc}
#
#   Condition codes:
#
#   User controllable options: None
set -xa

cd $DATA
 
set +x
echo '------------------------------------------------------'
echo "              IBM $envir PROCESSING                "
echo "      `date`      "
echo "     JOB WWRW2$HOUR   CYCLE TIME IS  ... $cycle"
echo ' '
echo ' $VARIABLE info for this execution:'
echo " Processing executable environment is ............ $envir"
echo " Temporary processing file directory is .......... $DATA"
echo " unique machine processing id is ................. $pid"
echo "________________________________________________"
set -xa
 
# mod the cycle time by 12
MOD12=`expr $MAPHOUR % 12`

if [ $MOD12 -eq 0 ]
then

###########################################
#  Run exrdshpmpc.sh to dump marine data for MPC to quality control
#  note:  this script dumps data for 00Z, 06Z, 12Z, or 18Z synoptic period
#  +/- 3 hours from start of synoptic period.
###########################################
  $USHgraph/exrdshpmpc.sh $MAPHOUR
fi

###########################################
msg='JOB COMPLETED NORMALLY'
postmsg "$msg"
###########################################

set +x
echo " ***** RW2 PROCESSING COMPLETED NORMALLY *****"
echo " ***** RW2 PROCESSING COMPLETED NORMALLY *****"
echo " ***** RW2 PROCESSING COMPLETED NORMALLY *****"
set -xa

