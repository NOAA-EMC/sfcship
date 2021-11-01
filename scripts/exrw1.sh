set +x
# Program Name:  EXRW1
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
#
set -xa

cd $DATA   

###############################################
#  Run exrdshpmpc.sh to dump marine data for OPC to quality control
#  note:  this script dumps data for 00Z, 06Z, 12Z, or 18Z synoptic period
#  +/- 3 hours from start of synoptic period.
###############################################
if test ${cyc} -ne '09' -a ${cyc} -ne '21'
then
   $USHgraph/exrdshpmpc.sh $MAPHOUR
fi

##############################################
# archive surface marine data
###############################################

if test ${MAPHOUR} = '00' -o ${MAPHOUR} = '06' -o ${MAPHOUR} = '12' -o ${MAPHOUR} = '18'
then
   sh $USHgraph/exdatarch.sh
   sh $USHgraph/exarchqm.sh
fi

####################################################
# call the master script "marine_run_stats.sh"  at
# 12Z on 1st and 16th day of month.
####################################################

dayofmon=`echo $PDY | cut -c7-8`
if test ${MAPHOUR} = '12'
then
   if [ $dayofmon = '01' ] ; then
        sh $USHgraph/marine_main_run_stats.sh $dayofmon
   fi
fi

###############################################
msg='JOB COMPLETED NORMALLY'
postmsg "$jlogfile" "$msg"
###############################################

set +x
echo " ***** RW1 PROCESSING COMPLETED NORMALLY *****"
echo " ***** RW1 PROCESSING COMPLETED NORMALLY *****"
echo " ***** RW1 PROCESSING COMPLETED NORMALLY *****"
set -xa

