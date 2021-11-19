#!/bin/bash
######################################################################
echo "----------------------------------------------------------------------  "
echo "exship_names.sh.ecf- This script copies an updated VOS ship list from   "
echo "                     NDBC and merges the file with a copy of the previous"
echo                       ship_names list for the marine monthly statistics  "
echo                       processing on the 1st day of month                 "
#  -------------------------------------------------------------------------

echo "------------------------------------------------------------------------"
echo "History:                                                                "
echo "  Jul 16 2003 - Original script - D. Keyser from exglobal_prep_post.sh.ecf"
echo "  Dec 12 2019 - Added for the sfcship (raws) transition to Dell         "
###############################################################################

# NOTE: NET is gfs for the gdas RUN (as for the gfs RUN)
# -------------------------------------------------------

set -aux

# Make sure we are in the $DATA directory
cd $DATA

msg=" processing of SHIP_NAMES file has begun on `hostname`"
postmsg "$msg"
 
# -----------------------------------------------------------------------------
# PROCESS_MASTER_SHIP_STNLST can only be YES  (where default is YES)
# -----------------------------------------------------------------------------
PROCESS_MASTER_SHIP_STNLST=${PROCESS_MASTER_SHIP_STNLST:-YES}
if [ "$PROCESS_MASTER_SHIP_STNLST" = 'YES' ]; then
 
   msg="UPDATE MASTER SHIP STATION LIST FOR $PDY"
   postmsg "$msg"
   set +x
   echo
   echo "$msg"
   echo
   set -x

#############################################################################
#  Update the "Master Ship Station List" based on any information read from
#   the updated VOS ship list from NDBC
#############################################################################

   ${USHsfcship}/mstr_shp_stn_lst_update.sh

fi # test for PROCESS_MASTER_SHIP_STNLST=YES

#######################

# GOOD RUN
set +x
echo " "
echo " ****** PROCESSING COMPLETED NORMALLY"
echo " ****** PROCESSING COMPLETED NORMALLY"
echo " ****** PROCESSING COMPLETED NORMALLY"
echo " ****** PROCESSING COMPLETED NORMALLY"
echo " "
set -x
msg='ENDED NORMALLY.'
postmsg "$msg"
################## END OF SCRIPT #######################
