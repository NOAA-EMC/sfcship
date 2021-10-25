##################################################################
# Name: marine_stats_sortrunqm.sh   Author:
# Abstract:  This script remove dups from QM archive file and do
#            a keyed sort so that the final file is sorted
#            alphabetically, then by obs date time.
#            This script also run the final step which adds
#            the QM stats to the monthly buoy stats.
#
# HISTORY:  Dec.- 2019 migrate to phase3
##################################################################

set -x

###############################################################
# remove dups from QM archive file and do a keyed
# sort so final file is sorted alphabetically, then by obs
# date time.
##############################################################

origfile=$COMIN/sfc_marine.qmarch.prev_mon    
tempoutfile=$DATA/qmarch.uniq
outfile=$DATA/qmarch.sort

sort -T $DATA -u $origfile > $tempoutfile
sort -T $DATA -k 1d,1 -k 2n,2 $tempoutfile > $outfile

###############################################################
# run the final step which adds the QM stats to the monthly buoy
# stats.
###############################################################

$USHhrly/marine_stats_eomqm.sh > $DATA/outqm
exit
