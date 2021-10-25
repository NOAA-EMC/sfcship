################################################################
# Name:  marine_stats_eomqm.sh   Author:
# Abstract:  This script adds current month's data qm's to buoy
#            stats.  NOTE: you must first remove duplicates from
#            archqm.dat, then sort archqm.dat by callsign first,
#            date second.  You also must sort newbuoy.stats.tst
#            first using the following:
#               sort -t, -k 2d,2 -k 3d,3 -k 5d,5
#            before running this script.
# 
# HISTORY:  NOV. 2019- migrate to Phase3
#################################################################

set -x

QMFILE=$DATA/qmarch.sort
STATFILE=$DATA/newbuoy.stats
OUTFILE1=$DATA/final.newbuoy.stats.temp1
OUTFILE2=$DATA/final.newbuoy.stats.temp2
OUTFILE3=$DATA/final.newbuoy.stats
 
export pgm=eomqm
prep_step
startmsg

#
#input files

export FORT12="$STATFILE"
export FORT13="$QMFILE"

#
#output files

export FORT51="$OUTFILE1"
export FORT52="$OUTFILE2"

#
#run compiled fortran

$EXEChrly/marine_stats_eomqm
err=$?; export err; #err_chk
echo $err
 
#cat the temp output files together
cat $OUTFILE1 $OUTFILE2 > $OUTFILE3

#
# cat the final output file to the end of the buoy header txt file.

cat $FIXhrly/buoy_header.txt $OUTFILE3 > $DATA/buoy.stats

#
# Send the buoy.stats to NCOSRV for e-mail-ing the buoy.stats file
# as an attachment to the end users.
#
 if [ $SENDCOM = "YES" ] ; then

   mv $DATA/buoy.stats $MARARCH/.

   if [ $SENDDBN = "YES" ] ; then
       $DBNROOT/bin/dbn_alert TEXT BUOY_STATS $job $MARARCH/buoy.stats
   fi

 fi

exit
#
      
