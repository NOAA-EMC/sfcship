#######################################################################
# Name:  marine_stats_sortarch.sh   Author:  Bhavana Rakesh
# Abstract: This script remove dups from archive file, repove slashes
#           in callsigns, and do a keyed sort so final file is sorted
#           by data type, then alphabetically, then by obs date time
#
# History:    Unknown - First implementation of this script.
#           Dec. 2019 - Migrate to Phase3.
#######################################################################

set -x
msg="marine_stats_sortarch.sh has begun"
#
# remove dups from archive file, remove slashes in callsigns, and do a keyed
# sort so final file is sorted by data type, then alphabetically, then by obs
# date time.
#
 echo "com directory  in  " $COMIN 
 echo "DATA directory  in  " $DATA 
 echo "current directory " `pwd`
 if test -s $COMIN/sfc_marine.archive.prev_mon
    then
       origfile=$COMIN/sfc_marine.archive.prev_mon
       tempoutfile=archive.uniq
       rmslashout=noslash.out
       outfile=archive.sort
 
       sort -T $DATA -u $origfile > $tempoutfile

       IN_rmslash=$tempoutfile
       export pgm=rmslash
       prep_step
       startmsg
       export FORT10="$IN_rmslash"
       export FORT51="$rmslashout"
       $EXEChrly/marine_stats_rmslash
 
       sort -T $DATA -k 2n,2 -k 1d,1 -k 5n,5 $rmslashout > $outfile
 
    else
       msg="$COMIN/sfc_marine.archive.prev_mon file not available"
       echo $msg
       postmsg "$jlogfile" "$msg"
      exit
    fi
exit
