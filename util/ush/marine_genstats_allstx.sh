#!/bin/bash
#######################################################################
# Name: marine_genstats_allstx.sh  Author: Chris Caruso
# Abstract:  This script produces monthly stats and is sent to
#            Bracknell. Next a copy of this file is made in which 
#            platform names are added. This copy file is then 
#            uploaded to the public web server.  Finally, this 
#            script also produces monthly buoy stats.
#
# HISTORY:   Nov. 2019- migrated to phase3                             
#######################################################################

set -x

##################################################################
# first, produce monthly stats that Steve Lilly sends to Bracknell
##################################################################

wc -l $DATA/archive.sort | awk '{printf "%8s %s\n", $1, $2}' > $DATA/numrep.total.all
grep -i ship $DATA/archive.sort | wc -l | awk '{printf "%8s %s\n", $1, $2}' > $DATA/numrep.ship.all

export pgm=genall
prep_step
startmsg

#input files
export FORT11="$DATA/archive.sort"   
export FORT12="$DATA/numrep.total.all"   
export FORT13="$DATA/numrep.ship.all"   

#output files
export FORT40="$DATA/slp.stats.all"
export FORT41="$DATA/air.stats.all"
export FORT42="$DATA/wdi.stats.all"
export FORT43="$DATA/wsp.stats.all"
export FORT44="$DATA/sst.stats.all"

#run compiled fortran
$EXEChrly/marine_stats_genall > $DATA/out.all

#cat output files together

cat $DATA/slp.stats.all $DATA/air.stats.all > $DATA/tmp1      
cat $DATA/wdi.stats.all $DATA/wsp.stats.all > $DATA/tmp2
cat $DATA/sst.stats.all > $DATA/tmp3       

cat $DATA/tmp1 $DATA/tmp2 $DATA/tmp3 > $DATA/all.stats

########################################################
# copy file to bracknell.stats.  Main script will move
# this file to $MARARCH (/dcom/us007003/marine_archive/$year/$mon)
########################################################

cp $DATA/all.stats bracknell.stats

######################################################################
# Add platform names to file for version that's uploaded to the public
# web server.
######################################################################

export pgm=newstats
prep_step
startmsg

#input files
export FORT11="${VOSarch_IN:-$COMROOTp2/arch/prod/VOS}/ship_names"
export FORT12="$DATA/all.stats"
export FORT20="$DATA/newall.stats"
$EXEChrly/marine_stats_newstats

##################################################################
# now produce monthly buoy stats (step 1 of buoy stat production)
##################################################################
export pgm=genstx
prep_step
startmsg

#input files
export FORT11="$DATA/archive.sort"

#output files
export FORT40="$DATA/newbuoy.stats.tmp"


#run compiled fortran
$EXEChrly/marine_stats_genstx > $DATA/out.stx

##########################################
# sort output file from previous step
############################################
sort -t, -k 2d,2 -k 3d,3 -k 5d,5 $DATA/newbuoy.stats.tmp > $DATA/nsort

################################################################
# cat output file from previous step to end of header file
################################################################
cat $FIXhrly/buoy_hdr_oneline.txt $DATA/nsort > $DATA/newbuoy.stats

###########################################################################
# now continue with script marine_stats_sortrunqm.sh which copies and sorts the file
# /com/hourly/prod/hourly.PDY/sfc_marine.qmarch.prev_mon and then 
# runs marine_stats_eomqm.sh to complete the monthly buoy statistics.
#################################################################################
exit
