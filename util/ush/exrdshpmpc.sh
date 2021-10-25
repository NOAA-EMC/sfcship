####################################################################
# Name:  exrdshpmpc.sh          Author:  Chris Caruso
# Abstract: This script dumps the marine data for +/- 3 hours from
#           the start of each synoptic period, writes the data into
#           an ascii file, and sends an alert to DBNet to have the
#           data downloaded to the MPC HP's for quality control via 
#           the new UNIX version of QUIPS (the interactive QC software).
#
# History: Feb. 17, 1999 - First implementation of this new script.
#          May 17, 1999  - Fixed a problem with setting of SYNOPHR (the
#                          expression I was using only gave back a 1 digit
#                          answer if answer was lt 10.  Then this single
#                          digit was being catted onto the yyyymmdd for
#                          dumpjb, which doesn't know what to do with the
#                          single digit hour!).  Hardcoded in new value
#                          of SYNOPHR to solve this problem so data will
#                          be dumped for all runs.
#          Aug 16, 2000  - Modifying to dump new tide gauge data.      
#          Apr 25, 2012  - change to use 'shpall' instead of 'ships'
#                          to account for split ship tanks.
#         Jan, 2013      - Transitioned to WCOSS (Linux). Changed
#                          all 'XLFUNIT_  ' env vars to 'FORT  '
#         Oct, 2019      - Migrate to Phase3.
#####################################################################

cd $DATA
MAPHR=$1

##########################################
#
# START FLOW OF CONTROL
#
# 1)  Dump bufr marine data (ships, drifting buoys, moored buoys, 
#     CMAN, and tide gauge data).
#
# 2)  Execute rdshpmpc.x to retrieve selected parameters and write to
#     and ascii output file.
#
# 3)  Alert DBNet that output ascii file is ready to go to the MPC
#     HP workstations.
#
##########################################

set -xa
msg="Script exrdshpmpc.sh starting."
postmsg "$jlogfile" "$msg"

########################################
#
#  check value of MAPHR passed in from exrw scripts.
#  if MAPHR is 03, 09, 15, or 21, then subtract 3 from it
#  and pass that into rdshpmpc.x on command line for synoptic
#  period we're currently working on.
#
########################################

if test ${MAPHR} -eq '03' 
then
  SYNOPHR=00
elif test ${MAPHR} -eq '09'
then
  SYNOPHR=06
elif test ${MAPHR} -eq '15'
then
  SYNOPHR=12
elif test ${MAPHR} -eq '21'
then
  SYNOPHR=18
else
  SYNOPHR=$MAPHR
fi

#
# set up output file name
#

ZHOUR=`date -u +%H`

OUTFILE=sfcship.${job}.${cyc}  # e.g. sfcship.jrw001.12

#  check to see if we're still in the same synoptic period for the
#  data we want to dump.  If not, exit (i.e. if clock time is after
#  1800Z and this script is trying to dump data valid for the 1200Z
#  synoptic period, then exit from this script).  Running this script
#  under that circumstance would cause problems with Paula Freeman's
#  QC software (e.g. could cause 1200Z data to be compared to a fcst
#  valid at 1800Z!!!).
#

if test ${SYNOPHR} -ne '18'
then
  toolate=`expr $SYNOPHR + 6`

  if test ${ZHOUR} -ge ${toolate} -o ${ZHOUR} -lt ${SYNOPHR}
  then
    msg1="RUNNING IN NEW SYNOPTIC PERIOD - DO NOT DUMP DATA FOR"
    msg2="EARLIER SYNOPTIC PERIOD -- ABORTING script exrdshpmpc"
    postmsg "$jlogfile" "$msg1"
    postmsg "$jlogfile" "$msg2"
    exit  
  fi
else
  toolate=00
  echo $toolate
  if test ${ZHOUR} -ge ${toolate} -a ${ZHOUR} -lt ${SYNOPHR}
  then
    msg1="RUNNING IN NEW SYNOPTIC PERIOD - DO NOT DUMP DATA FOR"
    msg2="EARLIER SYNOPTIC PERIOD -- ABORTING script exrdshpmpc"
    postmsg "$jlogfile" "$msg1"
    postmsg "$jlogfile" "$msg2"
    exit  
  fi
fi

export FORM=${PDY}${SYNOPHR}

ix=0

for TYPE in shpall dbuoy mbuoy lcman tideg
do
   case $TYPE in
     shpall)group="001.001";;
     dbuoy)group="001.002";;
     mbuoy)group="001.003";;
     lcman)group="001.004";;
     tideg)group="001.005";;
   esac

   $DUMPJB ${PDY}${SYNOPHR} 3 $TYPE
   err=$?
   if [ "$err" -eq 0 ]
   then
      ix=`expr $ix + 1`
      count=`grep "$group  HAS" $TYPE.out | awk '{print $3}'`
      msg="Dumped ${count} $TYPE reports for ${PDY}${SYNOPHR}."
      postmsg  "$jlogfile" "$msg"
      if [ "$ix" -eq 1 ]
      then
        count=`grep "001.013  HAS" $TYPE.out | awk '{print $3}'`
        msg="Dumped ${count} $TYPE reports for ${PDY}${SYNOPHR}."
        postmsg  "$jlogfile" "$msg"
      fi
   else
      msg="No $TYPE data for ${PDY}${SYNOPHR}!"
      postmsg  "$jlogfile" "$msg"
   fi
done

if [ "$ix" -eq '0' ]
then
   msg="ERROR NO DATA TO DUMP TO MPC FOR QC - ABORTING script exrdshpmpc"
   postmsg "$jlogfile" "$msg"
   exit
fi

export pgm="hrly_rdshpmpc"
prep_step

export FORT11="shpall.${PDY}${SYNOPHR}"
export FORT12="dbuoy.${PDY}${SYNOPHR}"
export FORT13="mbuoy.${PDY}${SYNOPHR}"
export FORT14="lcman.${PDY}${SYNOPHR}"
export FORT15="tideg.${PDY}${SYNOPHR}"
export FORT20="sfcshp.dat"

startmsg
$EXECsfcship/hrly_rdshpmpc $SYNOPHR >> $pgmout 2>errfile
export err=$?; err_chk

if [ "$err" -ne 0 ]
then
   msg7="NO SURFACE MARINE DATA WRITTEN FOR HP QC... error = $err"
   postmsg "$jlogfile" "$msg7"
else
   if test "$SENDCOM" = 'YES'
   then
#     cp sfcshp.dat $comout/$OUTFILE
      cp sfcshp.dat $COMOUT/$OUTFILE
      if test "$SENDDBN" = 'YES'
      then
         $DBNROOT/bin/dbn_alert QAP sfc_obs $job $COMOUT/$OUTFILE
      fi
   fi 
fi

msg='Script exrdshpmpc.sh completed Normally.'
postmsg "$jlogfile" "$msg"

# ---------------END OF EXRDSHPMPC SCRIPT ----------------------------
