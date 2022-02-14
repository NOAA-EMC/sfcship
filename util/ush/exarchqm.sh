#!/bin/bash
#####################################################################
# NAME:   exarchqm.sh           Author: Chris Caruso
# Abstract:  This script dumps surface marine data from the gdas
#            prepbufr file and saves the quality marks.  It runs
#            4 times daily (one for each synoptic period).  At the
#            start of the next month, the output file containing the
#            the archived data is used in computation of the monthly
#            buoy statistics which are forwarded to the Data Buoy
#            Cooperation Panel.
#
#            if archiving t00z data - retrieves same day's 00Z gdas
#            prepbufr file.
#           (current day).  Retrieves 00Z marine data qm's +/- 3 hours.
#
#            if archiving t06z data - retrieves same day's 06Z gdas
#            prepbufr file.
#           (current day).  Retrieves 06Z marine data qm's +/- 3 hours.
#
#            if archiving t12z data - retrieves previous day's 12Z gdas
#            prepbufr file.
#           (yesterday's data).  Retrieves 12Z marine data qm's +/-
#           3 hours.
#
#            if archiving t18z data - retrieves previous day's 18Z gdas
#            prepbufr file.
#           (yesterday's data).  Retrieves 18Z marine data qm's +/-
#           3 hours.
#
# HISTORY:  Jan, 2013- Transitioned to WCOSS (Linux). Changed
#                      all 'XLFUNIT_  ' env vars to 'FORT  '
#           Apr, 2013- Move previous month archive file to
#                      /com/hourly/prod/hourly.PDY
#                      insead of /pcom/hourly.
#           Dec, 2019- Transitioned to Dell p3.
#           Dec, 2020- Modify for GFSv16.
#
# INPUT ARGUMENTS:  $1 - the cycle for which we're archiving
#                   (not the current cycle).  Either t00z,
#                   t06z, t12z, or t18z.  We'll archive 12 hours
#                   after the synoptic period, so t00z data will be
#                   archived at t12z the same day, t06z data archived
#                   at t18z the same day, t12z data archived at t00z
#                   the next day, and t18z data archived at t06z the
#                   next day.  Done to allow ARCHQM to archive the
#                   majority of the data received at NCEP for any one
#                   synoptic period.
#
#####################################################################

set +x

date=$PDY
datem1=$PDYm1
ARCHFILE=sfc_marine.qmarch.curr_mon
#
#  set archive cycle back 12 hours from current cycle (i.e. archive
#  yesterday's 12Z data at 00Z today, etc.)
#
if test $cycle = 't00z'
then
   archcyc='t12z'
   acyc=12
elif test $cycle = 't06z'
then
   archcyc='t18z'
   acyc=18
elif test $cycle = 't12z'
then
   archcyc='t00z'
   acyc=00
elif test $cycle = 't18z'
then
   archcyc='t06z'
   acyc=06
else
   echo ""
   msg="NONfatal error from $job - invalid cycle $cycle invoking exarchqm.sh"
   postmsg "$msg"
   exit
fi

set +x
echo "########################################################"
echo "#  Begin archiving the surface marine data"
echo "########################################################"
set -x

msg="Current cycle is $cycle.  Archive sfc marine quality marks for $archcyc."
postmsg "$msg"

#
#  look for a gdas prepbufr file
#  --------------------------
echo
echo "Looking for a gdas prepbufr file"
if test $archcyc = 't00z'
then
  hour=00
  datey4d=${date}
#
#  check to see if we're archiving t00z of first day of month.
#
  dayofmon=`echo $date | cut -c7-8`
  if test $dayofmon = '01'
  then
#
#   copy qm archive file from last day of previous month and start new 
#   qm archive file for the new (current) month.
#   Archive file from last day of previous month should be located in today's
#   COMIN directory, since 12Z and 18Z archives ran earlier today.  If not,
#   use yesterday's (COMINm1) archive file and make a new, empty archive file
#   in COMIN.
#
    if test $SENDCOM = YES
    then
      if test -f $COMIN/$ARCHFILE
      then
        cp $COMIN/$ARCHFILE $COMIN/sfc_marine.qmarch.prev_mon
        export err=$?
        if test $err -ne 0
        then
          echo "Copy of monthly marine qm archive file failed.  Rerun of this job required."
          msg="Copy of monthly marine qm archive file failed.  Rerun of this job required."
          postmsg "$msg"
          err_chk
         else
           rm -f $COMIN/$ARCHFILE                  
           touch $COMIN/$ARCHFILE                    
         fi
      else
        cp $COMINm1/$ARCHFILE $COMIN/sfc_marine.qmarch.prev_mon
        export err=$?
        if test $err -ne 0
        then
          echo "Copy of monthly marine qm archive file failed.  Rerun of this job required."
          msg="Copy of monthly marine qm archive file failed.  Rerun of this job required."
          postmsg "$msg"
          err_chk
        else
          touch $COMIN/$ARCHFILE                    
        fi
      fi
    fi
  fi
elif test $archcyc = 't06z'
then
  hour=06
  datey4d=${date}
elif test $archcyc = 't12z'
then
  hour=12
  datey4d=${datem1}
elif test $archcyc = 't18z'
then
  hour=18
  datey4d=${datem1}
fi
SPrep=${COMINobsproc_gdas}.$datey4d/${hour}/atmos/gdas.t${hour}z.prepbufr
#
#  check to see if a qm archive file exists for today.  if not, then
#  copy qm archive file from yesterday's /com and append new data to it.
#
if test -f $COMIN/$ARCHFILE
then
   cp $COMIN/$ARCHFILE .
else
   cp $COMINm1/$ARCHFILE .
fi

export pgm=archqm 
prep_step
startmsg

#
#test for input file. run compiled fortran with assigned command line args.
#
if [ -s $SPrep ]
then
   cp $SPrep sprep; echo; echo "Using production gdas prepbufr"; echo

#  input file - gdas prepbufr
#  output file - sfc_marine.qmarch.curr_mon

   export FORT11="sprep"
   export FORT61="$ARCHFILE"

   $EXECgraph/archqm $acyc >> $pgmout 2>errfile
   export err=$?
   if test $err -ne 0 
   then
      msg="NONfatal error from $job - error return status $err from ARCHQM"
      postmsg "$msg"
   else
      if test $SENDCOM = YES
      then
         cp $ARCHFILE $COMOUT
      fi
      msg="archqm for ${datey4d}${acyc} completed normally"
      postmsg "$msg"
   fi

else
   echo; echo "No prepbufr file found!"
   msg="NONfatal error from $job - No prepbufr file found"
   postmsg "$msg"
   exit
fi
