####################################################################
# Name: exdatarch.sh             Author:  Chris Caruso
# Abstract:  This script dumps surface marine data and compares that
#            data to first guess fields.  Data and differences between
#            data and first guess are archived 4 times daily.  At the
#            start of the next month, the output file containing the
#            archived data is used in computation of 2 sets of monthly
#             marine statistics (one for Bracknell, England and the NCO
#            web site; the other for the Data Buoy Cooperation Panel).
#            Note that this script archives data from 12 hours earlier.
#
#            If archiving t00z data - retrieves previous day's 18Z gdas
#            + 6 as primary (current day) or previous day's 18Z gfs
#            + 06 as backup. Uses previous day's 18Z gdas + 6 sfc temp
#            (skin temp) for sst field.  Retrieves 00Z marine data +/-
#            3 hours.
#
#            If archiving t06z data - retrieves same day's 00Z gdas +
#            6 as primary (current day) or same day's 00Z gfs + 06 as
#            backup.  Uses same day's 00Z gdas + 6 sfc temp (skin
#            temp) for sst field.  Retrieves 06Z marine data +/- 3
#            hours.
#
#            If archiving t12z data - retrieves same day's 06Z gdas +
#            6 as primary (yesterday's data) or same day's 06Z gfs +
#            06 as backup.  Uses same day's 06Z gdas + 6 sfc temp
#            (skin temp) for sst field.  Retrieves 12Z marine data
#            +/- 3 hours.
#
#            If archiving t18z data - retrieves same day's 12Z gdas +
#            6 as primary (yesterday's data) or same day's 12Z gfs +
#            06 as backup.  Uses same day's 12Z gdas + 6 sfc temp
#            (skin temp) for sst field.  Retrieves 18Z marine data
#            +/- 3 hours.
#
# History: Aug. 16, 2000. Add sfc marine datatype tideg to list
#                         of those to be archived.
#          Jan,     2013  Transitioned to WCOSS (Linux). Changed
#                         all 'XLFUNIT_  ' env vars to 'FORT  '
#          April,   2013  Move previous month archive file to
#                         /com/hourly/prod/hourly.PDY
#                         instead of /pcom/hourly.
#          Dec,     2019  Transitioned to Dell p3.
#          Dec,     2020  Modify for GFSv16
#
# Input Arguments:  $1 - the cycle for which we're archiving
#                   (not the current cycle).  Either t00z, t06z,
#                   t12z, or t18z.  We'll archive 12 hours after
#                   the synoptic period.
#
#                   t00z data will be archived at t12z the same day.
#
#                   t06z data archived at t18z the same day.
#
#                   t12z data archived at t00z the next day.
#
#                   t18z data archived at t06z the next day.
#
#                   Done to allow DATARCH to archive the majority
#                   of the data received at NCEP for any one synoptic
#                   period.
#
####################################################################

set +x

date=$PDY
datem1=$PDYm1
ARCHFILE=sfc_marine.archive.curr_mon
#
#  set archive cycle back 12 hours from current cycle (i.e. archive
#  yesterday's 12Z data at 00Z today, etc.)
#
if test $cycle = 't00z'
then
   archcycle='t12z'
   acyc=12
elif test $cycle = 't06z'
then
   archcycle='t18z'
   acyc=18
elif test $cycle = 't12z'
then
   archcycle='t00z'
   acyc=00
elif test $cycle = 't18z'
then
   archcycle='t06z'
   acyc=06
else
   echo ""
   msg="NONfatal error from $job - invalid cycle $cycle invoking exdatarch.sh"
   postmsg "$jlogfile" "$msg"
   exit
fi

set +x
echo "########################################################"
echo "#  Begin archiving the surface marine data"
echo "########################################################"
set -x

msg="Current cycle is $cycle.  Archive surface marine data for $archcycle."
postmsg "$jlogfile" "$msg"

#
#  look for a guess file
#  ---------------------
echo
echo "Looking for a guess file"
if test $archcycle = 't00z'
then
  hrm6=18
  dt06=${datem1}
  dumpdat=${date}
#
#  check to see if we're archiving t00z of first day of month.
#
  dayofmon=`echo $date | cut -c7-8`
  if test $dayofmon = '01'
  then
#
#   copy archive file from last day of previous month and start new 
#   archive file for the new (current) month.
#   Archive file from last day of previous month should be located in today's
#   COMIN directory, since 12Z and 18Z archives ran earlier today.  If not,
#   use yesterday's (COMINm1) archive file and make a new, empty archive file
#   in COMIN.
#
    if test $SENDCOM = YES
    then
      if test -f $COMIN/$ARCHFILE
      then
        cp $COMIN/$ARCHFILE $COMIN/sfc_marine.archive.prev_mon
        export err=$?
        if test $err -ne 0
        then
          echo "Copy of monthly marine archive file failed. Rerun of this job required."
          msg="Copy of monthly marine archive file failed. Rerun of this job required."
          postmsg "$jlogfile" "$msg"
          err_chk
        else
          rm -f $COMIN/$ARCHFILE                  
          touch $COMIN/$ARCHFILE                    
        fi
      else
        cp $COMINm1/$ARCHFILE $COMIN/sfc_marine.archive.prev_mon
        export err=$?
        if test $err -ne 0
        then
          echo "Copy of monthly marine archive file failed. Rerun of this job required."
          msg="Copy of monthly marine archive file failed. Rerun of this job required."
          postmsg "$jlogfile" "$msg"
          err_chk
        else
          touch $COMIN/$ARCHFILE                    
        fi
      fi
    fi
  fi
elif test $archcycle = 't06z'
then
  hrm6=00
  dt06=${date}
  dumpdat=${date}
elif test $archcycle = 't12z'
then
  hrm6=06
  dt06=${datem1}
  dumpdat=${datem1}
elif test $archcycle = 't18z'
then
  hrm6=12
  dt06=${datem1}
  dumpdat=${datem1}
fi

SG06_grib2=${COMINgdas}.$dt06/${hrm6}/atmos/gdas.t${hrm6}z.pgrb2.1p00.f006
SG06b_grib2=${COMINgfs}.$dt06/${hrm6}/atmos/gfs.t${hrm6}z.pgrb2.1p00.f006

# create grib1 on pgb 
# assign SG06 to newly generated grib1 file
$CNVGRIB -g21 ${SG06_grib2} ${DATA}/gdas.t${hrm6}z.pgrbf06
SG06=${DATA}/gdas.t${hrm6}z.pgrbf06

$CNVGRIB -g21 ${SG06b_grib2} ${DATA}/gfs.t${hrm6}z.pgrbf06
SG06b=${DATA}/gfs.t${hrm6}z.pgrbf06

#
export dump=bufr
#export BACK=on
export loud=on
#
$DUMPJB ${dumpdat}${acyc} 3 ships
$DUMPJB ${dumpdat}${acyc} 3 dbuoy
$DUMPJB ${dumpdat}${acyc} 3 mbuoy
$DUMPJB ${dumpdat}${acyc} 3 lcman
$DUMPJB ${dumpdat}${acyc} 3 tideg
#
#  check to see if an archive file exists for today.  if not, then
#  copy archive file from yesterday's /com and append new data to it.

if test -f $COMIN/$ARCHFILE
then
   cp $COMIN/$ARCHFILE .
else
   cp $COMINm1/$ARCHFILE .
fi

export pgm=datarch
prep_step
startmsg

# input files - marine data
export FORT20="ships.ibm"
export FORT21="dbuoy.ibm"
export FORT22="mbuoy.ibm"
export FORT23="lcman.ibm"
export FORT24="tideg.ibm"
#
#output files
export FORT41="$ARCHFILE"
#

#test for input files. run compiled fortran with assigned command line args.
if [ -s $SG06 ]
then
   cp $SG06 fcst06; echo; echo "Using $SG06"; echo
   cp $SG06b fcst06b; echo; echo "Using $SG06b as backup"; echo
   export FORT11="fcst06"
   export FORT12="fcst06b"
#
#check for index files.  if not present, make them.
# For GFSv15 grib1 index files not created, do so now
#
   $GRBINDEX $SG06 fcst06i; echo "making index file"
   export FORT31="fcst06i"
   $GRBINDEX $SG06b fcst06bi; echo "making index file"
   export FORT32="fcst06bi"

   $EXECgraph/datarch $acyc GDAS >> $pgmout 2>errfile
   export err=$?
   if test $err -ne 0 
   then
      msg="NONfatal error from $job - error return status $err from DATARCH"
      postmsg "$jlogfile" "$msg"
   else
      if test $SENDCOM = YES
      then
         cp $ARCHFILE $COMOUT
      fi
      msg="datarch for ${dumpdat}${acyc} completed normally"
      postmsg "$jlogfile" "$msg"
   fi

elif [ -s $SG06b ]
then
   echo "primary first guess not found..."
   cp $SG06b fcst06b; echo; echo "Using $SG06b as backup"; echo
   $GRBINDEX $SG06b fcst06bi; echo "making index file"
   export FORT32="fcst06bi"

   $EXECgraph/datarch $acyc GFS >> $pgmout 2> errfile
   export err=$?
   if test $err -ne 0 
   then
      msg="NONfatal error from $job - error return status $err from DATARCH"
      postmsg "$jlogfile" "$msg"
   else
      if test $SENDCOM = YES
      then
         cp $ARCHFILE $COMOUT
      fi
      msg="datarch for ${dumpdat}${acyc} completed normally"
      postmsg "$jlogfile" "$msg"
   fi


else
   echo; echo "No guess file found!"
   msg="NONfatal error from $job - No guess file found"
   postmsg "$jlogfile" "$msg"
   exit
fi
