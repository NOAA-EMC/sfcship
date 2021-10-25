####################################################################
# Name:   marine_maine_main_run_stats.sh   Author: Bhavana Rakesh & V. Krishna Kumar
# Abstract:  This script will call a series of util/ush scripts to
#            perform the monthly marine statistics.
#              1) marine_mstats_sortarch.sh
#                   sort the monthly archive file for the
#                   previous month, removing duplicates first, 
#                   cleaning up call signs, and finally doing a
#                   keyed sort so that the output file is sorted 
#                   by report type first, then alphabetically and
#                   by obs date/time within each report type.
#              2) marine_mstats_genstats_allstx.sh
#                   will generate the monthly stats file that is 
#                   used for monthly WMO/NCO verification codes 
#                   along with the monthly buoy stats and copy/post 
#                   the final files to relevant locations on the echo
#                   CCS/web.
#              3) marine_mstats_sortrunqm.sh
#                   will produce the output file "buoy.stats" which
#                   contains the final monthly buoy statistics.
#              4) marine_mstats__akiss.sh
#                   will generate stats for the Gulf of Alaska
#                   (with argument AK) and International SeaKeepers
#                   Society (with argument ISS).
#              5) marine_mstats_sync_sms.sh
#                   will rsync the files to the webserver rzdm.
#              6) marine_stats_sortarch_midmon.sh
#                   runs on the 16h day of the month to calculate
#                   the mid montly counts.  The results are copied
#                   on to the web upload location and rsynced to the 
#                   RZDM webserver.
#
# History: Aug. 2008 - First implementation of this script.
#          Dec. 2019 - Migrate to Phase3.
####################################################################

cd $DATA

########################################
set -x
msg="marine_main_run_stats.sh HAS BEGUN!"
postmsg "$jlogfile" "$msg"
########################################
# Run the following section only for the 1st of the month

if [ $1 -eq '01' ] ; then
# 
#Check to see that the archive files in $COMIN are those for
#the previous month and not for some month before that.
#
    if test -s $COMIN/sfc_marine.qmarch.prev_mon
    then
       tail $COMIN/sfc_marine.qmarch.prev_mon > tail_sfc_marine.qmarch.prev_mon
    else
       msg="$COMIN/sfc_marine.qmarch.prev_mon file not available"
       echo $msg 
       postmsg "$jlogfile" "$msg"
      exit
    fi

    cur_mon=`echo $PDY | cut -c5-6`
    cur_year=`echo $PDY | cut -c3-4` 
    Year=`echo $PDY | cut -c1-4`
    if [ $cur_mon -ne 01 ] ; then
       prev_mon=`expr $cur_mon - 1`
       if [ $prev_mon -le 9 ] ; then
          prev_mon=0$prev_mon 
       fi
    else
#
# To run stats for December $yyyy on January 2nd 
#
       prev_mon=12 
       cur_year=`expr $cur_year - 1`
       Year=`expr $Year - 1`
       if [ $cur_year -lt 10 ] ; then
          cur_year=0$cur_year
       fi
    fi

#
# create lower case month string here
#

mon_string='jan feb mar apr may jun jul aug sep oct nov dec'
lmonth=`echo $mon_string | awk -v lmon=$prev_mon '{ print $lmon }'`

#
# Create the directory for saving the marine stats for all months
#
  mkdir -p $MARARCH/$Year
  export Current_Year=$Year
#
# Checking if the majority of the tailed file dates are for the
# last month
#
    nlin_cnt=0
    cnt_pmon=0
    cnt_ppmon=0
    nlin_las=`cat tail_sfc_marine.qmarch.prev_mon | wc -l`

    while [ "$nlin_cnt" -lt "$nlin_las" ]
    do
          nlin_cnt=`expr $nlin_cnt + 1`
          read x
          pmon_str=`echo $x | awk '{print $2}' | cut -c5-6`

          if [ $pmon_str -eq $prev_mon ] ; then
             cnt_pmon=`expr $cnt_pmon + 1`
          else
             cnt_ppmon=`expr $cnt_ppmon + 1`
          fi
    done < tail_sfc_marine.qmarch.prev_mon
#
# Call marine_stats_sortarch.sh here if date counts are for last month 
#
            if [ $cnt_pmon -gt $cnt_ppmon ] ; then
               echo " Call marine_stats_sortarch.sh after verifying "
               echo " majority of the dates are for last month "
               sh $USHhrly/marine_stats_sortarch.sh
            else
               echo "Exiting because the counts for the majority of "
               echo "the last month dates failed"
               exit
            fi
                
##############################################################
# Call marine_genstats_allstx.sh
##############################################################

if test -s $DATA/archive.sort
    then
    siz_arch_sort=`ls -l $DATA/archive.sort | awk '{print $5}'`
    if [ $siz_arch_sort -gt 0 ] ; then
       echo "call marine_genstats_allstx.sh"
       sh $USHhrly/marine_genstats_allstx.sh
#
       if test "$SENDCOM" = 'YES'
       then
          cal_month=`cal $prev_mon $cur_year | head -1 | awk '{print $1}'`

# create a sub directory for the month in $MARARCH/$Year

          mkdir -p $MARARCH/$Year/${cal_month}
          cp $DATA/bracknell.stats $MARARCH/$Year/${cal_month}/.

###############################################################
#   Copy stats to web upload location here
###############################################################
          mkdir -p $WEBmstats/global/${lmonth}
          cp $DATA/newall.stats $WEBmstats/global/${lmonth}/surf_mar.${lmonth}.stats.txt
	  export err=$?; err_chk
       fi
    else
       msg="archive.sort file size is zero bytes -- exit here"
       echo $msg
       postmsg "$jlogfile" "$msg"
       exit
    fi
else
   msg="archive.sort file is missing -- exit here"
   echo $msg
   postmsg "$jlogfile" "$msg"
   exit
fi

################################################################
#	Sort the archive file which contains quality marks for 
#	the previous month, and remove duplicates and add QM
#	Output = $DATA/buoy.stats
################################################################
       sh $USHhrly/marine_stats_sortrunqm.sh

################################################################
#	Check if the above buoy.stats is not 0 size.  If it is good,
#	email file to buoy-qir@vedur.is with subject as
#	'STAT NCO ALL 2008 10' where 10 is the month number
# The e-mailing of the file will be done by the Data Flow in the
# ncoserver
##################################################################
#
######################################################################
#	produce stats for the Gulf of Alaska
######################################################################

        sh $USHhrly/marine_genstats_akiss.sh AK

	#if the run is complete and then copy file to web upload location
        mkdir -p $WEBmstats/ak/${lmonth}
	cp $DATA/newak.stats  $WEBmstats/ak/${lmonth}/surf_mar.ak.${lmonth}.stats.txt
	export err=$?; err_chk

#####################################################################
#	produce stats for International SeaKeepers Society(ISS)
######################################################################

        sh $USHhrly/marine_genstats_akiss.sh ISS
	
	#if the run is complete and then copy file to web upload location
        mkdir -p $WEBmstats/iss/${lmonth}
        cp $DATA/iss.stats  $WEBmstats/iss/${lmonth}/surf_mar.iss.${lmonth}.stats.txt
	export err=$?; err_chk

######################################################################
#	Upload AK, ISS and global stats to rzdm
#####################################################################

       sh $USHhrly/marine_stats_sync_smstats.sh

fi
exit
	
