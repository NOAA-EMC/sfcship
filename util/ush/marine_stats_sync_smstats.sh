#!/bin/ksh
set -x
#######################################
# Description:  Rsync files from a source directory to a destination
#               directory.
#
# Programmer        Date         Description
# ----------------  -----------  ----------------------------------
# Ricardo Romero    19-Jul-2002  Initial Version.
# SPA SH            11-Dec-2019  Tranition to Dell
#######################################

#######################################
# Begin user configuration section.
#######################################

# Date command. Used for log file entries.
ZDATE="date -u"

# The ssh program.
SSH="/usr/bin/ssh"

# The rsync program.
RSYNC="/usr/bin/rsync"


# The rsync program options.
# -v               # Increase verbosity. Can specify up to three -v.
# --recursive      # Recursively copy the source location
# --links          # Copy symlinks as symlinks
# --perms          # Preserve permissions
# --times          # Preserve timestamps
# --whole-file     # Transfer the entire file instead of partial chunks
# --rsh=$SSH       # Specify rsh replacement
# --stats          # Give some file transfer stats
# --delete         # Delete files on the destination that are no longer on the source
# --progress       # Show progress during transfer
# Other options are available. See the rsync web site at www.rsync.org.
RSYNC_OPTS="
-v \
--progress \
--recursive \
--links \
--perms \
--times \
--whole-file \
--rsh=$SSH \
--delete \
--stats
"

# The user and remote system for syncing.
# 1) Replace my_login_id with the login id to use on the remote system
# 2) Replace remote_system with the IP address or DNS name of system
#    you are logging into.
### production change here #### REMOTEUSER="$remuser@ncorzdm"
REMOTEUSER=${REMOTEUSER:-"nwprod@ncorzdm.ncep.noaa.gov"}


# Source directory.
# 1) Replace "/my/source/directory/" with the source directory you will
#    be rsyncing from.
SRC_DIR=${SRC_DIR:-"$COMROOT/${NET}/prod/smstats"}

# Destination directory.
# 1) Replace "/my/destination/directory" with the destination directory
#    you will be syncing to.
DEST_DIR=${DEST_DIR:-"/home/www/nco/htdocs/pmb/qap"}

# Specify the source and destination for syncing.

# Uncomment the following 2 lines if the source
# location is on a remote system and the destination
# location is the local system.
#SRC="${REMOTEUSER}:${SRC_DIR}"
#DEST="${DEST_DIR}"

# otherwise, uncomment the following 2 lines if the
# source location is on the local system and the
# destination location is a remote system.
SRC="${SRC_DIR}"
DEST="${REMOTEUSER}:${DEST_DIR}"

# Directory to store log file.  This directory will be created
# if it doesn't already exist.
# 1) Replace "$HOME/log" with your log file directory.
LOGDIR=${LOGDIR:-"$COMROOT/logs/prod"}
#LOGDIR=$TEMP

# Maximum number of log files to keep. This will keep
# MAXLOGS + 1 number of log files, since it uses the
# "find" command with the "-mtime +${MAXLOGS}" parameter.
MAXLOGS=3

# Command to produce a process listing on the system
# this script is running on. One of these should produce
# a listing where the process id (PID) of this script is shown.
#PS="ps -auwx"
PS="ps -ef"

# End user configuration section.
#######################################

#######################################
# Begin program configuration section.
# There are no user configurable options here.

# Fully qualified name of this script.
MYFULLNAME=$0

# Base name of this script.
MYNAME=`basename $0`

# Process id of this script.
MYPID=$$

# Today's date. Used in creating log file name date string.
TODAY="`${ZDATE} +%Y%m%d`"

# Log file name.
LOG_PREFIX="${MYNAME}.log."
LOGFILE="${LOGDIR}/${LOG_PREFIX}${TODAY}"

# Location of the lock file that shows this script is
# already running.
LOCKFILE="${MYFULLNAME}.LCK"
#LOCKFILE=${TEMP}/${MYNAME}.LCK

# End program configuration section.
#######################################

#######################################
# Begin functions section.

# Check for log directory. Create if necessary.
check_log_dir()
{
  # Create the logfile directory if it doesn't already exist.
  if [ ! -d $LOGDIR ]; then
    mkdir -p $LOGDIR
    if [ $? -eq 0 ]; then
      log_it "Successfully created directory $LOGDIR."
    else
      log_it "Error creating directory $LOGDIR."
      close_and_exit 20
    fi
  fi
}

# Log script messages.
log_it()
{
  MESSAGE="$1"
  echo "[`${ZDATE}`] $MESSAGE" >>$LOGFILE
}

# Check for lock file as indicator that another instance
# of this script is already running.
check_lock ()
{
  # See if we are currently running by checking for the lock file.
  if [ -e $LOCKFILE ]; then
    log_it "Lockfile exists.  Checking to see if $MYNAME is really"
    log_it "running, or if it died prematurely."

    running_pid=`cat $LOCKFILE`
    is_running=`$PS | grep $running_pid | grep -v grep | wc -l`
    if [ $is_running -ge 1 ]; then
      log_it "$MYNAME is already running."
      close_and_exit 1
    else
      log_it "$MYNAME does not appear to be running.  Removing $LOCKFILE."
      remove_lock
    fi
  fi
}

# Create the script lock file for this instance.
create_lock()
{
  # Create the lock file, by placing our pid into it.
  echo "$MYPID" > $LOCKFILE
  if [ $? -eq 0 ]; then
    log_it "Successfully created lock file $LOCKFILE."
  else
    log_it "Problems creating lock file $LOCKFILE.  Cannot continue."
    close_and_exit 2
  fi
}

# Remove the lock file.
remove_lock()
{
  if [ -e $LOCKFILE ]; then
    rm -f $LOCKFILE
    if [ $? -eq 0 ]; then
      log_it "Lock file $LOCKFILE successfully removed."
    else
      log_it "Error removing lock file $LOCKFILE."
    fi
  else
    log_it "Lock file $LOCKFILE not found.  Cannot remove."
  fi
}

# Log exit status and exit script.
close_and_exit()
{

  err_num=$1
  if [ $err_num -gt 0 ]; then
    log_it "Exit status = $err-Num."
  fi

  remove_lock

  log_it "Finished $MYNAME."

  exit
}

# End functions section.
#######################################

# Make sure log directory exists.
check_log_dir

log_it "Starting $MYNAME."

# See if we are currently running by checking for the lock file.
check_lock

# Create the lock file, by placing our pid into it.
create_lock

log_it "Checking to see if we can log in to $REMOTEUSER ..."
log_it "  Running: $SSH $REMOTEUSER \"pwd\""

$SSH $REMOTEUSER "pwd" >>$LOGFILE 2>>$LOGFILE
if [ $? -eq 0 ]; then

  log_it "Syncing from $SRC to $DEST."
  log_it "  Running: $RSYNC $RSYNC_OPTS $SRC $DEST"

  $RSYNC $RSYNC_OPTS $SRC $DEST >>$LOGFILE 2>>$LOGFILE
  if [ $? -gt 0 ]; then
    log_it "Error doing $RSYNC of $SRC to ${DEST}."
    close_and_exit 30
  fi

else

  log_it "Cannot connect to $REMOTEUSER."
  close_and_exit 40

fi


# Clean up logfile.
if [ $MAXLOGS -gt 0 ]; then 

  log_it "Cleaning up log files in $LOGDIR."
  find $LOGDIR \( -name "${LOG_PREFIX}*" \) -type f -mtime +${MAXLOGS} -exec rm -f {} \;

else

  log_it "Cleanup of log files disabled. MAXLOGS is $MAXLOGS."

fi

close_and_exit 0
