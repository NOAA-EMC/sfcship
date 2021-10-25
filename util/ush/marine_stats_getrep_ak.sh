#############################################################
# Name: marine_stats_getrep_ak.sh   Author: 		    
# Abstract:						     
#							     
# HISTORY:  NOV. 2019- migrate to phase3  		     
#############################################################

set -x
INFILE=$1
echo "In marine_stats_getrep_ak.sh INFILE =  " $INFILE
grep ELQB8   $INFILE > ship1
grep KF002   $INFILE >> ship1
grep KNBD    $INFILE >> ship1
grep NL9H    $INFILE >> ship1
grep NWS0001 $INFILE >> ship1
grep NWS0002 $INFILE >> ship1
grep NWS0003 $INFILE >> ship1
grep NWS0004 $INFILE >> ship1
grep NWS0005 $INFILE >> ship1
grep NWS0006 $INFILE >> ship1
grep NWS0007 $INFILE >> ship1
grep NWS0008 $INFILE >> ship1
grep NWS0009 $INFILE >> ship1
grep NWS0010 $INFILE >> ship1
grep NWS0011 $INFILE >> ship1
grep NWS0012 $INFILE >> ship1
grep NWS0015 $INFILE >> ship1
grep NWS0016 $INFILE >> ship1
grep NWS0017 $INFILE >> ship1
grep NWS0018 $INFILE >> ship1
grep TESTUS1 $INFILE >> ship1
grep WADZ    $INFILE >> ship1
grep WAHG    $INFILE >> ship1
grep WAQ2746 $INFILE >> ship1
grep WAW9232 $INFILE >> ship1
grep WBB5799 $INFILE >> ship1
grep WBD5759 $INFILE >> ship1
grep WBM5091 $INFILE >> ship1
grep WBM8733 $INFILE >> ship1
grep WBN3008 $INFILE >> ship1
grep WBN4383 $INFILE >> ship1
grep WBN5978 $INFILE >> ship1
grep WBN8469 $INFILE >> ship1
grep WBO2511 $INFILE >> ship1
grep WBP4766 $INFILE >> ship1
grep WCE8951 $INFILE >> ship1
grep WCN3586 $INFILE >> ship1
grep WCQ8110 $INFILE >> ship1
grep WCT3784 $INFILE >> ship1
grep WCT5737 $INFILE >> ship1
grep WCY2920 $INFILE >> ship1
grep WDA4486 $INFILE >> ship1
grep WDC7518 $INFILE >> ship1
grep WDD2703 $INFILE >> ship1
grep WMVC    $INFILE >> ship1
grep WMVF    $INFILE >> ship1
grep WN4201  $INFILE >> ship1
grep WNGW    $INFILE >> ship1
grep WSD7078 $INFILE >> ship1
grep WSQ8098 $INFILE >> ship1
grep WTDF    $INFILE >> ship1
grep WTDH    $INFILE >> ship1
grep WTDK    $INFILE >> ship1
grep WTDM    $INFILE >> ship1
grep WTDO    $INFILE >> ship1
grep WTEA    $INFILE >> ship1
grep WTEB    $INFILE >> ship1
grep WTEC    $INFILE >> ship1
grep WTEE    $INFILE >> ship1
grep WTEF    $INFILE >> ship1
grep WTEJ    $INFILE >> ship1
grep WTEO    $INFILE >> ship1
grep WTEP    $INFILE >> ship1
grep WTER    $INFILE >> ship1
grep WTEU    $INFILE >> ship1
grep WTEY    $INFILE >> ship1
grep WTW9262 $INFILE >> ship1
grep WYH6327 $INFILE >> ship1
grep WYZ3112 $INFILE >> ship1
cat ship1 > ak.ships
