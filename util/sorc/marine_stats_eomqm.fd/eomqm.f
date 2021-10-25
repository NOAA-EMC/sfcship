C     MAIN PROGRAM:  EOMQM
C PURPOSE: to extract Quality Mark data for monthly buoy stats
c          and write it into buoy stats file.
c          run at beginning of month.
C INPUT FILES:  fort.12:  newbuoy.stats
c               fort.13:  qmarch.sort
C OUTPUT FILES: fort.51:  final.newbuoy.stats.temp1
c               fort.52:  final.newbuoy.stats.temp2
C HISTORY LOG:
C     980108  CARUSO       - NEW PROGRAM.
c   20000112  caruso magee - change header and line lengths to 82 chars
c                            from 80 chars to account for 4 digit year 
c                            in date string.
c   20030303  caruso magee - remove control file - not needed.         
C====================================================================
C
      CHARACTER*12 CDATE
      CHARACTER*12 cdateprev
      CHARACTER*8 ID
      CHARACTER*8 IDLAST
      character*9 savstr(4000)
      CHARACTER*82 string, ihdr
      character*82 string1, string2
      logical numeric, foundid
      INTEGER KSKIP
      DATA IDLAST/'        '/
      DATA savstr/4000*'         '/

      INTEGER NUMPQM, NUMAQM, NUMWQM
      DATA NUMPQM/0/, NUMAQM/0/,NUMWQM/0/

      logical aqmprevgood, wqmprevgood, aqmcurrgood, wqmcurrgood
      logical endfile
      data aqmprevgood/.false./, aqmcurrgood/.false./
      data wqmprevgood/.false./, wqmcurrgood/.false./

      DATA KSKIP/0/
C-----------------------------------------------------------------------
c       read in header from newbuoy.stats file (to skip it).
C-----------------------------------------------------------------------
      rewind(12)
      read(12,1054) IHDR
 1054 FORMAT(A82)
      print*,' ihdr = '
      print 1054,ihdr
      write(51,1054) IHDR
      n = 0   !need for counter later when writing to output.
C-----------------------------------------------------------------------
C                  READ PLATFORM OBSERVATION
C-----------------------------------------------------------------------
      endfile = .false.
 25   READ(13,1001,err=995,END=100) ID,CDATE,XLAT,XLON,PRES,PMQM,ATMP,
     *                             ATQM,WDDIR,WDSPD,WDQM
 1001 FORMAT(A8,2X,A12,1X,F6.0,1X,F6.0,1X,2(F6.1,1X,F5.0),F6.1,
     *       1X,F6.1,1X,F5.0)
      print 1055, id,cdate
 1055 format(' id = ',a8,' and cdate = ',a12)
C-----------------------------------------------------------------------
C                  SEARCH FOR BUOY OBS.  IGNORE ALL OTHERS.
C-----------------------------------------------------------------------
      CALL CHKID(ID,NUMERIC)
      IF(.NOT.NUMERIC) then
        KSKIP = KSKIP + 1
        GO TO 25
      ENDIF
C-----------------------------------------------------------------------
C    CHECK TO SEE IF BUOY OB JUST READ IN IS FROM SAME BUOY WE'RE 
C    WORKING ON.  IF SO, INCREMENT QM'S IF NECESSARY.  OTHERWISE,
C    WRITE OUT COUNTS FROM PREVIOUS BUOY, ZERO OUT COUNTS, AND START
C    AGAIN WITH NEW BUOY.
C-----------------------------------------------------------------------
      IF(IDLAST.EQ.'        ') then
        IDLAST = ID
        cdateprev = cdate
      endif
 99   continue
      IF(ID.EQ.IDLAST) THEN
        IF(PMQM.LE.3.0.AND.PMQM.GE.0.) then
C-----------------------------------------------------------------------
c     Pressure is present.  Check air and wind and increment if
c     present and not bad.
C-----------------------------------------------------------------------
          if(cdate.ne.cdateprev.or.numpqm.eq.0) NUMPQM = NUMPQM + 1
          if((cdate.ne.cdateprev).or.
     *       (cdate.eq.cdateprev.and..not.aqmprevgood)) then
             IF(ATQM.LE.3.0.AND.ATQM.GE.0.) then
               NUMAQM = NUMAQM + 1
               aqmcurrgood = .true.
             else
               aqmcurrgood = .false.
             endif
          endif
          if((cdate.ne.cdateprev).or.
     *       (cdate.eq.cdateprev.and..not.wqmprevgood)) then
            IF(WDQM.LE.3.0.AND.WDQM.GE.0.) then
              NUMWQM = NUMWQM + 1
              wqmcurrgood = .true.
            else
              wqmcurrgood = .false.
            endif
          endif
          cdateprev = cdate
        endif
        aqmprevgood = aqmcurrgood
        wqmprevgood = wqmcurrgood                 
        if(.not.endfile) GO TO 25
      else      
        PRINT 1044,IDLAST
 1044   FORMAT(' HIT LAST REPORT FROM BUOY ',A8)
C-----------------------------------------------------------------------
C         READ IN NEWBUOY.STATS TIL WE FIND A MATCH ON THE BUOY ID
C         AND WRITE COUNTS TO NEW FILE.
C-----------------------------------------------------------------------
        rewind(12)
        foundid = .false.
  35    read(12,1003,end=1057) string
 1003   FORMAT(a82)
c        print 202,string
c        print 203,idlast
c 202    format(' string = ',a82)
c 203    format(' idlast = ',a8)
        if(string(10:14).ne.idlast(1:5)) then
          if(.not.foundid) then  !haven't found match on id yet.
            go to 35
          else
            go to 50
          endif
        else
          foundid = .true.
          if(string(16:18).eq.' AP') then
            write(string(51:54),'(i4)') numpqm
            WRITE(51,1003) string
          elseif(string(16:18).eq.' AT') then
            write(string(51:54),'(i4)') numaqm
            WRITE(51,1003) string
          elseif(string(16:18).eq.' WD'.or.string(16:18).eq.' WS'
     *       .or.string(16:18).eq.' WV') then
            write(string(51:54),'(i4)') numwqm
            WRITE(51,1003) string
          else   !sst stat line
            WRITE(51,1003) string
            go to 35
          endif
          go to 35
        endif
C-----------------------------------------------------------------------
C                     WRITE DESIRED RECORDS
C-----------------------------------------------------------------------
 50     continue
        print 1006,idlast,xlat,xlon
 1006   format(1x,'id = ',a8,' lat = ',f6.0,' lon = ',f6.0) 
        NUMPQM = 0
        NUMAQM = 0
        NUMWQM = 0
        IDLAST = ID
        cdateprev = cdate
        aqmprevgood = .false.
        wqmprevgood = .false.
        IF(PMQM.LE.3.0.AND.PMQM.GE.0.) then
C-----------------------------------------------------------------------
c     Pressure is present.  Check air and wind and increment if
c     present and not bad.
C-----------------------------------------------------------------------
          if(cdate.ne.cdateprev.or.numpqm.eq.0) NUMPQM = NUMPQM + 1
          if((cdate.ne.cdateprev).or.
     *       (cdate.eq.cdateprev.and..not.aqmprevgood)) then
             IF(ATQM.LE.3.0.AND.ATQM.GE.0.) then
               NUMAQM = NUMAQM + 1
               aqmcurrgood = .true.
             else
               aqmcurrgood = .false.
             endif
          endif
          if((cdate.ne.cdateprev).or.
     *       (cdate.eq.cdateprev.and..not.wqmprevgood)) then
            IF(WDQM.LE.3.0.AND.WDQM.GE.0.) then
              NUMWQM = NUMWQM + 1
              wqmcurrgood = .true.
            else
              wqmcurrgood = .false.
            endif
          endif
          cdateprev = cdate
        endif
        aqmprevgood = aqmcurrgood
        wqmprevgood = wqmcurrgood                 
        if(.not.endfile) GO TO 25
      ENDIF         
      go to 101
 1057 print 1058,idlast
 1058 format(' no match found in fort.12 for id = ',a8)
      idlast = '        '
      NUMPQM = 0
      NUMAQM = 0
      NUMWQM = 0
      IDLAST = ID
      cdateprev = cdate
      aqmprevgood = .false.
      wqmprevgood = .false.
      IF(PMQM.LE.3.0.AND.PMQM.GE.0.) then
C-----------------------------------------------------------------------
c     Pressure is present.  Check air and wind and increment if
c     present and not bad.
C-----------------------------------------------------------------------
        if(cdate.ne.cdateprev.or.numpqm.eq.0) NUMPQM = NUMPQM + 1
        if((cdate.ne.cdateprev).or.
     *     (cdate.eq.cdateprev.and..not.aqmprevgood)) then
           IF(ATQM.LE.3.0.AND.ATQM.GE.0.) then
             NUMAQM = NUMAQM + 1
             aqmcurrgood = .true.
           else
             aqmcurrgood = .false.
           endif
        endif
        if((cdate.ne.cdateprev).or.
     *     (cdate.eq.cdateprev.and..not.wqmprevgood)) then
          IF(WDQM.LE.3.0.AND.WDQM.GE.0.) then
            NUMWQM = NUMWQM + 1
            wqmcurrgood = .true.
          else
            wqmcurrgood = .false.
          endif
        endif
        cdateprev = cdate
      endif         
      aqmprevgood = aqmcurrgood
      wqmprevgood = wqmcurrgood                 
      if(.not.endfile) GO TO 25
 100  continue
      print*,' hit end of file'
      endfile = .true.
      go to 99
 101  PRINT 1004, KSKIP
 1004 FORMAT(1X,I6,' SKIPPED')
C----------------------------------------------------------------------
c  check newbuoy.stats for lines that weren't written to final.newbuoy.
c  stats.temp1.  If line in newbuoy.stats not present in new output file,
c  write it out to final.newbuoy.stats.temp2, which will be appended to 
c  final.newbuoy.stats.temp1
C----------------------------------------------------------------------
      rewind(12)
      read(12,1054) IHDR
 39   rewind(51)
      read(51,1054) IHDR
 40   read(12,1003,end=1060) string1     !newbuoy.stats
 45   read(51,1003,end=1061) string2     !final.newbuoy.stats.temp1
      if(string1(10:25).ne.string2(10:25)) then
        go to 45
      else   !string in input is present in output. get next input line.
        go to 40
      endif
 1061 write(52,1003) string1    !final.newbuoy.stats.temp2
      go to 39
 1060 continue
      go to 999
 995  print*,' error reading qm file'
 999  continue
      STOP
      END
C******************************************************************
C     SUBROUTINE: CHKID   -   CHECK ID
C     PURPOSE:  CHECK ID TO SEE IF 5 DIGITS AND IF ALL NUMBERS.
C     IF NOT, ID IS A SHIP OR CMAN CALLSIGN, SO SET NUMERIC TO FALSE.
C     IF 5 DIGITS AND ALL NUMERIC, SET NUMERIC TO TRUE, SINCE THIS
C     ID IS A BUOY. 
C     AUTHOR:  C. CARUSO
C     DATE:  4/28/95
C------------------------------------------------------------------
      SUBROUTINE CHKID(KID,NUMERIC)
      CHARACTER*8 KID
      LOGICAL NUMERIC
      CHARACTER*1 LETTER(26)
      DATA LETTER/'A','B','C','D','E','F','G','H','I','J','K','L',
     1'M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z'/
      DO JJ = 1,26
        KK = INDEX(KID,LETTER(JJ))
        IF(KK.NE.0) THEN
          NUMERIC = .FALSE.
          RETURN
        ENDIF
      enddo
      IF(KID(2:2).EQ.' '.OR.KID(3:3).EQ.' '.OR.KID(4:4).EQ.' '
     *  .OR.KID(5:5).EQ.' '.OR.KID(6:6).NE.' '
     *  .or.kid(7:7).ne.' ') THEN
        NUMERIC = .FALSE.
      ELSE
        NUMERIC = .TRUE.
      ENDIF
      RETURN
      END
