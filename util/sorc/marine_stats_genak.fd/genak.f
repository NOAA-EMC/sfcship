C*******************************************************************
C     PURPOSE: THIS PROGRAM GENERATES BRACKNELL FORMAT STATS AND
C     DBCP FORMAT STATS, WHICH ARE MORE COMPREHENSIVE, AND ARE
C     LOCATED IN A DIFFERENT FILE.
C
C     CHANGES:  20 APR 95 CC ADDED COMPUTATION OF RATE, SAVED
C                 LAST POSITION FOR OUTPUT, ONLY WRITING OUT
C                 REPORT TYPES 561, 562, AND PUT IN A SUBROUTINE
C                 TO CATCH 5 DIGIT ALL-NUMERIC CALLSIGNS WHICH COME
C                 IN WITH A 522 (BUOYS COMING IN WITH SHIP FORMAT).
C               03 OCT 95 CC CHANGED OPC TO NCO (NCEP CENTRAL OPER-
C                 ATIONS).
c               15 May 97 CC centered header text for air temp. and
c                 winds.
c               03 Aug 98 cc changed gross error values to agree with
c                 the values suggested by the DBCP, except for sst.  
c                 Wind speed was changed from 30 kts to 15 m/s and 
c                 wind dir from 140 degrees to 100 degrees.
c               16 Jun 99 cc added additional blank lines and fixed some
c                 spacing for output files.
c               03 Jan 00 ccm correcting code for reports that have all
c                 obs GEs (wasn't printing correct data at end).
c               16 Aug 00 ccm adding code to read tide gauge data.     
c               25 Jan 01 ccm modifying for gulf of ak ships.  computing
c                 stats for all obs, regardless of count, and removed
c                 restrictions on mean diff., std. deviation, and # of
c                 gross errors.  Will print diff even if only 1 ob.
c                 won't compute sd unless more than 1 ob.
c               03 Feb 03 ccm Adding code to write month/year and trailing
c                 lines within this program instead of doing so manually.
c               18 Aug 04 ccm Adding one blank line to top of output file so
c                 <PRE> tag formats html stats correctly.
C*******************************************************************
      PROGRAM GENAK
      CHARACTER*80 IHDR, IHDR1, CBLNK
      CHARACTER*12 KPDATE
      character*12 IDATE
      CHARACTER*8 ID, IDT, KPID
      CHARACTER*3 TYP(5), REPTYP
      CHARACTER*32 PARM(6)
      CHARACTER*9 CMON(12)
      CHARACTER*4 CYR
      CHARACTER*14 CHDRMON
      CHARACTER*48 CENDHDR1
      CHARACTER*27 CENDHDR2 
      CHARACTER*27 CENDHDR3
C
      INTEGER ITYPE, IGROSS(8)
      INTEGER KPLAT, KPLON, KPTYPE
      INTEGER NUMSHP, NUMFIX, NUMDRF, NUMCMN, NUMTDG
      INTEGER NO(8), IBAD(8), LASTLT, LASTLN
      INTEGER ND(8), NV(8), ISD(8), MEAN(8)
      INTEGER ITOTOB(5), LAT, LON
      INTEGER LU1, LU2
      INTEGER IDAT(8), ICMONLEN(12), NUMREP
C
      REAL SUMD(8), SUMSQ(8)
      REAL SD(8), DMEAN(8)
      REAL DIF(8)
      REAL SVDIF(30000,8), ENDLAT, ENDLON
      real savsd, savbias
      REAL TOTOBS(5),BAD(5)  !used for output only
C
      DATA KPDATE/'            '/, IDATE/'            '/
      DATA ID/'        '/, IDT/'        '/, KPID/'        '/
      DATA TYP/'SHP','CMN','TG ','FB ','DB '/
      DATA PARM/'SEA-LEVEL PRESSURE FOR ALL HOURS',
     1          ' AIR TEMPERATURE FOR ALL HOURS  ',
     2          '  WIND DIRECTION FOR ALL HOURS  ',
     3          '    WIND SPEED FOR ALL HOURS    ',
     4          'SEA SURFACE TEMP. FOR ALL HOURS ',
     5          'COMBINED WAVE HGT. FOR ALL HOURS'/
      DATA CMON/'JANUARY','FEBRUARY','MARCH','APRIL','MAY',
     +          'JUNE','JULY','AUGUST','SEPTEMBER','OCTOBER',
     +          'NOVEMBER','DECEMBER'/
      DATA ICMONLEN/7,8,5,5,3,4,4,6,9,7,8,8/
C
c gross errors:  slp - 15.0 mb, air - 15.0 C
c                wdir - 100 deg, wspd - 15.0 m/s
c                sst - 15.0 C
c (previously, was  wdir - 140 deg, wspd - 30.0 kts
c                sst - 15.0 C).
c
      DATA ITYPE/0/, IGROSS/150,150,1000,150,150,1000,1000,1000/
      DATA KPLAT/0/, KPLON/0/, KPTYPE/0/
      DATA NUMSHP/0/, NUMFIX/0/, NUMDRF/0/, NUMCMN/0/, NUMTDG/0/
      DATA NO/8*0/, IBAD/8*0/
      DATA ITOTOB/5*0/, LAT/0/, LON/0/
      DATA LU1/11/, LU2/12/
C
      DATA SUMD/8*0.0/, SUMSQ/8*0.0/
      DATA SD/8*0.0/, DMEAN/8*0.0/
      DATA DIF/8*0.0/
      DATA TOTOBS/5*0.0/,BAD/5*0.0/
C
      DATA IEND/0/,KTDUP/0/
      DATA IHDR(01:35)/'     NCEP CENTRAL OPERATIONS (NCO) '/
      DATA IHDR(36:63)/'PLATFORM STATS - GULF OF AK'/
      DATA IHDR1(01:40)/'PLATFORM   TYPE   NO. REPORTS   MEAN DIF'/
      DATA IHDR1(41:80)/'F   SD DIFF   # GROSS ERR'/
      DATA CENDHDR1(01:31)/'TOTAL NUMBER OF GULF OF ALASKA '/
      DATA CENDHDR1(32:48)/'OBSERVATIONS FOR '/
      DATA CENDHDR2(01:27)/'(EXCLUDING DUPLICATES) WAS '/
      DATA CENDHDR3(01:27)/'.  TOTAL NUMBER OF IDS WAS '/
C-----------------------------------------------------------------------
C  FILL CBLNK WITH BLANKS
C-----------------------------------------------------------------------
      DO I = 1,80
        CBLNK(I:I) = ' '
      ENDDO
C-----------------------------------------------------------------------
C     CALL W3UTCDAT.  WILL TAKE THE CURRENT MONTH
C     AND YEAR AND BACK IT TO THE PREVIOUS MONTH (AND YEAR, IF JANUARY).
C-----------------------------------------------------------------------
      CALL W3UTCDAT ( IDAT )
      IF ( IDAT(2) .eq. 01 ) THEN
         IMON = 12
         IYR = IDAT(1) - 1
      ELSE
         IMON = IDAT(2) - 1
         IYR = IDAT(1)
      ENDIF
      WRITE(CYR(1:4),FMT='(I4)') IYR
      CHDRMON(1:14) = CMON(IMON)//' '//CYR(1:4)
C-----------------------------------------------------------------------
C                        WRITE HEADER
C-----------------------------------------------------------------------
      WRITE(40,'(A)') ' ' 
      WRITE(40,777) IHDR
 777  FORMAT(A63)
      WRITE(40,555) CHDRMON
 555  FORMAT(25X,A14)
      WRITE(40,888) PARM(1)
 888  FORMAT(16X,A32)
      WRITE(40,777) CBLNK
      WRITE(40,1000) IHDR1
 1000 FORMAT(A80)
      WRITE(40,777) CBLNK

      WRITE(41,777) CBLNK
      WRITE(41,777) CBLNK
      WRITE(41,555) CHDRMON
      WRITE(41,888) PARM(2)
      WRITE(41,777) CBLNK
      WRITE(41,1000) IHDR1
      WRITE(41,777) CBLNK
      
      WRITE(42,777) CBLNK
      WRITE(42,777) CBLNK
      WRITE(42,555) CHDRMON
      WRITE(42,888) PARM(3)
      WRITE(42,777) CBLNK
      WRITE(42,1000) IHDR1
      WRITE(42,777) CBLNK
      
      WRITE(43,777) CBLNK
      WRITE(43,777) CBLNK
      WRITE(43,555) CHDRMON
      WRITE(43,888) PARM(4)
      WRITE(43,777) CBLNK
      WRITE(43,1000) IHDR1
      WRITE(43,777) CBLNK
      
      WRITE(44,777) CBLNK
      WRITE(44,777) CBLNK
      WRITE(44,555) CHDRMON
      WRITE(44,888) PARM(5)
      WRITE(44,777) CBLNK
      WRITE(44,1000) IHDR1
      WRITE(44,777) CBLNK

      ISTA = 0
C-----------------------------------------------------------------------
C         READ NUMBER OF REPORTS IN THE WHOLE INPUT FIlE (UNIT 10).
C-----------------------------------------------------------------------
      READ(LU2,1004) NUMREP
 1004 FORMAT(I8)
C-----------------------------------------------------------------------
C         READ SHIP REPORT ** PRESSURE, AIR TEMP,
C         WIND DIRECTION, WIND SPD, SST, skip WV HGT (missing),
C         other wind direction stats (for wind dir, order is:
C         (3) ob - fg, (6) ob - 90 - fg, (7) ob + 90 - fg,
C         (8) ob + 180 - fg.
C-----------------------------------------------------------------------
      NREP = 0
   25 READ(LU1,1002,END=100) ID,ITYPE,LAT,LON,IDATE,ND(1),NV(1),ND(2),
     *     NV(2),ND(3),NV(3),ND(4),NV(4),ND(5),NV(5),ND(6),NV(6),
     *     ND(7),NV(7),ND(8),NV(8)
 1002 FORMAT(1X,A8,1X,I3,1x,I5,1x,I5,1X,A12,1x,5(I5,I5),10x,3(i5,i5))
C-----------------------------------------------------------------------
C         SKIP REPORTS W/ID: BBXX/BUOY/SHIP/PLAT ETC.
C-----------------------------------------------------------------------
      K = INDEX(ID,'BUOY')
      IF(K.NE.0) GO TO 25
      K = INDEX(ID,'RIGG')
      IF(K.NE.0) GO TO 25
      K = INDEX(ID,'SHIP')
      IF(K.NE.0) GO TO 25
      K = INDEX(ID,'PLAT')
      IF(K.NE.0) GO TO 25
      K = INDEX(ID,'TEST ')
      IF(K.NE.0) GO TO 25
      K = INDEX(ID,'BBXX')
      IF(K.NE.0) GO TO 25
      NREP = NREP + 1
      IF(IDT.EQ.'        ') IDT=ID
C-----------------------------------------------------------------------
C     GO COMPUTE STATS IF NEW ID. SAVE LAST LAT/LON OF PREVIOUS ID.
C     CONVERT LAST LAT/LON TO REAL AND CONVERT LON TO +/-.
C-----------------------------------------------------------------------
      IF(ID.NE.IDT) THEN
        PRINT *,' FOUND NEW ID ',ID,'  LAST ID WAS ',IDT
        LASTLT = (NINT(KPLAT/10.0))
        ENDLAT = LASTLT/10.0
        IF(KPLON.LE.18000) THEN
          LASTLN = (NINT(-KPLON/10.0))
        ELSE
          LASTLN = (NINT((36000-KPLON)/10.0))
        ENDIF
        ENDLON = LASTLN/10.0
        IF(KPTYPE.EQ.522) NUMSHP = NUMSHP + 1
        IF(KPTYPE.EQ.531) NUMCMN = NUMCMN + 1
        IF(KPTYPE.EQ.532) NUMTDG = NUMTDG + 1
        IF(KPTYPE.EQ.561) NUMFIX = NUMFIX + 1
        IF(KPTYPE.EQ.562) NUMDRF = NUMDRF + 1
        GO TO 50
      ENDIF
C-----------------------------------------------------------------------
C               CK FOR DUPL RPT - SKIP IF YES
C-----------------------------------------------------------------------
      IF(NREP.GT.1) THEN
        IF((KPLAT.EQ.LAT).AND.(KPLON.EQ.LON).AND.(KPDATE.EQ.IDATE))THEN
          KTDUP=KTDUP+1
          NREP = NREP - 1
          GO TO 25
        ENDIF
      ENDIF
C-----------------------------------------------------------------------
C                  PERFORM GROSS ERROR CHECKS
C     SLP IS 10'S, 1'S, AND DECIMAL IF GE 1000 MB, ELSE SLP IS WHOLE
C     SLP VALUE TIMES 10 (9950 IS 995.0 MB).
C     OTHER PARMS, 9999 IMPLIES MISSING.
c   nd(1) = slp, nd(2) = air temp., nd(3) = wind dir, nd(4) = wind spd,
c   nd(5) = sst, nd(6) thru nd(8) = wind dir.
c   Convert wind speed from kts*10 to m/s*10 before doing gross error chk.
C-----------------------------------------------------------------------
   30 CONTINUE
      DO 32 II = 1,8
        IF(II.GT.1.AND.NV(II).EQ.999) NV(II)=-99
        IF(II.EQ.3.or.ii.ge.6) then   !wind dir
          IF(NV(II).EQ.990) NV(II)=-99
        endif
        IF(NV(II).EQ.-99) ND(II) = 9999
        IF(ND(II).GE.9999) GO TO 32
        if(ii.ne.4) then
          IF(ABS(ND(II)).LT.IGROSS(II)) GO TO 32
        else
          iwspddif = abs(nd(ii)) / 1.9443
          if(iwspddif.lt.igross(ii)) go to 32
        endif
        ND(II) = 9999
C       NREP = NREP - 1
        IBAD(II) = IBAD(II)+1
   32 CONTINUE
C-----------------------------------------------------------------------
C               IF ALL FIELDS ARE MISSING - SKIP REPORT
c  no need to check wind dir stats in elements 6-8.  if 3 is missing,
c  6-8 would be missing too.  save id, lat/lon, type in case all reports
c  from any one buoy are GEs.
C-----------------------------------------------------------------------
      IF(ND(1).GE.9999.AND.ND(2).GE.9999.AND.ND(3).GE.9999
     1                .AND.ND(4).GE.9999.AND.ND(5).GE.9999) THEN
        NREP = NREP - 1
        KPID = ID
        KPLAT = LAT
        KPLON = LON
        KPTYPE = ITYPE
        GO TO 25
      ENDIF
C-----------------------------------------------------------------------
C                       INCREMENT REPORT COUNT
C-----------------------------------------------------------------------
      KPID = ID
      KPLAT = LAT
      KPLON = LON
      KPDATE = IDATE
      KPTYPE = ITYPE
C-----------------------------------------------------------------------
C             CONVERT INTEGER DIFFS (ND) TO REAL.
C             CONVERT WIND SPEED AND WIND SPEED DIFF TO M/S.
C             SAVE DIFFS (SVDIF) & SUM OF DIFFS (SUMD).
c  note:  elements 6, 7, and 8 contain wind dir info for
c  (6) (obs - 90) - fg
c  (7) (obs + 90) - fg
c  (8) (obs +180) - fg
C-----------------------------------------------------------------------
      DO 40 II = 1,8
        IF(ND(II).GE.9999) GO TO 40
        DIF(II) = ND(II)/10.0
        IF(II.EQ.4) THEN
          DIF(II) = DIF(II)/1.9443
        ENDIF
 40   CONTINUE
      DO 41 II = 1,8
        IF(ND(II).GE.9999) GO TO 41
        NO(II) = NO(II) + 1
C  SAVE DIFFS
        SUMD(II) = SUMD(II) + DIF(II)
        M = NO(II)
        SVDIF(M,II) = DIF(II)
   41 CONTINUE
      GO TO 25
C-----------------------------------------------------------------------
C                  COMPUTE MEAN DIFF (MEAN).
C                  SKIP COMPUTATIONS IF <1 VALUES.
C                  IF ONLY ONE OB, THIS IS ONLY THE DIFF AND NOT
C                  A MEAN DIFF!
C-----------------------------------------------------------------------
   50 CONTINUE
      DO 57 II = 1,8
        IF(NO(II).LT.1) GO TO 57
          DMEAN(II) = SUMD(II)/NO(II)
          MEAN(II) = DMEAN(II)*10.0
   57 CONTINUE
C-----------------------------------------------------------------------
C                    COMPUTE STANDARD DEVIATION OF DIFF.
C                    FOR PLATFORMS THAT HAVE MORE THAN ONE OB.
C-----------------------------------------------------------------------
      DO 60 II = 1,8
        M = NO(II)
        IF(M.NE.0) THEN
           DO 61 I = 1,M
             SUMSQ(II) = SUMSQ(II) + (SVDIF(I,II) - DMEAN(II))**2
   61      CONTINUE
        ENDIF
   60 CONTINUE
      DO 62 II = 1,8
        M = NO(II)
        IF(M.LE.1) GO TO 62
        SD(II) = SQRT(SUMSQ(II)/M)
        ISD(II) = SD(II)*10.0
   62 CONTINUE
C-----------------------------------------------------------------------
c  find the smallest sd of the 4 wind direction sd's and write that
c  one out to the stats. Use the corresponding bias according to
c  the following formula:
c     if smallest sd is for (ob + 90) - fg, then use
c     bias = (bias of (obs + 90) - fg) - 90.
c     if smallest sd is for (ob - 90) - fg, then use
c     bias = (bias of (obs - 90) - fg) - (-90).
c
c         for wind dir, order is:
c         (3) ob - fg,  (6) ob - 90 - fg, (7) ob + 90 - fg,
c         (8) ob + 180 - fg.
c
c  check on number of good obs for all 4 categories.  if num for any
c  one category is le 1, set sd and mean for that category to 99.9
c  (missing).  This should ensure that sd of other categories will
c  be less than the missing one (previously, if le 1 obs, sd was still
c  zero, which would be selected as smallest sd in below logic.)
c  NOTE:  using no and ibad from wind dir stored in (3), no matter which
c  set of sd and mean is being used.  this is what I do in genstx.f.
C-----------------------------------------------------------------------
      do jj = 6,8
        if(no(jj).le.1) then
          sd(jj) = 99.9
          dmean(jj) = 999.9
        endif
      enddo

      if(no(3).gt.1) then
        if((sd(3).le.sd(6)).and.
     *     (sd(3).le.sd(7)).and.
     *     (sd(3).le.sd(8))) then
c          print*,' using (ob - fg) sd and bias'
          savsd = sd(3)
          savbias = dmean(3)
        elseif((sd(6).le.sd(3)).and.
     *         (sd(6).le.sd(7)).and.
     *         (sd(6).le.sd(8))) then
c          print*,' using ((ob-90) - fg) sd and bias'
          savsd = sd(6)
          savbias = dmean(6) + 90.
         elseif((sd(7).le.sd(3)).and.
     *         (sd(7).le.sd(6)).and.
     *         (sd(7).le.sd(8))) then
c          print*,' using ((ob+90) - fg) sd and bias'
          savsd = sd(7)
          savbias = dmean(7) - 90.
        elseif((sd(8).le.sd(3)).and.
     *         (sd(8).le.sd(6)).and.
     *         (sd(8).le.sd(7))) then
c          print*,' using ((ob+180) - fg) sd and bias'
          savsd = sd(8)
          savbias = dmean(8) - 180.
        endif
        sd(3) = savsd
        if(savbias.lt.-180.) then
          savbias = savbias + 360.
        elseif(savbias.gt.180.0.and.savbias.lt.999.8) then
          savbias = savbias - 360.
        endif
        dmean(3) = savbias
      endif
c      print*,' sd(3) = ',sd(3),' dmean(3) = ',dmean(3)

C-----------------------------------------------------------------------
C                          WRITE STAT FILE
C-----------------------------------------------------------------------
      IF(KPTYPE.EQ.522) REPTYP = TYP(1)
      IF(KPTYPE.EQ.531) REPTYP = TYP(2)
      IF(KPTYPE.EQ.532) REPTYP = TYP(3)
      IF(KPTYPE.EQ.561) REPTYP = TYP(4)
      IF(KPTYPE.EQ.562) REPTYP = TYP(5)
C  PRINT SLP.
      DO 31 JJ = 1,5
        ITOTOB(JJ) = NO(JJ) + IBAD(JJ)
        TOTOBS(JJ) = ITOTOB(JJ)
        BAD(JJ) = IBAD(JJ)
        IF(ITOTOB(JJ).GE.1) THEN
          IF(JJ.EQ.1) THEN
C           SLP
            WRITE(40,1003)KPID,REPTYP,NO(JJ),
     1      DMEAN(JJ),SD(JJ),IBAD(JJ)
 1003       FORMAT(A7,4X,A3,7X,I4,9X,F6.1,6X,F5.1,8X,I4)
          ELSEIF(JJ.EQ.2) THEN
C           AIR TEMP.
            WRITE(41,1003)KPID,REPTYP,NO(JJ),
     1      DMEAN(JJ),SD(JJ),IBAD(JJ)
          ELSEIF(JJ.EQ.3) THEN
C           WIND DIR.
            WRITE(42,1003)KPID,REPTYP,NO(JJ),
     1      DMEAN(JJ),SD(JJ),IBAD(JJ)
          ELSEIF(JJ.EQ.4) THEN
C           WIND SPD (M/S)
            WRITE(43,1003)KPID,REPTYP,NO(JJ),
     1      DMEAN(JJ),SD(JJ),IBAD(JJ)
          ELSEIF(JJ.EQ.5) THEN
C           SST
            WRITE(44,1003)KPID,REPTYP,NO(JJ),
     1      DMEAN(JJ),SD(JJ),IBAD(JJ)
          ENDIF
        ENDIF
 31   CONTINUE
   80 IDT = ID
      ISTA = ISTA + 1
      IF(IEND.NE.0) GO TO 200
C------------------------------------------------------------------
C       RESET ALL VARIABLES COMPUTED TO 0 FOR NEXT PLATFORM.
C       SUMD - SUM OF DIFFS.  DMEAN - MEAN DIFFS (REAL).
C       SD - STD DEV. (REAL). MEAN - MEAN DIFFS (INT).
C       ISD - STD DEV. (INT).  SUMSQ - SUM OF SQUARES
C       SUMRAT - SUM OF RATES (REAL). DMNRAT - MEAN RATE (REAL).
C       MNRATE - MEAN RATE (INT).
C------------------------------------------------------------------
      NREP = 0
      DO 85 II = 1,8
        IBAD(II) = 0
        if(ii.le.5) then
          BAD(II) = 0.0
          ITOTOB(II) = 0
          TOTOBS(II) = 0.0
        endif
        NO(II) = 0
        SUMD(II) = 0.0
        DMEAN(II) = 0.0
        MEAN(II) = 0
        SD(II) = 0.0
        ISD(II) = 0
        SUMSQ(II) = 0.0
   85 CONTINUE
      GO TO 30
  100 CONTINUE
      IEND = 1
      GO TO 50
  200 CONTINUE
C------------------------------------------------------------------
C     PRINT END TEXT TO END OF SST STATS
C------------------------------------------------------------------
      WRITE(44,777) CBLNK
      WRITE(44,993) CENDHDR1,CMON(IMON),CYR
 993  FORMAT(A,A,1X,A4)
      WRITE(44,994) CENDHDR2,NUMREP,CENDHDR3,ISTA      
 994  FORMAT(A,I6,A,1X,I2,'.')
      
      PRINT 1005, KTDUP
 1005 FORMAT(' NUMBER OF DUPL REPORTS:',I4)
      PRINT 1006, ISTA
 1006 FORMAT('0   TOTAL NO. IDS:',I5)
      PRINT 1007, NUMSHP, NUMFIX
 1007 FORMAT(' NUM OF SHIP OBS = ',I6,' NUM OF FIXED BUOY OBS = ',I6)
      PRINT 1008, NUMDRF, NUMCMN
 1008 FORMAT(' NUM OF DRIFTER OBS= ',I6,' NUM OF CMAN OBS= ',I6)
      PRINT 1009, NUMTDG
 1009 FORMAT(' NUM OF TIDE GAUGE OBS= ',I6)
      STOP
      END
