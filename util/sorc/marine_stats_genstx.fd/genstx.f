C     PROGRAM GENSTX
C     PURPOSE: THIS PROGRAM GENERATES DBCP FORMAT STATS.
C
C     CHANGES:  20 APR 95 CC ADDED COMPUTATION OF RATE, SAVED
C                 LAST POSITION FOR OUTPUT, ONLY WRITING OUT
C                 REPORT TYPES 561, 562, AND PUT IN A SUBROUTINE
C                 TO CATCH 5 DIGIT ALL-NUMERIC CALLSIGNS WHICH COME
C                 IN WITH A 522 (BUOYS COMING IN WITH SHIP FORMAT).
C               16 JUN 95 CC ADDED CODE TO COMPUTE STATS FOR WIND
C                 VECTORS.  NOT PRINTING BIAS FOR WIND VECTORS
C                 ALTHOUGH IT'S SET TO ZERO.  PRINTING 5 BLANKS FOR
C                 WIND VECTOR BIAS.
C               23 JUN 95 CC ADDED GROSS ERROR CHK FOR WIND VECTORS.
C                 GROSS ERROR FOR VECTORS IS DIFF. OF 10 M/S OR MORE.
C               03 OCT 95 CC CHANGED OPC TO NCO (NCEP CENTRAL OPER-
C                 ATIONS).
c               16 may 97 cc corrected report type.  if 561, comes
c                 from Bufr moored buoy tank, which meant obs was
c                 in BBXX format, not ZZYY as for drifters.
c               21 nov 97 cc added code to compute stats for 3 addtl
c                 wind dir numbers.  In addition to (ob - fg), stats
c                 also done for ((ob - 90) - fg), ((ob + 90) - fg),
c                 & ((ob + 180) - fg).  Of the 4 wind dir stats,
c                 the lowest sd is the one printed out to the buoy
c                 stats file.
c               01 dec 97 cc readjusting new biases computed for
c                 wind direction series to assure they're between
c                 -180 and +180.  Also corrected bias printout so
c                 that positive biases can be up to 999.9 (have
c                 enough columns to print this).  Min bias is
c                 -999., due to column space restrictions.
c                 Took out checks on negative sd and abs on comparisons
c                 between sd(3), sd(6), sd(7), and sd(8), since sd
c                 can't be negative.
c               05 dec 97 cc corrected write of dmean to unit 40, so
c                 if mean is .lt. -99.9, will write out as f5.0 (no
c                 tenths), e.g. dmean = -100.3 would write out as -100.
c               27 apr 98 cc changed to read 4 digit year from input file.
c               03 aug 98 cc changed gross error limits to suggested
c                 values by DBCP.  changed wind dir from 140 deg to 100 
c                 deg, wind speed from 30 kts (15.43 m/s) to 15 m/s,
c                 wind vector from 10 m/s to 25 m/s, and sst from 15C to
c                 5 C.
c               29 jun 99 cc corrected part of code where smallest sd
c                 is being determined.  for element (8), wasn't comparing
c                 to elements (3), (6), and (7) but only against (8) itself!
c               03 jan 00 ccm corrected code to print correct id, lat/lon,
c                 type if all reports from a buoy were GEs.
c               12 jan 00 ccm change date to output 4 digit year.
c                             increase header length by 2 chars so columns
c                             will align.
c               16 aug 00 ccm add code to skip cman and tide gauge reports.
c               24 Mar 03 ccm adding one line to temporarily toss reps with
C                 lons gt 0 and less and 1W (this long. band is temp off the
C                 f.g. grid), and changed IDATE to CDATE, since this is a
C                 char string.
C*******************************************************************
      PROGRAM GENSTX
      CHARACTER*12 KPDATE
      character*12 CDATE
      CHARACTER*8  ID, IDT, KPID
      CHARACTER*3  SNS(6)     !slp, air, wdir, wspd, sst, wvec
      CHARACTER*2  CMON, CDAYS
      CHARACTER*1  GTSCOD, FIELD
      INTEGER    ITYPE, IGROSS(8)
      INTEGER    KPLAT, KPLON, KPTYPE
      INTEGER    NUMSHP,NUMFIX,NUMDRF
      INTEGER    NO(9), IBAD(9), LASTLT, LASTLN
      INTEGER    ND(8), NV(8)
      REAL SUMD(9),  SUMSQ(9),  SUMRAT(9)
      REAL SD(9),    DMEAN(9),  DMNRAT(9)
      REAL DIFSQ(9), SUMDSQ(9), RMS(9)
      REAL RATE(9),  VAL(8), DIF(8), FG(8)
      REAL UWND,     VWND,   UFG,    VFG, THETA, THETAF
      REAL UVECDF,   VVECDF
      REAL RAD,      RADF, PI
      REAL SVSPD(25000), SVSPFG(25000)
      REAL GROSS
      REAL SVDIF(25000,8), ENDLAT, ENDLON
      real savsd, savbias
      LOGICAL NUMERIC
C
      DATA IEND/0/,KTDUP/0/
      DATA KPDATE/'            '/
      DATA IDT/'        '/
      DATA SNS/' AP',' AT',' WD',' WS','SST',' WV'/
      DATA FIELD/'G'/
      DATA IGROSS/150,150,1000,150,50,1000,1000,1000/
      DATA KPLAT/0/, KPLON/0/, KPTYPE/0/
      DATA NUMSHP/0/,NUMFIX/0/,NUMDRF/0/
      DATA NO/9*0/, IBAD/9*0/
      DATA SUMD/9*0.0/, SUMSQ/9*0.0/, SUMRAT/9*0.0/
      DATA SD/9*0.0/, DMEAN/9*0.0/, DMNRAT/9*0.0/
      DATA DIFSQ/9*0.0/, SUMDSQ/9*0.0/, RMS/9*0.0/
      DATA RATE/9*0.0/
      data savsd/0.0/, savbias/0.0/
      DATA PI/3.14159/
      DATA GROSS/25.0/
      DATA NUMERIC/.FALSE./
C-----------------------------------------------------------------------
C     Start Here
C-----------------------------------------------------------------------
      ISTA = 1
C-----------------------------------------------------------------------
C         READ SHIP REPORT ** PRESSURE, AIR TEMP,
C         WIND DIRECTION, WIND SPD, SST, skip wave info (missing),
c         other wind direction stats (for wind dir, order is:
c         (3) ob - fg,  (6) ob - 90 - fg, (7) ob + 90 - fg, 
c         (8) ob + 180 - fg.
C-----------------------------------------------------------------------
      NREP = 0
   25 READ(11,1002,END=100) ID,ITYPE,LAT,LON,CDATE,ND(1),NV(1),ND(2),
     *      NV(2),ND(3),NV(3),ND(4),NV(4),ND(5),NV(5),nd(6),nv(6),
     *      nd(7),nv(7),nd(8),nv(8)
 1002 FORMAT(1X,A8,1X,I3,1x,I5,1x,I5,1X,A12,1x,5(I5,I5),10x,3(i5,i5))
      print 1002,id,itype,lat,lon,idate,nd(1),nv(1),nd(2),nv(2),nd(3),
     *  nv(3),nd(4),nv(4),nd(5),nv(5),nd(6),nv(6),nd(7),nv(7),nd(8),
     *  nv(8)
C-----------------------------------------------------------------------
C         SKIP REPORTS W/ID: BBXX/BUOY/SHIP/PLAT ETC.
C         SKIP CMAN AND TIDE GAUGE REPORTS (TYPES 531 AND 532).
C         SKIP GENERIC SHIP REPORTS (TYPE 523).
C-----------------------------------------------------------------------
      K = INDEX(ID,'BUOY')
      IF(K.NE.0) GO TO 25
      K = INDEX(ID,'RIGG')
      IF(K.NE.0) GO TO 25
      K = INDEX(ID,'SHIP')
      IF(K.NE.0) GO TO 25
      K = INDEX(ID,'PLAT')
      IF(K.NE.0) GO TO 25
      K = INDEX(ID,'BBXX')
      IF(K.NE.0) GO TO 25
      IF(ITYPE.EQ.523.OR.ITYPE.EQ.531.OR.ITYPE.EQ.532) GO TO 25
C-----------------------------------------------------------------------
C         START COUNTING REPS HERE.
C-----------------------------------------------------------------------
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
        IF(KPTYPE.EQ.561) NUMFIX = NUMFIX + 1
        IF(KPTYPE.EQ.562) NUMDRF = NUMDRF + 1
        GO TO 50
      ENDIF
C-----------------------------------------------------------------------
C               CK FOR DUPL RPT - SKIP IF YES
C-----------------------------------------------------------------------
      IF(NREP.GT.1) THEN
        IF((KPLAT.EQ.LAT).AND.(KPLON.EQ.LON).AND.(KPDATE.EQ.CDATE))THEN
          KTDUP=KTDUP+1
          NREP = NREP - 1
          GO TO 25
        ENDIF
      ENDIF
C-----------------------------------------------------------------------
C                  PERFORM GROSS ERROR CHECKS
C-----------------------------------------------------------------------
   30 CONTINUE
      DO 32 II = 1,8
C-----------------------------------------------------------------------
C     SLP IS 10'S, 1'S, AND DECIMAL IF GE 1000 MB, ELSE SLP IS WHOLE
C     SLP VALUE TIMES 10 (9950 IS 995.0 MB).
C     OTHER PARMS, 9999 IMPLIES MISSING.
c   nd(1) = slp, nd(2) = air temp., nd(3) = wind dir, nd(4) = wind spd,
c   nd(5) = sst, nd(6) thru nd(8) = wind dir.
c   Convert wind speed from kts*10 to m/s*10 before doing gross error chk. 
C-----------------------------------------------------------------------
        IF(II.GT.1.AND.NV(II).EQ.999) NV(II)=-99
        IF(II.EQ.3.or.ii.ge.6) then     !wind dir
          IF(NV(II).EQ.990) NV(II)=-99
        endif
        IF(NV(II).EQ.-99) ND(II) = 9999
        IF(ND(II).GE.9999) GO TO 32
        if(ii.ne.4) then
          IF(ABS(ND(II)).LT.IGROSS(II)) GO TO 32
        else                    !wind speed
          iwspddif = abs(ND(II)) / 1.9443
          if(iwspddif.lt.igross(ii)) go to 32
        endif
        ND(II) = 9999
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
      KPDATE = CDATE
      KPTYPE = ITYPE
      CMON = KPDATE(5:6)
C-----------------------------------------------------------------------
C     SAVE NO. OF DAYS IN THE MONTH FOR OUTPUT
C-----------------------------------------------------------------------
      IF(CMON.EQ.'01'.OR.CMON.EQ.'03'.OR.CMON.EQ.'05'.OR.CMON.EQ.'07'
     1  .OR.CMON.EQ.'08'.OR.CMON.EQ.'10'.OR.CMON.EQ.'12') CDAYS = '31'
      IF(CMON.EQ.'02') CDAYS = '28'
      IF(CMON.EQ.'04'.OR.CMON.EQ.'06'.OR.CMON.EQ.'09'.OR.CMON.EQ.'11')
     1   CDAYS = '30'
C-----------------------------------------------------------------------
C             CONVERT INTEGER VALUES (NV) AND DIFFS (ND) TO REAL.
C             CONVERT WIND SPEED AND WIND SPEED DIFF TO M/S.
C             COMPUTE FIRST GUESS VALUES FROM DIFFS AND VALUES
C             (OBS).  NEEDED TO COMPUTE RATES (OBS/FG).
C             SAVE DIFFS (SVDIF) & SUM OF DIFFS (SUMD).
c  note:  elements 6, 7, and 8 contain wind dir info for 
c  (6) (obs - 90) - fg
c  (7) (obs + 90) - fg
c  (8) (obs +180) - fg
C-----------------------------------------------------------------------
      DO 40 II = 1,8
        DIF(II) = ND(II)/10.0
        IF(II.NE.3.AND.II.NE.4.and.ii.lt.6) THEN
          VAL(II) = NV(II)/10.0  !slp, air temp., or sst
        ELSE
          VAL(II) = NV(II)     !wind speed or direction
        ENDIF
        IF(II.EQ.4) THEN       !wind speed
          VAL(II) = VAL(II)/1.9443
          DIF(II) = DIF(II)/1.9443
        ENDIF
 40   CONTINUE
      DO 41 II = 1,8
        IF(ND(II).GE.9999) GO TO 41
        NO(II) = NO(II) + 1
C     HANDLE VAL OF SLP DIFFERENTLY.  THOUSANDS AND HUNDREDS CUT OFF.
        IF(II.EQ.1) THEN
          IF(VAL(II).GT.800.0) THEN
            FG(II) = VAL(II) - DIF(II)
          ELSE
            VAL(II) = VAL(II) + 1000.0
            FG(II) = VAL(II) - DIF(II)
          ENDIF
        ELSE
           FG(II) = VAL(II) - DIF(II)
        ENDIF
C  ADJUST WDIR FG IF COMPUTED TO BE LT 0 OR GT 360.
        IF(II.EQ.3.or.ii.ge.6) THEN
          IF(FG(II).LT.0.0)   FG(II) = FG(II) + 360.0
          IF(FG(II).GT.360.0) FG(II) = FG(II) - 360.0
        ENDIF
c        print*,' for ii = ',ii,' fg = ',fg(ii)
c        print*,' val = ',val(ii),' dif = ',dif(ii)
C  COMPUTE RATE AND SAVE DIFFS
        IF(FG(II).EQ.0.0.AND.VAL(II).EQ.0.0) THEN
          RATE(II) = 1.0
        ELSEIF(FG(II).NE.0.0.AND.VAL(II).NE.0.0) THEN
          RATE(II) = VAL(II)/FG(II)
        ENDIF
        SUMRAT(II) = SUMRAT(II) + RATE(II)
        SUMD(II) = SUMD(II) + DIF(II)
        M = NO(II)
        SVDIF(M,II) = DIF(II)
        DIFSQ(II) = DIF(II)**2
        SUMDSQ(II) = SUMDSQ(II) + DIFSQ(II)
   41 CONTINUE
C-----------------------------------------------------------------------
C     COMPUTE WIND VECTORS AND STATISTICS. store in element 9.
c     compute using wind direction stored in element 3 and wind speed
c     stored in element 4.
C-----------------------------------------------------------------------
      IF(ND(3).NE.9999.AND.ND(4).NE.9999) THEN
        NO(9) = NO(9) + 1
      ELSE
        GO TO 25
      ENDIF
      THETA = 90.0 - VAL(3)
      RAD = THETA * (PI/180.0)
      UWND = -(VAL(4) * COS(RAD))
      VWND = -(VAL(4) * SIN(RAD))
      THETAF = 90.0 - FG(3)
      RADF = THETAF * (PI/180.0)
      UFG = -(FG(4) * COS(RADF))
      VFG = -(FG(4) * SIN(RADF))
      UVECDF = UWND - UFG
      VVECDF = VWND - VFG
      IF(ABS(UVECDF).GE.GROSS.OR.ABS(VVECDF).GE.GROSS) THEN
        IBAD(9) = IBAD(9) + 1
        NO(9) = NO(9) - 1
        GO TO 25
      ENDIF
      MM = NO(9)
      SVSPD(MM) = VAL(4)
      SVSPFG(MM) = FG(4)
      IF(FG(4).EQ.0.0.AND.VAL(4).EQ.0.0) THEN
          RATE(9) = 1.0
      ELSEIF(FG(4).NE.0.0.AND.VAL(4).NE.0.0) THEN
          RATE(9) = VAL(4)/FG(4)
      ENDIF
      SUMRAT(9) = SUMRAT(9) + RATE(9)
      DIFSQ(9) = (UWND - UFG)**2 + (VWND - VFG)**2
      SUMDSQ(9) = SUMDSQ(9) + DIFSQ(9)
      GO TO 25
C-----------------------------------------------------------------------
C                  COMPUTE MEAN DIFF (DMEAN) AND MEAN RATE (DMNRAT).
C                  COMPUTE RMS.
C                  SKIP COMPUTATIONS IF <2 VALUES.
C                  FOR WIND VECTORS, SET MEAN TO 0.
C-----------------------------------------------------------------------
   50 CONTINUE
      DO 57 II = 1,9
        IF(NO(II).LT.2) GO TO 57
        IF(II.LT.9) THEN
          DMEAN(II) = SUMD(II)/NO(II)
        ELSE               !wind vector
          DMEAN(II) = 0.0
        ENDIF
        DMNRAT(II) = SUMRAT(II)/NO(II)
        RMS(II) = SQRT(SUMDSQ(II)/NO(II))
c        print*,' for ii = ',ii,' dmean = ',dmean(ii)
   57 CONTINUE
C-----------------------------------------------------------------------
C                    COMPUTE STANDARD DEVIATION OF DIFF.
C                    STANDARD DEVIATION IS COMPUTED DIFFERENTLY FOR
C                    WIND VECTORS (II = 9).
C-----------------------------------------------------------------------
      DO 60 II = 1,9
        M = NO(II)
        IF(M.NE.0) THEN
          IF(II.LT.9) THEN  !not wind vector
           DO 61 I = 1,M
             SUMSQ(II) = SUMSQ(II) + (SVDIF(I,II) - DMEAN(II))**2
   61      CONTINUE
          ELSE		    !wind vector
           DO 63 I = 1,M
            IF(DMNRAT(II).EQ.0.0) DMNRAT(II) = 1.0
            SUMSQ(II)=SUMSQ(II)+((SVSPD(I)/DMNRAT(II))-SVSPFG(I))**2
   63      CONTINUE
          ENDIF
        ENDIF
   60 CONTINUE
      DO 62 II = 1,9
        M = NO(II)
        IF(M.LE.1) GO TO 62
        SD(II) = SQRT(SUMSQ(II)/M)
c        print*,' for ii = ',ii,' m = ',m
c        print*,' sumsq = ',sumsq(ii),' sd = ',sd(ii)
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
      IF(KPTYPE.EQ.522) CALL CHKID(KPID,NUMERIC)
      IF(KPTYPE.EQ.561.OR.KPTYPE.EQ.562.OR.
     1  (NUMERIC.AND.KPTYPE.EQ.522)) THEN
        IF(KPTYPE.EQ.522.or.kptype.eq.561) GTSCOD = 'S'
        IF(KPTYPE.EQ.562) GTSCOD = 'B'
        DO 31 JJ = 1,5
          IF(NO(JJ).NE.0.OR.IBAD(JJ).NE.0) THEN
            IF(RMS(JJ).GT.99.9) RMS(JJ) = 99.9
            IF(DMNRAT(JJ).GT.99.9) DMNRAT(JJ) = 99.9
            IF(DMNRAT(JJ).LT.-9.9) DMNRAT(JJ) = -9.9
            IF(SD(JJ).GT.99.9) SD(JJ) = 99.9
            IF(DMEAN(JJ).GT.999.9) DMEAN(JJ) = 999.9
            IF(DMEAN(JJ).LT.-999.0) DMEAN(JJ) = -999.0
            IF(DMEAN(JJ).LT.-99.9) then
              WRITE(40,1003)KPDATE(1:6),CDAYS,KPID(1:5),SNS(JJ),GTSCOD,
     1        ENDLAT,ENDLON,NO(JJ),IBAD(JJ),DMEAN(JJ),SD(JJ),RMS(JJ),
     2        DMNRAT(JJ),FIELD
 1003         FORMAT(A6,A2,',',A5,',',A3,', ALL,',A1,',  NCO,',F5.1,',',
     1        F6.1,',',I4,',    ,',I3,',',F5.0,',',F4.1,',',F4.1,',',
     2        F4.1,',',A1)
            else
              WRITE(40,1004)KPDATE(1:6),CDAYS,KPID(1:5),SNS(JJ),GTSCOD,
     1        ENDLAT,ENDLON,NO(JJ),IBAD(JJ),DMEAN(JJ),SD(JJ),RMS(JJ),
     2        DMNRAT(JJ),FIELD
 1004         FORMAT(A6,A2,',',A5,',',A3,', ALL,',A1,',  NCO,',F5.1,',',
     1        F6.1,',',I4,',    ,',I3,',',F5.1,',',F4.1,',',F4.1,',',
     2        F4.1,',',A1)
            endif
          ENDIF
 31     CONTINUE
c---------------------------------------------------------------------
c  write wind vector stats
c---------------------------------------------------------------------
        IF(NO(9).NE.0.OR.IBAD(9).NE.0) THEN
          IF(RMS(9).GT.99.9) RMS(9) = 99.9
          IF(DMNRAT(9).GT.99.9) DMNRAT(9) = 99.9
          IF(DMNRAT(9).LT.-9.9) DMNRAT(9) = -9.9
          IF(SD(9).GT.99.9) SD(9) = 99.9
          WRITE(40,1005)KPDATE(1:6),CDAYS,KPID(1:5),SNS(6),GTSCOD,
     1      ENDLAT,ENDLON,NO(9),IBAD(9),SD(9),RMS(9),
     2      DMNRAT(9),FIELD
 1005     FORMAT(A6,A2,',',A5,',',A3,', ALL,',A1,',  NCO,',F5.1,',',
     1      F6.1,',',I4,',    ,',I3,',     ,',F4.1,',',F4.1,',',
     2      F4.1,',',A1)
        ENDIF
      ENDIF
   80 IDT = ID
      ISTA = ISTA + 1
      IF(IEND.NE.0) GO TO 200
C------------------------------------------------------------------
C       RESET ALL VARIABLES COMPUTED TO 0 FOR NEXT PLATFORM.
C       SUMD - SUM OF DIFFS.  DMEAN - MEAN DIFFS (REAL).
C       SD - STD DEV. (REAL).
C       SUMSQ - SUM OF SQUARES
C       SUMRAT - SUM OF RATES (REAL). DMNRAT - MEAN RATE (REAL).
C------------------------------------------------------------------
      DO 85 II = 1,9
        IBAD(II) = 0
        NO(II) = 0
        SUMD(II) = 0.0
        DMEAN(II) = 0.0
        DIFSQ(II) = 0.0
        SUMDSQ(II) = 0.0
        RMS(II) = 0.0
        SD(II) = 0.0
        SUMSQ(II) = 0.0
        RATE(II) = 0.0
        SUMRAT(II) = 0.0
        DMNRAT(II) = 0.0
   85 CONTINUE
      GO TO 30
  100 IEND = 1
      GO TO 50
  200 PRINT 1009, KTDUP
 1009 FORMAT(' NUMBER OF DUPL REPORTS:',I4)
      PRINT 1006, ISTA
 1006 FORMAT('0   TOTAL NO. IDS:',I5)
      PRINT 1007, NUMSHP
 1007 FORMAT(' NUM OF 522 PLATFORMS = ',I6)
      PRINT 1008, NUMFIX, NUMDRF
 1008 FORMAT(' NUM OF FIXED BUOYS = ',I6,' NUM OF DRIFTERS = ',I6)
      STOP
      END
C******************************************************************
C     SUBROUTINE: CHKID   -   CHECK ID
C     PURPOSE:  CHECK ID TO SEE IF 5 DIGITS AND IF ALL NUMBERS.
C     IF ANY ARE LETTERS, ID IS A SHIP CALLSIGN, SO SET NUMERIC TO 
C     FALSE.  IF ALL NUMERIC, SET NUMERIC TO TRUE, SINCE THIS
C     ID MAY BE A BUOY.  DONE SINCE THERE ARE BUOYS COMING IN WITH
C     FM 13 FORMAT, AND DECODERS PUT THEM INTO SHIP TANK IF LENGTH IS.
C     OTHER THAN 5 DIGITS.                                       
C
C     AUTHOR:  C. CARUSO
C     DATE:  4/28/95
C     CHANGES:  24 MAR 05 Magee    No longer checking for blanks
C                                  in ID (compressed out by decoder). 
C                                  Check for all numeric, regardless of
C                                  length and include in buoy stats.  
C------------------------------------------------------------------
      SUBROUTINE CHKID(KPID,NUMERIC)
      CHARACTER*8 KPID
      LOGICAL NUMERIC
      CHARACTER*1 LETTER(26)
      DATA LETTER/'A','B','C','D','E','F','G','H','I','J','K','L',
     1'M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z'/
      NUMERIC = .true.
      DO JJ = 1,26
        KK = INDEX(KPID,LETTER(JJ))
        IF(KK.NE.0) THEN
          NUMERIC = .FALSE.
          RETURN
        ENDIF
      ENDDO
      RETURN
      END
