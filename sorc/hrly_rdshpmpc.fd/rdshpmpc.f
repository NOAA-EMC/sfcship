C$$$  MAIN PROGRAM DOCUMENTATION BLOCK  ***
C
C MAIN PROGRAM: HRLY_RDSHPMPC
C   PRGMMR: CARUSO MAGEE     ORG: NP12        DATE: 2012-04-25
C
C ABSTRACT:  OLD: READS SHIP/BUOY/CMAN OBSERVATIONS FROM BUFR AND FTP'S THEM
C            TO THE MPC VAXS FOR QC. NEW: READS SHIPS/BUOYS/CMAN/TIDE GAUGE
C            OBS FROM BUFR AND DBN_ALERTS THEM TO OPC WORKSTATIONS FOR QC
C            VIA CREWSS SOFTWARE.
C
C PROGRAM HISTORY LOG:
C   86-01-22 PERROTTI - ORIGINAL AUTHOR v1.0.0
C   88-04-15 MARSHALL - UPDATED TO FORTRAN77 v2.0.0
C   88-09-30 MARSHALL - UPDATED FOR ROGER BAUER TESTS v2.0.1
C   88-10-12 MARSHALL - CHANGED BUILDING CYCLE TIME v2.0.2
C   88-11-10 MARSHALL - CHANGED BUILDING CYCLE TIME(FROM NMC HDR)
C   88-11-20 MARSHALL - UPDATE TO GENERATE ERICA FILE v2.0.4
C   88-12-12 MARSHALL - ADDED CHK FOR CAT 52 DATA v2.0.5 
C   89-04-26 MARSHALL - RESTORED CODE TO SKIP OBS ALREADY SENT
C   90-05-29 MARSHALL - COMMENT OUT SST STATEMENT v2.0.6
C   90-06-20 MARSHALL - DELETED ERICA CODING v2.1.0
C   90-06-25 MARSHALL - FT02 TO CATDS W/STEP TO COPY TO PRINT-FILE v2.1.1
C   90-09-10 MARSHALL - CHECK FOR BAD CHARS IN SHIPID v2.1.2
C   91-01-23 MARSHALL - ADD NOBS TO W3LOG/CONSOLE MESSAGES v2.1.3
C   92-03-10 MARSHALL - CHANGED CONTROL RECORD TO FORMATTED v2.1.4
C   92-12-09 CARUSO - REPLACED LOW CLOUD AMT WITH WAVE HGT AND  
C                     LOW CLOUD TYPE WITH SWELL HGT.  ALSO ADDED
C                     DOCUMENTATION FOR OBS ELEMENTS. v2.2.0
C   92-12-09 CARUSO - REPLACED CLOUD HGT WITH WAVE PERIOD AND
C                     MIDDLE CLOUD TYPE WITH SWELL PERIOD. v2.2.1
C   92-12-15 CARUSO - REPLACED HIGH CLOUD TYPE WITH SWELL DIRECTION. v2.2.2
C   93-12-02 CARUSO - ADDED CHECK ON SYNOPTIC TIME FOR DETERMINING
C                     DUPLICATES. WAS LOSING DATA SINCE ONLY LAT AND
C                     LONG WHERE CHECKED IF CALLSIGN WAS THE SAME. v2.3.0
C   93-12-22 CARUSO - ADDED CHECK ON SYNOPTIC TIME OF REPORT VERSUS
C                     SYSTEM TIME. SOME BUOY OBS HAVE RECEIPT TIMES
C                     AFTER START OF SYNOPTIC PERIOD BUT ARE FROM
C                     PREVIOUS PERIOD AND SO WE DON'T WANT THEM.
C                     E.G. BUOY 44773 SENDS REPORT WITH OBS TIME OF
C                     1475Z AND REPORT IS RECEIVED AFTER 1800Z. THIS
C                     REPORT IS MADE A PART OF THE 18Z DATASET BUT IT
C                     IS MORE THAN 3 HRS OLD AND BELONGS WITH THE 12Z
C                     DATASET, SO WE DON'T WANT TO DOWNLOAD IT TO THE
C                     VAXES. v2.3.1
c   96-07-25 caruso - beginning rewrite to use bufr files off Cray,
c                     instead of O.N.124 off 9000. v3.0.0
c   98-03-05 caruso - adding inquire command to check for input bufr
c                     files.  if all are missing, stop.
c   98-05-08 caruso - removing calls to non-y2k s/r's in w3lib. replacing
c                     with w3utcdat.  upgrading to f90. v4.0.0
c   98-12-03 caruso - renumbering fortran logical units to comply with
c                     standards (was using unit 2 for output.  changed this
c                     to unit 20.  was using units 10 thru 13 for input.
c                     changed this to units 11 thru 14).  adding calls
c                     to w3tagb, w3tage, and exit for production.  making
c                     sure doc blocks are up to production standards.
c   98-12-29 caruso - adding correction to callsign so if shipid has
c                     lower case letters, they'll be uppercase on output
c                     for quips (note:  this may not be needed for unix
c                     version). v4.0.2
c   99-02-19 caruso - modifying code to put into production on the Crays
c                     (adding calls to w3tagb, w3tage) and added a few
c                     print statements. v5.0.0
c 2000-01-28 caruso magee - modifying code to use XLFUNIT_ form for input
C                     files instead of ln -sf.  Need to add call to getenv
c                     to read in filename and do the inquire on it. v5.0.1
c 2000-05-03 caruso magee - modifying code to skip obs from callsign 'TEST'.
c 2000-07-20 caruso magee - modifying code to read tide gauge data (new     
c                           BUFR subtype 001.005).  Make new arbitrary station
C                           type 532 for tide gauge stations. v5.1.0
c 2000-08-07 caruso magee - modifying code to read ship ice accretion and
c                           store in place of past wx. v5.1.1
c 2000-09-11 caruso magee - modifying code to read swell wave data from CMANs.
c 2001-10-18 caruso magee - modifying code to check for illegal characters in 
c                           the callsigns and compress them out. v5.1.3
c 2012-04-25 caruso magee - print NOBS as I8 (I4 is too small).  v5.1.4
c 2013-01-09 Wen    Meng  - replace getenv into get_environment_variable,
c                           change the vaule of envvar from 'XLFUNIT_'
c                           into 'FORT  '
C
c USAGE:
C   INPUT FILES:
c	fort.11 - ship bufr file (001.001 and 001.013 combined)
c	fort.12 - drifting buoy bufr file (001.002)
c	fort.13 - moored buoy bufr file (001.003)
c	fort.14 - cman bufr file (001.004)
c	fort.15 - tidg bufr file (001.005)
C
C   OUTPUT FILES:
C     	fort.20 - sfcshp.dat
C
C   SUBPROGRAMS CALLED:
C     UNIQUE:      - BLDID
C
C     LIBRARY:
c       w3lib90    - w3tagb w3tage w3utcdat
c       bufrlib90  - openbf readmg readsb ufbint
c       system     - get_environment_variable
C
C   EXIT STATES:
C     COND =   0   - SUCCESSFUL RUN
C          = 120   - ERROR IN SBR BLDID
C          = 300   - NO NEW REPORTS FOUND
C
C   REMARKS:
C
C ATTRIBUTES:
C   LANGUAGE:  FORTRAN 90
C   machine:  ibm
C
C$$$
C
      CHARACTER*80 ID
      CHARACTER*8 SHIPID, outstr
      character*80 string
      character*8 ahl(4)
      character*8 inout, ctemp, subset
      character*1 lowercase(26), uppercase(26), c

      integer     idat(8)
      integer     ISYNOP
      INTEGER     IYEAR4d, IMONTH, IDAY, IHOUR
      INTEGER     OBS(22)

      REAL        RMISS
      DATA        RMISS/1.E10/
      real        arr(10,255)
      real        xtemp, xtempid
      REAL        XTEMP1(22)
      integer     lubfr, lundx, iret1, iret2, ict
      integer     idate
c     integer     getoarg
      integer(4)  narg, iargc, numarg
      character*8 cbuf
      integer     icyc
      integer(8)  iexit

c      character*11 envvar
c      data         envvar/'XLFUNIT_   '/
      character*6  envvar
      data         envvar/'FORT  '/
      character*80 cfilnam
      integer      iunit(5)
      data         iunit/11, 12, 13, 14, 15/
      logical      am_i_here
      logical      shiphere, dbuoyhere, mbuoyhere, cmanhere, tidghere
      data         shiphere /.false./
      data         dbuoyhere/.false./
      data         mbuoyhere/.false./
      data         cmanhere /.false./
      data         tidghere /.false./
      integer      ioutunit
      data         ioutunit/20/ 

      logical      anygood
C
      EQUIVALENCE(xtempid,SHIPID)
      EQUIVALENCE(xtemp,ctemp)
c
      CALL W3TAGB('HRLY_RDSHPMPC',2012,0116,0085,'NP12')
C-----------------------------------------------------------------------
C                       GET cycle time
C-----------------------------------------------------------------------
      narg = iargc()
      do numarg = 1,narg
        call getarg(numarg,cbuf)
      enddo
      read(cbuf,555) icyc
 555  format(i2)
C-----------------------------------------------------------------------
C                       GET system DATE/TIME
C-----------------------------------------------------------------------
      call w3utcdat(idat)
      PRINT 1000, iDAT
 1000 FORMAT(' ****** EXECUTE PGM RDSHPMPC - ',8i6,' ******'/)
      NOBS  = 0
      NSKIP = 0
      IEXIT = 0
C-----------------------------------------------------------------------
c  set up lowercase and uppercase arrays
C-----------------------------------------------------------------------
      do jj = 1,26
        lowercase(jj) = char(96+jj)
        uppercase(jj) = char(64+jj)
      enddo
C-----------------------------------------------------------------------
c  check for existence of all 5 input files.  only process those
c  files that are there.
C-----------------------------------------------------------------------
      do ii = 1,5
        write(envvar(5:6),fmt='(i2.2)') iunit(ii)
        call get_environment_variable(envvar,cfilnam)
        inquire(file=cfilnam,exist=am_i_here,iostat=ierr)
        if(.not.am_i_here) then  !file is missing
          print 860, ii, cfilnam
 860      format(//,'  FILE ',i2,' NAME = ',a80,' DOES NOT EXIST...')
          print *,' DO NOT TRY TO READ FROM THIS UNIT.'
        else                     !file is present
          if(ii.eq.1) shiphere = .true.
          if(ii.eq.2) dbuoyhere = .true.
          if(ii.eq.3) mbuoyhere = .true.
          if(ii.eq.4) cmanhere = .true.
          if(ii.eq.5) tidghere = .true.
        endif
      enddo
      if(.not.shiphere.and..not.dbuoyhere.and.
     *   .not.mbuoyhere.and..not.cmanhere.and..not.tidghere) then
        write(6,1140)
 1140   FORMAT(' **** ERROR NO INPUT BUFR FILES PRESENT')
        go to 999
      endif
C-----------------------------------------------------------------------
C                    BUILD SFCSHP HEADER RECORD
C-----------------------------------------------------------------------
      iyear4d = idat(1)
      imonth  = idat(2)
      iday    = idat(3)
      ihour   = idat(5)
      imin    = idat(6)
      CALL BLDID(ID,IYEAR4D,IMONTH,IDAY,icyc,ihour,imin,IERR)
      IF(IERR.NE.0) THEN 
        PRINT *,' BLDID ERROR, IERR=',IERR
        iexit = 120
        go to 999
      endif    
C-----------------------------------------------------------------------
C                    WRITE SFCSHP HEADER RECORD
C-----------------------------------------------------------------------
      WRITE(ioutunit,1013)ID
 1013 FORMAT(A80)
      print 1014,id
 1014 format(' header id = ',a80)
C-----------------------------------------------------------------------
C  1. OPEN THE FILE                                                    |
C----------------------------------------------------------------------|
C     OPENBF:   Opens the file for reading.                            |
C     INPUT:    LUBFR:  Specifies the logical unit containing the file |
C                       to be read from.                               |
C               INOUT:  Opens the file for reading when INOUT is set   |
C                       to IN.                                         |
C               LUNDX:  Specifies the logical unit containing the BUFR |
C                       tables.  If the tables are contained           |
C                       in the BUFR messages, LUNDX = LUBFR.           |
C     OUTPUT:   NONE                                                   |
C----------------------------------------------------------------------|
      lunit = 10
      do 100 I = 1,5
        if((i.eq.1.and.shiphere).or.
     *     (i.eq.2.and.dbuoyhere).or.
     *     (i.eq.3.and.mbuoyhere).or.
     *     (i.eq.4.and.cmanhere) .or.
     *     (i.eq.5.and.tidghere)) then
          INOUT = 'IN'
          LUBFR = LUNIT + I
          LUNDX = LUBFR 
          CALL OPENBF( LUBFR, INOUT, LUNDX )
C----------------------------------------------------------------------|
C  2. DECODE ALL BUFR MESSAGES IN THE FILE                             |
C----------------------------------------------------------------------|
          IRET1 = 0
          ICT = 0
C----------------------------------------------------------------------|
C    3. ADVANCE THE POINTER TO THE NEXT BUFR MESSAGE IN THE FILE       |
C       AND READ THE BUFR MESSAGE INTO AN INTERNAL BUFFER              |
C----------------------------------------------------------------------|
C       READMG:   Advances the input message pointer to the next       |
C                 BUFR message in the file and reads the message,      |
C                 without change, into an internal buffer.             |
C       INPUT:    LUBFR:  Defined under OPENBF.                        |
C       OUTPUT:   SUBSET: Contains the table A mnemonic associated     |
C                         with the type of data in the BUFR message.   |
C                 IDATE:  Contains the year, month, day, and hour      |
C                         from Section 1 of the BUFR message.          |
C                 IRET1:  If IRET = 0, the read has been successful    |
C                         If IRET = -1, and end-of-file has been read. |
C----------------------------------------------------------------------|
 160      CALL READMG( LUBFR, SUBSET, IDATE, IRET1 )
          If ( IRET1 .EQ. -1 ) THEN
C----------------------------------------------------------------------|
C     3a. WE HAVE REACHED THE END-OF-FILE.  PRINT THAT FACT AND QUIT  
C         IF LAST UNIT WAS READ.  IF WE HAVEN'T READ THRU UNIT 13 YET,
C         GO TO NEXT INPUT UNIT TO READ.
C----------------------------------------------------------------------|
            PRINT *, ' '
            PRINT *, ' Read EOF '
            IF(I.EQ.5) GO TO 999
            GO TO 100
          ENDIF
          IF ( IRET1 .EQ. 0 ) THEN
C----------------------------------------------------------------------|
C     3b. WE HAVE READ A BUFR MESSAGE.  INCREMENT ICT AND GO ON!       |
C----------------------------------------------------------------------|
            ICT = ICT + 1
C----------------------------------------------------------------------|
C      4. READ AND UNPACK THE BUFR MESSAGE                             |
C----------------------------------------------------------------------|
C         READSB:   Reads and unpacks the BUFR message placed in       |
C                   the internal buffer by READMG into an indexed      |
C                   and expanded internal buffer.                      |
C         INPUT:    LUBFR:  Defined under OPENBF                       |
C         OUTPUT:   IRET2:  Set to 0 (zero) when operation was         |
C                           successful and data is available for       |
C                           user access.  Set to -1 when all           |
C                           subsets have been processed, and           |
C                           another call to READMG is required to      |
C                           read the next BUFR message.                |
C----------------------------------------------------------------------|
C         INITIALIZE IRET2 TO 0 AND READ THIS UNPACKED BUFR MESSAGE    |
C         UNTIL SUBSETS HAVE BEEN READ.                                |
C----------------------------------------------------------------------|
            IRET2 = 0
 170        CALL READSB (LUBFR, IRET2)
            if(iret2.eq.0) then
C----------------------------------------------------------------------|
C        5. ACCESS RELEVANT PARTS OF THE UNPACKED BUFR MESSAGE         |
C----------------------------------------------------------------------|
C           UFBINT: Transfers data values either (a) from the internal |
C                   buffer produced by READSB to the user, or (b) from |
C                   the user to an internal buffer for subsequent      |
C                   writing out by WRITSB.  When using UFBINT for      |
C                   purpose (a), the following definitions apply:      |
C           INPUT:  LUBFR:  Defined under OPENBF.                      |
C                   I1:     The maximum first dimension of ARR.  This  |
C                           dimension must be large enough to hold     |
C                           the maximum number of parameters to be     |
C                           accessed and placed into array ARR.        |
C                   I2:     The maximum second dimension of ARR.  This |
C                           dimension must be large enough to hold     |
C                           the maximum number of repetitions of the   |
C                           set of parameters to be accessed and       |
C                           placed into array ARR.                     |
C                   STR:    String of mnemonics indicating which       |
C                           parameters to be placed into array ARR.    |
C                           Note that there cannot be more than I1     |
C                           mnemonics in STR.                          |
C           OUTPUT: ARR:    A two-dimensional array containing real    |
C                           or character data values.                  |
C                   NRET:   The number of sets of requested parameters |
C                           - those indicated by the mnemonics in      |
C                           STR - that were retrieved by UFBINT and    |
C                           placed into array ARR.                     |
C----------------------------------------------------------------------|
              STRING = ' BUHD ICLI BULTIM BBB '
              CALL UFBINT (LUBFR, ARR, 10, 255, NRET, STRING)
              XTEMP = ARR(1,1)
              AHL(1) = CTEMP
              XTEMP = ARR(2,1)
              AHL(2) = CTEMP
              XTEMP = ARR(3,1)
              AHL(3) = CTEMP
              XTEMP = ARR(4,1)
              AHL(4) = CTEMP
C-----------------------------------------------------------------------
C                    SET SYNOPTIC PERIOD VARIABLE ISYNOP
C-----------------------------------------------------------------------
              IF(idat(5).GE.00.AND.idat(5).LT.06) ISYNOP = 0000
              IF(idat(5).GE.06.AND.idat(5).LT.12) ISYNOP = 0600
              IF(idat(5).GE.12.AND.idat(5).LT.18) ISYNOP = 1200
              IF(idat(5).GE.18.AND.idat(5).LT.24) ISYNOP = 1800
C-----------------------------------------------------------------------
C                 STORE DATA IN TRANSMISSION ARRAY
C-----------------------------------------------------------------------
              NOBS = NOBS + 1
C---------------------------------------------------------------------|
C       ACCESS THE DATE/TIME OF THIS OBSERVATION.         
C---------------------------------------------------------------------|
              STRING = ' YEAR MNTH DAYS HOUR MINU '
              CALL UFBINT (LUBFR, ARR, 10, 255, NRET, STRING)
              IYR = ARR(1,1)
              IMO = ARR(2,1)
              IDA = ARR(3,1)
              IHR = ARR(4,1)
              IMN = ARR(5,1)
              iobtim= (ihr * 100) + imn
              obs(3) = iobtim
C-----------------------------------------------------------------------
C        OBS(1) = LATITUDE, OBS(2) = LONGITUDE, OBS(3)  = OBS TIME
C-----------------------------------------------------------------------
              STRING = ' CLON CLAT'
              CALL UFBINT (LUBFR, ARR, 10, 255, NRET, STRING)
              XTEMP1(1)= ARR(2,1)
              XTEMP1(2) = ARR(1,1)
C----------------------------------------------------------------------|
C           CONVERT LAT AND LON TO FORMAT QUIPS NEEDS
C----------------------------------------------------------------------|
              XTEMP1(1) = XTEMP1(1) * 100.
              XTEMP1(2) = XTEMP1(2) * 100.
              IF(XTEMP1(2).GT.0.) THEN
                XTEMP1(2) = 36000. - XTEMP1(2)
              ELSEIF(XTEMP1(2).LT.0) THEN
                XTEMP1(2) = -(XTEMP1(2))
              ENDIF
              if(xtemp1(2).gt.36000.0.or.xtemp1(2).lt.0.) then
                if(nobs.ge.1) nobs = nobs - 1
                go to 170
              elseif(xtemp1(1).gt.9000.0.or.xtemp1(1).lt.-9000.) then
                if(nobs.ge.1) nobs = nobs - 1
                go to 170
              endif
              OBS(1) = nint(XTEMP1(1))
              OBS(2) = nint(XTEMP1(2))
C-----------------------------------------------------------------------
C           CHECK OBS TIME AGAINST SYNOPTIC PERIOD. IF OBS TIME IS
C           VALID FOR THE PREVIOUS SYNOPTIC PERIOD, OR IF OBS TIME IS
C           VALID FOR THE NEXT SYNOPTIC PERIOD, SKIP THE REPORT.
C-----------------------------------------------------------------------
        IF(ISYNOP.EQ.0000.AND.(OBS(3).LT.2100.AND.OBS(3).GE.0300))
     *    GO TO 181
        IF(ISYNOP.EQ.0600.AND.(OBS(3).LT.0300.OR.OBS(3).GE.0900))
     *    GO TO 181
        IF(ISYNOP.EQ.1200.AND.(OBS(3).LT.0900.OR.OBS(3).GE.1500)) 
     *    GO TO 181
        IF(ISYNOP.EQ.1800.AND.(OBS(3).LT.1500.OR.OBS(3).GE.2100)) 
     *    GO TO 181
C-----------------------------------------------------------------------
C           CHECK FOR DUPLICATE REPORTS - SKIP IF YES
C-----------------------------------------------------------------------
              GO TO 182
  181         NSKIP = NSKIP + 1
              GO TO 170
C---------------------------------------------------------------------
C           ACCESS THE CALL SIGN FOR THIS OBSERVATION.         
C           Call signs are returned in    
C           ARR(1,1).  Mnemonic is RPID.
c           Check for lower case letters in callsign. Convert to
c           upper case where found.
C---------------------------------------------------------------------
 182          STRING = ' RPID '
              CALL UFBINT (LUBFR, ARR, 10, 255, NRET, STRING)
              xtempid = ARR(1,1)
              do jk = 1,8
                do kk = 1,26
                  if(shipid(jk:jk).eq.lowercase(kk)) then
                     shipid(jk:jk) = uppercase(kk)
                  endif
                enddo
              enddo
C---------------------------------------------------------------------
C             check for illegal characters and compress them out 
C             so that id begins in column 1 and ends with blanks (if
C             less than 8 characters)
C---------------------------------------------------------------------
              ip = 0
              outstr = '        '
              do jk = 1,8
                c = shipid(jk:jk)
                if((c.ge.'0'.and.c.le.'9').or.
     +             (c.ge.'A'.and.c.le.'Z')) then
                  ip = ip + 1
                  outstr(ip:ip) = c
                endif
              enddo
              shipid = outstr
C-----------------------------------------------------------------------
C     SKIP OBS FROM CALLSIGN 'TEST'.
C-----------------------------------------------------------------------
              if(index(shipid,'TEST ').ne.0) go to 170
C-----------------------------------------------------------------------
C     OBS(4) = SST.  OBS(5) = REPORT TYPE, OBS(6) = SLP
C     OBS(7) = STATION PRESSURE, OBS(8) = WIND DIR, OBS(9) = WIND SPD
C     OBS(10) = AIR TEMP, OBS(11) = DEW PT.
C     OBS(12) = HORIZONTAL VISIBILITY
C     NOTE: HORIZ. VIS. IS NOT AVAILABLE FROM DRIFTING BUOYS.
c
c     Note2: Also, report types like O.N.124 not available from bufr
c            file on IBM.  Check unit from which you're reading to
c            determine report type.  For unit 10 (001.001), check id.
c            If id is 'SHIP', set type to 523, else type is 522.
C-----------------------------------------------------------------------
              IF(I.EQ.1) THEN
C               ship
                STRING = ' HOVI WDIR WSPD TMDB TMDP SST1 PRES PMSL '
                CALL UFBINT (LUBFR, ARR, 10, 255, NRET, STRING)
                XTEMP1(4)= ARR(6,1)
                XTEMP1(6)= ARR(8,1)
                XTEMP1(7)= ARR(7,1)
                XTEMP1(8)= ARR(2,1)
                XTEMP1(9)= ARR(3,1)
                XTEMP1(10)= ARR(4,1)
                XTEMP1(11)= ARR(5,1)
                XTEMP1(12)= ARR(1,1)
                if(shipid.eq.'SHIP') then
                  xtemp1(5) = 523.
                else
                  xtemp1(5) = 522.
                endif
              ELSEIF(I.EQ.2) THEN
C               dbuoy
                STRING = ' WDIR WSPD '
                CALL UFBINT (LUBFR, ARR, 10, 255, NRET, STRING)
                XTEMP1(8) = ARR(1,1)
                XTEMP1(9) = ARR(2,1)
                STRING = ' TMDB TMDP SST1 PRES PMSL '
                CALL UFBINT (LUBFR, ARR, 10, 255, NRET, STRING)
                XTEMP1(4) = ARR(3,1)
                XTEMP1(6) = ARR(5,1)
                XTEMP1(7) = ARR(4,1)
                XTEMP1(10) = ARR(1,1)
                XTEMP1(11) = ARR(2,1)
                XTEMP1(12) = RMISS
                xtemp1(5) = 562.
              ELSEIF(I.EQ.3.OR.I.EQ.4) THEN
C               mbuoy or cman
                STRING = ' HOVI WDIR WSPD TMDB TMDP SST1 '
                CALL UFBINT (LUBFR, ARR, 10, 255, NRET, STRING)
                XTEMP1(4)= ARR(6,1)
                XTEMP1(8)= ARR(2,1)
                XTEMP1(9)= ARR(3,1)
                XTEMP1(10)= ARR(4,1)
                XTEMP1(11)= ARR(5,1)
                XTEMP1(12)= ARR(1,1)
                STRING = ' PRES PMSL '
                CALL UFBINT (LUBFR, ARR, 10, 255, NRET, STRING)
                XTEMP1(6)= ARR(2,1)
                XTEMP1(7)= ARR(1,1)
                if(i.eq.3) then
                  xtemp1(5) = 561.
                else
                  xtemp1(5) = 531.
                endif
              ELSE
C               tide gauge  
                STRING = ' SST1 '
                CALL UFBINT (LUBFR, ARR, 10, 255, NRET, STRING)
                XTEMP1(4)= ARR(1,1)
                STRING = ' TMDB PMSL WDIR WSPD '
                CALL UFBINT (LUBFR, ARR, 10, 255, NRET, STRING)
                XTEMP1(6) = ARR(2,1)
                XTEMP1(7) = RMISS
                XTEMP1(8) = ARR(3,1)
                XTEMP1(9) = ARR(4,1)
                XTEMP1(10)= ARR(1,1)
                XTEMP1(11)= RMISS
                XTEMP1(12)= RMISS
                xtemp1(5) = 532.
              ENDIF
C----------------------------------------------------------------------|
C           CHECK FOR MISSING VALUES
C----------------------------------------------------------------------|
              DO JJ = 4,12
                IF(XTEMP1(JJ).EQ.RMISS) XTEMP1(JJ) = 999999.0
              ENDDO
C----------------------------------------------------------------------|
C           CONVERT SEA SURFACE TEMPERATURE FROM K TO C AND 
C           MULTIPLY TIMES 10 FOR OUTPUT. COPY SST TO OBS(4).
C           COPY REPORT TYPE TO OBS(5).
C----------------------------------------------------------------------|
              IF(XTEMP1(4).lt.999998.) 
     *          XTEMP1(4) = (XTEMP1(4) - 273.15) * 10.0
              obs(4) = nint(xtemp1(4)) 
              obs(5) = xtemp1(5)
C----------------------------------------------------------------------|
C           CONVERT MSLP FROM PASCALS (db) TO HECTOPASCALS (mb) AND
C           MULTIPLY TIMES 10 FOR OUTPUT. DITTO STA PRESSURE.
C           MSLP = (MSLP * 0.01) * 10.0
C           NOTE: CONVERTING TO MB AND THEN MULTIPLYING BY 10 IS THE
C           SAME AS MULTIPLYING PASCALS BY 0.1, SO DO THIS.
C           COPY SLP AND STA PRESS TO OBS(6) AND OBS(7).
C----------------------------------------------------------------------|
              IF(XTEMP1(6).lt.999998.) XTEMP1(6) = XTEMP1(6) * 0.1
              IF(XTEMP1(7).lt.999998.) XTEMP1(7) = XTEMP1(7) * 0.1
              obs(6) = nint(xtemp1(6))
              obs(7) = nint(xtemp1(7))
C----------------------------------------------------------------------|
C           CONVERT WIND SPEED FROM M/S TO KTS AND COPY WDIR AND
C           WSPD TO OBS ARRAY.
C----------------------------------------------------------------------|
              IF(XTEMP1(9).lt.999998.) XTEMP1(9) = XTEMP1(9) * 1.94254
              obs(8) = xtemp1(8)
              obs(9) = nint(xtemp1(9))
C----------------------------------------------------------------------|
C           CONVERT DRY BULB TEMPERATURE AND DEW POINT FROM K TO C AND 
C           MULTIPLY TIMES 10 FOR OUTPUT. COPY TO OBS(10) AND OBS(11).
C----------------------------------------------------------------------| 
              IF(XTEMP1(10).lt.999998.) 
     *          XTEMP1(10) = (XTEMP1(10) - 273.15) * 10.0
              IF(XTEMP1(11).lt.999998.) 
     *          XTEMP1(11) = (XTEMP1(11) - 273.15) * 10.0
              OBS(10) = NINT(XTEMP1(10))
              OBS(11) = NINT(XTEMP1(11))
C----------------------------------------------------------------------|
c           convert horiz. vis. from meters to ship's code card value
c           (needed by quips).
C           COPY HORIZ. VIS. TO OBS(12).
C----------------------------------------------------------------------|
              if(xtemp1(12).lt.999998.) then
                if(xtemp1(12).LT.50.0) obs(12) = 90
                if(xtemp1(12).ge.50.0.and.xtemp1(12).lt.200.0)
     *             obs(12) = 91
                if(xtemp1(12).ge.200.0.and.xtemp1(12).lt.500.0)
     *             obs(12) = 92
                if(xtemp1(12).ge.500.0.and.xtemp1(12).lt.1000.0)
     *             obs(12) = 93
                if(xtemp1(12).ge.1000.0.and.xtemp1(12).lt.2000.0)
     *             obs(12) = 94
                if(xtemp1(12).ge.2000.0.and.xtemp1(12).lt.4000.0)
     *             obs(12) = 95
                if(xtemp1(12).ge.4000.0.and.xtemp1(12).lt.10000.0)
     *             obs(12) = 96
                if(xtemp1(12).ge.10000.0.and.xtemp1(12).lt.20000.0)
     *             obs(12) = 97
                if(xtemp1(12).ge.20000.0.and.xtemp1(12).lt.50000.0)
     *             obs(12) = 98
                if(xtemp1(12).ge.50000.0) obs(12) = 99
              else
                OBS(12) = XTEMP1(12)
              endif
C-----------------------------------------------------------------------
C  OBS(13) = PRESENT WX, OBS(14) = ICE ACCRETION (DEPTH)
C  OBS(15) = TOTAL CLOUD COVER.
C  FOR DRIFTING BUOYS AND TIDE GAUGE, ALL THESE ARE MISSING.
C  FOR MOORED BUOYS AND CMAN, TOCC AND IDTH ARE MISSING.
C-----------------------------------------------------------------------
              IF(I.EQ.1) THEN
C               ship         
                STRING = ' TOCC PRWE '
                CALL UFBINT (LUBFR, ARR, 10, 255, NRET, STRING)
                XTEMP1(13) = ARR(2,1)
                XTEMP1(15) = ARR(1,1)
              ELSEIF(I.EQ.2.OR.I.EQ.5) THEN
C               dbuoy or tide gauge
                XTEMP1(13) = RMISS
                XTEMP1(15) = RMISS
              ELSEIF(I.EQ.3.OR.I.EQ.4) THEN
C               mbuoy or cman
                STRING = ' PRWE '
                CALL UFBINT (LUBFR, ARR, 10, 255, NRET, STRING)
                XTEMP1(13) = ARR(1,1)
                XTEMP1(15) = RMISS
              ENDIF
C-----------------------------------------------------------------------
c  GET ICE ACCRETION (DEPTH) IN METERS.
C-----------------------------------------------------------------------
              IF(I.EQ.1) THEN
C               ship         
                STRING = ' IDTH '
                CALL UFBINT (LUBFR, ARR, 10, 255, NRET, STRING)
                XTEMP1(14) = ARR(1,1)
              ELSE
c               dbuoy, mbuoy, cman, or tide gauge
                XTEMP1(14) = RMISS
              ENDIF
C----------------------------------------------------------------------|
C           CHECK FOR MISSING VALUES
C           IF ICE DEPTH NOT MISSING, MULTIPLY IDTH IN METERS BY 100 
C           AND ROUND TO NEAREST WHOLE INT BEFORE STORING.
C----------------------------------------------------------------------|
              DO JJ = 13,15
                IF(XTEMP1(JJ).EQ.RMISS) XTEMP1(JJ) = 999999.0
              ENDDO
              IF(XTEMP1(14).lt.999998.) 
     *          XTEMP1(14) = XTEMP1(14) * 100.0
              OBS(13) = XTEMP1(13)
              OBS(14) = NINT(XTEMP1(14))
C----------------------------------------------------------------------|
c           convert cloud cover from percent back to ship's code card
c           value.
C----------------------------------------------------------------------|
              if(xtemp1(15).lt.999998.0) then
                if(xtemp1(15).eq.0.) obs(15) = 0
                if(xtemp1(15).ge.12.5.and.xtemp1(15).lt.25.)
     *             obs(15) = 1
                if(xtemp1(15).ge.25.0.and.xtemp1(15).lt.37.5)
     *             obs(15) = 2
                if(xtemp1(15).ge.37.5.and.xtemp1(15).lt.50.)
     *             obs(15) = 3
                if(xtemp1(15).ge.50.0.and.xtemp1(15).lt.62.5)
     *             obs(15) = 4
                if(xtemp1(15).ge.62.5.and.xtemp1(15).lt.75.)
     *             obs(15) = 5
                if(xtemp1(15).ge.75.0.and.xtemp1(15).lt.87.5)
     *             obs(15) = 6
                if(xtemp1(15).ge.87.5.and.xtemp1(15).lt.100.)
     *             obs(15) = 7
                if(xtemp1(15).eq.100.) obs(15) = 8
              else
                OBS(15) = XTEMP1(15)
              endif
C-----------------------------------------------------------------------
C  OBS(21) = 3 HR PRESS TENDENCY.
C  OBS(22) = 3 HR PRESS CHANGE IN PASCALS.
C-----------------------------------------------------------------------
              IF(I.LT.5) THEN
c               ship, dbuoy, mbuoy, cman
                STRING = ' CHPT 3HPC '
                CALL UFBINT (LUBFR, ARR, 10, 255, NRET, STRING)
                XTEMP1(21) = ARR(1,1)
                XTEMP1(22) = ARR(2,1)
              ELSE
c               tide gauge                
                XTEMP1(21) = RMISS   
                XTEMP1(22) = RMISS   
              ENDIF
C----------------------------------------------------------------------|
C           CHECK FOR MISSING VALUES
C----------------------------------------------------------------------|
              DO JJ = 21,22
                IF(XTEMP1(JJ).EQ.RMISS) XTEMP1(JJ) = 999999.0
              ENDDO
C----------------------------------------------------------------------|
C           CONVERT 3HPC FROM PASCALS (db) TO HECTOPASCALS (mb) AND
C           MULTIPLY TIMES 10 FOR OUTPUT. 
C           3HPC = (3HPC * 0.01) * 10.0
C           NOTE: CONVERTING TO MB AND THEN MULTIPLYING BY 10 IS THE
C           SAME AS MULTIPLYING PASCALS BY 0.1, SO DO THIS.
c           Pressure change amount has sign (+/-),
c           we don't want this so do abs value.
C           COPY 3HPC AND PRESS. TENDENCY TO OBS(22) AND OBS(21).
C----------------------------------------------------------------------|
              IF(XTEMP1(22).lt.999998.) 
     *          XTEMP1(22) = ABS(XTEMP1(22) * 0.1)
              obs(22) = nint(xtemp1(22))
              OBS(21) = XTEMP1(21)
C-----------------------------------------------------------------------
C     OBS(16) = WIND WAVE HGT IN METERS
C     OBS(18) = WIND WAVE PERIOD IN SECONDS
C     NOTE: WIND WAVES NOT AVAILABLE FROM DRIFTING BUOYS OR TIDE GAUGE.
C     COPY WIND WAVE HGT AND PERIOD INTO OBS(16) AND (18).
C-----------------------------------------------------------------------
              IF(I.NE.2.AND.I.NE.5) THEN
                STRING = ' POWW HOWW '
                CALL UFBINT (LUBFR, ARR, 10, 255, NRET, STRING)
                XTEMP1(16) = ARR(2,1)
                XTEMP1(18) = ARR(1,1)
              ELSE
                XTEMP1(16) = RMISS
                XTEMP1(18) = RMISS
              ENDIF
C----------------------------------------------------------------------|
C           CHECK FOR MISSING VALUES
C----------------------------------------------------------------------|
              IF(XTEMP1(16).EQ.RMISS) XTEMP1(16) = 999999.0
              IF(XTEMP1(18).EQ.RMISS) XTEMP1(18) = 999999.0
C----------------------------------------------------------------------|
C           CONVERT WIND WAVE HGT TO HALF METERS
C----------------------------------------------------------------------|
              IF(XTEMP1(16).lt.999998.) XTEMP1(16) = XTEMP1(16) * 2.0
              OBS(16) = NINT(XTEMP1(16))
              OBS(18) = XTEMP1(18)
C-----------------------------------------------------------------------
C     OBS(17) = SWELL HGT IN METERS
C     OBS(19) = SWELL PERIOD IN SECONDS
C     OBS(20) = SWELL DIRECTION (DEGREES FROM TRUE)
C     NOTE: SWELL WAVE INFO NOT AVAILABLE FROM DRIFTING BUOYS OR
C     TIDE GAUGE.
C-----------------------------------------------------------------------
              IF(I.EQ.1.OR.I.EQ.3.OR.I.EQ.4) THEN
c               ship, moored buoy, or CMAN
                STRING = ' DOSW POSW HOSW '
                CALL UFBINT (LUBFR, ARR, 10, 255, NRET, STRING)
                IF (NRET .GE. 1) THEN
                  XTEMP1(17) = ARR(3,1)
                  XTEMP1(19) = ARR(2,1)
                  XTEMP1(20) = ARR(1,1)
                ELSE
                  XTEMP1(17) = RMISS
                  XTEMP1(19) = RMISS
                  XTEMP1(20) = RMISS
                ENDIF
              ELSE
c               drifting buoy or tide gauge.
                XTEMP1(17) = RMISS
                XTEMP1(19) = RMISS
                XTEMP1(20) = RMISS
              ENDIF
C-----------------------------------------------------------------------
C           CHECK FOR MISSING VALUES
C-----------------------------------------------------------------------
              IF(XTEMP1(17).EQ.RMISS) XTEMP1(17) = 999999.0
              IF(XTEMP1(19).EQ.RMISS) XTEMP1(19) = 999999.0
              IF(XTEMP1(20).EQ.RMISS) XTEMP1(20) = 999999.0
C-----------------------------------------------------------------------
C           CONVERT SWELL WAVE HGT TO HALF METERS
C-----------------------------------------------------------------------
              IF(XTEMP1(17).lt.999998.) XTEMP1(17) = XTEMP1(17) * 2.0
              OBS(17) = NINT(XTEMP1(17))
              OBS(19) = XTEMP1(19)
              OBS(20) = XTEMP1(20)
C-----------------------------------------------------------------------
C           SET MISSING DATA TO -99.  if any one parameter has valid
c           data, set logical anygood to true and write data.  if all
c           data are missing, don't write data.  Note:  check obs(4) (SST)
c           separately, since obs(5) will never be missing.
C-----------------------------------------------------------------------
              anygood = .false.
              IF(OBS(4).GE.999998.0) then
                OBS(4) = -99
              else
                anygood = .true.
              endif
              DO IJ = 6, 22
                IF(OBS(IJ).GE.999998.0) then
                  OBS(IJ) = -99
                else
                  anygood = .true.
                endif
              enddo
C-----------------------------------------------------------------------
C            WRITE SFCSHP (fort.20) REPORT
C-----------------------------------------------------------------------
              if(anygood) then
                WRITE(ioutunit,1015) OBS(1),OBS(2),OBS(3),SHIPID,
     *           OBS(4),OBS(5),OBS(6),OBS(7),OBS(8),OBS(9),OBS(10),
     *           OBS(11),OBS(12),OBS(13),OBS(14),OBS(15),OBS(16),
     *           OBS(17),OBS(18),OBS(19),OBS(20),OBS(21),OBS(22)
 1015           FORMAT(3I6,4X,A8,6I6/13I6)
              endif
            endif
            IF(IRET2.EQ.0) GO TO 170
          ENDIF
          GO TO 160
        endif
 100  CONTINUE
 999  PRINT 1020, NOBS,NSKIP
 1020 FORMAT(' TOTAL OBS=',I8,' SKIPPED OBS=',I8)
C-----------------------------------------------------------------------
C               IF NO NEW REPORTS - STOP 300
C-----------------------------------------------------------------------
      IF(NOBS.EQ.0) iexit = 300
      CALL W3TAGE('HRLY_RDSHPMPC')
      CALL ERREXIT(IEXIT)
      CALL W3TAGE('HRLY_RDSHPMPC')
      STOP
      END
C=======================================================================
      SUBROUTINE BLDID(ID,IYR4D,IMO,IDA,ICYC,ihr,imin,IER)
C$$$  SUBPROGRAM DOCUMENTATION  BLOCK
C                                                                    .
C SUBPROGRAM:    BLDID       BUILD ID RECORD
C   PRGMMR: CARUSO           ORG: W/NP12     DATE: 1998-12-03
C
C ABSTRACT: BUILDS IDENTIFICATION RECORD FOR COMPASS FILES.
C
C PROGRAM HISTORY LOG:
C   87-09-02  PERROTTI ORIGINAL AUTHOR
c   98-12-03  CARUSO   changing to have 4 digit year in header
c                      (characters 'C2' that were in columns 7-8 are no 
c                      longer used, so replace them with first 2 digits 
c                      of year).
C
C USAGE:    CALL BLDID(ID,IYR4D,IMO,IDA,ICYC,ihr,imin,IER)
C   INPUT ARGUMENT LIST:
c     iyr4d    - 4 digit year
c     imo      - month
c     ida      - day
c     icyc     - cycle
c     ihr      - hour
c     imin     - minute
C
C   OUTPUT ARGUMENT LIST:
C     ID       - 80 BYTE ID RECORD
C     IER      - ERROR RETURN CODE
C                0: GOOD RETURN
C
C   SUBPROGRAMS CALLED:
C     none
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  cray
C
C$$$
C
      CHARACTER*80 ID
      CHARACTER*80 OD
      DATA OD(1:34)/'OD0000C2999999999999WASHINGTONNMC2'/
      SAVE
      IER = 0
C-----------------------------------------------------------------------
C                     PRESERVE ID PRE-FORMAT
C-----------------------------------------------------------------------
      do i = 35,80
        od(i:i) = ' '
      enddo
c###check with paula to see if I need this L at the end (remnant!)
      od(72:72) = 'L'  
      ID = OD
      ID(1:6) = 'QC0001'
C-----------------------------------------------------------------------
C                   STORE DATE AND TIME IN ID
C-----------------------------------------------------------------------
      WRITE(ID(7:10),'(I4.4)')  IYR4D
      WRITE(ID(11:12),'(I2.2)') IMO
      WRITE(ID(13:14),'(I2.2)') IDA
      WRITE(ID(15:16),'(I2.2)') ICYC
      WRITE(ID(17:18),'(I2.2)') IHR
      WRITE(ID(19:20),'(I2.2)') Imin
      RETURN
      END
