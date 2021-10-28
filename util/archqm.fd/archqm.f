C$$$  MAIN PROGRAM DOCUMENTATION BLOCK
C
C MAIN PROGRAM: ARCHQM
C   PRGMMR: CARUSO           ORG: NP12        DATE: 2000-06-27
C
C abstract:  retrieves the fnl prepda file and extracts the quality marks
C   for sfc marine reports 4 times daily (00/06/12/18 GMT).  The output file
C   is used to generate monthly statistics for the Data Buoy Cooperation 
C   Panel.                                           
C
C program history log:
c   97-09-xx  Caruso - original author
c   99-09-xx  caruso - converting to ibm. 
c 2000-06-14  caruso magee - comment out calls to w3tagb, w3tage for
c               parallel run of this code until it goes into prod.
c               change lower case chars in callsigns to uppercase.
c               add err messaging.
c 2000-06-14  caruso magee - uncomment calls to w3tagb and w3tage.
c
c usage:
c   input files:
c     fort.11  - fnl prepda file  
c
c   output files:
c     fort.61  - sfc_marine.qmarch.curr_mon
c
c   subprograms called:
c     unique:  
c     library:
c       w3lib90:    w3utcdat  w3tagb  w3tage
c       bufrlib90:  openbf  readmg  readsb  ufbint closbf
c       system:     errexit consol
c
c   exit states:
c     cond = 0 - successful run
c          = 2 - error - no sfcshp subsets found in fnl prepda file.
c
c remarks:
c   this was previously only run in checkout
c
c attributes:
c   language:  fortran 90
c   machine:  ibm
c
c$$$

      common/dateln/lendat
      integer lendat
                                                                                
      INTEGER  IDATE             
      integer  jdat(8)
      integer  ndaymon(12)
      data     ndaymon/31,28,31,30,31,30,31,31,30,31,30,31/

      character*1 lowercase(26), uppercase(26)
      CHARACTER*8  SUBSET, SHIPID
      character*10 cdate
      character*8  inout

      CHARACTER*80 STRING
      REAL   ARR(10,255), XTEMPID
      
      REAL   RMISS
      data   RMISS/1.E10/

      EQUIVALENCE(XTEMPID,SHIPID)

      integer(4)  narg, iargc, numarg
      character*8 cbuf
      integer     icyc
      integer     ishpcnt

      integer     lubfr, ioutlun
      DATA        LUBFR/11/, IOUTLUN/61/                               

      character*33 endmsg
      character*33 errmsg(2)
      data errmsg
     */'ARCHQM COMPLETED SUCCESSFULLY:   ',
     * 'ERROR NO SFCSHP FOUND IN PREPDA: '/

      iexit = 0
      call w3tagb('ARCHQM',2000,0179,0057,'NP12')
C-----------------------------------------------------------------------
C                  GET DATE/TIME FOR HEADER
C-----------------------------------------------------------------------
      call w3utcdat(jdat)
      print 1000, jdat
 1000 FORMAT('1', 5X,'***** ARCHQM BEGAN EXECUTION *****'/10X,8i4//)
C-----------------------------------------------------------------------
c  get cycle time from command line
C-----------------------------------------------------------------------
      narg = iargc()
      do numarg = 1,narg
        call getarg(numarg,cbuf)
      enddo
      read(cbuf,555) icyc
 555  format(i2)
      print*,' icyc from command line = ',icyc
c-----------------------------------------------------------------------
c  set up uppercase and lowercase arrays
c-----------------------------------------------------------------------
      do jj = 1,26
        lowercase(jj) = char(96+jj)
        uppercase(jj) = char(64+jj)
      enddo 
C-----------------------------------------------------------------------
c                open unit 61 so we can append data to it.
C-----------------------------------------------------------------------
            open(ioutlun,position='append')
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
      inout = 'IN'
      lundx = lubfr
      CALL OPENBF(LUBFR,inout,lundx)
      iret1 = 0                                           
      ishpcnt = 0
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
c                                                                      |
C     FIND SUBSET 'SFCSHP' (HOLDS MARINE DATA)                         |                        
C----------------------------------------------------------------------|
 160  continue
      lendat = 10
      CALL READMG(LUBFR,SUBSET,IDATE,IRET1)
      if(iret1.eq.-1) then                                                
C----------------------------------------------------------------------|
C     3a. WE HAVE REACHED THE END-OF-FILE.  PRINT THAT FACT AND QUIT
C         IF LAST UNIT WAS READ. 
C----------------------------------------------------------------------|
        print*,' '
        print*,' read EOF '
        go to 999
      elseif(iret1.eq.0) then
C----------------------------------------------------------------------|
C     3b. WE HAVE READ A BUFR MESSAGE.                                 |
C     SPLIT DATE UP INTO COMPONENTS.                          
C     Look for surface marine data only.
c     Save integer idate into character cdate for later use.
C----------------------------------------------------------------------|
        IF(SUBSET.NE.'SFCSHP') THEN
          go to 160
        ELSE
          ishpcnt = ishpcnt + 1
        ENDIF
        WRITE(CDATE,fmt='(I10)') IDATE   
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
        iret2 = 0
 170    CALL READSB(LUBFR,IRET2)                                                
        if(iret2.eq.0) THEN                                                  
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
C                                                                               
C     FIRST GET ID, HR, LAT, LONG. 
C----------------------------------------------------------------------|
          STRING = ' SID DHR YOB XOB '
          CALL UFBINT(LUBFR,ARR,10,255,NRET,STRING)                                
          DHOUR = ARR(2,1)*100.
          XLAT = ARR(3,1)
          XLON = ARR(4,1)
          XLAT1 = XLAT * 100.
          XLON1 = XLON * 100.
          IF(XLON1.GT.0.) THEN
            XLON1 = 36000. - XLON1
          ELSEIF(XLON1.LT.0) THEN
            XLON1 = -XLON1
          ENDIF
          IF(XLON1.GT.36000.0.OR.XLON1.LT.0.) GO TO 170
          XTEMPID = ARR(1,1)
c----------------------------------------------------------------------
c  check for lowercase letters in shipid.  convert to uppercase where
c  found.
c----------------------------------------------------------------------
          do jk = 1,8
            do kk = 1,26
              if(shipid(jk:jk).eq.lowercase(kk)) then
                 shipid(jk:jk) = uppercase(kk)
              endif
            enddo
          enddo 
C----------------------------------------------------------------------|
C  ASSEMBLE OBS TIME.  DHOUR = OBS TIME - CYCLE TIME
C----------------------------------------------------------------------|
          idhour = nint(dhour)
          read(cdate(7:8),fmt='(i2)') iday
          read(cdate(5:6),fmt='(i2)') iimon
          read(cdate(1:4),fmt='(i4)') iiyear
          if(idhour.lt.0.and.icyc.eq.0) then  !obs is from previous day
            ihour = 2400 + idhour
            if(iday.ne.1) then       !not the first day of the month
              iiday = iday - 1
            else                     !first day of the month
              nmon = iimon
              if(nmon.ne.1) then     !not January
                iiday = ndaymon(nmon-1)
                iimon = nmon - 1
                if((mod(iiyear,4).eq.0).and.nmon.eq.3)  then

C                  Add one to day if leap year and current date is Mar. 1

                   iiday = iiday + 1 
                endif
              else                   !current date is Jan. 1    
                iiday = 31           !day
                iimon = 12           !month
                iiyear = iiyear - 1  !year
              endif
            endif
          else
            IHOUR = IDHOUR + icyc * 100
            iiday = iday
          endif
          iobdatim = iiyear * 100000000 + iimon * 1000000 +
     *      iiday * 10000 + ihour
C----------------------------------------------------------------------|
C  GET PRESSURE AND QMARK, WIND AND QMARK, AND TEMP AND QMARK.
C----------------------------------------------------------------------|
          STRING = ' DDO FFO WQM TOB TQM POB PQM '
          CALL UFBINT(LUBFR,ARR,10,255,NRET,STRING)                                
          WDDIR = ARR(1,1)
          WDSPD = ARR(2,1)
          WDQM = ARR(3,1)
          ATMP = ARR(4,1) 
          ATQM = ARR(5,1)
          PMSL = ARR(6,1)
          PMQM = ARR(7,1)
C----------------------------------------------------------------------|
c  check for missing values and replace with -999.
C----------------------------------------------------------------------|
          if(WDDIR.ge.rmiss) wddir = -999.
          if(WDSPD.ge.rmiss) wdspd = -999.
          if(WDQM.ge.rmiss) wdqm = -999.
          if(ATMP.ge.rmiss) atmp = -999.
          if(ATQM.ge.rmiss) atqm = -999.
          if(PMSL.ge.rmiss) pmsl = -999.
          if(PMQM.ge.rmiss) pmqm = -999.
C----------------------------------------------------------------------|
c  write to output file
C----------------------------------------------------------------------|
          write(unit=ioutlun,fmt=1001,iostat=ios) shipid, iobdatim,
     *                   xlat1, xlon1, pmsl, pmqm, atmp,
     *                   atqm, wddir, wdspd, wdqm
 1001     format(a8,2x,i12,1x,f6.0,1x,f6.0,1x,2(f6.1,1x,f5.0),f6.1,
     *           1x,f6.1,1x,f5.0)
        endif
        if(iret2.eq.0) go to 170
      endif                                                                                
      GO TO 160                                                                                
C----------------------------------------------------------------------|
C     COME HERE WHEN ALL REPORTS HAVE BEEN READ IN -- CLOSE FILE                
C     AND QUIT.                                                                 
C----------------------------------------------------------------------|
  999 CONTINUE                                                                  
      CALL CLOSBF(LUBFR)                                                       

      if( ishpcnt .eq. 0 ) then
        iexit = 2
        print*,' No sfcshp subsets found in fnl prepda file'
        endmsg = errmsg(iexit)
      else
        print*,' Found ',ishpcnt,' sfcshp subsets!'
        iexit = 0
        PRINT 1007
 1007   FORMAT(///5X,'***** ARCHQM COMPLETED EXECUTION *****')
        endmsg = errmsg(1)
      endif
                                                                                
C      call consol(endmsg)
      call w3tage('ARCHQM')
      call errexit(iexit)
      call w3tage('ARCHQM')
      STOP                                                                      
      END                                                                       
