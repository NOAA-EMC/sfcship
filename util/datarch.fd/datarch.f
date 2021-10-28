C$$$  MAIN PROGRAM DOCUMENTATION BLOCK
C
C MAIN PROGRAM: DATARCH
C   PRGMMR: CARUSO           ORG: NP12        DATE: 2001-06-05
C
C abstract:  WRITES QC STATS TO FILE: sfc_marine.archive.curr_mon
C   4 TIMES EACH DAY (00/06/12/18 GMT).  THIS FILE IS
C   USED TO GENERATE MONTHLY SUMMARIES FOR BRACKNELL,
C   ENGLAND and for the DATA BUOY COOPERATION PANEL.
C
C program history log:
c   91-11-xx  Fred Marshall original author
c   95-05-01  Caruso - ADDED ANOTHER OUTPUT FILE SIMILAR TO
C               .OPNL BUT WITH ENTIRE PRESSURE SAVED IF LT 1000 MB.
C   95-11-16  Caruso - FIXED TIME CHECK FOR 03Z MEMBER. SOME
C               REPORTS WERE FROM 23Z ON THE PREVIOUS DAY, SO WERE
C               VALID FOR THE PREVIOUS SYNOPTIC PERIOD.
c   96-07-15  caruso - Copied model retrieval code from
c               newtestfld, which works with grib models on
c               cray4.  No longer using sst analysis; using sfc
c               temp fcsts, which are skin temps for the globe.
c               Got rid of gtsst and rdsst as a result of this.
c               Also, got rid of rdgdas and rdavn and replaced
c               them with rdfld, which reads both avn and gdas.
c	        Renamed this code datarch - Data Archive.
c   97-10-20  caruso - added corrective code for y2k
c   97-11-21  caruso - adding code to compute and save
c               additional wind dir stats (in addition to
c               (obs - fg), will now also compute ((obs - 90) - fg),
c               ((obs + 90) - fg), & ((obs + 180) - fg)).  needed
c               for buoy stats.
c   99-04-15  caruso - switching to use 1x1 degree grib 
c               model fields.
c   99-08-30  caruso - converting to ibm.  add calls to baopenr for
c               grib files.
c 2000-02-02  caruso magee - comment out calls to w3tagb, w3tage for
c               parallel run of this code until it goes into prod.
c               change unprintable chars and blanks in callsigns to
c               slashes to help with sort on IBM (was getting confused
c               by blanks embedded within callsigns).
c 2000-03-27  caruso magee - uncomment calls to w3tagb and w3tage.
c 2000-08-16  caruso magee - add tide gauge data to list of those to be
c               archived.
c 2000-09-06  caruso magee - modify callsign char check to change illegal chars
c                            that have ascii number gt 122 to / like I do with
c                            all other illegal chars (was still being set to
c                            a ' ') (see s/r RDSHP). 
c 2001-06-05  caruso magee - Add bulletin header to output file. Needed to
c                            separate Intl. SeaKeepers reports from other
c                            reports from the same callsign (i.e. bridge 
c                            reports) which come in w/ a diff. bull. hdr.       
c                            Also, moved code which checks to see if
C                            obs is valid for next synoptic period to a more
c                            reasonable location (right after obs time is 
c                            read), as opposed to reading lat/long, etc. first.
c 2002-03-25  caruso magee - Replace ICLI with new mnemonic BORG.         
c 2003-03-19  caruso magee - Removed prints that were commented out and fixed
C                            an error in s/r gtdif (see gtdif docblock for
C                            details). Corrected name of output file (unit 41).
c
c usage:
c   input files:
c     fort.20  - surface ship data
c     fort.21  - drifting buoy data
c     fort.22  - moored buoy data
c     fort.23  - cman data
c     fort.24  - tide gauge data
c     fort.11  - grib file (either avn or gdas) 
c     fort.12  - backup grib file (avn) (may not be used)
c     fort.31  - grib index file (if backup is used, then fort.32)
c     fort.32  - backup grib index file (if fort.12 is used)
c
c   output files:
c     fort.41  - sfc_marine.archive.curr_mon 
c
c   subprograms called:
c     unique:  iw3getpop  gtgdas  gtavn  gtwsd  rdshp  gtdif  rdfld
c     library:
c       w3lib90:    w3utcdat  w3ft03  baopenr  w3fc05  getgb  sbytes  gbytes 
c                   getgbp    w3tagb  w3tage
c       bacio:      baopenr
c       bufrlib90:  openbf  readmg  readsb  ufbint
c       system:     errexit get_environment_variable
c    
c   exit states:
c     cond = 0 - successful run
c          = 1 - error getting first guess flds. abort.
c
c remarks:
c   this was previously only run in checkout
c
c attributes:
c   language:  fortran 90
c   machine:  ibm
c
c$$$
c
      character*4 model, cmodel
      character*8 cbuf(2)
      integer(4)  narg, iargc, numarg 
      integer(8)  iexit
      INTEGER     IDAT(8)
      INTEGER     ICYC
      REAL        FLD(65160,6)    !360x181

c      character*11 envvar
c      data         envvar/'XLFUNIT_   '/
      character*6 envvar
      data         envvar/'FORT  '/
      character*80 cfilnam
      integer      iunit(5)
      data         iunit/20, 21, 22, 23, 24/
      logical      am_i_here
      logical      shiphere, dbuoyhere, mbuoyhere, cmanhere, tideghere
      data         shiphere /.false./
      data         dbuoyhere/.false./
      data         mbuoyhere/.false./
      data         cmanhere /.false./
      data         tideghere /.false./

      character*33 endmsg
      character*33 errmsg(2)
      data errmsg
     */'DATARCH COMPLETED SUCCESSFULLY:  ',
     * 'ERROR NO FIRST GUESS FIELD FOUND:'/
      DATA MODEL/'XXXX'/
      logical lbkup

      iexit = 0
      call w3tagb('DATARCH',2003,0137,0060,'NP12')
C-----------------------------------------------------------------------
C                  GET DATE/TIME FOR HEADER
C-----------------------------------------------------------------------
      call w3utcdat(idat)
 1000 FORMAT('1', 5X,'***** DATARCH BEGAN EXECUTION *****'/10X,8i4//)
      print 1008, idat
 1008 format(' integer date is ',8i4//)
C-----------------------------------------------------------------------
C                  get CYCLE TIME and cmodel (which model we're using)
C-----------------------------------------------------------------------
      narg = iargc()
      do numarg = 1,narg
        call getarg(numarg,cbuf(numarg))
      enddo 
      read(cbuf(1),555) icyc
 555  format(i2)
      print*,' icyc from command line = ',icyc
      read(cbuf(2),556) cmodel
 556  format(a4)
      print*,' cmodel from command line = ',cmodel
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
          print 861, ii, cfilnam
 861      format(' found file ',i2,' name = ',a80)
          if(ii.eq.1) shiphere = .true.
          if(ii.eq.2) dbuoyhere = .true.
          if(ii.eq.3) mbuoyhere = .true.
          if(ii.eq.4) cmanhere = .true.
          if(ii.eq.5) tideghere = .true.
        endif
      enddo
      if(.not.shiphere.and..not.dbuoyhere.and.
     *   .not.mbuoyhere.and..not.cmanhere.and..not.tideghere) then
        write(6,1140)
 1140   FORMAT(' **** ERROR NO INPUT BUFR FILES PRESENT')
        go to 999
      endif
C-----------------------------------------------------------------------
c  GET GDAS FIELDS
C-----------------------------------------------------------------------
      if(cmodel.eq.'GDAS') then
        LBKUP = .false.
        CALL GTGDAS(FLD,icyc,LBKUP,idat,IGDASERR)
        MODEL = 'GDAS'
        IF(IGDASERR.NE.0) then
C-----------------------------------------------------------------------
C                      GET AVN FIELDS
C-----------------------------------------------------------------------
          print*,' no gdas fields...trying for avn fields now'
          LBKUP = .true.
          CALL GTAVN(FLD,icyc,LBKUP,idat,IAVNERR)
          IF(IAVNERR.NE.0) then
            PRINT 1002
            iexit = 2
            go to 999
          else
            MODEL = 'AVN '
          endif
        endif
      elseif(cmodel(1:3).eq.'AVN') then
        LBKUP = .true.
        CALL GTAVN(FLD,icyc,LBKUP,idat,IAVNERR)
        IF(IAVNERR.NE.0) then
          PRINT 1002
          iexit = 2
          go to 999
        else
          MODEL = 'AVN '
        endif
      endif
 1002 FORMAT(' ****  data archive RUN FAILED - NO GDAS/AVN DATA ****')

  889 continue
      PRINT 1004, MODEL,idat(2),idat(3),idat(1),icyc
 1004 FORMAT(' USING MODEL: ',A4,' FOR: ',2(I2,'/'),I4,2X,I2,'Z CYCLE'/)
C-----------------------------------------------------------------------
C                  CONVERT U & V COMPONENTS TO WSPD and direction
C-----------------------------------------------------------------------

      CALL GTWSD(FLD)

C-----------------------------------------------------------------------
C                   SET WAVE HEIGHT fld TO MISG
C-----------------------------------------------------------------------

      DO I = 1,65160
        FLD(I,6) = 999999.
      enddo

C-----------------------------------------------------------------------
C                      GET SHIP REPORTS
C-----------------------------------------------------------------------

      CALL RDSHP(shiphere,dbuoyhere,mbuoyhere,cmanhere,tideghere,
     *           FLD,icyc)
 999  continue
      if(iexit.ne.0) then
        print*,' DATARCH FAILED'
        endmsg = errmsg(iexit)
      else
        PRINT 1007
 1007   FORMAT(///5X,'***** DATARCH COMPLETED EXECUTION *****')
        endmsg = errmsg(1)
      endif
C      call consol(endmsg)
      call w3tage('DATARCH')
      call errexit(iexit)
      call w3tage('DATARCH')
      STOP
      END
C=======================================================================
      SUBROUTINE RDSHP(shiphere,dbuoyhere,mbuoyhere,cmanhere,tideghere,
     *                 FLD,icyc)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM: rdshp      read sfc marine data 
C   PRGMMR: caruso           ORG: NP12        DATE: 2001/06/05 
C
C abstract:  reads surface marine data
C
C program history log:
c 1990-02-xx  Fred Marshall -  original author
c 1993-05-18  chris caruso - added loop to read in additional input
c                            data file containing intermediate data
c                            from nas (03, 09, 15, 21Z).
c 1996-08-02  chris caruso - changed this s/r to use bufr data files
c                            on cray.
c 1999-04-15  chris caruso - modified call to gtdif to use w3ft03 for
c                            interpolation. 
c 2000-08-16  caruso magee - add tideghere to input arg list and read/archive
c                            tide gauge data along with other sfc marine data.
c 2000-09-06  caruso magee - modify callsign char check to change illegal chars
c                            that have ascii number gt 122 to / like I do with
c                            all other illegal chars (was still being set to
c                            a ' '). 
c 2001-01-16  caruso magee - add check on model gridpt. out of range. if so,   
c                            subtract 360 to put back onto grid (will only 
c                            occur if lon = 0 somehow slides thru or roundoff
c                            makes sti = 361.).  Add check to convert lon = 0
c                            to lon = 360 to correct same issue (was computing
c                            model gridpt. of 361 if lon = 0, and horiz. res.
c                            of model is only 360 (prime meridian not dupli-  
c                            cated).  
c 2001-06-05  caruso magee - Add bulletin header to output file. Needed to
c                            separate Intl. SeaKeepers reports from other
c                            reports from the same callsign (i.e. bridge 
c                            reports) which come in w/ a diff. bull. hdr.       
c                            Also, moved code which checks to see if
C                            obs is valid for next synoptic period to a more
c                            reasonable location (right after obs time is 
c                            read), as opposed to reading lat/long, etc. first.
c 2003-03-21  caruso magee - fixing botched repair of grid point problem made
c                            on 2001-01-16.  Subtracting 360 from sti if sti   
c                            was .gt. 360 still kept the point off the grid, 
c                            but put it west of g.p. 1 instead of east of g.p.
c                            360.  Fix is to pass in sti as is and add g.p. 361
c                            (duplicate prime meridian) in s/r GTDIF to allow 
c                            for interpolation of f.g. to platforms located   
c                            between the prime meridian and 1 W longitude.    
c
c usage:  call rdshp(shiphere,dbuoyhere,mbuoyhere,cmanhere,tideghere,
c                    fld,icyc)
c   input arguments:
c     shiphere  - logical.  if true, read from unit holding ship data.
c     dbuoyhere - logical.  if true, read from unit holding dbuoy data.
c     mbuoyhere - logical.  if true, read from unit holding mbuoy data.
c     cmanhere  - logical.  if true, read from unit holding lcman data.
c     tideghere - logical.  if true, read from unit holding tideg data.
c     fld       - array containing first guess fields
c     icyc      - integer.  which cycle we're accessing data for.  used
c            to ensure we don't process obs for previous or next synop.
c            period. 
c
c   output arguments:
c     none

c   input files:
c     fort.20  - surface ship data
c     fort.21  - drifting buoy data
c     fort.22  - moored buoy data
c     fort.23  - cman data
c     fort.24  - tide gauge data
c
c   output files:
c     fort.41  - sfc_marine.archive.curr_mon
c
c   subprograms called:
c     library:
c       bufrlib90:  openbf, readmg, readsb, ufbint
c    
c   exit states:
c
c remarks:
c
c attributes:
c   language:  fortran 90
c   machine:  ibm
c
c$$$
C
      CHARACTER*8  SHIPID, bultmp(4)
      character*80 string
      character*8 inout, subset
      character*1 lowercase(26), uppercase(26)
      logical shiphere,dbuoyhere,mbuoyhere,cmanhere,tideghere
C
      INTEGER  IDATE
      INTEGER  NDIF(9)
      INTEGER  NVAL(6)
      INTEGER  NVALS
      INTEGER OBS(11),DUPE(4000,3)
      INTEGER IRHR, IRMN, ITIM, idatim
      character*10 cdatim
      character*2 cyr
      integer icyc
C
      REAL FLD(65160,6)
      REAL XX(6)
      REAL VAL(6)
      REAL DIF(9)
C
      REAL RMISS
      real arr(10,255)
      real xtemp, xtempid
      REAL XTEMP1(22)
      REAL XTEMP0(4)
      integer lubfr, lundx, iret1, iret2, ict
      INTEGER LUNIT
C
      EQUIVALENCE(SHIPID,xtempid)
      EQUIVALENCE(xtemp0(1),bultmp(1))
      data rmiss/1.E10/
      data xtemp1(4)/999999./

      save
C
      KOUNT = 1
      NOBS  = 0
      NSKIP = 0
      LUNIT = 19
c---------------------------------------------------------------------
c  set up lowercase and uppercase arrays
c---------------------------------------------------------------------
      do jj = 1,26
        lowercase(jj) = char(96+jj)
        uppercase(jj) = char(64+jj)
      enddo
C-----------------------------------------------------------------------
c                open unit 41 so we can append data to it.
C-----------------------------------------------------------------------
            open(41,position='append')
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
      do 100 I = 1,5
        if((i.eq.1.and.shiphere).or.
     *     (i.eq.2.and.dbuoyhere).or.
     *     (i.eq.3.and.mbuoyhere).or.
     *     (i.eq.4.and.cmanhere).or. 
     *     (i.eq.5.and.tideghere)) then
        INOUT = 'IN'
        LUBFR = LUNIT + I
        LUNDX = LUBFR
        print 777,i,lubfr
 777    format(' read from file ',i2,' unit = ',i2)
        CALL  OPENBF( LUBFR, INOUT, LUNDX )
C----------------------------------------------------------------------|
C  2. DECODE ALL BUFR MESSAGES IN THE FILE                             |
C----------------------------------------------------------------------|
        IRET1 = 0
        ICT = 0
C----------------------------------------------------------------------|
C    3. ADVANCE THE POINTER TO THE NEXT BUFR MESSAGE IN THE FILE       |
C       AND READ THE BUFR MESSAGE INTO AN INTERNAL BUFFER              |
C----------------------------------------------------------------------|
 160    CALL  READMG( LUBFR, SUBSET, IDATE, IRET1 )
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
C         INITIALIZE IRET2 TO 0 AND READ THIS UNPACKED BUFR MESSAGE    |
C         UNTIL SUBSETS HAVE BEEN READ.                                |
C----------------------------------------------------------------------|
          IRET2 = 0
 170      CALL READSB (LUBFR, IRET2)
          if(iret2.eq.0) then
C----------------------------------------------------------------------|
C        5. ACCESS RELEVANT PARTS OF THE UNPACKED BUFR MESSAGE         |
C                    SET SYNOPTIC PERIOD VARIABLE ISYNOP
C-----------------------------------------------------------------------
            IF(icyc.eq.00) ISYNOP = 0000
            IF(icyc.eq.06) ISYNOP = 0600
            IF(icyc.eq.12) ISYNOP = 1200
            IF(icyc.eq.18) ISYNOP = 1800
C-----------------------------------------------------------------------
C                 STORE DATA IN OBSERVATION ARRAY
C-----------------------------------------------------------------------
            NOBS = NOBS + 1
C---------------------------------------------------------------------|
C       ACCESS THE DATE/TIME OF THIS OBSERVATION.
C---------------------------------------------------------------------|
            STRING = ' YEAR MNTH DAYS HOUR MINU'
            CALL UFBINT (LUBFR, ARR, 10, 255, NRET, STRING)
            IYR = ARR(1,1)
            IMO = ARR(2,1)
            IDA = ARR(3,1)
            IHR = ARR(4,1)
            IMN = ARR(5,1)
            idatim= (iyr*100000000) + (imo*1000000) + (ida*10000) + 
     *              (ihr * 100) + imn
            itim = (ihr * 100) + imn
            obs(4) = idatim
C-----------------------------------------------------------------------
C           CHECK OBS TIME AGAINST SYNOPTIC PERIOD. IF OBS TIME IS
C           VALID FOR THE PREVIOUS SYNOPTIC PERIOD, OR IF OBS TIME IS
C           VALID FOR THE NEXT SYNOPTIC PERIOD, SKIP THE REPORT.
C-----------------------------------------------------------------------
            IF((ISYNOP.EQ.0000.AND.(itim.LT.2100.AND.itim.GE.0300)).or.
     *         (ISYNOP.EQ.0600.AND.(itim.LT.0300.OR.itim.GE.0900)).or.
     *         (ISYNOP.EQ.1200.AND.(itim.LT.0900.OR.itim.GE.1500)).or.
     *         (ISYNOP.EQ.1800.AND.(itim.LT.1500.OR.itim.GE.2100))) then
              NSKIP = NSKIP + 1
              print*,' skipping ob...'
              print 383,xtemp1(2),xtemp1(3),itim
              GO TO 170
            endif
C-----------------------------------------------------------------------
C        OBS(2) = LATITUDE, OBS(3) = LONGITUDE
C-----------------------------------------------------------------------
            STRING = ' CLON CLAT'
            CALL UFBINT (LUBFR, ARR, 10, 255, NRET, STRING)
            XTEMP1(2)= ARR(2,1)
            XTEMP1(3) = ARR(1,1)
C-----------------------------------------------------------------------
C           CONVERT LAT AND LON TO old quips format (0 to 360 westbound)
C-----------------------------------------------------------------------
            XTEMP1(2) = XTEMP1(2) * 100.
            XTEMP1(3) = XTEMP1(3) * 100.
            IF(XTEMP1(3).GT.0.) THEN
              XTEMP1(3) = 36000. - XTEMP1(3)
            ELSEIF(XTEMP1(3).LT.0) THEN
              XTEMP1(3) = -(XTEMP1(3))
            ELSE
              XTEMP1(3) = 36000.
            ENDIF
            if(xtemp1(3).gt.36000.0.or.xtemp1(3).lt.0.) then
              if(nobs.ge.1) nobs = nobs - 1
              print*,' bad longitude...'
              print 383,xtemp1(2),xtemp1(3),itim
 383          format(' lat = ',f8.2,' lon = ',f8.2,' itime = ',i4)
              go to 170
            elseif(xtemp1(2).gt.9000.0.or.xtemp1(2).lt.-9000.) then
              if(nobs.ge.1) nobs = nobs - 1
              print*,' bad latitude...'
              print 383,xtemp1(2),xtemp1(3),itim
              go to 170
            endif
            OBS(2) = nint(XTEMP1(2))
            OBS(3) = nint(XTEMP1(3))
C-----------------------------------------------------------------------
C       ACCESS THE RECEIPT DATE/TIME OF THIS OBSERVATION.
C---------------------------------------------------------------------|
            STRING = ' RCHR RCMI'
            CALL UFBINT (LUBFR, ARR, 10, 255, NRET, STRING)
            IRHR = ARR(1,1)
            IRMN = ARR(2,1)
            irtim= (irhr * 100) + irmn
            obs(11) = irtim
C-----------------------------------------------------------------------
C       Access the bulletin header.
C-----------------------------------------------------------------------
            STRING = ' BUHD BORG BULTIM BBB '
            CALL UFBINT (LUBFR, ARR, 10, 255, NRET, STRING)
            xtemp0(1) = arr(1,1)
            xtemp0(2) = arr(2,1)
            xtemp0(3) = arr(3,1)
            xtemp0(4) = arr(4,1)
Cwm         change for WCOSS
            if(IBFMS(xtemp0(4)) .eq. 0)then
               xtemp0(4) = arr(4,1)
            else
Cwm         BBB value is missing
               xtemp0(4) = arr(4,1)
               call byteswap(xtemp0(4),8,1)
            endif   
c           print*, "KKK xtemp0(4) ", arr(4,1), IBFMS(arr(4,1)) 
c           write(6,119)LUBFR, arr(4,1)
c 119        format(I3,Z16)
C---------------------------------------------------------------------
C           ACCESS THE CALL SIGN FOR THIS OBSERVATION.
C           Call signs are returned in
C           ARR(1,1).  Mnemonic is RPID.
C---------------------------------------------------------------------
            STRING = ' RPID '
            CALL UFBINT (LUBFR, ARR, 10, 255, NRET, STRING)
            xtempid = ARR(1,1)
C-----------------------------------------------------------------------
C                  CHECK ID FOR ILLEGAL CHARACTER
c  if found, change to a slash.  convert lowercase letters to uppercase.
C-----------------------------------------------------------------------
            DO II = 1,8
              M = MOVA2I(SHIPID(II:II))
              IF(M.LT.48) SHIPID(II:II) = '/'
              if(m.gt.57.and.m.lt.65) shipid(ii:ii) = '/'
              if(m.gt.90.and.m.lt.97) shipid(ii:ii) = '/'
              IF(M.GT.122) SHIPID(II:II) = '/'
              do kk = 1,26
                if(shipid(ii:ii).eq.lowercase(kk)) then
                   shipid(ii:ii) = uppercase(kk)
                endif
              enddo
            enddo
C-----------------------------------------------------------------------
C                  SKIP REPORTS W/6 BLANKS IN ID
C-----------------------------------------------------------------------
            K = INDEX(SHIPID,'      ')
            if(k.ne.0) then
              print*,' bad id...'
              print 353,shipid, xtemp1(2), xtemp1(3), itim
  353         format(1x,' shipid = ',a8,' lat lon = ',2f8.2,
     *        ' tim = ',i4)
              go to 170
            endif  
C-----------------------------------------------------------------------
c     obs(1) = report type
C     OBS(5) = SLP,  OBS(6) = AIR TEMP,  
C     OBS(7) = WIND spd, OBS(8) = WIND Dir,
C     OBS(9) = SST.
c           Also, report types like O.N.124 not available from bufr
c           file on IBM.  Check unit from which you're reading to
c           determine report type.  For unit 10 (001.001), check id.
c           If id is 'SHIP', set type to 523, else type is 522.
C-----------------------------------------------------------------------
            IF(I.EQ.1) THEN
              STRING = ' WDIR WSPD TMDB SST1 PMSL '
              CALL UFBINT (LUBFR, ARR, 10, 255, NRET, STRING)
              XTEMP1(5) = ARR(5,1)
              XTEMP1(6) = ARR(3,1)
              XTEMP1(7) = ARR(2,1)
              XTEMP1(8) = ARR(1,1)
              XTEMP1(9) = ARR(4,1)
              if(shipid.eq.'SHIP') then
                xtemp1(1) = 523.
              else
                xtemp1(1) = 522.
              endif
            ELSEIF(I.EQ.2) THEN
              STRING = ' WDIR WSPD '
              CALL UFBINT (LUBFR, ARR, 10, 255, NRET, STRING)
              XTEMP1(7) = ARR(2,1)
              XTEMP1(8) = ARR(1,1)
              STRING = ' TMDB SST1 PMSL '
              CALL UFBINT (LUBFR, ARR, 10, 255, NRET, STRING)
              XTEMP1(5) = ARR(3,1)
              XTEMP1(6) = ARR(1,1)
              XTEMP1(9) = ARR(2,1)
              xtemp1(1) = 562.
            ELSEIF(I.EQ.3.OR.I.EQ.4) THEN
              STRING = ' WDIR WSPD TMDB SST1 '
              CALL UFBINT (LUBFR, ARR, 10, 255, NRET, STRING)
              XTEMP1(6) = ARR(3,1)
              XTEMP1(7) = ARR(2,1)
              XTEMP1(8) = ARR(1,1)
              XTEMP1(9) = ARR(4,1)
              STRING = ' PMSL '
              CALL UFBINT (LUBFR, ARR, 10, 255, NRET, STRING)
              XTEMP1(5) = ARR(1,1)
              if(i.eq.3) then
                xtemp1(1) = 561.
              else
                xtemp1(1) = 531.
              endif
            ELSE                           
              STRING = ' SST1 '
              CALL UFBINT (LUBFR, ARR, 10, 255, NRET, STRING)
              XTEMP1(9) = ARR(1,1)
              STRING = ' TMDB PMSL WDIR WSPD '
              CALL UFBINT (LUBFR, ARR, 10, 255, NRET, STRING)
              XTEMP1(5) = ARR(2,1)
              XTEMP1(6) = ARR(1,1)
              XTEMP1(7) = ARR(4,1)
              XTEMP1(8) = ARR(3,1)
              xtemp1(1) = 532.
            ENDIF

C-----------------------------------------------------------------------
C           CHECK FOR MISSING VALUES
C-----------------------------------------------------------------------
            DO JJ = 5,9
              IF(XTEMP1(JJ).EQ.RMISS) XTEMP1(JJ) = 999999.0
            ENDDO
C-----------------------------------------------------------------------
C           COPY REPORT TYPE TO OBS(1).
C-----------------------------------------------------------------------
            obs(1) = xtemp1(1)
C-----------------------------------------------------------------------
C           CONVERT PMSL FROM PASCALS (db) TO HECTOPASCALS (mb) AND
C           MULTIPLY TIMES 10 FOR OUTPUT.
C           PMSL = (PMSL * 0.01) * 10.0
C           NOTE: CONVERTING TO MB AND THEN MULTIPLYING BY 10 IS THE
C           SAME AS MULTIPLYING PASCALS BY 0.1, SO DO THIS.
C           COPY SLP TO OBS(5).
C-----------------------------------------------------------------------
            IF(XTEMP1(5).lt.999999.) XTEMP1(5) = XTEMP1(5) * 0.1
            obs(5) = nint(xtemp1(5))
C-----------------------------------------------------------------------
C           CONVERT DRY BULB TEMPERATURE FROM K TO C AND
C           MULTIPLY TIMES 10 FOR OUTPUT. COPY TO OBS(6).
C-----------------------------------------------------------------------
            IF(XTEMP1(6).LT.999999.)
     *        XTEMP1(6) = (XTEMP1(6) - 273.15) * 10.0
            OBS(6) = NINT(XTEMP1(6))
C-----------------------------------------------------------------------
C           CONVERT WIND SPEED FROM M/S TO KTS AND COPY WDIR AND
C           WSPD TO OBS ARRAY.
C-----------------------------------------------------------------------
            IF(XTEMP1(7).lt.999999.) XTEMP1(7) = XTEMP1(7) * 1.94254
            obs(7) = nint(xtemp1(7))
            obs(8) = xtemp1(8)
C-----------------------------------------------------------------------
C           CONVERT SEA SURFACE TEMPERATURE FROM K TO C AND
C           MULTIPLY TIMES 10 FOR OUTPUT. COPY SST TO OBS(9).
C-----------------------------------------------------------------------
            IF(XTEMP1(9).LT.999999.)
     *        XTEMP1(9) = (XTEMP1(9) - 273.15) * 10.0
            obs(9) = nint(xtemp1(9))
C-----------------------------------------------------------------------
C     OBS(16) = WIND WAVE HGT IN METERS
C     NOTE: WIND WAVES NOT AVAILABLE FROM DRIFTING BUOYS OR TIDE GAUGE.
C     COPY WIND WAVE HGT IN HOWW.
C-----------------------------------------------------------------------
            IF(I.NE.2.AND.I.NE.5) THEN
              STRING = ' HOWW '
              CALL UFBINT (LUBFR, ARR, 10, 255, NRET, STRING)
              HOWW = ARR(1,1)
            ELSE
              HOWW = RMISS
            ENDIF
C-----------------------------------------------------------------------
C           CHECK FOR MISSING VALUES
C-----------------------------------------------------------------------
            IF(HOWW.EQ.RMISS) HOWW = 999999.0
C-----------------------------------------------------------------------
C           CONVERT WIND WAVE HGT TO FEET
C-----------------------------------------------------------------------
            IF(HOWW.LT.999998.) HOWW = HOWW * 3.2808
C-----------------------------------------------------------------------
C     OBS(17) = SWELL HGT IN METERS
C     NOTE: SWELL WAVE INFO NOT AVAILABLE FROM DRIFTING BUOYS, CMAN,
C     OR TIDE GAUGE.
C-----------------------------------------------------------------------
            IF(I.EQ.1.OR.I.EQ.3) THEN
              STRING = ' HOSW '
              CALL UFBINT (LUBFR, ARR, 10, 255, NRET, STRING)
              IF (NRET .GE. 1) THEN
                HOSW = ARR(1,1)
              ELSE
                HOSW = RMISS
              ENDIF
            ELSE
              HOSW = RMISS
            ENDIF
C-----------------------------------------------------------------------
C           CHECK FOR MISSING VALUES
C-----------------------------------------------------------------------
            IF(HOSW.EQ.RMISS) HOSW = 999999.0
C-----------------------------------------------------------------------
C           CONVERT SWELL WAVE HGT TO FEET
C-----------------------------------------------------------------------
            IF(HOSW.LT.999998.) HOSW = HOSW * 3.2808
C-----------------------------------------------------------------------
C                  OBS(10) = COMBINED WAVE HEIGHT (CWH)
C-----------------------------------------------------------------------
            OBS(10) = 999999                        
            IF(HOWW.LT.999998.0.OR.HOSW.LT.999998.0) THEN
              IF(HOWW.LT.999998.0.AND.HOSW.GE.999998.0) 
     *           OBS(10) = NINT(HOWW)
              IF(HOWW.GE.999998.0.AND.HOSW.LT.999998.0) 
     *           OBS(10) = NINT(HOSW)
              IF(HOWW.LT.999998.0.AND.HOSW.LT.999998.0) THEN
                X = (HOWW**2 + HOSW**2)
                OBS(10) = NINT(SQRT(X))
              ENDIF
            ENDIF
C-----------------------------------------------------------------------
C                       SET MISSING DATA TO -99
C-----------------------------------------------------------------------
            DO IJ = 5,10
              IF(OBS(IJ).GE.999999) OBS(IJ) = -99
            enddo
C-----------------------------------------------------------------------
c       save lat and lon into real values
C-----------------------------------------------------------------------
            RLA = OBS(2)/100.
            RLO = OBS(3)/100.
C-----------------------------------------------------------------------
c   calculate i,j grid for observed lat/long (grib model grid is 1x1).
c   remember that lat and long are multiplied by 100, so multiply 90 and
c   1 by 100 as well for our equations (which would be 
c   sti = -lat + 90 + 1      and     stj = 360 - lon + 1).
c   Also remember that station longitude is 0 - 360 westbound, while
c   model is 1 - 360 eastbound.  Grid begins at north pole and GM, so
c   use negative of lat to calculate i gridpoint.
C-----------------------------------------------------------------------
            isti = 36000 - obs(3) + 100
            sti  = isti/100.
            istj = -(obs(2)) + 9000 + 100
            stj  = istj/100.
C-----------------------------------------------------------------------
C                     OBSERVED FIELDS
C   XX(1)=PMSL, XX(2)=TEMP, XX(3)=Wspd, XX(4)=wdir, XX(5)=SST,
C   XX(6)=WVHT
C   PMSL first.
C-----------------------------------------------------------------------
            IF(OBS(5).NE.-99) THEN
              XX(1) = OBS(5)/10.
            ELSE
              XX(1) = 9999.9
            ENDIF
C-----------------------------------------------------------------------
C  AIR TEMP
C-----------------------------------------------------------------------
            IF(OBS(6).NE.-99) THEN
              XX(2) = OBS(6)/10.
            ELSE
              XX(2) = 9999.9
            ENDIF
C-----------------------------------------------------------------------
C  WIND spd
C-----------------------------------------------------------------------
            IF(OBS(7).NE.-99) THEN
              XX(3) = OBS(7)
            ELSE
              XX(3) = 9999.9
            ENDIF
C-----------------------------------------------------------------------
C  WIND dir
C-----------------------------------------------------------------------
            IF(OBS(8).NE.-99) THEN
              XX(4) = OBS(8)
            ELSE
              XX(4) = 9999.9
            ENDIF
C-----------------------------------------------------------------------
C  SST
C-----------------------------------------------------------------------
            IF(OBS(9).NE.-99) THEN
              XX(5) = OBS(9)/10.
            ELSE
              XX(5) = 9999.9
            ENDIF
C-----------------------------------------------------------------------
C  CWH
C-----------------------------------------------------------------------
            IF(OBS(10).NE.-99) THEN
              XX(6) = OBS(10)
            ELSE
              XX(6) = 9999.9
            ENDIF
C-----------------------------------------------------------------------
C                  IF ALL OBS ARE MISG SKIP REPORT
C-----------------------------------------------------------------------
            DO II = 1,6
              IF(XX(II).NE.9999.9) GO TO 1944
            enddo
            print*,'  missing all obs...'
            GO TO 170
C-----------------------------------------------------------------------
C                      COMPUTE DIFS
C-----------------------------------------------------------------------
 1944       CALL GTDIF(FLD,sti,stj,XX,VAL,DIF)
C-----------------------------------------------------------------------
C                    CHECK WIND DIR DIFS
C-----------------------------------------------------------------------
            IF(DIF(4).lt.999.8) then
              IF(DIF(4).LT.-180.0) DIF(4) = DIF(4) + 360.0
              IF(DIF(4).GT. 180.0) DIF(4) = DIF(4) - 360.0
            endif
            IF(DIF(7).lt.999.8) then
              IF(DIF(7).LT.-180.0) DIF(7) = DIF(7) + 360.0
              IF(DIF(7).GT. 180.0) DIF(7) = DIF(7) - 360.0
            endif
            IF(DIF(8).lt.999.8) then
              IF(DIF(8).LT.-180.0) DIF(8) = DIF(8) + 360.0
              IF(DIF(8).GT. 180.0) DIF(8) = DIF(8) - 360.0
            endif
            IF(DIF(9).lt.999.8) then
              IF(DIF(9).LT.-180.0) DIF(9) = DIF(9) + 360.0
              IF(DIF(9).GT. 180.0) DIF(9) = DIF(9) - 360.0
            endif
C-----------------------------------------------------------------------
c  set val and dif for wave height to missing.
C-----------------------------------------------------------------------
            val(6) = 9999.9
            dif(6) = 9999.9
C-----------------------------------------------------------------------
C                    CONVERT DIFS TO INTEGER
C-----------------------------------------------------------------------
            DO II = 1,9
              IF(DIF(II).GE.0.0) NDIF(II) = (DIF(II) + .05) * 10.0
              IF(DIF(II).LT.0.0) NDIF(II) = (DIF(II) - .05) * 10.0
            enddo
C-----------------------------------------------------------------------
C     PRINT NDIF FOR DIAGNOSTIC PURPOSES
C-----------------------------------------------------------------------
C           PRINT 907, SHIPID,ITIM,NDIF(1),NDIF(2),NDIF(3),NDIF(4),
C    *      NDIF(5),NDIF(6)
C907        FORMAT(1X,A8,I4,6(I5))
C-----------------------------------------------------------------------
C                   SAVE 4 DIGITS OF OBS (CHAR)
C                        DO PRESSURE OBS FIRST
C                   ONLY SAVE TENS, ONES, AND DECIMAL FOR STATS.
C                   SAVE ENTIRE PRESSURE IF LESS THAN 1000 MB FOR
C                   MONTHLY STATS.
C-----------------------------------------------------------------------
            IF(OBS(5).NE.-99) then
              MM = (OBS(5)/1000) * 1000
              NVAL(1) = OBS(5) - MM
              IF(OBS(5).LT.10000) THEN
                NVALS = OBS(5)
              ELSE
                NVALS = NVAL(1)
              ENDIF
            else
              NVAL(1) = -99
              NVALS = -99
            endif
C-----------------------------------------------------------------------
C                        DO SFC TEMP OBS NEXT
C-----------------------------------------------------------------------
            IF(OBS(6).NE.-99) then
              NVAL(2) = OBS(6)
            else
              NVAL(2) = -99
            endif
C-----------------------------------------------------------------------
C                        DO WIND spd
C-----------------------------------------------------------------------
            IF(OBS(7).NE.-99) then
              NVAL(3) = OBS(7)
            else 
              NVAL(3) = -99
            endif
C-----------------------------------------------------------------------
C                        DO WIND dir
C-----------------------------------------------------------------------
            IF(OBS(8).NE.-99) then
              NVAL(4) = OBS(8)
            else
              NVAL(4) = -99
            endif
C-----------------------------------------------------------------------
C                        DO SST
C-----------------------------------------------------------------------
            IF(OBS(9).NE.-99) then
              NVAL(5) = OBS(9)
            else
              NVAL(5) = -99
            endif
C-----------------------------------------------------------------------
C                        DO WAVE HEIGHT
c  set ndif for wave height to missing since we have no wave hgt field.
C-----------------------------------------------------------------------
            IF(OBS(10).NE.-99) then
              NVAL(6) = OBS(10)
            else
              NVAL(6) = -99
            endif
            ndif(6) = 9999
C-----------------------------------------------------------------------
C          WRITE sfc_marine.archive.curr_mon DATASET.
C          NOTE: WDIR & WSPD ORDER SWITCHED ON OUTPUT
C-----------------------------------------------------------------------
              WRITE(41,1019) SHIPID,OBS(1),OBS(2),OBS(3),OBS(4),
     *          ndif(1),NVALS,NDIF(2),NVAL(2),NDIF(4),NVAL(4),NDIF(3),
     *          nval(3),NDIF(5),NVAL(5),NDIF(6),NVAL(6),
     *          ndif(7),nval(4),ndif(8),nval(4),ndif(9),nval(4),OBS(11),
     *          bultmp(1)(1:6),bultmp(2)(1:4),bultmp(3)(1:6),
     *          bultmp(4)(1:3)
 1019         FORMAT(1X,A8,1x,I3,1X,I5,1x,i5,1x,I12,1X,9(I5,I5),I5.4,1x,
     *               a6,1x,a4,1x,a6,1x,a3)
          endif
          IF(IRET2.EQ.0) GO TO 170
        ENDIF
        GO TO 160
        endif
 100  CONTINUE
 999  print 1020, NOBS,NSKIP
 1020 FORMAT(' TOTAL OBS=',I8,' SKIPPED OBS=',I4)
      RETURN
      END
c====================================================================
      SUBROUTINE GTGDAS(gdas,icyc,lbkup,idat,IRET)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM: gtgdas      get gdas flds 
C   PRGMMR: caruso           ORG: NP12        DATE: 1999-08-19
C
C abstract:  retrieves gdas first guess fields
C
C program history log:
c   90-02-xx  Fred Marshall -  original author
c   99-08-19  chris caruso - adding docblock.
c
c usage:  call gtgdas(gdas,icyc,lbkup,idat,iret)
c   input arguments:
c     icyc - integer.  which cycle we're accessing data for.  used
c            to ensure we don't process obs for previous or next synop.
c            period. 
c     lbkup - logical.  if true, using backup fields.
c     idat  - current date/time
c
c   output arguments:
c     gdas  - array containing first guess fields
c     iret -  return code
c
c   input files:
c     unit 11 or unit 12 - input grib file (avn or gdas)
c     unit 31 or unit 32 - input grib index file
c
c   output files:
c     none
c
c   subprograms called:
c     unique:  rdfld
c    
c   exit states:
c     non-zero - error reading gdas.  non-fatal.
c
c remarks:
c
c attributes:
c   language:  fortran 90
c   machine:  ibm
c
c$$$
      DIMENSION gdas(65160,6)
      dimension ID1(6), ID2(6), ID3(6), ID4(6), id5(6)
      character*8 cgrib
      character*1 pdsl1(28)
      character*1 pdsl2(28)
      character*1 pdsl3(28)
      character*1 pdsl4(28)
      character*1 pdsl5(28)
      integer idat(8)
      integer lidrec1(6)
      integer lidrec2(6)
      integer lidrec3(6)
      integer lidrec4(6)
      integer lidrec5(6)
      integer label1(12)
      integer label2(12)
      integer label3(12)
      integer label4(12)
      integer label5(12)
      integer jpds1(6)
      integer jpds2(6)
      integer jpds3(6)
      integer jpds4(6)
      integer jpds5(6)
      integer iret
      integer igrib1(16500)
      integer igrib2(16500)
      integer igrib3(16500)
      integer igrib4(16500)
      integer igrib5(16500)
      character*1 grib1(132000)
      character*1 grib2(132000)
      character*1 grib3(132000)
      character*1 grib4(132000)
      character*1 grib5(132000)
      equivalence(grib1(1),igrib1(1))
      equivalence(grib2(1),igrib2(1))
      equivalence(grib3(1),igrib3(1))
      equivalence(grib4(1),igrib4(1))
      equivalence(grib5(1),igrib5(1))
      equivalence(label1(1),jpds1(1))
      equivalence(label2(1),jpds2(1))
      equivalence(label3(1),jpds3(1))
      equivalence(label4(1),jpds4(1))
      equivalence(label5(1),jpds5(1))
      logical lbkup

      data cgrib/'GRIB0001'/
      data msk06/X'00000006'/
      data msk12/X'00000C00'/
      data msk18/X'00001200'/
      data msk24/X'00001800'/

c  jpds1 - mean slp
c  jpds2 - 1000 mb air temp.
c  jpds3 - 10 m u-wind
c  jpds4 - 10 m v-wind
c  jpds5 - skin temp. (sst)

      data jpds1/X'00001C02',X'07520380',X'02660000',X'00000000',
     *           X'00010600',X'0000000'/
      data jpds2/X'00001C02',X'07520380',X'0B6403E8',X'00000000',
     *           X'00010600',X'00000000'/
      data jpds3/X'00001C02',X'07520380',X'2169000A',X'00000000',
     *           X'00010600',X'00000000'/
      data jpds4/X'00001C02',X'07520380',X'2269000A',X'00000000',
     *           X'00010600',X'00000000'/
      data jpds5/X'00001C02',X'07520380',X'0B010000',X'00000000',
     *           X'00010600',X'00000000'/

      save

C-----------------------------------------------------------------------
C             ID'S FOR READING GDAS MODEL FORECASTS
C-----------------------------------------------------------------------
C     5 1x1 GDAS FIELDS ARE:
C                           1: MEAN SLP   - jpds1
C                           2: 1000MB TMP - jpds2
c                           3: 10 m u-comp - jpds3
c                           4: 10 m v-comp - jpds4
c                           5: sfc temp (skin temp) - jpds5
C-----------------------------------------------------------------------
C                     LOOP TO GET 10 GDAS FIELDS
C-----------------------------------------------------------------------
      print*,'in gtgdas'
      iret = 0

      write(6,223) cgrib
 223  format(1x,'cgrib = ',A8)
C-----------------------------------------------------------------------
c  get slp fields
C-----------------------------------------------------------------------
        CALL RDFLD(GDAS(1,1),label1,icyc,lbkup,idat,IERR)
        IF(IERR.NE.0) then
          print*,' error reading slp field'
          IRET = IERR
          GO TO 886
        else
          do jj = 1,65160
            gdas(jj,1) = gdas(jj,1)/100.0
          enddo
        endif
C-----------------------------------------------------------------------
c  get air temp fields
C-----------------------------------------------------------------------
        CALL RDFLD(GDAS(1,2),label2,icyc,lbkup,idat,IERR)
        IF(IERR.NE.0) then
          print*,' error reading air temp. field'
          iret = ierr
          GO TO 887
        endif

       write(6,225) jpds3
 225   format(1x,'jpds3 =',4(1X,Z16))
C-----------------------------------------------------------------------
c  get u-components 
C-----------------------------------------------------------------------
        CALL RDFLD(GDAS(1,3),label3,icyc,lbkup,idat,IERR)
        IF(IERR.NE.0) then
          print*,' error reading u-wind field'
          iret = ierr
          GO TO 888
        endif
C-----------------------------------------------------------------------
c  get v-components 
C-----------------------------------------------------------------------
        CALL RDFLD(GDAS(1,4),label4,icyc,lbkup,idat,IERR)
        IF(IERR.NE.0) then
          print*,' error reading v-wind field'
          iret = ierr
          GO TO 889
        endif
C-----------------------------------------------------------------------
c  get skin temp (sst) fields
C-----------------------------------------------------------------------
        CALL RDFLD(GDAS(1,5),label5,icyc,lbkup,idat,IERR)
        IF(IERR.NE.0) then
          print*,' error reading sst field'
          iret = ierr
          GO TO 890
        endif

      GO TO 999
  886 PRINT 1020, (ID1(I),I=1,6)
  887 PRINT 1020, (ID2(I),I=1,6)
  888 PRINT 1020, (ID3(I),I=1,6)
  889 PRINT 1020, (ID4(I),I=1,6)
  890 PRINT 1020, (ID5(I),I=1,6)
 1020 FORMAT('  GDAS 06 HR FIELD ID: ',6Z10,' NOT FOUND')
  999 RETURN
      END
C=======================================================================
      SUBROUTINE GTAVN(AVN,icyc,lbkup,idat,IERR)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM: gtavn      get avn flds 
C   PRGMMR: caruso           ORG: NP12        DATE: 1999-08-19
C
C abstract:  retrieves avn first guess fields
C
C program history log:
c   90-02-xx  Fred Marshall -  original author
c   99-08-19  chris caruso - adding docblock.
c
c usage:  call gtavn(avn,icyc,lbkup,idat,iret)
c   input arguments:
c     icyc - integer.  which cycle we're accessing data for.  used
c            to ensure we don't process obs for previous or next synop.
c            period. 
c     lbkup - logical.  if true, using backup fields.
c     idat  - current date/time
c
c   output arguments:
c     avn   - array containing first guess fields
c     iret -  return code
c
c   input files:
c     unit 11 or unit 12 - input grib file (avn or gdas)
c     unit 31 or unit 32 - input grib index file
c
c   output files:
c     none
c
c   subprograms called:
c     unique:  rdfld
c    
c   exit states:
c     non-zero - error reading avn.  may be fatal or non-fatal.
c                if we're reading main avn, then non-fatal.  if
c                we're going for backup avn, then fatal.
c
c remarks:
c
c attributes:
c   language:  fortran 90
c   machine:  ibm
c
c$$$
      DIMENSION AVN(65160,6), ID1(6), ID2(6), id3(6), id4(6), id5(6)
      character*8 cgrib
      character*1 pdsl1(28)
      character*1 pdsl2(28)
      character*1 pdsl3(28)
      character*1 pdsl4(28)
      character*1 pdsl5(28)
      integer idat(8)
      integer lidrec1(6)
      integer lidrec2(6)
      integer lidrec3(6)
      integer lidrec4(6)
      integer lidrec5(6)
      integer label1(12)
      integer label2(12)
      integer label3(12)
      integer label4(12)
      integer label5(12)
      integer jpds1(6)
      integer jpds2(6)
      integer jpds3(6)
      integer jpds4(6)
      integer jpds5(6)
      integer iret
      integer igrib1(16500)
      integer igrib2(16500)
      integer igrib3(16500)
      integer igrib4(16500)
      integer igrib5(16500)
      character*1 grib1(132000)
      character*1 grib2(132000)
      character*1 grib3(132000)
      character*1 grib4(132000)
      character*1 grib5(132000)
      equivalence(grib1(1),igrib1(1))
      equivalence(grib2(1),igrib2(1))
      equivalence(grib3(1),igrib3(1))
      equivalence(grib4(1),igrib4(1))
      equivalence(grib5(1),igrib5(1))
      equivalence(label1(1),jpds1(1))
      equivalence(label2(1),jpds2(1))
      equivalence(label3(1),jpds3(1))
      equivalence(label4(1),jpds4(1))
      equivalence(label5(1),jpds5(1))
      logical lbkup

      data cgrib/'GRIB0001'/

c  jpds1 - mean slp
c  jpds2 - 1000 mb air temp.
c  jpds3 - 10 m u-wind
c  jpds4 - 10 m v-wind
c  jpds5 - skin temp. (sst)

      data jpds1/X'00001C02',X'074D0380',X'02660000',X'00000000', 
     *           X'00010600',X'00000000'/
      data jpds2/X'00001C02',X'074D0380',X'0B6403E8',X'00000000',
     *           X'00010600',X'00000000'/
      data jpds3/X'00001C02',X'074D0380',X'2169000A',x'00000000',
     *           X'00010600',X'00000000'/
      data jpds4/X'00001C02',X'074D0380',X'2269000A',X'00000000',
     *           X'00010600',X'00000000'/
      data jpds5/X'00001C02',X'074D0380',X'0B010000',X'00000000',
     *           X'00010600',X'00000000'/

      save

C-----------------------------------------------------------------------
C     5 FIELDS ARE:
C                           1: MEAN SLP
C                           2: 1000MB TMP
c                           3: 10 m u-comp
c                           4: 10 m v-comp
c                           5: sfc temp (skin temp) - sst
C     LOOP TO GET 5 1x1 AVN FIELDS
C-----------------------------------------------------------------------
      print*,'in gtavn'
      iret = 0
C-----------------------------------------------------------------------
c  get slp and air temp fields 
C-----------------------------------------------------------------------
      CALL RDFLD(AVN(1,1),label1,icyc,lbkup,idat,IERR)
      IF(IERR.NE.0) then
        print*,' error reading slp field'
        iret = ierr
        GO TO 886
      else
        do jj = 1,65160
          avn(jj,1) = avn(jj,1)/100.0
        enddo
      endif

      CALL RDFLD(AVN(1,2),label2,icyc,lbkup,idat,IERR)
      IF(IERR.NE.0) then
        print*,' error reading air temp. field'
        iret = ierr
        GO TO 887
      endif

      write(6,223) cgrib
 223  format(1x,'cgrib = ',A8)
c      write(6,225) jpds1
 225  format(1x,'jpds1 =',4(1X,Z16))
C-----------------------------------------------------------------------
c  get u-components
C-----------------------------------------------------------------------
      CALL RDFLD(AVN(1,3),label3,icyc,lbkup,idat,IERR)
      IF(IERR.NE.0) then
        print*,' error reading u-wind field'
        iret = ierr
        GO TO 888
      endif
C-----------------------------------------------------------------------
c  get v-components
C-----------------------------------------------------------------------
      CALL RDFLD(AVN(1,4),label4,icyc,lbkup,idat,IERR)
      IF(IERR.NE.0) then
        print*,' error reading v-wind field'
        iret = ierr
        GO TO 889
      endif
C-----------------------------------------------------------------------
c  get skin temp (sst)
C-----------------------------------------------------------------------
      CALL RDFLD(AVN(1,5),label5,icyc,lbkup,idat,IERR)
      IF(IERR.NE.0) then
        print*,' error reading sst field'
        iret = ierr
        GO TO 890
      endif

      GO TO 999
  886 PRINT 1020, (ID1(I),I=1,6)
  887 PRINT 1020, (ID2(I),I=1,6)
  888 PRINT 1020, (ID3(I),I=1,6)
  889 PRINT 1020, (ID4(I),I=1,6)
  890 PRINT 1020, (ID5(I),I=1,6)
 1020 FORMAT('  AVN 06 HR FIELD ID: ',6Z10,' NOT FOUND')
  999 RETURN
      END
C=======================================================================
      SUBROUTINE RDFLD(XARRAY,label,icyc,lbkup,idat,IRC)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    RDFLD      READS IN ONE AVN OR GDAS FLD (1X1).
C   PRGMMR: CARUSO           ORG: W/NP21            DATE: 99-07-08
C
C ABSTRACT: READS DATA CARDS TO GET FILE NAME OF PACKED FIELD,IDS OF
C   THE PACKED FIELD,CONSTANTS NEEDED TO TAKE A SUB SET OUT OF PACKED
C   FIELD,LTITLE TO PUT ON MAP.
C
C PROGRAM HISTORY LOG:
C   90-02-xx  MARSHALL - original author
c   96-07-09  caruso - don't need separate rdgdas and rdavn.
c                      this can do both.
c   99-07-08  caruso - adding calls to baopenr for ibm
C
c usage:  call rdfld(xarray,ident,icyc,lbkup,idat,irc)
C
C   input arguments:
C     label - NMC 12 WORD GRIB ID
C     icyc  - synoptic period
c     lbkup - logical.  if true, using backup fcst fields
c             so look at different unit numbers
c     idat  - current date array
c
c   output arguments:
C     XARRAY( ) = ONE FIELD  (avn or gdas) (360x181)
C     IRC = return code
c         = 0, successful run
c         = 1, error opening grib input file via baopenr
c         = 2, error opening grib index file via baopenr
c         = 3, error from iw3getpop
c
c   input files:
c     unit 11 or unit 12 - input grib file (avn or gdas)
c     unit 31 or unit 32 - input grib index file
c
c subroutines called:
c  unique:   iw3getpop
c  library:
c    w3lib:  sbytes
c    bacio:  baopenr
c   system:  get_environment_variable
c
c attributes:
c   language:  fortran 90
c   machine:   ibm
c
c$$$
c-----------------------------------------------------------
      integer lugrbix, lugrb, ierr, label(12)
      character*132 mtitle
      integer idat(8)
      integer kpds(25)
      DIMENSION XARRAY(65160),IDENT(6),ID(12),JDENT(6)

      integer msk06, msk12, msk18, msk24
      data    msk06/X'00000006'/
      logical lbkup

      character*80 cgrbfn, cgrbifn
c      character*11 envvar
c      data         envvar/'XLFUNIT_   '/
      character*6 envvar
      data         envvar/'FORT  '/

      save

      IRC = 0

      print*,' in rdfld...lbkup = ',lbkup
      if(.not.lbkup) then
        lugrb = 11
        lugrbix = 31
      else
        lugrb = 12
        lugrbix = 32
      endif

c  pack 12 word id into 6 words.

c      call sbytes(ident,label,0,32,0,12)
cwm  using for WCOSS
      call sbytesc(ident,label,0,32,0,12)

      do i = 1,6
        JDENT(I) = IDENT(I)
      enddo
C-----------------------------------------------------------------------
C                 MOD PROJECTION TO FIRST ID WORD
C-----------------------------------------------------------------------

c     JDENT(5) = IOR(IDENT(5),msk06)
      print 505,jdent(5)
 505  format(' jdent(5) = ',z16)

c***check this out to see if the modification for itau worked!!
C-----------------------------------------------------------------------
c      open grib and index files via baopenr
C-----------------------------------------------------------------------
      write(envvar(5:6),fmt='(i2.2)') lugrb
      call get_environment_variable(envvar,cgrbfn)
      call baopenr(lugrb,cgrbfn,iret)
      if(iret.ne.0) then
        irc = 1
        go to 999
      endif 
      write(envvar(5:6),fmt='(i2.2)') lugrbix
      call get_environment_variable(envvar,cgrbifn)
      call baopenr(lugrbix,cgrbifn,iret)
      if(iret.ne.0) then
        irc = 2
        go to 999
      endif 

C-----------------------------------------------------------------------
C          READ IN MODEL FIELD
C-----------------------------------------------------------------------

      call iw3getpop(lugrbix,lugrb,label,xarray,mtitle,kpds,ierr)
      if(ierr.ne.0) then
        print 100
  100   format('cannot read using iw3get')
        irc = 3
      endif
 999  continue
      RETURN
      END
C=======================================================================
      SUBROUTINE GTWSD(FLD) 
C$$$  SUBPROGRAM DOCUMENTATION BLOCK 
C 
C SUBPROGRAM: gtwsd      get wind speed & dir from u and v winds 
C   PRGMMR: caruso           ORG: NP12        DATE: 1999-08-19
C
C abstract:  converts u and v winds to wind speed and direction.
C
C program history log:
c   90-02-xx  Fred Marshall -  original author
c   99-08-19  chris caruso - adding docblock.
c
c usage:  call gtwsd(fld)
c   input arguments:
c     fld  -  real(65160,6). on input, elements (x,3) and (x,4) contain
c             u-wind and v-wind, respectively.
c
c   output arguments:
c     fld  -  real(65160,6). on output, elements (x,3) and (x,4) contain
c             wind speed (in kts) and wind direction, respectively.
c
c   input files:
c     none
c
c   output files:
c     none
c
c   subprograms called:
c     w3lib90:  w3fc05
c    
c   exit states:
c     none
c
c remarks:
c
c attributes:
c   language:  fortran 90
c   machine:  ibm
c
c$$$
      REAL FLD(65160,6)
      REAL DIR(65160)
      REAL SPD(65160)

      save

C-----------------------------------------------------------------------
C                  COMPUTE WIND DIR & SPEED
C-----------------------------------------------------------------------
        DO I = 1,65160
          CALL W3FC05(FLD(I,3),FLD(I,4),DIR(I),SPD(I))
          IF(SPD(I).lt.0.0) spd(i) = 0.0
        enddo
C-----------------------------------------------------------------------
C                  CONVERT WSPD FROM MPS TO KNOTS
C                   AND STORE IN FLD 3
C                   STORE DIR IN FLD 4
C-----------------------------------------------------------------------
        DO J = 1,65160
          FLD(J,3) = SPD(J) * 1.94254
          FLD(J,4) = DIR(J)
        enddo
      RETURN
      END
C=======================================================================
      SUBROUTINE GTDIF(FLD,sti,stj,XX,VAL,DIF)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK 
C 
C SUBPROGRAM: gtdif      get difference between first guess and data.
C   PRGMMR: caruso           ORG: NP12        DATE: 1999-08-19
C
C abstract:  interpolates first guess fields to data location and
c            computes differences.
C
C program history log:
c   90-02-xx  F. Marshall      original author
c   99-08-19  Chris Caruso     adding docblock.
c   03-03-19  C. Caruso Magee  fixing an error which occurred whenever a
C                              platform would be located between 0 longitude
C                              and 1W, which is off the first guess grid 
C                              (grid points 1 to 360 eastbound, with g.p. 1
C                              being the prime meridian and g.p. 360 being
C                              1W).  Solution is to use a new array of size
C                              (361,181) with the prime meridian duplicated. 
c
c usage:  call gtdif(fld,sti,stj,xx,val,dif)
c   input arguments:
c     fld - real(65160,6) contains first guess fields.
c     sti - real i-gridpoint of station 
c     stj - real j-gridpoint of station
c     xx  - real(6) observed data values
c
c   output arguments:
c     val - real(6) first guess values at station location
c     dif - real(6) differences between xx's and val's (xx - val).
c
c   input files:
c     none
c
c   output files:
c     none
c
c   subprograms called:
c     w3lib90:  w3ft03
c    
c   exit states:
c     none
c
c remarks:
c
c attributes:
c   language:  fortran 90
c   machine:  ibm
c
c$$$
      REAL FLD(65160,6)
      real fldtmp(65160)
      real fldin(360,181)
      real fldinnew(361,181)
      equivalence(fldtmp(1),fldin(1,1))

      REAL XX(6)
      REAL VAL(6)
      REAL DIF(9)
      real sti, stj
      integer imax, jmax, iquad
      data  imax /361/
      data  jmax /181/
      data  iquad  /2/     !biquadratic

      save 

      DO 100 N = 1,5
C-----------------------------------------------------------------------
C   Save input argument fld into temp array fldtmp, which is equivalenced
C   to fldin (2-dimensional array of size (360,181)).  This fldin array will
C   then be copied into a new array that's 361x181 with g.p. 361 being the
C   same as g.p. 1 (i.e. prime meridian is duplicated), thus allowing for 
C   interpolation of first guess to platform location if platform is between 
C   1W and the prime meridian (i.e. between grid points 360 and 1).  Without 
C   this patch, obs in this longitude band were off the grid when their sti 
C   was passed into w3ft03, resulting in first guess value of 0 at the obs
C   location and bad diffs being computed.
C-----------------------------------------------------------------------
         do i = 1,65160
           fldtmp(i) = fld(i,n)
         enddo
C
         do jj = 1,181
           do ii = 1,360
             fldinnew(ii,jj) = fldin(ii,jj)
           enddo
           fldinnew(361,jj) = fldin(1,jj)
         enddo
C-----------------------------------------------------------------------
C                  COMPUTING interpolated VALUES
c  note:  may get negative wdir's from w3ft03, but this is ok, because
c  you get the same answer back in rdshp if you use the neg. wdir to
c  compute the dif here as if you would if you used wdir of (360 + neg. 
c  wdir) to compute dif here, then adding 360 to dif in rdshp (since dif
c  would be lt -180).
C-----------------------------------------------------------------------
        call w3ft03(fldinnew,val(n),sti,stj,imax,jmax,iquad)
C-----------------------------------------------------------------------
C                  CONVERT TEMPS TO CENTIGRADE
C-----------------------------------------------------------------------
        IF(N.EQ.2.OR.N.EQ.5) VAL(N) = VAL(N) - 273.15
        if(N.NE.4) then
          DIF(N) = XX(N) - VAL(N)
          ADIF = ABS(DIF(N))
          IF(ADIF.GT.999.9)  DIF(N) = 999.9
          IF(XX(N).GE.9999.) DIF(N) = 999.9
          IF(DIF(N).GE.999.9) XX(N) = 9999.9
        else
          dif(4) = xx(4) - val(4)
          dif(7) = (xx(4) - 90.) - val(4)
          dif(8) = (xx(4) + 90.) - val(4)
          dif(9) = (xx(4) +180.) - val(4)
          ADIF1 = ABS(DIF(4))
          ADIF2 = ABS(DIF(7))
          ADIF3 = ABS(DIF(8))
          ADIF4 = ABS(DIF(9))
          IF(ADIF1.GT.999.9)  DIF(4) = 999.9
          IF(ADIF2.GT.999.9)  DIF(7) = 999.9
          IF(ADIF3.GT.999.9)  DIF(8) = 999.9
          IF(ADIF4.GT.999.9)  DIF(9) = 999.9
          IF(XX(4).GE.9999.) then
            DIF(4) = 999.9
            DIF(7) = 999.9
            DIF(8) = 999.9
            DIF(9) = 999.9
          endif
          IF(DIF(4).GE.999.9) XX(4) = 9999.9
        endif
  100 CONTINUE
  999 RETURN
      END
c=======================================================================
      SUBROUTINE IW3GETPOP(LUGRBIX, LUGRB, LABEL, FLD, MTITLE, IERR)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM: IW3GETPOP      GET AND UNPACK CRAY GRIB FILES FOR POP AFOS.
C   AUTHOR: LUKE LIN         ORG: WD41        DATE: 96-11-04
C
C ABSTRACT: GET AND UNPACK CRAY GRIB FILES FROM AN 28-BYTE GRIB PDS. 
C           PLEASE READ THE REMARKS BEFORE USING IT.
C
C HISTORY LOG:
C   95-10-04  LUKE LIN
C   95-10-31  LUKE LIN   MODIFY FOR TESTING DIFFERENT GRID TYPE.
C   95-11-27  LUKE LIN   ADD AN OPTION FOR NOT TEST FCST HOUR -- CAC 96H/C
C                        AND MODIFY TO READ RGL(NGM) MODEL.
C   96-01-30  LUKE LIN   MODIFY TO CONVERT FROM 1 DEGREE TO 65*65 BY LINEAR.
C   96-02-14  LUKE LIN   MODIFY FOR NGM BOUNDARY LAYER RH CHART.
C   96-02-23  LUKE LIN   MODIFY FOR NGM SURFACE TO 500 MB LIFTED INDEX.
C   96-02-26  LUKE LIN   MODIFY FOR AVMER OR AVPOLAR AND OUTPUT WILL BE
C                        EITHER 2.5 OR 5.0 DEGREE DEPENDING ON OPTION
C   96-03-07  LUKE LIN   MODIFY FOR NGM SUPER C GRID.
C   96-08-12  LUKE LIN   USE IDERELL'S ROUTINE TO CONVERT 1' TO 65*65.
C   96-11-04  LUKE LIN   USE IDERELL'S ROUTINE TO CONVERT 1' TO 65*65.
c   99-08-19  caruso     replace getgb1 w/ getgb and getgbp for ibm.
c                        make kbms a logical*1 array for ibm (was
c                        logical on the cray).
C
C INPUT ARGUMENTS:
C   LUGRBIX    - AN UNIT NUMBER POINTS TO ONE DEGREE GRIB INDEX FILE.
C   LUGRB      - AN UNIT NUMBER POINTS TO ONE DEGREE GRIB FILE. SEE 
C                REMARK.
C   LABEL      - 12 WORDS LABEL ON INPUT.  WORDS 1-5 SHOULD CONTAIN
C                LABEL ID OF DESIRED FIELD FLD.
C
C OUTPUT ARGEMENTS:
C   FLD     -- BUFF CONTAINS UNPACK FIELD.
C   LABEL   -- ON OUTPUT THE FULL 12 WORDS OF THE LABEL WILL CONTAIN
C              APPROPRIATE VALUES.
C   MTITL   -- CHARACTER*132 MAP TITLE FROM INDEX FILE.
C              (1:86) IS MAP TITLE, FOLLOWINGS ARE MAP ID.
C   IERR    -- RETURN STATUS
C   IRETUR   - RETURN CONDITION;
C            0 - ALL OK
C            1 - W3FP13/GRIB BLOCK 0 NOT CORRECT
C            2 - W3FP13/LENGTH OF PDS NOT CORRECT
C            3 - W3FP13/COULD NOT MATCH TYPE INDICATOR
C            4 - W3FP13/GRID TYPE NOT IN TABLES
C            5 - W3FP13/COULD NOT MATCH TYPE LEVEL
C            6 - W3FP13/COULD NOT INTERPRET ORIGINATOR OF CODE
C            7 - GRIB TYPE ERROR -- UNKNOWN GRIB TYPE.
C           10 - UNCOGNIZE DATA TYPE FROM LABEL
C           11 - W3FT32/ MAPIN NOT RECOGNIZED
C           12 - W3FT32/ MAPOUT NOT RECOGNIZED
C           13 - W3FT32/ PARTICULAR POLA MAPOUT NOT RECOGNIZED
C           14 - W3FT32/ PARTICULAR LOLA MAPOUT NOT RECOGNIZED
C           15 - W3FT32/ PARTICULAR LOLA MAPIN NOT RECOGNIZED
C           16 - W3FT32/ PARTICULAR POLA MAPOUT NOT RECOGNIZED
C           17 - W3FT32/ PARTICULAR LOLA MAPIN NOT RECOGNIZED
C           18 - W3FT32/ PARTICULAR LOLA MAPOUT NOT RECOGNIZED
C           96 - GETGB/ERROR READING INDEX FILE
C           97 - GETGB/ERROR READING GRIB FILE
C           98 - GETGB/NUMBER OF DATA POINTS GREATER THAN JF
C           99 - GETGB/REQUEST NOT FOUND
C           -1 - GETGB/OTHER  W3FI63 GRIB UNPACKER RETURN CODE
C          101 - W3FP11/NON-FATAL ERROR, MTITLE NOT CORRECT
C
C USAGE:
C   INPUT FILES:
C     UNIT LUGRBIX -  INPUT ONE DEGREE GRIB INDEX FILE.  SEE REMARK.
C     UNIT LUGRB   -  INPUT ONE DEGREE GRIB FILE.  SEE REMARK.
C
C   OUTPUT FILES:
C     unit 6 -   PRINT OUTPUT (STANDARD FORTRAN OUTPUT FILE)
C
C   SUBPROGRAMS CALLED:
C     UNIQUE:     NONE
C
C     LIBRARY:
C       SPECIAL  - NONE
C       W3LIB    - W3FP11 , W3FP13, GETGB , GBYTES , W3FM08
C                - W3FI63 , W3FT43V,W3FT32, SBYTES
C
C   REMARKS:
C       ***** VERY IMPORTANT ******
C       O. THE INTENTION OF THIS SUBROUTINE IS ONLY FOR AD GRAPHIC SECTION
C          TO READ CRAY GRIB FILES FROM THEIR 84 OFFICE 6 WORD IDS.
C          IT IS NOT GENERAL ENOUGH FOR OTHER GROUP USERS.
C          IT CAN BE MODIFIED TO TAKE 28 BYTE PDS INSTEAD OF 6 WORD 84 ID.
C          BY THIS WAY, IT MAY BE MORE GENERAL THAN IT IS NOW.
C          THIS SUBROUTINE SHOULD NOT BE INCLUDED IN W3LIB AT PRESENT TIME.
C       1. USE ASSIGN COMMAND TO ASSIGN ONE-DEGREE GRIB INDEX TO UNIT LUGRBIX
C       2. USE ASSIGN COMMAND TO ASSIGN ONE-DEGREE GRIB TO UNIT LUGRB
C       FOR EXAMPLE:
C assign -a /com/avn/prod/avn.950912/gblav.T00Z.PGrbiF24 -s unblocked fort.31
C           where LUGRBIX=31
C assign -a /com/avn/prod/avn.950912/gblav.T00Z.PGrbF24 -s unblocked fort.11
C           where LUGRB=11
C       3. ONLY DEAL WITH INPUT DATA TYPE 26, 27 & 101.  OTHER DATA TYPE MAY
C          HAVE FATAL ERROR RETURN AT PRESENT TIME.
C       4. CONCERNING RGL CHARTS, THE 6TH WORD ID SHOULD BE '00000127'.
C       5. FOR NO FORECAST HOUR CHECKING SUCH AS CAC 96H/C, THE THIRD BYTE
C          OF THE 6TH WORD ID SHOULD BE '96'.
C
C
C ATTRIBUTES:
C   LANGUAGE: fortran 90
C   MACHINE:  CRAY
C
C$$$
C
      COMMON/HDCONS/ITAU,IDUMMY(3)
C     ..... ITAU COMES FROM SUBR RD1GRID.F
      parameter(ii=31, jj=21)
C
      integer ipgrib (ii*jj*3/8)
      character*1 pgrib(ii*jj*3)
      equivalence (pgrib(1),ipgrib(1))
C
      PARAMETER (IPTS=65160,IIP=360,JJP=181,MM=361)
      PARAMETER (IPTS2=10512,II2=144,JJ2=73)
      PARAMETER (MXSIZE=66000)
      PARAMETER (MXSIZ2=MXSIZE*2)
      PARAMETER (IIK=289,JJK=145,NPTS=IIK*JJK)
      PARAMETER (IJK=145,LJK=73,MPTS=IJK*LJK)
C
      parameter(jcc=147*110)
      parameter(ji=651,ig27=27,jo=65*65,jo2=129*129)
      real rlat(jo),rlon(jo)
      real rlat2(jo2),rlon2(jo2)
      integer ibi,ibo
      logical lo(jo)
      logical lo2(jo2)
      real    ho(jo)
      real    ho2(jo2)
      integer         kgdsi(22)
C
C
      INTEGER      KGDSO(22)
      CHARACTER    GDSO(42)
      INTEGER      LENGDS
      REAL         DIF,SMLNO
C
      INTEGER         LUGI,LUGRBIX
      INTEGER         LUGB,LUGRB
      INTEGER         LABEL(12)
      REAL            FLD(*)
      CHARACTER * 132 MTITLE
      INTEGER         IERR
      INTEGER         IRET
      INTEGER         ITAU
C
      REAL            C(MXSIZE)
      LOGICAL*1       KBMS(MXSIZE)
      REAL            CC(IIP,JJP)
      REAL            CC2(II2,JJ2)
      REAL            D(MXSIZE)
      REAL            DD(361,181)
      REAL            D2(MXSIZE)
      REAL            DD2(145,73)
      REAL            DD2N(145,37)
      REAL            DD2N1(5365)
      REAL            EE(4225)
      REAL            FF(4225)
      REAL            BLOLA(NPTS), CLOLA(MPTS)
      REAL            GG(73,37)
      REAL            GG2(2701)
C
      INTEGER         IGRIB(16500)
      CHARACTER * 1   GRIB(MXSIZ2)
C
      INTEGER         JPDS(4)
      INTEGER         JGDS(50)
      INTEGER         MPDS(25)
      INTEGER         KGDS(50)
      INTEGER         KPDS(25)
      CHARACTER * 1   PDS(28)
      CHARACTER * 1   PDSL(28)
C
C
       CHARACTER*1     CTEMP(8)
       INTEGER         ITEMP
C
       INTEGER         LIDREC(6)
       CHARACTER*8     CGRIB
       INTEGER         IMODE
C
      INTEGER      LABELP(6)
      CHARACTER*1  IFLAG
      CHARACTER*1  IDPDSC(28)
      INTEGER*8    IDPDS(4)
      INTEGER      MODELNH
      INTEGER      SFCMEANRH
      INTEGER      MSK1, MSK2, MSK3, MSK4
      INTEGER      MSKNH, MSKNGM
      INTEGER      ITYPE, IMODEL, FLAG
      INTEGER      CAC96HC
      INTEGER      BLRHHDS
      INTEGER      BLRHGRB
      INTEGER      MSKFFFF
      INTEGER      LFTFLG
      INTEGER      AVPOLRV
      INTEGER      AVTYPE
C
      SAVE
C
      EQUIVALENCE     (C(1),CC(1,1),CC2(1,1))
      EQUIVALENCE     (D(1),DD(1,1))
      EQUIVALENCE     (D2(1),DD2(1,1))
      EQUIVALENCE     (DD2N1(1),DD2N(1,1))
      EQUIVALENCE     (GRIB(1),IGRIB(1))
      EQUIVALENCE     (JPDS(1),PDSL(1),IGRIB(2))
      EQUIVALENCE     (CTEMP,ITEMP)
      EQUIVALENCE     (IDPDSC(1),IDPDS(1))
      EQUIVALENCE     (PDS(1),LABELP(1))
C
      DATA        MODELNH   /Z'00000000034E0000'/
      DATA        SFCMEANRH /Z'0000000005809100'/
      DATA        MSK1      /Z'00000000FFFFFF00'/
      DATA        MSK2      /Z'00000000000000FF'/
      DATA        MSK3      /Z'000000000000FFFF'/
      DATA        MSK4      /Z'0000000000FF0000'/
      DATA        MSKNH     /Z'000000000000084D'/
      DATA        MSKNGM    /Z'0000000000000127'/
      DATA        CAC96HC   /Z'0000000000960000'/
      DATA        BLRHHDS   /Z'346C006400000000'/
      DATA        BLRHGRB   /Z'346B265F00000000'/
      DATA        MSKFFFF   /Z'FFFFFFFF00000000'/
      DATA        LFTFLG    /Z'8365326400000000'/
      DATA        AVPOLRV   /Z'0000000000990000'/
      DATA        AVTYPE    / 130 /
C
C***********************************************************************
C
C
      IRET   = 0
      IERR = 0
      LUGI = LUGRBIX
      LUGB = LUGRB
C
              WRITE(6,201)(LABEL(I),I=1,12)
  201         FORMAT(1X,'LABEL=',/,(1X,6(Z16,1X),/,6(Z16,1X),/))
c              CALL SBYTES(LABELP,LABEL,0,32,0,12)
              CALL SBYTESC(LABELP,LABEL,0,32,0,12)
cwm byte swap
c      DO I = 1,6
c      CALL  BYTESWAP(LABELP(I),8,1)
c      ENDDO
C             ..... PACK 12-WORD ID TO 6-WORD CRAY ID
C
C     READ  GRIB FILE USING INDEX FILE
C
      JREW   = 0
      MPDS   = -1
      JGDS   = -1
c     MPDS(3) = mova2i(PDS(7))
      MPDS(5) = mova2i(PDS(9))
      MPDS(6) = mova2i(PDS(10))
      MPDS(7) = mova2i(PDS(11)) * 256 + mova2i(PDS(12))
      MPDS(14) = mova2i(PDS(19))

C
      WRITE(6,224)(LABELP(I),I=1,6)
  224 FORMAT(1X,'LABELP=',/,(1X,6(Z16,1X),/))

C  old call to getgb1
c     CALL GETGB1(LUGB,LUGI,ipts,JREW,MPDS,JGDS,
c    &      PGRIB,KBYTES,KREW,KPDS,KGDSi,KBMS,FLD,IRET)

      CALL GETGB (LUGB,LUGI,ipts,JREW,MPDS,JGDS,
     &      KBYTES,KREW,KPDS,KGDSi,KBMS,FLD,IRET)
      CALL GETGBP(LUGB,LUGI,ipts,KREW-1,MPDS,JGDS,
     &      KBYTES,KREW,KPDS,KGDSi,GRIB,IRETX)
C
      IF (IRET .NE. 0) THEN
          PRINT *,' **FATAL ERROR FROM GETGB.', IRET
          IERR = IRET
          print*,' error but kpds = ',kpds
          GO TO 999 
      ENDIF
C
      print *,'after getgb  kpds = ',kpds
C
      ITAU = MOVA2I(PDS(19))
C
      print *,' ITAU=', itau
      print *, ' Kds = '
      write(*, 14)(kpds(i),i=1,16)
  14  format( 4(2x, 4(z16,1x),/))
C
  700 CONTINUE
C
            CALL GBYTES(LIDREC,LABEL,0,32,0,12)
  999 CONTINUE
      RETURN
      END
