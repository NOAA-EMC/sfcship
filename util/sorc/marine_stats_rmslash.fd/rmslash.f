C$$$  MAIN PROGRAM DOCUMENTATION BLOCK  ***
C
C MAIN PROGRAM: HRLY_RDSHPMPC
C   PRGMMR: Bhavana Rakesh and Krishna Kumar  ORG: NP12        DATE: 2008-10-10
C
C ABSTRACT:  Reads monthly surface marine archive file, removes slashes from callsigns, and
C            writes back out to noslash.out.
C
C PROGRAM HISTORY LOG:
c 2000-05-09 Magee  - First used in checkout mode.                       
c 2008-07-15 Rakesh - Moving into production.                            
C
c USAGE:
C   INPUT FILES:
C     fort.10 - archive.uniq (in local working directory).  Monthly archive file
C               containing surface marine observed data and diffs to first guess.
C
C   OUTPUT FILES:
C       fort.51 - noslash.out -- new version of archive file with cleaned-up 
C                 callsigns.
C
C   SUBPROGRAMS CALLED:
C     None
C
C     LIBRARY:
c       None
C
C   EXIT STATES:
C     COND =   0   - SUCCESSFUL RUN
C
C   REMARKS:
C
C ATTRIBUTES:
C   LANGUAGE:  FORTRAN 90
C   machine:  ibm
C
C$$$
C
      character*12 IDATE
      CHARACTER*8 ID
      character*8 bullhdr(4)
      INTEGER ITYPE
      INTEGER ND(8), NV(8)
      INTEGER LAT, LON
      INTEGER LU, luo
      integer itmp1, itmp2
      integer irtim
      data lu /10/, luo/51/

 25   READ(LU,1002,END=100) ID,ITYPE,LAT,LON,IDATE,ND(1),NV(1),ND(2),
     *     NV(2),ND(3),NV(3),ND(4),NV(4),ND(5),NV(5),itmp1, itmp2,
     *     ND(6),NV(6), ND(7),NV(7),ND(8),NV(8), irtim,bullhdr(1)(1:6),
     *     bullhdr(2)(1:4),bullhdr(3)(1:6),bullhdr(4)(1:3)
 1002 FORMAT(1X,A8,1X,I3,1x,I5,1x,I5,1X,A12,1x,9(I5,I5),1x,i4,1x,a6,1x,
     *       a4,1x,a6,1x,a3)
      IF ( ID .eq. '        ' ) go to 25
      do j = 1,8
        slashpos = index(ID,'/')
        IF ( slashpos .ne. 0 ) then
          do i = slashpos, 8
            if ( i.lt.8) then
              ID(i:i) = ID(i+1:i+1)
            else
              ID(i:i) = ' '
            endif
          enddo
        endif
      enddo
      IF ( ID .eq. '        ' ) go to 25
      write(luo,1003) id, itype, lat, lon, idate, nd(1), nv(1), nd(2),
     *      nv(2), nd(3), nv(3), nd(4), nv(4), nd(5), nv(5), itmp1, 
     *      itmp2, nd(6), nv(6), nd(7), nv(7), nd(8), nv(8), irtim,
     *      bullhdr(1)(1:6),bullhdr(2)(1:4),bullhdr(3)(1:6),
     *      bullhdr(4)(1:3)
 1003 FORMAT(1X,A8,1X,I3,1x,I5,1x,I5,1X,A12,1x,9(I5,I5),1x,i4.4,1x,a6,
     *       1x,a4,1x,a6,1x,a3)
      go to 25
 100  continue
      stop
      end

