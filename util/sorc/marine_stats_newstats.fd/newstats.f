c
c$$$  MAIN PROGRAM DOCUMENTATION BLOCK
c                .      .    .
c MAIN PROGRAM:  newstats        Read the ship names and its monthly old stat file
c                                and generate the new stat file for monthly marine stats
c
c   PRGMMR: Krishna Kumar & Bhavana Rakesh  ORG: W/NP12     DATE: 2008-10-12
c
c ABSTRACT: Read the ship names and its monthly old stat file to generate the new stat file 
c
c PROGRAM HISTORY LOG:
c   2007-10-20  Chris Magee                       Origination.
c   2008-10-12  Krishna Kumar and Bhavana Rakesh  Prepare this code for
c                                                 operational use
c USAGE:
c**INTERFACE.
c  ----------
c    Input:
c      fort.11 -  read in ship names file and monthly stats file
c      fort.12 -  read in the old stat file
c
c    Output:
c      fort.20 -  write the new stat file 
c
c Attributes:
c Language:       AIX FORTRAN 90
c$$$
      program newstats
      character*80 oldstat
      character*94 newstat
      character*94 newheader
      character*10 callsign(90000)
      character*25 name(90000)
      character*25 notfound, blank
      logical shpcmntg, buoy
      data notfound(1:7)/'UNKNOWN'/
      data newheader(1:47)
     */'PLATFORM   SHIP/PLAT. NAME           TYPE   NO.'/
      data newheader(48:94)
     */' REPORTS    MEAN DIFF    SD DIFF    # GROSS ERR'/
c       12345678901234567890123456789012345678901234567890
c      
      do k = 8,25
        notfound(k:k) = ' '
      enddo
      do jk = 1,25
        blank(jk:jk) = ' '
      enddo
c--------------------------------------------------------------------      
c  first read in ship_names file and store into
c  array.
c--------------------------------------------------------------------      
      nplat = 1
 10   continue
      read(11,fmt='(a10,a25)',end=150) callsign(nplat),name(nplat)
      nplat = nplat + 1
      go to 10
 150  continue
      nplat = nplat - 1
      print*,' nplat = ',nplat
c--------------------------------------------------------------------      
c  read monthly stats file.  if line read in contains the string
c  'SHP', 'CMN', or 'TG', look for a match in the previously defined
c  arrays and write out the line.
c--------------------------------------------------------------------      
 160  continue
c--------------------------------------------------------------------      
c  blank out newstat and oldstat for next line.
c--------------------------------------------------------------------      
      do jj = 1,80
        oldstat(jj:jj) = ' '
      enddo
      do kk = 1,94
        newstat(kk:kk) = ' '
      enddo
      shpcmntg = .false.
      buoy = .false.
      
      read(12,fmt='(a80)',end=300) oldstat
      if(oldstat(12:14).eq.'SHP' .or.
     *   oldstat(12:14).eq.'CMN' .or.
     *   oldstat(12:14).eq.'TG ') then
        newstat(12:36) = notfound(1:25)
        do j = 1,nplat
	  if(oldstat(1:8).eq.callsign(j)(1:8)) then
	    newstat(12:36) = name(j)
	  endif
        enddo
        shpcmntg = .true.
      elseif(oldstat(12:14).eq.'FB '.or.
     *       oldstat(12:14).eq.'DB ') then
        newstat(12:36) = blank(1:25)
        buoy = .true.
      endif
      if(shpcmntg.or.buoy) then
        newstat(1:11) = oldstat(1:11)
        newstat(38:90) = oldstat(12:64)
      elseif(oldstat(1:8).eq.'PLATFORM') then
        newstat(1:94) = newheader(1:94)
      else
        newstat(1:80) = oldstat(1:80)
      endif
      
      write(20,fmt='(a94)') newstat
      go to 160
      
 300  continue
      stop
      end
