C *************************************************************************
	subroutine wimage (nx,ny,lsize,larray,fin,fout)


C  Create a FITS primary array containing a 2-D image

	implicit integer (i-n)
        implicit real (a-h)
        implicit real (o-z)

      integer status,readwrite,inunit,outunit,blocksize,
     1   naxes(2),nkeys,nspace                                          ! JWF B60519
c    1   bitpix,naxis,naxes(2),nkeys,nspace                             ! JWF B60519
c     integer i,j,group,fpixel,nelements,ncombine                       ! JWF B60519
      integer i,group,fpixel,nelements                                  ! JWF B60519
      real larray(lsize)
c     real ra0,dec0,crpix10,crpix20,cd01,cd02,rot0                      ! JWF B60519
c     real sumut                                                        ! JWF B60519
c     character*80 record,comment,s0                                    ! JWF B60519
      character*80 record                                               ! JWF B60519
      character*(*) fin,fout
c     logical simple,extend, zexist, erase                              ! JWF B60519
      logical zexist, erase                                             ! JWF B60519


C  The STATUS parameter must be initialized before using FITSIO.  A
C  positive value of STATUS is returned whenever a serious error occurs.
C  FITSIO uses an `inherited status' convention, which means that if a
C  subroutine is called with a positive input value of STATUS, then the
C  subroutine will exit immediately, preserving the status value. For 
C  simplicity, this program only checks the status value at the end of 
C  the program, but it is usually better practice to check the status 
C  value more frequently.

      status=0
	zexist = .false.
	erase = .true.

	call deletefile(fout,status,zexist, erase)
c	call deletefile(fout,status)

C  Get  unused Logical Unit Numbers to use to open the FITS files.
        call ftgiou(inunit,status)
        call ftgiou(outunit,status)

c	write (6,*) 'test0 ',status,outunit

C  The input FITS file is opened with READONLY access, and the output
C  FITS file is opened with WRITE access.
	readwrite=0
	blocksize=1
        call ftopen(inunit,fin,readwrite,blocksize,status)

c	write (6,*) 'test1 ',status

	status=0
c       readwrite=1
cblocksize=1
c       call ftopen(outunit,fout,readwrite,blocksize,status)


C  Create the new empty FITS file.  The blocksize parameter is a
C  historical artifact and the value is ignored by FITSIO.

	write (6,'(a)') fout(1:50)
      blocksize=1
      call ftinit(outunit,fout,blocksize,status)

c	write (6,*) 'test2 ',status

C  This do-loop of calls to FTGREC and FTPREC copies all the keywords from
C  the input to the output FITS file.  Notice that the specified number
C  of rows in the output table, as given by the NAXIS2 keyword, will be
C  incorrect.  This value will be modified later after it is known how many
C  rows will be in the table, so it does not matter how many rows are specified
C  initially.

C  Find the number of keywords in the input table header.
      call ftghsp(inunit,nkeys,nspace,status)

      do i=1,nkeys
          call ftgrec(inunit,i,record,status)
          call ftprec(outunit,record,status)
      end do

c	write (6,*) 'testH ',status


C  Initialize parameters about the FITS image.
C  BITPIX = 16 means that the image pixels will consist of 16-bit
C  integers.  The size of the image is given by the NAXES values. 
C  The EXTEND = TRUE parameter indicates that the FITS file
C  may contain extensions following the primary array.
c     simple=.true.
c     bitpix=-32
c     naxis=2
      naxes(1)=nx
      naxes(2)=ny
c     extend=.false.

C  Write the required header keywords to the file
c      call ftphpr(outunit,simple,bitpix,naxis,naxes,0,1,extend,status)

C  Write the array to the FITS file.
C  The last letter of the subroutine name defines the datatype of the
C  array argument; in this case the 'J' indicates that the array has an
C  integer*4 datatype. ('I' = I*2, 'E' = Real*4, 'D' = Real*8).
C  The 2D array is treated as a single 1-D array with NAXIS1 * NAXIS2
C  total number of pixels.  GROUP is seldom used parameter that should
C  almost always be set = 1.
      group=1
      fpixel=1
      nelements=naxes(1)*naxes(2)
      call ftppre(outunit,group,fpixel,nelements,larray,status)

c	write (6,*) 'test3 ',status

C  Write another optional keyword to the header
C  The keyword record will look like this in the FITS file:
C
C  EXPOSURE=                 1500 / Total Exposure Time
C

	call FTDKEY(outunit,'NAXIS1',status)
        status=0
	call ftpkyj(outunit,'NAXIS1',nx,'array size',status)
	status=0
	call FTDKEY(outunit,'NAXIS2',status)
        status=0
        call ftpkyj(outunit,'NAXIS2',ny,'array size',status)

	write (6,*) 'write status ',status

C  The FITS file must always be closed before exiting the program. 
C  Any unit numbers allocated with FTGIOU must be freed with FTFIOU.
	call ftclos(inunit, status)
      call ftfiou(inunit, status)
      call ftclos(outunit, status)
      call ftfiou(outunit, status)

      return
      end


C *************************************************************************
      subroutine deletefile(filename,status, zexist, erase)

C  A simple little routine to delete a FITS file

      integer status,unit,blocksize
      character*(*) filename
	logical zexist, erase

C  Simply return if status is greater than zero
      if (status .gt. 0)return

C  Get an unused Logical Unit Number to use to open the FITS file
      call ftgiou(unit,status)

C  Try to open the file, to see if it exists
      call ftopen(unit,filename,1,blocksize,status)

      if (status .eq. 0)then
C         file was opened;  so now delete it 
          call ftdelt(unit,status)
c	write (6,*) 'delete the file '
      else if (status .eq. 103)then
C         file doesn't exist, so just reset status to zero and clear errors
          status=0
          call ftcmsg
      else
C         there was some other error opening the file; delete the file anyway
          status=0
          call ftcmsg
          call ftdelt(unit,status)
c	 write (6,*) 'force delete the file '

	call access(filename,zexist,erase)
	write (6,*) 'success'

      end if

C  Free the unit number for later reuse
      call ftfiou(unit, status)
      
      return
      end


