      subroutine headpar(nx,ny,fname,crval1,crval2,cdelt1,cdelt2,
     *          crot,crpix1,crpix2,istat)

C  Read parameter values from FITS header.

      implicit integer (i-n)
        implicit real(4) (a-h)
        implicit real(4) (o-z)

      integer status,unit,readwrite,blocksize,naxes(2),nfound
c     integer group,firstpix,nbuffer,npixels,i                          ! JWF B60516
c     real*8 cd1a,cd1b,cd2a,cd2b,tDB,rat,angle                          ! JWF B60516
c     real*4 datamin,datamax,nullval                                    ! JWF B60516
      real*8 crval1,crval2,cdelt1,cdelt2,crot,crpix1,crpix2
c     logical anynull                                                   ! JWF B60516
      character*(*) fname
      character*80 comment

C  The STATUS parameter must always be initialized.
      status=0

C  Get an unused Logical Unit Number to use to open the FITS file.
      call ftgiou(unit,status)

c      write (6,'(a,a)') 'reading ',fname(1:72)
C  Open the FITS file 
      readwrite=0
      call ftopen(unit,fname,readwrite,blocksize,status)
      if (status /= 0) then
          write(6,'(a)') 'HEADPAR: Could not read '//trim(fname)
          istat = 3
          return
      endif

C  Determine the size of the image.
      call ftgknj(unit,'NAXIS',1,2,naxes,nfound,status)


C  Check that it found both NAXIS1 and NAXIS2 keywords.
      if (nfound .ne. 2)then
          print *,'HEADPAR: Failed to read the NAXISn keywords.'
        istat = 4
          return
       end if

      nx = naxes(1)
        ny = naxes(2)

c      write (6,*) nx,ny

cccc
      crval1 = 0.
      call ftgkyd(unit, 'CRVAL1', crval1, comment, status)
      if (status.gt.0) then
            crval1 = 0.
            status=0
      endif

      crval2 = 0.
        call ftgkyd(unit, 'CRVAL2', crval2, comment, status)
        if (status.gt.0) then
c               crval1 = 0.                                             ! JWF B60520
                crval2 = 0.                                             ! JWF B60520
                status=0
        endif
      
      call ftgkyd(unit, 'CRPIX1', crpix1, comment, status)
        if (status.gt.0) status=0
      
      call ftgkyd(unit, 'CRPIX2', crpix2, comment, status)
        if (status.gt.0) status=0

      cdelt1 = 0.
        cdelt2 = 0.
      call ftgkyd(unit, 'CDELT1', cdelt1, comment, status)
        if (status.gt.0) status=0
        
        call ftgkyd(unit, 'CDELT2', cdelt2, comment, status)
        if (status.gt.0) status=0

      call ftgkyd(unit, 'CROTA2', crot, comment, status)
        if (status.gt.0) then
            crot = 0.
            status=0
      endif

C  The FITS file must always be closed before exiting the program. 
C  Any unit numbers allocated with FTGIOU must be freed with FTFIOU.
      call ftclos(unit, status)
      call ftfiou(unit, status)

       return
      end

