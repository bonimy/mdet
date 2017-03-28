      subroutine mreadimage(nx,ny,lsize,array,fname,crval1,crval2,
     1     cdelt1,cdelt2,crot,crpix1,crpix2,istat)

!  Read a FITS image.  This is the subroutine from T. Jarrett's file
!  readfits.f, with minor modifications for use with MDET.


      implicit integer (i-n)
        implicit real(4) (a-h)
        implicit real(4) (o-z)

      integer status,unit,readwrite,blocksize,naxes(2),nfound
c     integer group,firstpix,nbuffer,npixels,i                          ! JWF B60516
      integer group,firstpix,nbuffer,npixels                            ! JWF B60516
      real*8 cd1a,cd1b,cd2a,cd2b,tDB,rat,angle
c     real*4 datamin,datamax,nullval,array(lsize)                       ! JWF B60516
      real*4 nullval,array(lsize)                                       ! JWF B60516
      real*8 crval1,crval2,cdelt1,cdelt2,crot,crpix1,crpix2             ! JWF B60521
      logical anynull
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
          write(6,'(a)') 'MREADIMAGE: Could not read '//trim(fname)
          istat = 3
          return
      endif

C  Determine the size of the image.
      call ftgknj(unit,'NAXIS',1,2,naxes,nfound,status)


C  Check that it found both NAXIS1 and NAXIS2 keywords.
      if (nfound .ne. 2)then
          print *,'MREADIMAGE: Failed to read the NAXISn keywords.'
        istat = 4
          return
      endif

      nx = naxes(1)
        ny = naxes(2)
cccc
      crval1 = 0.
c      call ftgkye(unit, 'CRVAL1', crval1, comment, status)                ! JWF B60521
      call ftgkyd(unit, 'CRVAL1', crval1, comment, status)                ! JWF B60521
      if (status.gt.0) then
            crval1 = 0.
            status=0
      endif

      crval2 = 0.
c       call ftgkye(unit, 'CRVAL2', crval2, comment, status)            ! JWF B60521
        call ftgkyd(unit, 'CRVAL2', crval2, comment, status)            ! JWF B60521
        if (status.gt.0) then
                crval1 = 0.
                status=0
        endif
      
c      call ftgkye(unit, 'CRPIX1', crpix1, comment, status)                ! JWF B60521
      call ftgkyd(unit, 'CRPIX1', crpix1, comment, status)                ! JWF B60521
        if (status.gt.0) status=0
      
c      call ftgkye(unit, 'CRPIX2', crpix2, comment, status)                ! JWF B60521
      call ftgkyd(unit, 'CRPIX2', crpix2, comment, status)                ! JWF B60521
        if (status.gt.0) status=0

      cdelt1 = 0.
        cdelt2 = 0.
c      call ftgkye(unit, 'CDELT1', cdelt1, comment, status)                ! JWF B60521
      call ftgkyd(unit, 'CDELT1', cdelt1, comment, status)                ! JWF B60521
        if (status.gt.0) status=0
        
c       call ftgkye(unit, 'CDELT2', cdelt2, comment, status)            ! JWF B60521
        call ftgkyd(unit, 'CDELT2', cdelt2, comment, status)            ! JWF B60521
        if (status.gt.0) status=0

c      call ftgkye(unit, 'CROTA2', crot, comment, status)                  ! JWF B60521
      call ftgkyd(unit, 'CROTA2', crot, comment, status)                  ! JWF B60521
        if (status.gt.0) then
            crot = 0.
            status=0
      endif

c  CD matrix


      if ((abs(cdelt1).le.1.e-5).and.(abs(cdelt2).le.1.e-5)) then

        cd1_1=0.
        cd1_2=0.
        cd2_1=0.
        cd2_2=0.

       call ftgkye(unit, 'CD1_1', cd1_1, comment, status)
       status=0

       call ftgkye(unit, 'CD1_2', cd1_2, comment, status)
         status=0

       call ftgkye(unit, 'CD2_1', cd2_1, comment, status)
         status=0
      
       call ftgkye(unit, 'CD2_2', cd2_2, comment, status)
         status=0


         if (cd2_2.ne.0.) then
                  rat = CD1_2 / CD2_2
                  angle = -datan (rat) * 57.2957795d0

              tdb = angle/57.2957795d0

                cd2a = CD2_2 / dcos(tdb)
                cd2b = -CD1_2 / dsin(tdb)

            if ((abs(CD2_2).gt.abs(CD1_2))) then
                      cdelt2 = cd2a*1.
                else
                      cdelt2 = cd2b*1.
                endif

                cd1a = -CD1_1 / dcos(tdb)
                cd1b = -CD2_1 / dsin(tdb)

            if ((abs(CD1_1).gt.abs(CD2_1))) then
                     cdelt1 = cd1a*1.
                else
                     cdelt1 = cd1b*1.
                endif

                  crot = angle*1.

      else if (cd1_1.ne.0.) then

                    rat = CD2_1 / CD1_1
                  angle = datan (rat) * 57.2957795d0
                  tdb = angle/57.2957795d0

                  cd2a = CD2_2 / dcos(tdb)
                  cd2b = -CD1_2 / dsin(tdb)

                  if ((abs(CD2_2).gt.abs(CD1_2))) then
                    cdelt2 = cd2a*1.
                   else
                    cdelt2 = cd2b*1.
                  endif

                  cd1a = -CD1_1 / dcos(tdb)
                  cd1b = -CD2_1 / dsin(tdb)
                  if ((abs(CD1_1).gt.abs(CD2_1))) then
                   cdelt1 = cd1a*1.      
                    else
                   cdelt1 = cd1b*1.
                  endif

                  crot = angle*1.

        endif

        endif



C  Initialize variables
      npixels=naxes(1)*naxes(2)

      group=1
      firstpix=1
      nullval=-999
c     datamin=1.0E30                                                    ! JWF B60519
c     datamax=-1.0E30                                                   ! JWF B60519
      nbuffer=npixels

      call ftgpve(unit,group,firstpix,nbuffer,nullval,
     1            array,anynull,status)


C  The FITS file must always be closed before exiting the program. 
C  Any unit numbers allocated with FTGIOU must be freed with FTFIOU.
      call ftclos(unit, status)
      call ftfiou(unit, status)

       return
      end

