      program mdet

!----------------------------------------------------------------------------
! MDET: Multiband DETection.
!
! This program performs the detection step for multiple bands using the output
! from the image coadder AWAIC. 
!
! COMMAND LINE SYNTAX:
!      ./mdet [-c] -imageN <FITS file name> -sigimageN <FITS file name>
!                -cmaskN <FITS file name> -backwindow <real> 
!                -threshold <real> -outlist <text file name> -fwhmN <real> 
!                -focalpix <real> -matchout <FITS file name> 
!                -svboutN <FITS file name> -svnprec <sigfigs>
!
!      where N = 1,2,3 or 4.
!
!       If the -c option is included, the flux threshold will automatically
!      be adjusted to allow for confusion.
!
! INPUT PARAMETERS: 
!      imageN            =      name of FITS file containing input coadded
!                        image for band N (all input images are assumed 
!                        to be coregistered)
!      sigimageN      =      name of FITS file containing the corresponding
!                        uncertainty the coadded image
!      cmaskN            =      name of FITS file containing the coadd mask
!      backwindow      =      width of median filtering window [coadd pixels]
!      threshold      =      detection threshold [sigmas]
!      outlist            =      name of output file containing a list of
!                        candidate detections
!      fwhmN            =      nominal PSF FWHM for band N [arcsec].  Default
!                        values: 6., 6., 9., 15. for the 4 bands, resp.
!      focalpixN      =      focal-plane pixel size for band N [arcsec].
!                        Default values: 2.75, 2.75, 2.75, 5.5
!      matchout      =      name of output matched filter image;  if not
!                        specified, then do not write out
!      svbN            =      name of FITS file for the output slowly-varying 
!                        background image in band N; if not specified,
!                        then do not write out
!       sigfigs         =       if >0, # of sigfigs to keep in svb pixels
!
! OUTPUTS:
!  (1) List of candidate sources (text file).  The format is as follows:
!
!      The first 6 records are header lines, the first of which gives the
!      number of candidates.  The subsequent records (7, ... Ncandidates+6) 
!      list the following quantities:
!
!      n, RA, Dec, SNR, Rsat1, Rsat2, Rsat3, Rsat4
!
!       where:
!        n      =      candidate number
!        RA      =      RA [deg]
!        Dec      =      Dec [deg]
!        SNR      =      detection SNR of this candidate
!        RsatN =      saturation radius [arcsec], where N = 1,...4
!
!  (2) Set of slowly-varying background images (FITS format).
!      These represent median-filtered versions of the input images.
!
! EXIT CODES:
!      0      =      normal termination
!      1      =      images misregistered (non-fatal error; proceed anyway)
!      2      =      missing input parameters
!      3      =      file not found
!      4      =      problem with FITS keyword values
!      5      =      array allocation failure
!
! DEVELOPMENT HISTORY:
!      Written by K. A. Marsh, IPAC.
!
!      Version 0.0: IDL prototype (mdet.pro) 2008 Feb 28
!      Version 1.0: Fortran translation (mdet.f) 2008 Apr 24
!      Version 1.1: Blank out bright stars during background estim. 2008 Jul 9
!      Version 2.0: Explicit declaration of real(4)
!      Version 2.5: Add -svnprec to command line (Tim) and increase the
!                 avoidance radius to 1.3*fwhm for distinguishing 
!                 neighboring peaks;  2009 March 19
!      Version 2.9: Raise threshold in confused regions; 2009 June 16
!      Version 3.0: For a saturated star, take candidate position as centroid
!                 of the saturated region;  2009 Oct 12
!      Version 3.5: Optimize parameters for saturated star processing.
!      Version 3.5mf:      Multiframe version.
!                  Saturated star bug fix 2010 Oct 4.
!                  Change WCS variables to double precision 2010 Oct 7
!      Version 4:   Bug fix to saturated star filtering.
!               4.1; Fixed (at a cost in speed) inability to get sat. radii
!                    after masking is done.
!                    Other small performance enhancements.
!--------------------------------------------------------------------------

        implicit real(4) (a-h,o-z)
      implicit integer (i-n)

      real(4), allocatable :: Array(:,:), Larray(:), Lsubarray(:)
      real(4), allocatable :: C(:,:), Cm(:,:), Cb(:,:), U(:,:), b(:,:), bb(:,:), csig(:,:)
      real(4), allocatable :: bsc(:,:)
      real(4), allocatable :: match(:,:), matchset(:,:,:)
      real(4), allocatable :: pbstar(:)
      real(4), allocatable :: rsatset(:,:,:), srbstar(:)
      integer, allocatable :: ibstar(:),jbstar(:),satmask(:,:),Msk(:,:)
      logical, allocatable :: notseen(:,:), mbox(:,:)
      real(4), parameter :: dtor = 0.0174533
      real(4), parameter :: blankpix = -999.
      integer, parameter :: maxsources = 1000000
      integer, parameter :: maxann = 10000
      character(len=256), dimension(99) :: arg
c      character(len=256) :: s0,outlist,matchout,infile,outfile            ! JWF B60516
      character(len=256) :: s0,outlist,matchout                           ! JWF B60516
c      character(len=256) :: image1,image2,image3,image4                   ! JWF B60516
c      character(len=256) :: sigimage1,sigimage2,sigimage3,sigimage4       ! JWF B60516
c      character(len=256) :: cmask1,cmask2,cmask3,cmask4                   ! JWF B60516
      character(len=256) :: imagelist(4),sigimagelist(4),cmasklist(4)
c      character(len=256) :: svblist(4),svb1,svb2,svb3,svb4                ! JWF B60516 
      character(len=256) :: svblist(4)                                    ! JWF B60516 
c   character(len=80) :: offsetform                                     ! JWF B60516
      integer sigfigs
       real(8) ra, dec, x8, y8, ra0, dec0, x0, y0, pixel8, theta
      real(8) crval1, crval2, cdelt1, cdelt2, crota, crpix1, crpix2
      real(8) ra0c, dec0c, pixelc, thetac, x0c, y0c, rot
       real(8) crval1m, crval2m, cdelt1m, cdelt2m, rotm, crpix1m, crpix2m
      real(4) fwhm(4),focalpix(4),satradii(4),annvalues(maxann)
      real(4) xcan(maxsources), ycan(maxsources), scan(maxsources)
      real(4) xband(maxsources), yband(maxsources), sband(maxsources)
      integer bandlist(4), backwindow
c      logical confused,multiframe,newpeak,keepblanking,dbgmatched         ! JWF B60516
      logical confused,multiframe,newpeak,keepblanking                    ! JWF B60516
      integer*4 ixbr, iybr                                                ! JWF B60516

      data outlist,matchout /" "," "/
      data backwindow,threshold,fwhm /0, 0., 6., 6., 9., 15./
      data focalpix /2.75, 2.75, 2.75, 5.5/
      data bandlist /0,0,0,0/
      data ixbr/0/, iybr/0/                                               ! JWF B60516
      
      write (6,'(a)') 'MDET Version 4.1;  2013 June 14'
      print *, '$Id: mdet.f 12790 2013-06-15 00:38:05Z tim $'

      istat = 0
c      dbgmatched = .false.

! Read the command arguments
      narg = 0
      L = 1
      j = 0

      do while (L > 0)
          j = j+1
          L = 0
          call getarg (j,s0)
          L = numchar (s0)
          narg = narg+1
          arg(narg) = s0
      enddo

      call mparg (narg,arg,backwindow,threshold,fwhm,focalpix,nbands,bandlist,
     *          imagelist,sigimagelist,cmasklist,outlist,matchout,svblist,
     *          confused,multiframe,sigfigs,istat)
      if (istat > 1) call exit(istat)

! Get some FITS header information.
      call headpar(nx,ny,imagelist(1),ra0,dec0,cdelt1,pixel8,
     *          theta,x0,y0,istat)
      pixel = pixel8
      if (istat > 1) call exit(istat)
      if (pixel < 0 .or. cdelt1 > 0) then 
          print *,'MDET: Negative CDELT1 and positive CDELT2 required'
          call exit(4)
      endif
c      cd0 = cos(dec0*dtor)                                                ! JWF B60516
      noffsets = 0

      if (nbands >= 2) then
          do iband = 2,nbands 
            call headpar(naxis1,naxis2,imagelist(iband),crval1,
     *                crval2,cdelt1,cdelt2,crota,crpix1,crpix2,istat)
            if (istat > 1) call exit(istat)
            if (naxis1 < nx) nx = naxis1
            if (naxis2 < ny) ny = naxis2
          enddo
          if (noffsets > 0) print *,' '
      endif

! Allocate space for matched filter arrays.
      allocate (matchset(nx,ny,nbands))
      allocate (match(nx,ny))
      allocate (rsatset(nx,ny,nbands))
      allocate (satmask(nx,ny))
        if (.not.allocated(matchset) .or. .not.allocated(match) .or.
     *          .not.allocated(rsatset) .or. .not.allocated(satmask)) call exit(5)
      rsatset = 0.

! Read input images for all bands.
      do iband = 1,nbands 
          allocate (C(nx,ny))
          allocate (Cm(nx,ny))
          allocate (Cb(nx,ny))
          allocate (U(nx,ny))
          allocate (Msk(nx,ny))
          allocate (b(nx,ny))
          allocate (bb(nx,ny))
          allocate (bsc(nx,ny))
          allocate (csig(nx,ny))
          if (.not.allocated(C) .or. .not.allocated(U) .or.
     *            .not.allocated(b) .or. .not.allocated(csig)) call exit(5)

! Read the coadded image.
          call headpar(nxc,nyc,imagelist(iband),ra0c,dec0c,cdelt1,
     *            pixelc,thetac,x0c,y0c,istat)
          Lsize = nxc*nyc
          allocate (Larray(Lsize))
          allocate (Lsubarray(Lsize))
          allocate (Array(nxc,nyc))
          if (.not.allocated(Larray) .or. .not.allocated(Lsubarray) .or. 
     *            .not.allocated(Array)) call exit(5)
          call mreadimage(nxc,nyc,lsize,Larray,imagelist(iband),
     *            crval1,crval2,cdelt1,cdelt2,rot,crpix1,crpix2,istat)
          if (istat > 1) call exit(istat)
          call shuffle(Larray,nxc,nyc,Array)
          C = Array(1:nx, 1:ny)
          peakimage = maxval(C)

! Read uncertainty image.
          call headpar(nxu,nyu,sigimagelist(iband),crval1,crval2, 
     *            cdelt1,cdelt2,rot,crpix1,crpix2,istat)
          if (istat > 1) call exit(istat)
          if (nxu /= nxc .or. nyu /= nyc) then
            print *,
     *                'MDET: Coadd and uncertainty images incompatible'
            call exit(4)
          endif
          call mreadimage(nxu,nyu,lsize,Larray,sigimagelist(iband),
     *            crval1,crval2,cdelt1,cdelt2,rot,crpix1,crpix2,istat)
          if (istat > 1) call exit(istat)
          call shuffle(Larray,nxc,nyc,Array)
          U = Array(1:nx, 1:ny)
          Umax = maxval(U)
          where(U == blankpix .or. U == 0) U = Umax

! Replace uncertainty image with its median.
          if (multiframe) then
            nonblank = 0
            do i = 1,lsize
                if(Larray(i) /= blankpix .and. Larray(i) /= 0) then
                  nonblank = nonblank+1
                  Lsubarray(nonblank) = Larray(i)
                endif
            enddo
              call medsort(Lsubarray,nonblank,umed,q1,q2)
            U = umed
            !print *,'Unc image replaced with median = ',umed
          endif

! Read coadd mask.
          call headpar(nxm,nym,cmasklist(iband),crval1m,crval2m, 
     *            cdelt1m,cdelt2m,rotm,crpix1m,crpix2m,istat)
          if (istat > 1) call exit(istat)
          if (nxm /= nxc .or. nym /= nyc) then
            print *,
     *                'MDET: Coadd mask incompatible with image'
            call exit(4)
          endif
          call mreadimage(nxm,nym,lsize,Larray,cmasklist(iband),
     *            crval1m,crval2m,cdelt1m,cdelt2m,rotm,crpix1m,crpix2m,istat)
          if (istat > 1) call exit(istat)
          call shuffle(Larray,nxc,nyc,Array)
          Msk = nint(Array(1:nx, 1:ny))

! Calculate slowly-varying background and write it out.
          width = 2*nint(backwindow/2.) + 1

! First mask out the NBRIGHT stars for which SNR > BTHRESH, using a mask width
! corresponding to half the width of the median filtering window.  To detect
! these NBRIGHT stars, use an approximate procedure in which the background
! estimation is based on sampling every ISKIPth pixel.  Limit the masked area
! to at most a third of the image.
          Cb = C
          where(Msk==100) Cb=blankpix
          iskip = 8
          bthresh = 100.
          fwhmpix = fwhm(bandlist(iband))/(3600.*pixel)
          iblank = min(nint(25.*fwhmpix), int(width/2.0))

          if (multiframe) then
            call svb_mf(Cb,U,nx,ny,width,iskip,bb,csig,multiframe)
          else
              call svb(Cb,U,nx,ny,width,iskip,bb,csig)
          endif

          call findpeaks((C-bb)/U,nx,ny,bthresh,xcan,ycan,scan,nbright,
     *            maxsources)
          allocate (ibstar(nbright))
          allocate (jbstar(nbright))
          allocate (srbstar(nbright))
          allocate (pbstar(nbright))
          Cm = C
          Cb = C
          bsc = 0.
          satmask = 0
          ibright = 0
          srbstar = 0.
          nsatflagged = 0
          nsatr = 0
          totsat = sum(Msk, MASK=Msk==100)/100. ! Total no. saturated pixels
          isearchrad = nint(focalpix(bandlist(iband))/(3600.*pixel))

          print * 
          write(*,'(a,i1,a,i6,a,f8.1)') 
     1               'Band=', bandlist(iband), ', NBrt=',nbright,', TotSat=',totsat

          if (nbright /= 0) then
            jpeak = 0
            keepblanking = .true.
            do while (jpeak < nbright)
                jpeak = jpeak + 1
                ipeak = nbright-jpeak+1
                ibr = nint(xcan(ipeak))
                jbr = nint(ycan(ipeak))
                newpeak = .false.
                if (Cb(ibr,jbr) /= blankpix) newpeak = .true.
c                dbgmatched = abs(ibr-1081).le.iblank .and. abs(jbr-1055).le.iblank
c                if(dbgmatched) write(*,'(a,i1,1x,i6,i5,i5,1pe9.2,l,1x,0pf5.3,i7,i6)') 
c     1                      '    b,n,x,y,snr,newpk,bfrac,nsat,nsave = ',
c     1                      bandlist(iband),ipeak,ibr,jbr,scan(ipeak),
c     1                      newpeak,blankfrac,nsatflagged,ibright
c
                iblo = max(ibr-iblank, 1)
                ibhi = min(ibr+iblank, nx)
                jblo = max(jbr-iblank, 1)
                jbhi = min(jbr+iblank, ny)

                if(keepblanking) then
                  do j = jblo,jbhi
                  do i = iblo,ibhi
                    Cm(i,j) = blankpix
                  enddo
                  enddo
                  blankpixcount = sum(Cm, MASK=Cm==blankpix)/blankpix
                  blankfrac = blankpixcount/(nx*ny)
                  if (blankfrac > 0.3333) keepblanking = .false.
                end if ! keepblanking

                if (newpeak) then
                  ! blank out an area around this peak so it won't be used again
                  blankr = 5*fwhmpix
                  jblankr = nint(blankr)
                  blankrsq = blankr**2
                  ilo = max(ibr-jblankr, 1)
                  ihi = min(ibr+jblankr, nx)
                  jlo = max(jbr-jblankr, 1)
                  jhi = min(jbr+jblankr, ny)
                  nsat = 0
                  do j = jlo,jhi
                  do i = ilo,ihi
                    if (float((i-ibr)**2+(j-jbr)**2)<=blankrsq) then
                      Cb(i,j) = blankpix
                    end if
                    if(Msk(i,j) == 100) nsat = nsat + 1
                  enddo
                  enddo

                  nsatpix = 0
                  if(keepblanking) then
                  ! get sat radius

                        ! Get the regional peak
                  cmax = -1.e35
                  do j = jblo,jbhi
                    do i = iblo,ibhi
                      if (C(i,j)-bb(i,j) > cmax) then
                        ixbr = i
                        iybr = j
                        cmax = C(i,j) - bb(i,j)
                      endif
                    enddo
                  enddo

                  ibright = ibright + 1
                  ibstar(ibright) = ixbr
                  jbstar(ibright) = iybr      
                  pbstar(ibright) = cmax

                  call segmenter(Msk,nx,ny,iblo,ibhi,jblo,jbhi,
     1                                 isearchrad,ixsat,iysat,satradius,nsatpix)
                  else 
                  ! Special call on smaller area after masking is done
                  ! to compute sat radius for this source. Only run if
                        ! it's possible for the result to be big enough for the
                  ! worth the trouble
                  if(nsat .gt. 3.14*(fwhmpix*0.75)**2) then
                    call segmenter(Msk,nx,ny,ilo,ihi,jlo,jhi,
     1                                   isearchrad,ixsat,iysat,satradius,nsatpix)
                  end if
                  end if

                  if (nsatpix > 0) then
                  nsatr = nsatr + 1
                  isatradius = nint(satradius)
                  rsatarcsec = satradius*pixelc*3600.
                  dsatsq = (2.*satradius)**2
                  ilo = max(ixsat-2*isatradius, 1)
                  ihi = min(ixsat+2*isatradius, nx)
                  jlo = max(iysat-2*isatradius, 1)
                  jhi = min(iysat+2*isatradius, ny)
                  do j = jlo,jhi
                  do i = ilo,ihi
                      if (float((i-ixsat)**2+(j-iysat)**2)<=dsatsq) then
                        if(satmask(i,j) .eq. 0) then
                                  if(Msk(i,j) .eq. 100) nsatflagged = nsatflagged + 1
                          satmask(i,j) = 1
                          rsatset(i,j,iband) = rsatarcsec
                        endif
                      endif
                  enddo
                  enddo
                  if(keepblanking) then
                    ibstar(ibright) = ixsat
                    jbstar(ibright) = iysat
                    srbstar(ibright) = satradius
                  end if
c                  if(dbgmatched) print *, '     Saved: n,xs,ys,satradius = ',
c     1                                 ibright,ixsat,iysat,satradius
                  endif ! nsatpix > 0

                endif ! newpeak
            enddo ! jpeak<nbright

          endif ! nbright>0

          print *,'Number of non-zero saturation radii = ', nsatr

! Also blank out all areas flagged with saturation in prep for svb computation ! TPC
! ... or not
          where(Msk==100) Cm = blankpix  ! TPC

! Now median filter the remaining pixels.
          print *,'Computing SVB for band ',bandlist(iband),' ...'
          if (multiframe) then
            call svb_mf(Cm,U,nx,ny,width,1,b,csig,multiframe)
          else
            call svb(Cm,U,nx,ny,width,1,b,csig)
          endif

! Increase the local confusion error in the vicinity of each saturated star.
          if (ibright > 0) then
            annwid = fwhmpix/4.
            rin = 3.*fwhmpix
            nannuli = (iblank - rin)/annwid

            do ii = 1,ibright
              ixbr = ibstar(ii)
              iybr = jbstar(ii)
              if (satmask(ixbr,iybr)==1) then
                iblo = max(ixbr-iblank, 1)
                ibhi = min(ixbr+iblank, nx)
                jblo = max(iybr-iblank, 1)
                jbhi = min(iybr+iblank, ny)
                do j = jblo,jbhi
                do i = iblo,ibhi
                  rval = sqrt(float((i-ixbr)**2 + (j-iybr)**2))
                  if (rval < rin) bsc(i,j) = pbstar(ii)
                enddo
                enddo
                do ian = 1,nannuli
                  ran = rin + (ian-1)*annwid
                  nvalues = 0
                  do j = jblo,jbhi
                  do i = iblo,ibhi
                      rval = sqrt(float((i-ixbr)**2 + (j-iybr)**2))
                      if (rval > ran .and. rval <= ran+annwid .and.
     *                              nvalues < maxann) then
                        nvalues = nvalues + 1
                        annvalues(nvalues) = C(i,j) - b(i,j)
                      endif
                  enddo
                  enddo
                  if (nvalues > 4) then
                      call medsort(annvalues,nvalues,amed,q1,q2)
                      annsig = (q2 - q1)/2.
                      do j = jblo,jbhi
                      do i = iblo,ibhi
                        rval = sqrt(float((i-ixbr)**2 + (j-iybr)**2))
                        if (rval > ran .and. rval <= ran+annwid) 
     *                        bsc(i,j) = sqrt(amed**2 + annsig**2) 
                      enddo
                      enddo
                  else
                      do j = jblo,jbhi
                      do i = iblo,ibhi
                        bsc(i,j) = pbstar(ii)
                      enddo
                      enddo
                  endif
                enddo
                bsc(ixbr,iybr) = 0.
                C(ixbr, iybr) = 100.*(C(ixbr, iybr) + 
     *                  peakimage*srbstar(ii)**2)
              endif ! satmask
            enddo
          endif
          deallocate(ibstar)
          deallocate(jbstar)
          deallocate(srbstar)
          deallocate(pbstar)

! Add this term to the sum.
          where(C /= blankpix) C = C - b
          where(C < 0) C = 0
          if (.not.confused) csig = 0.
          where(bsc > csig) csig = bsc
          C = C**2 / (U**2 + csig**2)
          match = match + C

          do j = 1,ny
          do i = 1,nx
            matchset(i,j,iband) = C(i,j)
          enddo
          enddo

! If requested, reduce pixel precision
          if(sigfigs .gt. 0) then
            iscale =  2**floor(sigfigs/log10(2.0)+1)
            do j = 1,ny
            do i = 1,nx
              call impixprcf(b(i,j), iscale, out)
              b(i,j) = out;
            enddo
            enddo
          end if
          if (svblist(iband) /= ' ') then
              !!!! b = (C-bb)/U !!! DEBUG TPC!!!
                 call writeimage(b,nx,ny,imagelist(1),svblist(iband))
          end if
          deallocate (Larray)
          deallocate (Lsubarray)
          deallocate (Array)
          deallocate (C)
          deallocate (Cm)
          deallocate (Cb)
          deallocate (U)
          deallocate (Msk)
          deallocate (b)
          deallocate (bb)
          deallocate (bsc)
          deallocate (csig)
      enddo

! Calculate multiband matched filter image and its set of single-band
! counterparts.
      match = sqrt(match)
      matchset = sqrt(matchset)
      
! Write out the matched filter image if necessary.
      if (matchout /= ' ') 
     *          call writeimage(match,nx,ny,imagelist(1),matchout)

! Find candidate sources.
      call findpeaks(match,nx,ny,threshold,xcan,ycan,scan,
     *          nsources,maxsources)

! Weed out duplicates, i.e. those within a certain critical radius of each 
! other.
      if (nsources > 0) then
          allocate (notseen(nx,ny))
          if (.not.allocated(notseen)) call exit(5)
          notseen = .true.
          rcrit = 1.3*minval((fwhm(1:nbands)/3600.)/pixel)
          satmask = 0
          do iband = 1,nbands
            satmask = satmask + nint(100.*rsatset(1:nx,1:ny,iband))
          enddo
          if (multiframe) then
            call weedout_mf(xcan,ycan,scan,nsources,rcrit,pixel,notseen,
     *                satmask,rsatset,nx,ny,nbands,0)
          else
            call weedout(xcan,ycan,scan,nsources,rcrit,notseen,satmask,nx,ny)
          endif

! Append the list with the results of single-band detections.
          if (nbands > 1) then 
            do iband = 1,nbands
                match = matchset(1:nx,1:ny,iband)
                call findpeaks(match,nx,ny,threshold,xband,yband,
     *                  sband,newsources,maxsources)
                if (newsources > 0) then

! First, regenerate the "notseen" array.
                  notseen = .true.
                      rcrit = 1.3*(fwhm(bandlist(iband))/3600.)/pixel
                  imb = nint(rcrit)
                  nmb = 2*imb + 1
                  allocate (mbox(nmb,nmb))
                  mbox = .true.
                  rcritsq = rcrit**2
                  icmb = imb + 1
                  cmb = float(icmb)
                  do j = 1,nmb
                  do i = 1,nmb
                        if((i-cmb)**2+(j-cmb)**2<=rcritsq) mbox(i,j)=.false. 
                  enddo
                  enddo
                  do n = 1,nsources
                          i = nint(xcan(n))
                          j = nint(ycan(n))
                      ilo = max(i-imb, 1)
                      ihi = min(i+imb, nx)
                      jlo = max(j-imb, 1)
                      jhi = min(j+imb, ny)
                      do jj = jlo,jhi
                      do ii = ilo,ihi
                            if (.not.mbox(icmb-i+ii,icmb-j+jj)) 
     *                            notseen(ii,jj)=.false.
                      enddo
                      enddo
                  enddo
                  deallocate(mbox)

! Now get rid of sources from this band which have been seen before.
                  satmask = nint(100.*rsatset(1:nx,1:ny,iband))
                  if (multiframe) then
                        call weedout_mf(xband,yband,sband,newsources,rcrit,
     *                      pixel,notseen,satmask,rsatset,nx,ny,nbands,iband)
                  else
                        call weedout(xband,yband,sband,newsources,
     *                      rcrit,notseen,satmask,nx,ny)
                  endif

! Finally, append the unique sources from this band to the master list.
                      call appendlist(xcan,ycan,scan,nsources,
     *                      xband,yband,sband,newsources,
     *                      maxsources)
                endif
            enddo
          endif
          deallocate(notseen)
          write(6,'("There were",i6," candidates above SNR =",f6.2)')
     *            nsources,threshold
      else
          print *,'No candidate sources found'
      endif

! Convert (x,y) pixel locations to RA and Dec and write out the candidate list.
      open (unit=1, file=outlist, status='unknown', form='formatted')
      write(*,'(a)') 'Writing out '//outlist
      write(1,'("\Nsrcs =",i10)') nsources
      write(1,'(a)') '| Src  |    RA    |    Dec   |   SNR   | Rsat1 | Rsat2 | Rsat3 | Rsat4 |'
      write(1,'(a)') '|  i   |    r     |     r    |    r    |   r   |   r   |   r   |   r   |'
      write(1,'(a)') '|      |   deg    |    deg   |         | arcsec| arcsec| arcsec| arcsec|'

      do n = 1,nsources
          x8 = xcan(n)
          y8 = ycan(n)
          call pix2eq(x8,y8,ra0,dec0,x0,y0,pixel8,theta,ra,dec)
          satradii = 0.
          do iband = 1,nbands
            rsatband = rsatset(nint(xcan(n)),nint(ycan(n)),iband)
            if (rsatband > 0.) satradii(iband) = rsatband + focalpix(iband)
          enddo
          if (ra < 0.) ra = ra + 360.
          if (ra >= 360.) ra = ra - 360.
          write(1,'(i7,2f11.5,1pe10.3,0pf8.2,3f8.2)') n,ra,dec,scan(n),
     *            satradii
      enddo

      close(unit=1)
      deallocate (match)
      deallocate (matchset)
      deallocate (rsatset)
      deallocate (satmask)
      call exit (istat)

      end
c
      include 'includes_mdet.f'
      