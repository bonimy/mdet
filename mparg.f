	subroutine mparg (narg,arg,backwindow,threshold,fwhm,focalpix,nbands,
     *	    bandlist,imagelist,sigimagelist,cmasklist,outlist,matchout,svblist,
     *      confused,multiframe,sigfigs,istat)

! Parse the command line input strings and return the input parameters
! needed by MDET.

	implicit integer (i-n)
        implicit real(4) (a-h)
        implicit real(4) (o-z)

	character(len=256) :: arg(narg),image1,image2,image3,image4
	character(len=256) :: sigimage1,sigimage2,sigimage3,sigimage4
	character(len=256) :: cmask1,cmask2,cmask3,cmask4
	character(len=256) :: imagelist(4),sigimagelist(4),cmasklist(4)
	character(len=256) :: svblist(4),outlist,matchout,svb1,svb2,svb3,svb4
	character(len=20) :: backwin,thresh,fw,sf
	character(len=25) :: key
	real(4) fwhm(4),focalpix(4)
	integer nkey,narg,numchar,bandlist(4),L,backwindow,sigfigs
	logical confused,multiframe

	data image1,image2,image3,image4 /" "," "," "," "/
	data sigimage1,sigimage2,sigimage3,sigimage4 /" "," "," "," "/
	data cmask1,cmask2,cmask3,cmask4 /" "," "," "," "/
	data svb1,svb2,svb3,svb4 /" "," "," "," "/
	
	key = '-image1'

        call findkey (narg,arg,key,nkey)
        if (nkey.gt.0) then
          L = numchar(arg(nkey+1))
          image1 = arg(nkey+1)(1:L)
        endif

	key = '-image2'

	call findkey (narg,arg,key,nkey)
	if (nkey.gt.0) then
	  L = numchar(arg(nkey+1))
	  image2 = arg(nkey+1)(1:L)
	endif

	key = '-image3'

        call findkey (narg,arg,key,nkey)
        if (nkey.gt.0) then
          L = numchar(arg(nkey+1))
          image3 = arg(nkey+1)(1:L)
        endif

	key = '-image4'

        call findkey (narg,arg,key,nkey)
        if (nkey.gt.0) then
          L = numchar(arg(nkey+1))
          image4 = arg(nkey+1)(1:L)
        endif

	key = '-sigimage1'

        call findkey (narg,arg,key,nkey)
        if (nkey.gt.0) then
          L = numchar(arg(nkey+1))
          sigimage1 = arg(nkey+1)(1:L)
        endif

	key = '-sigimage2'

        call findkey (narg,arg,key,nkey)
        if (nkey.gt.0) then
          L = numchar(arg(nkey+1))
          sigimage2 = arg(nkey+1)(1:L)
        endif

        key = '-sigimage3'

        call findkey (narg,arg,key,nkey)
        if (nkey.gt.0) then
          L = numchar(arg(nkey+1))
          sigimage3 = arg(nkey+1)(1:L)
        endif

	key = '-sigimage4'

        call findkey (narg,arg,key,nkey)
        if (nkey.gt.0) then
          L = numchar(arg(nkey+1))
          sigimage4 = arg(nkey+1)(1:L)
        endif

	key = '-cmask1'

        call findkey (narg,arg,key,nkey)
        if (nkey.gt.0) then
          L = numchar(arg(nkey+1))
          cmask1 = arg(nkey+1)(1:L)
        endif

	key = '-cmask2'

        call findkey (narg,arg,key,nkey)
        if (nkey.gt.0) then
          L = numchar(arg(nkey+1))
          cmask2 = arg(nkey+1)(1:L)
        endif

        key = '-cmask3'

        call findkey (narg,arg,key,nkey)
        if (nkey.gt.0) then
          L = numchar(arg(nkey+1))
          cmask3 = arg(nkey+1)(1:L)
        endif

	key = '-cmask4'

        call findkey (narg,arg,key,nkey)
        if (nkey.gt.0) then
          L = numchar(arg(nkey+1))
          cmask4 = arg(nkey+1)(1:L)
        endif

	key = '-backwindow'

        call findkey (narg,arg,key,nkey)
        if (nkey.gt.0) then
          L = numchar(arg(nkey+1))
          backwin = arg(nkey+1)(1:L)
	  read(backwin,*) backwind
	  backwindow = nint(backwind)
        endif

	key = '-threshold'

        call findkey (narg,arg,key,nkey)
        if (nkey.gt.0) then
          L = numchar(arg(nkey+1))
          thresh = arg(nkey+1)(1:L)
	  read(thresh,*) threshold
        endif

	key = '-fwhm1'

        call findkey (narg,arg,key,nkey)
        if (nkey.gt.0) then
          L = numchar(arg(nkey+1))
          fw = arg(nkey+1)(1:L)
	  read(fw,*) fwhm(1)
        endif

	key = '-fwhm2'

        call findkey (narg,arg,key,nkey)
        if (nkey.gt.0) then
          L = numchar(arg(nkey+1))
          fw = arg(nkey+1)(1:L)
	  read(fw,*) fwhm(2)
        endif

	key = '-fwhm3'

        call findkey (narg,arg,key,nkey)
        if (nkey.gt.0) then
          L = numchar(arg(nkey+1))
          fw = arg(nkey+1)(1:L)
	  read(fw,*) fwhm(3)
        endif

	key = '-fwhm4'

        call findkey (narg,arg,key,nkey)
        if (nkey.gt.0) then
          L = numchar(arg(nkey+1))
          fw = arg(nkey+1)(1:L)
	  read(fw,*) fwhm(4)
        endif

	key = '-focalpix1'

        call findkey (narg,arg,key,nkey)
        if (nkey.gt.0) then
          L = numchar(arg(nkey+1))
          fw = arg(nkey+1)(1:L)
	  read(fw,*) focalpix(1)
        endif

	key = '-focalpix2'

        call findkey (narg,arg,key,nkey)
        if (nkey.gt.0) then
          L = numchar(arg(nkey+1))
          fw = arg(nkey+1)(1:L)
	  read(fw,*) focalpix(2)
        endif

	key = '-focalpix3'

        call findkey (narg,arg,key,nkey)
        if (nkey.gt.0) then
          L = numchar(arg(nkey+1))
          fw = arg(nkey+1)(1:L)
	  read(fw,*) focalpix(3)
        endif

	key = '-focalpix4'

        call findkey (narg,arg,key,nkey)
        if (nkey.gt.0) then
          L = numchar(arg(nkey+1))
          fw = arg(nkey+1)(1:L)
	  read(fw,*) focalpix(4)
        endif

	key = '-outlist'

        call findkey (narg,arg,key,nkey)
        if (nkey.gt.0) then
          L = numchar(arg(nkey+1))
          outlist = arg(nkey+1)(1:L)
        endif

	key = '-matchout'

        call findkey (narg,arg,key,nkey)
        if (nkey.gt.0) then
          L = numchar(arg(nkey+1))
          matchout = arg(nkey+1)(1:L)
        endif

	key = '-svb1'

        call findkey (narg,arg,key,nkey)
        if (nkey.gt.0) then
          L = numchar(arg(nkey+1))
          svb1 = arg(nkey+1)(1:L)
        endif

	key = '-svb2'

        call findkey (narg,arg,key,nkey)
        if (nkey.gt.0) then
          L = numchar(arg(nkey+1))
          svb2 = arg(nkey+1)(1:L)
        endif

        key = '-svb3'

        call findkey (narg,arg,key,nkey)
        if (nkey.gt.0) then
          L = numchar(arg(nkey+1))
          svb3 = arg(nkey+1)(1:L)
        endif

	key = '-svb4'

        call findkey (narg,arg,key,nkey)
        if (nkey.gt.0) then
          L = numchar(arg(nkey+1))
          svb4 = arg(nkey+1)(1:L)
        endif

	key = '-svbprec'

        call findkey (narg,arg,key,nkey)
	sigfigs = 0
        if (nkey.gt.0) then
          L = numchar(arg(nkey+1))
          sf = arg(nkey+1)(1:L)
	  read(sf,*) sigf
	  sigfigs = nint(sigf)
        endif

	key = '-c'

        call findkey (narg,arg,key,nkey)
	confused = .false.
        if (nkey.gt.0) confused = .true.

	key = '-m'

        call findkey (narg,arg,key,nkey)
	multiframe = .false.
        if (nkey.gt.0) multiframe = .true.

	write(6,'(/a)') 'INPUT PARAMETERS:'
	write(6,'(a)') 'image1 = '//trim(image1)
	write(6,'(a)') 'image2 = '//trim(image2)
	write(6,'(a)') 'image3 = '//trim(image3)
	write(6,'(a)') 'image4 = '//trim(image4)
	write(6,'(a)') 'sigimage1 = '//trim(sigimage1)
	write(6,'(a)') 'sigimage2 = '//trim(sigimage2)
	write(6,'(a)') 'sigimage3 = '//trim(sigimage3)
	write(6,'(a)') 'sigimage4 = '//trim(sigimage4)
	write(6,'(a,f6.1,a)') 'backwindow = ',backwind,' coadd pixels'
	write(6,'(a,f6.2,a)') 'threshold = ',threshold,' sigmas'
	write(6,'(a,4f6.2,a)') 'fwhm1-4 = ',fwhm,' arcsec'
	write(6,'(a,4f6.2,a)') 'focalpix1-4 = ',focalpix,' arcsec'
	write(6,'(a)') 'outlist = '//trim(outlist)
	write(6,'(a)') 'matchout = '//trim(matchout)
	write(6,'(a)') 'svb1 = '//trim(svb1)
	write(6,'(a)') 'svb2 = '//trim(svb2)
	write(6,'(a)') 'svb3 = '//trim(svb3)
	write(6,'(a)') 'svb4 = '//trim(svb4)
	write(6,'(a,i2)') 'SVB sigfigs = ',sigfigs
	if (confused) print *,'Flux threshold will be adjusted for confusion'
	if (multiframe) print *,'USING MULTIFRAME VERSION'
	print *,' '

! How many bands will we be processing?
	nbands = 0
	if (image1 /= ' ') then
	    nbands = nbands + 1
	    bandlist(nbands) = 1
	    imagelist(nbands) = image1
	    sigimagelist(nbands) = sigimage1
	    cmasklist(nbands) = cmask1
	    svblist(nbands) = svb1
	endif
	if (image2 /= ' ') then
	    nbands = nbands + 1
	    bandlist(nbands) = 2
	    imagelist(nbands) = image2
	    sigimagelist(nbands) = sigimage2
	    cmasklist(nbands) = cmask2
	    svblist(nbands) = svb2
	endif
	if (image3 /= ' ') then
	    nbands = nbands + 1
	    bandlist(nbands) = 3
	    imagelist(nbands) = image3
	    sigimagelist(nbands) = sigimage3
	    cmasklist(nbands) = cmask3
	    svblist(nbands) = svb3
	endif
	if (image4 /= ' ') then
	    nbands = nbands + 1
	    bandlist(nbands) = 4
	    imagelist(nbands) = image4
	    sigimagelist(nbands) = sigimage4
	    cmasklist(nbands) = cmask4
	    svblist(nbands) = svb4
	endif

	if (nbands == 0) then
	    print *,'MDET: No input images specified'
	    istat = 2
	endif
	if (backwindow == 0) then
	    print *,
     *		'MDET: I need a BACKWINDOW size [coadd pixels] (real)'      
	    istat = 2
	endif
	if (threshold == 0) then
	    print *,'MDET: I need a THRESHOLD [sigmas] (real)'
	    istat = 2
	endif
	if (outlist == ' ') then
	    print *,'MDET: I need an OUTLIST (character string)'
	    istat = 2
	endif

	return

	end




	subroutine findkey (narg,arg,key,nkey)
	character*(*) arg(narg),key
	character*256 string
	integer nkey,nk,n,j,narg,numchar

	nk = numchar(key)
	nkey = 0

	do j=1,narg
	  string = arg(j)
	  n = numchar(string)

	  if (string(1:n).eq.key(1:nk)) then
		nkey = j
		return
	  endif
	enddo

	return
	end
