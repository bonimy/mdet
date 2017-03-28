	subroutine svb(a,unca,nx,ny,width,iskip,b,csig)

! Estimate slowly varying background of image, A, based on median-filtering 
! window of specified width [pixels].  Also do confusion estimation.
!
! Normally, the input parameter ISKIP should be set equal to 1.  But to make
! a quick estimate at the expense of some accuracy, set iskip at some larger
! integer, e.g. 8.

	implicit real(4) (a-h,o-z)
	implicit integer(i-n)
	real(4) a(nx,ny),unca(nx,ny),b(nx,ny),csig(nx,ny)
	real(4), allocatable :: abig(:,:), box(:,:), lbox(:)
	real(4), allocatable :: uncabig(:,:), ubox(:,:)
	real(4), allocatable :: kernel(:,:), kbox(:,:), kwt(:,:)
	real(4), allocatable :: vmed(:,:),bsum(:,:), bwt(:,:)
	real(4), allocatable :: vcon(:,:),csum(:,:),ckbox(:,:)
	real(4), parameter :: blankpix = -999.
	integer hw
	real(8) w8

! Pad the edges.
	w8 = width
	hw = int(w8)/2
	nxbig = nx + 4*hw + 1
	nybig = ny + 4*hw + 1
	allocate (abig(nxbig,nybig))
	allocate (uncabig(nxbig,nybig))
	ix = nx/2 + 1
	iy = ny/2 + 1
	ixbig = nxbig/2 + 1
	iybig = nybig/2 + 1
	abig = blankpix
	uncabig = blankpix
	do j = 1,ny
	do i = 1,nx
	    abig(ixbig+i-ix, iybig+j-iy) = a(i,j)
	    uncabig(ixbig+i-ix, iybig+j-iy) = unca(i,j)
	enddo
	enddo
	if (iskip /= 1 .and. iskip > hw/4) iskip = 1

! Calculate medians on a coarse grid.
	isampx = ix/hw
	isampy = iy/hw
	nsampx = 2*isampx + 1
	nsampy = 2*isampy + 1
	allocate (vmed(nsampx,nsampy))
	vmed = 0
	allocate (vcon(nsampx,nsampy))
	vcon = 0
	nbsize = 2*hw + 1
	allocate (box(nbsize,nbsize))
	allocate (ubox(nbsize,nbsize))
	nbsq = nbsize**2
	allocate (lbox(nbsq))

	do j = -isampy,isampy
	do i = -isampx,isampx
	    ivalue = ixbig + i*hw
	    jvalue = iybig + j*hw
	    box = abig(ivalue-hw:ivalue+hw, jvalue-hw:jvalue+hw)
	    ubox = uncabig(ivalue-hw:ivalue+hw, jvalue-hw:jvalue+hw)

	    if (any(box == blankpix) .or. iskip /= 1) then
		usum = 0.
		k = 0
		do jj = 1,nbsize,iskip
		do ii = 1,nbsize,iskip
		    if (box(ii,jj) /= blankpix) then
			k = k+1
			lbox(k) = box(ii,jj)
			usum = usum + ubox(ii,jj)
		    endif
		enddo
		enddo
		if (k /= 0) then
		    call medsort(lbox,k,bmed,q1,q2)
		    uncmean = usum/k
		else
		    bmed = blankpix
		    q1 = 0.
		    q2 = 0.
		    uncmean = 0.
		endif
	    else
	 	call medsort(box,nbsq,bmed,q1,q2)
		uncmean = sum(ubox)/nbsq
	    endif

	    vmed(i+isampx+1,j+isampy+1) = bmed
	    sigpix = (q2 - q1)/2.
	    vcon(i+isampx+1,j+isampy+1) = sqrt(max(sigpix**2-uncmean**2, 0.))
	enddo
	enddo

	call medsort(vmed, nsampx*nsampy, bsum0, q1, q2)
	call medsort(vcon, nsampx*nsampy, csum0, q1, q2)
	
! Interpolate using a Gaussian kernel.
	fwhm = 1.5*float(hw)
	ik = (3*hw)/2 
	ksize = 2*ik + 1
	allocate (kernel(ksize,ksize))
	allocate (kbox(ksize,ksize))
	allocate (kwt(ksize,ksize))
	call gausscalc(kernel,ksize,fwhm)
	kernel = kernel - kernel(1,ik+1)
	where (kernel < 0) kernel = 0
	allocate (bsum(nxbig,nybig))
	allocate (bwt(nxbig,nybig))
	allocate (csum(nxbig,nybig))
	allocate (ckbox(ksize,ksize))
	bsum = 0
	bwt = 0

	do j = -isampy,isampy
	do i = -isampx,isampx
	    ivalue = ixbig + i*hw
	    jvalue = iybig + j*hw
	    kbox = vmed(i+isampx+1,j+isampy+1)
	    ckbox = vcon(i+isampx+1,j+isampy+1)
	    kwt = kernel
	    where(kbox == blankpix) kwt = 0
	    kbox = kbox*kwt
	    ckbox = ckbox*kwt
	    do jj = -ik,ik
	    do ii = -ik,ik
		bsum(ivalue+ii,jvalue+jj) = bsum(ivalue+ii,jvalue+jj)
     *		    + kbox(ik+ii+1,ik+jj+1)
		bwt(ivalue+ii,jvalue+jj) = bwt(ivalue+ii,jvalue+jj)
     *		    + kwt(ik+ii+1,ik+jj+1)
		csum(ivalue+ii,jvalue+jj) = csum(ivalue+ii,jvalue+jj)
     *		    + ckbox(ik+ii+1,ik+jj+1)
	    enddo
	    enddo
	enddo
	enddo

	where(bwt /= 0) bsum = bsum/bwt
	if (any(bwt == 0)) where(bwt == 0) bsum = bsum0
	where(bwt /= 0) csum = csum/bwt
	if (any(bwt == 0)) where(bwt == 0) csum = csum0

	do j = 1,ny
	do i = 1,nx
	    b(i,j) = bsum(ixbig+i-ix, iybig+j-iy)
	    csig(i,j) = csum(ixbig+i-ix, iybig+j-iy)
	enddo
	enddo

	deallocate(abig)
	deallocate(uncabig)
	deallocate(box)
	deallocate(ubox)
	deallocate(lbox)
	deallocate(vmed)
	deallocate(kernel)
	deallocate (kbox)
	deallocate (kwt)
	deallocate(bsum)
	deallocate(bwt)
	deallocate(vcon)
	deallocate(csum)
	deallocate (ckbox)
	return

	end
