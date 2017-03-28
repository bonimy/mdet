	subroutine weedout_mf(x,y,sc,nsources,rcrit,pixel,notseen,satmask,
     *	    rsatset,nx,ny,nbands,isingleband)

! Weed out likely duplicate sources, i.e. those within a certain critical 
! radius of a previously-seen source.  The source list comes in ascending
! order of source strength and leaves in descending order.

	implicit real(4) (a-h,o-z)
	implicit integer(i-n)
	real(4), allocatable :: xw(:),yw(:),sw(:)
	logical, allocatable :: mbox(:,:)
	real(4) x(*),y(*),sc(*)
	real(4) rsatset(nx,ny,nbands)
	integer satmask(nx,ny)
	logical notseen(nx,ny)

	imb = nint(rcrit)
	nmb = 2*imb + 1
	allocate (mbox(nmb,nmb))
	mbox = .true.
	rcritsq = rcrit**2
	icmb = imb + 1
	cmb = float(icmb)
	do j = 1,nmb
	do i = 1,nmb
	    if((i-cmb)**2+(j-cmb)**2 <= rcritsq) mbox(i,j)=.false.       
	enddo
	enddo

	allocate (xw(nsources))
	allocate (yw(nsources))
	allocate (sw(nsources))
	m = 0

	do nn = 1,nsources
	    n = nsources - nn + 1
	    i = nint(x(n))
	    j = nint(y(n))
	    if (notseen(i,j)) then 
	      m = m+1
	      xw(m) = x(n)
	      yw(m) = y(n)
	      sw(m) = sc(n)
	      if (isingleband==0) then
	  	rcritx = 2.*maxval(rsatset(i,j,1:nbands))/(3600.*pixel)
	      else
	  	rcritx = 2.*rsatset(i,j,isingleband)/(3600.*pixel)
	      endif
	      if (satmask(i,j)==0 .or. rcritx < rcrit) then 
		ilo = max(i-imb, 1)
		ihi = min(i+imb, nx)
		jlo = max(j-imb, 1)
		jhi = min(j+imb, ny)
		do jj = jlo,jhi
		do ii = ilo,ihi
		    if (.not.mbox(icmb-i+ii,icmb-j+jj)) 
     *			notseen(ii,jj)=.false.
		enddo
		enddo
	      else
		rcritsqx = rcritx**2
		imbx = nint(rcritx)
		ilo = max(i-imbx, 1)
		ihi = min(i+imbx, nx)
		jlo = max(j-imbx, 1)
		jhi = min(j+imbx, ny)
		do jj = jlo,jhi
		do ii = ilo,ihi
		    if((ii-i)**2+(jj-j)**2 <= rcritsqx) notseen(ii,jj)=.false.
		enddo
		enddo
	      endif
	    endif
	enddo

	deallocate(mbox)
	nsources = m

	do i = 1,nsources
	    x(i) = xw(i)
	    y(i) = yw(i)
	    sc(i) = sw(i)
	enddo
	    
	deallocate(xw)
	deallocate(yw)
	deallocate(sw)
	return

	end
