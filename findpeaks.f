	subroutine findpeaks(a,nx,ny,threshold,x,y,s,nsources,maxsources)

! Find all local maxima stronger than THRESHOLD in image A, and return a list 
! of pixel locations x,y and signal levels, ordered in increasing strength.

	implicit real(4) (a-h,o-z)
	implicit integer(i-n)
	real(4) a(nx,ny),x(*),y(*),s(*),box(3,3)

	nsources = 0

	do j = 2,ny-1
	do i = 2,nx-1
	  if (a(i,j) > threshold) then
	    box = a(i-1:i+1, j-1:j+1)
	    box(2,2) = -1.e35
	    if (a(i,j) > maxval(box)) then
	      if (nsources < maxsources) then
		nsources = nsources + 1
		x(nsources) = i
		y(nsources) = j
		s(nsources) = a(i,j)
	      endif
	    endif
	  endif
	enddo
	enddo

	if (nsources == 0) return
	if (nsources == maxsources) 
     *	    write(6,'(a)') 'FINDPEAKS: Maximum source count exceeded'

! Order in increasing strength.
	call groupsort(nsources,s,x,y)
	return

	end

