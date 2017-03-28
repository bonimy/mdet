	subroutine writeimage(Array,nx,ny,refimage,outfile)

! Write out a FITS file using the header from REFIMAGE.

	implicit real(4) (a-h,o-z)
	implicit integer(i-n)

	character*(*) refimage,outfile
	real(4) Array(nx,ny)
	real(4), allocatable :: Larray(:)

	Lsize = nx*ny
	allocate (Larray(Lsize))
	k = 0
 	do j = 1,ny
 	do i = 1,nx
	    k = k+1
	    larray(k) = Array(i,j)
	enddo
	enddo

	print *,'Writing out '//trim(outfile)
	call mwimage (nx,ny,lsize,larray,refimage,outfile)
	deallocate (Larray)
	return

	end
