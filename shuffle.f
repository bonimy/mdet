	subroutine shuffle(Larray,nx,ny,Array)

! Rearrange 1-d vector LARRAY as a 2-d ARRAY.

	real(4) Array(nx,ny),Larray(*)
	integer nx,ny,i,j,k

	k = 0
	do j = 1,ny
	do i = 1,nx
	    k = k+1
	    Array(i,j) = Larray(k)
	enddo
	enddo
	return

	end
