	subroutine medsort(a,n,amed,q1,q2)

! Calculate median after sorting with HEAPSORT algorithm.
! Also calculate the 15.87% and 84.13% quantiles (q1 and q2).

	implicit real(4) (a-h,o-z)
	implicit integer(i-n)
	real(4), allocatable :: as(:)
	real(4) a(*)

	if (n <= 1) return
	allocate (as(n))
	as = a(1:n)
	call sort(n,as)

	if (mod(n,2) == 1) then
	    amed = as(n/2+1)
	else
	    amed = (as(n/2) + as(n/2+1))/2.
	endif

	q1 = as(max(nint(0.1587*n),1))
	q2 = as(nint(0.8413*n))
	deallocate(as)
	return

	end

