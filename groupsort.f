	subroutine groupsort(n,ra,x,y)

!    Sorts an array RA of length N into ascending numerical order using the 
!    Heapsort algorithm. N is input; RA is replaced by its sorted rearrangement.
!    X and Y are arrays associated with RA and which get sorted along with
!    RA.

	implicit real(4) (a-h,o-z)
	implicit integer(i-n)
	real(4) ra(*),x(*),y(*)

	if (n <= 1) return
	L = n/2+1
	ir = n

10	continue
	    if(L.gt.1) then
		L = L-1
		rra = ra(L)
		rx = x(L)
		ry = y(L)
	    else
		rra = ra(ir)
		ra(ir) = ra(1)
		rx = x(ir)
		x(ir) = x(1)
		ry = y(ir)
		y(ir) = y(1)
		ir = ir-1
		if(ir.eq.1) then
		    ra(1) = rra
		    x(1) = rx
		    y(1) = ry
		    return
		endif
	    endif
	    i = L
	    j = L+L
20	    if(j.le.ir) then
		if(j.lt.ir) then
		    if(ra(j).lt.ra(j+1)) j = j+1
		endif
		if(rra.lt.ra(j)) then
		    ra(i) = ra(j)
		    x(i) = x(j)
		    y(i) = y(j)
		    i = j
		    j = j+j
		else
		    j = ir+1
		endif
	    go to 20
	    endif
	    ra(i) = rra
	    x(i) = rx
	    y(i) = ry
	go to 10
	end
