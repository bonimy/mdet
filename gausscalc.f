	subroutine gausscalc(gauss,ncells,fwhm)

! Calculate a Gaussian image on a square array of NCELLS x NCELLS.

	implicit real(4) (a-h,o-z)
	implicit integer(i-n)
	real(4) gauss(ncells,ncells)
	real(4), allocatable :: xsq(:), arg(:)

	coef = -4.*alog(2.)/fwhm**2
	allocate (xsq(ncells))
	allocate (arg(ncells))
	icent = ncells/2 + 1

	do i = 1,ncells
	    xsq(i) = (i-icent)**2
	enddo

	do j = 1,ncells
	    arg = coef*(xsq + (j-icent)**2)
	    where(arg > -50.) gauss(1:ncells,j) = exp(arg)
	enddo

	deallocate(xsq)
	deallocate(arg)
	return

	end
