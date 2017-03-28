	subroutine pix2eq(i,j,ra0,dec0,iref,jref,pixel,theta,ra,dec)

! Transformation from pixel coordinates to equatorial (RA, Dec) based on sine 
! projection.
!
! Input parameters:
!	i,j		= pixel location
!	ra0,dec0	= reference position [deg]
!	iref,jref	= reference pixel
!	pixel		= pixel size [deg]
!	theta		= rotation angle (CROTA2)
!
! Output parameters:
!	ra,dec		= equatorial position [deg]

	implicit real(8) (a-z)
	double precision alpha0,delta0,ra,dec
	real(8), parameter :: dtor = 0.01745329251994

	scale = 1.d0/pixel
	alpha0 = dble(ra0)*dtor
	delta0 = dble(dec0)*dtor

	sample = (i-iref)*cos(theta*dtor) + (j-jref)*sin(theta*dtor)       
	line = (i-iref)*sin(theta*dtor) - (j-jref)*cos(theta*dtor)
	x = sample*dtor/scale
	y = line*dtor/scale
	D = asin(sqrt(x**2 + y**2))
	B = atan2(-x,y)
	xx = sin(delta0)*sin(D)*cos(B) + cos(delta0)*cos(D)
	yy = sin(D)*sin(B)
	alpha = alpha0 + atan2(yy,xx)
	delta = asin(sin(delta0)*cos(D) - cos(delta0)*sin(D)*cos(B))
	ra = alpha/dtor
	dec = delta/dtor
	return

	end

