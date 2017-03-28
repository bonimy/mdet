      subroutine appendlist(x,y,sc,ncurrent,xband,yband,scband,
     *          nband,mx)

! Append the multiband detection list with the results of single-band 
! detections.

      implicit real(4) (a-h,o-z)
      implicit integer(i-n)
      real(4) x(mx),y(mx),sc(mx),xband(mx),yband(mx),scband(mx)

      if (ncurrent+nband > mx) then
          write(6,'(a)') 'APPENDLIST:  Maximum source count exceeded'
          nband = mx - ncurrent
      endif

      do i = 1,nband
          x(ncurrent+i) = xband(i)
          y(ncurrent+i) = yband(i)
          sc(ncurrent+i) = scband(i)
      enddo

      ncurrent = ncurrent + nband
      call groupsort(ncurrent,sc,x,y)
      xband = x
      yband = y
      scband = sc

      do i = 1,ncurrent
          x(ncurrent-i+1) = xband(i)
          y(ncurrent-i+1) = yband(i)
          sc(ncurrent-i+1) = scband(i)
      enddo

      return

      end
