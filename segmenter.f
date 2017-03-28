      subroutine segmenter(Msk,nx,ny,iblo,ibhi,jblo,jbhi,isearchrad,
     *          ixsat,iysat,satradius,nsatpix)

! Segment a saturated-pixel mask into objects, each characterized by
! a location and number of saturated pixels.  The criterion for membership
! is to be within a radius ISEARCHRAD of an object.

      implicit real(4) (a-h,o-z)
      implicit integer (i-n)

      integer, allocatable :: objectmask(:,:)
      integer, parameter :: isatvalue = 100      ! saturation value
      integer, parameter :: minsat = 9      ! minimum no. of "contiguous"
                                    ! saturated pixels to indicate
                                    ! a saturated star
      integer Msk(nx,ny)
      logical it_has_neighbors

      allocate (objectmask(nx,ny))
      objectmask = 0
      irsq = isearchrad**2
      nsatpix = 0
      ixsat = 0
      iysat = 0
      satradius = 0
      nobjects = 0

c      nsat = sum(Msk(iblo:ibhi,jblo:jbhi), MASK=Msk(iblo:ibhi,jblo:jbhi)==100)/100
c      if(nsat==0) return
c      ! Fake radius estimate. If all the saturated pixels in the region are
c        ! grouped together and assumed to form a circle and still don't make
c      ! a big enough circle to be worth the trouble of finding, then we'll quit
c      rsatmin = isearchrad*1.5 ! approx. 0.75*fwhm(pixels)
c      if(nsat .lt. 3.14*rsatmin**2) return

      nhits = 1

      do while(nhits /= 0)
        nobjects = nobjects + 1

! Search array for untagged saturated pixels.
        nhits = 0
        j = jblo+isearchrad-1
        do while (j < jbhi-isearchrad .and. nhits==0)
          j = j+1
          i = iblo+isearchrad-1
          do while (i < ibhi-isearchrad .and. nhits==0)
            i = i+1
            if (Msk(i,j)==isatvalue .and. objectmask(i,j)==0) then
            nhits = nhits+1
            iloc = i
            jloc = j
            goto 90 ! Done all we need to do
              endif
          enddo
        enddo
 90        continue

! Found one, so define new object.
        if (nhits > 0) then
          objectmask(iloc,jloc) = nobjects

! Search for neighbors.
          it_has_neighbors = .true.
          do while (it_has_neighbors)
            it_has_neighbors = .false.
            do j = jblo+isearchrad,jbhi-isearchrad
            do i = iblo+isearchrad,ibhi-isearchrad

              if (Msk(i,j)==isatvalue .and. objectmask(i,j)==0) then
              do jj = j-isearchrad,j+isearchrad
              do ii = i-isearchrad,i+isearchrad
                if (objectmask(ii,jj)==nobjects) then
                  if ((i-ii)**2 + (j-jj)**2 <= irsq) then
                  objectmask(i,j) = nobjects
                  it_has_neighbors = .true.
                  goto 100 ! done all we need to do
                  endif
                endif
              enddo
              enddo
                endif
 100            continue ! next i,j
            enddo
            enddo
          enddo

          npixels = 0
          do j=jblo+isearchrad,jbhi-isearchrad
          do i=iblo+isearchrad,ibhi-isearchrad
            if (objectmask(i,j)==nobjects) npixels = npixels + 1
          enddo
          enddo
          if (npixels < minsat) npixels = 0
          if (npixels > nsatpix) then
            nsatpix = npixels
            sumx = 0.
            sumy = 0.
            do j=jblo+isearchrad,jbhi-isearchrad
            do i=iblo+isearchrad,ibhi-isearchrad
              if (objectmask(i,j)==nobjects) then
                sumx = sumx + i
                sumy = sumy + j
              endif
            enddo
            enddo
            xsat = sumx/npixels
            ysat = sumy/npixels
            sumr = 0.
            do j=jblo+isearchrad,jbhi-isearchrad
            do i=iblo+isearchrad,ibhi-isearchrad
              if (objectmask(i,j)==nobjects) 
     *                  sumr = sumr + (i-xsat)**2 + (j-ysat)**2
            enddo
            enddo
            satradius = sqrt(sumr/npixels)
            ixsat = nint(xsat)
            iysat = nint(ysat)
          endif

        endif ! nhits>0

      enddo ! nhits /= 0

      deallocate (objectmask)
      return

      end
