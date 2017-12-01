      subroutine weedout(x,y,sc,nsources,rcrit,notseen,satmask,nx,ny)

! Weed out likely duplicate sources, i.e. those within a certain critical 
! radius of a previously-seen source.  The source list comes in ascending
! order of source strength and leaves in descending order.

      implicit real(4) (a-h,o-z)
      implicit integer(i-n)
      real(4), allocatable :: xw(:),yw(:),sw(:)
      logical, allocatable :: mbox(:,:), mbox_sat(:,:)
      real(4) x(*),y(*),sc(*)
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

! Increase the avoidance radius for saturated sources.
      rmult = 5.
      imbx = nint(rmult*rcrit)
      nmbx = 2*imbx + 1
      allocate (mbox_sat(nmbx,nmbx))
      mbox_sat = .true.
      rcritsqx = (rmult*rcrit)**2
      icmbx = imbx + 1
      cmbx = float(icmbx)
      do j = 1,nmbx
      do i = 1,nmbx
          if((i-cmbx)**2+(j-cmbx)**2 <= rcritsqx) mbox_sat(i,j)=.false.       
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
            if (satmask(i,j)==0) then 
            ilo = max(i-imb, 1)
            ihi = min(i+imb, nx)
            jlo = max(j-imb, 1)
            jhi = min(j+imb, ny)
            do jj = jlo,jhi
            do ii = ilo,ihi
                if (.not.mbox(icmb-i+ii,icmb-j+jj)) 
     *                  notseen(ii,jj)=.false.
            enddo
            enddo
            else
            ilo = max(i-imbx, 1)
            ihi = min(i+imbx, nx)
            jlo = max(j-imbx, 1)
            jhi = min(j+imbx, ny)
            do jj = jlo,jhi
            do ii = ilo,ihi
                if (.not.mbox_sat(icmbx-i+ii,icmbx-j+jj)) 
     *                  notseen(ii,jj)=.false.
            enddo
            enddo
            endif
          endif
      enddo

      deallocate(mbox)
      deallocate(mbox_sat)
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
