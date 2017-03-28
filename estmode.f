      function estmode(a,n,amed,sigma)

! Estimate mode of the distribution of N values of an array A.

      implicit real(4) (a-h,o-z)
      implicit integer(i-n)
      integer, allocatable :: h(:)
      real(4) a(*)

      frsig = 0.04            ! precision, expressed as a fraction of sigma

      maxsigs = 100            ! maximum number of sigmas to search on
                        ! either side of median

      if (n <= 0) return
      binsize = sigma*frsig
      maxdevs = nint(maxsigs/frsig)
      nbins = 2*maxdevs + 1
      allocate (h(nbins))
      h = 0

      do i = 1,n
          ibin = maxdevs + nint((a(i) - amed)/binsize)
          if (ibin > 0 .and. ibin <= nbins) h(ibin) = h(ibin) + 1
      enddo

      npeak = -2**30
      ibinmode = 0                                                        ! JWF B60519
      do ibin = 1,nbins
          if (h(ibin) > npeak) then
            npeak = h(ibin)
            ibinmode = ibin
          endif
      enddo

      estmode = amed + (ibinmode - maxdevs)*binsize
      if (npeak < 6) estmode = amed
      deallocate(h)
      return

      end
