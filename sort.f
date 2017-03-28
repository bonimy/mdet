      subroutine sort(n,ra)

c    Sorts an array RA of length N into ascending numerical order using the 
c    Heapsort algorithm. N is input; RA is replaced by its sorted rearrangement.

      implicit real(4) (a-h,o-z)
      implicit integer(i-n)
      real(4) ra(*)

      if (n <= 1) return
      L = n/2+1
      ir = n

10      continue
          if(L.gt.1) then
            L = L-1
            rra = ra(L)
          else
            rra = ra(ir)
            ra(ir) = ra(1)
            ir = ir-1
            if(ir.eq.1) then
                ra(1) = rra
                return
            endif
          endif
          i = L
          j = L+L
20          if(j.le.ir) then
            if(j.lt.ir) then
                if(ra(j).lt.ra(j+1)) j = j+1
            endif
            if(rra.lt.ra(j)) then
                ra(i) = ra(j)
                i = j
                j = j+j
            else
                j = ir+1
            endif
          go to 20
          endif
          ra(i) = rra
      go to 10
      end
