c
c---- NUMCHAR ----------------------------------------------------
c
        INTEGER FUNCTION NUMCHAR(CSTRING)

C       This function determines the length of the character string
C       in cstring.

C       Author: Richard J. Stover
C       Added strong typing: D. Van Buren, T. Jarrett

      implicit integer (i-n)
        implicit real(4) (a-h)
        implicit real(4) (o-z)

        CHARACTER*(*) CSTRING
        Integer*4     I

        IF(CSTRING .EQ. ' ') THEN
          NUMCHAR = 0
        ELSE
        DO 8701 I=LEN(CSTRING),1,-1

        IF (CSTRING(I:I) .NE. ' ' .AND. ICHAR(CSTRING(I:I)) .NE. 0)
     &        GOTO 50
        IF (ICHAR(CSTRING(I:I)) .EQ. 0) CSTRING(I:I) = ' '
 8701      CONTINUE
 50        NUMCHAR=I
         END IF

         RETURN
         END

      integer function fitchar(cstring,chr)
c       written by T. jarrett

      implicit integer (i-n)
        implicit real (a-h)
        implicit real (o-z)

      CHARACTER*(*) CSTRING,chr

      L = numchar (chr)
      ilen = numchar (cstring)

      IF(CSTRING .EQ. ' ') THEN
            fitchar = 0
      else
            do 100 i=1,ilen-(L-1)
                  if (CSTRING(I:I+(L-1)).eq.chr(1:L)) then
                        fitchar = i
                        goto 50
                  endif

 100            continue
            fitchar = 0
      endif       

 50      return
      end

      subroutine sclean (string)

      implicit integer (i-n)
        implicit real (a-h)
        implicit real (o-z)


      CHARACTER*(*) STRING
      character*132 tmp

      L = numchar (string)

      idex = 1
      do 100 i=1,L
            if (string(i:i).ne.' ') then
                  idex = i
                  goto 50
            endif
 100      continue

 50      tmp(1:L) = string(1:L)
      string = ' '
      string(1:L-(idex-1)) = tmp(idex:L)

      return
      end

      real*8 function rstringcon (char)

      implicit integer (i-n)
        implicit real (a-h)
        implicit real (o-z)

      character*(*) char

      read (char,*) rstringcon

      return
      end

      real*4 function rstrcon  (char)
      implicit integer (i-n)
        implicit real (a-h)
        implicit real (o-z)

      character*(*) char
            read (char,*) rstrcon
      return
      end

      function istringcon (char)
      implicit integer (i-n)
        implicit real (a-h)
        implicit real (o-z)

      character*(*) char

      read (char,*) istringcon

      return
      end

      subroutine access(char,zexist,erase)

      implicit integer (i-n)
        implicit real (a-h)
        implicit real (o-z)

      character*(*) char
      logical erase,zexist
      character*30 command

      zexist=.false.

      inquire (file=char,exist=zexist)
      if ((zexist).and.(erase)) then
            command = '/bin/rm'
            call unix (command,char)
      endif

      return
      end

      subroutine unix(command,char)

      implicit integer (i-n)
        implicit real (a-h)
        implicit real (o-z)

      character*(*) char,command
      character*132 result

      ic = numchar (command)
      ichr = len (char)

      do k=1,ic
            result (k:k) = command(k:k)
      enddo

      result (ic+1:ic+1)=' '
      do k=1,ichr
            result(k+ic+1:k+ic+1) = char(k:k)
      enddo

      ii = len(result)

c      write (6,'(a)') result(1:ii)
      i = system (result(1:ii))
      if (i .ne. 0) print *,'stringstuff WARNING: system code =',i,       ! JWF B60619
     +                    ' on command ',result(1:ii)                   ! JWF B60619
      
      return
      end


c read string fields
      subroutine sfields (string,nf,sout)

      implicit integer (i-n)
        implicit real (a-h)
        implicit real (o-z)

      character*(*) string,sout
      character*1 ch
      integer fields(500,2)

c assumptions: 1st field = first non-blank string

      sout = ' '

      L = NUMCHAR (string)
      n = 0

      imax = 0
      do 100 j=1,L

            if (j.le.imax) goto 100
            ch = string(j:j)
            if (ch.ne.' ') then
                  imin = j
                  do i=j+1,L
                        ch = string(i:i)
                        if (ch.eq.' ') then
                              imax = i-1
                              n = n + 1
                              fields (n,1) = imin
                              fields (n,2) = imax
                              if (n.eq.nf) goto 99
                              GOTO 100
                        endif
                  enddo
                  n = n + 1
                  fields (n,1) = imin
                  fields (n,2) = L
                  imax = L
                  if (n.eq.nf) goto 99
            endif
 100      continue

      
 99      if (n.lt.nf) then
            sout = ' '
      else
            i1 = fields (nf,1)
            i2 = fields (nf,2)
            sout = string(i1:i2)
      endif


      return
      end



       subroutine header_parse (head,keyword,idex)

      implicit integer (i-n)
        implicit real (a-h)
        implicit real (o-z)

        character*(*) head,keyword
        character*25 s0,result,upcase

      result = upcase (keyword)
      keyword = result

c        call upcase(keyword)

        if (keyword(1:4).eq.'NULL') then
                idex=0
                return
        endif

c       L = numchar (head)                                              ! JWF B60519
        LK = numchar(keyword)


        imax = 5000
        idex = 0

        do 50 I=1,imax
                call sfields (head,I,s0)
c               call upcase(s0)
            result = upcase(s0)
            s0 = result
                LL = numchar(s0)
                if (LL.eq.0) goto 47
                if (LL.ne.LK) goto 50

                if (s0(1:LL).eq.keyword(1:LK)) then
                        idex=I
                        goto 47
                endif
 50     continue

 47     return
        end

c
c Stolen from JWF, inserted by TPC
c

c      subroutine upcase(field)
c      character*25 field
c      character*1  tmpchar
c      integer*4    i,k,lnblnk
c      byte         tmpbyte
c      equivalence (tmpbyte,tmpchar)
c      k = lnblnk(field)
c      do 10 i = 1, k
c        tmpchar = field(i:i)
c        if ((tmpbyte .gt. 96) .and. (tmpbyte .lt. 123)) then
c          tmpbyte = tmpbyte - 32
c          field(i:i) = tmpchar
c        end if
c10     continue
c       return
c       end


      function upcase(string) result(upper)
      character(len=*), intent(in) :: string
      character(len=len(string)) :: upper
c      integer :: j, L                                                     ! JWF B60619
      integer :: j                                                        ! JWF B60619

      do j = 1,len(string)
        if(string(j:j) >= "a" .and. string(j:j) <= "z") then
             upper(j:j) = achar(iachar(string(j:j)) - 32)
        else
             upper(j:j) = string(j:j)
        end if
      end do
      end function upcase

