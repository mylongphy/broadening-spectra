      implicit real*8(a-h,o-z)
      parameter (N=500000,n2=20000)
      CHARACTER (80)INFILE, OUTFILE
	dimension dlamda(N),dlamda1(N),dints(N),dints1(N),
     1          dlam(N),din(N)
c      open(1,file='wave-osci.dat',status='old')
      write(*,*)"the name of the inputfile"
      read(*,*)INFILE
      write(*,*)"the name of the outputfile"
      read(*,*)OUTFILE
      open(1,file=INFILE,status='old')
c      open(1,file='cross-out.dat',status='old')
      write(*,*)"the maxmum of energy"
      read(*,*)maxen
      write(*,*)"the minimum of energy"
      read(*,*)minen
      write(*,*)"the FWHM"
      read(*,*)hf
      open(2,file=OUTFILE,status='unknown')
    !  open(3,file='test.txt',status='unknown')
      do i=1,N
        dlamda(i)=0.d0
        dlamda1(i)=0.d0 
        dints1(i)=0.d0
        dints(i)=0.d0
      end do
      m=1
      emin=minen
      emax=maxen
      delt=hf
      do 100 k=1,N
	    read(1,*,end=101)dlam(k),din(k)
!		write(3,90)dlam(k),din(k)
		if (abs(dlam(k)).gt.emin.and.abs(dlam(k)).lt.emax) then
	        m=m+1
			dlamda(m)=abs(dlam(k)-0.0)
			dints(m)=abs(din(k))
!	    write(*,*)'*****************'
		end if
100   continue
90	format(1Pe13.6,2x,1pe13.6)
      rewind 1
101   do i1=1,n2                             !
   !         dlamda1(i1)=emin+(emax-emin)/dble(N)*dfloat(i1)
        dlamda1(i1)=emin+(emax-emin)/n2*dfloat(i1)

        do i=1,m
          dints1(i1)=dints1(i1)+dints(i)/dsqrt(2.d0*3.14158d0)/delt
     X    *2.355* dexp(-2.355d0**2*(dlamda1(i1)-dlamda(i))**2
     X     /delt**2/2.d0)
        end do
      if(dints1(i1).lt. 0.00001 ) then 
       dints1(i1)=0
	endif

	   write(2,90) dlamda1(i1),dints1(i1)

      end do
110	format()
      close(1)
	close(2)
	stop
      end
