      subroutine fortfunc1(ii,ff)
      integer ii
      real*4  ff

!      write(6,100) ii, ff
! 100  format('ii=',i2,' ff=',f6.3)
 
      write(*,*) ii, ff

      return
      end

      subroutine fortfunc2()
      integer ii
      real*4  ff

	  ii=6
	  ff=6.6
!      write(6,100) ii, ff
! 100  format('ii=',i2,' ff=',f6.3)
 
      write(*,*) ii, ff

      return
      end
	  
      subroutine paranmrd()
      
      write(*,'(" In paranmrd ")') 
	  
      return
      end