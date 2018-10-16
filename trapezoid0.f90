program trapezoid0

  implicit none
  integer,parameter::n=20
  real :: u,a,b
  integer :: i

  print*,"Intervalo [a,b]"  
  read*,a,b
  
  
call trapezoid_integration(n,a,b)

STOP

  contains

subroutine trapezoid_integration(n,a,b)
      implicit none
      integer :: n
      real ::  a,b
      real :: integral,u,h,error
      integer :: i

      integral = 0.0

      do
        error=((b-a)/(-1.0*(float(n)**2)))*(derivada(b)-derivada(a))

        do i=0,n
            u = a + ((b-a)*float(i))/float(n)

         
            if ((i.eq.0).or.(i.eq.n)) then
               integral = integral+integrand(u)
            else
               integral = integral+(2.0*integrand(u))
            end if
        
         
         end do

      
         print*, error
         print*, n

         if(error<.00001)exit

       n=n*2
       
    end do
          
      h=(b-a)/float(n)
      integral = (h/2.0)*integral

      write (*,*) '#trapezoidal integration = ',integral
    end subroutine trapezoid_integration
    
function integrand(x) result (value)
      implicit none
      real :: x
      real :: value

     if (x .lt. 0.00001) then
         x = 0.00001
      end if

      value = (x**4)*EXP(X)/((EXP(X)-1.0)**2)
    end function integrand

function derivada(x) result (value)
      implicit none
      real :: x
      real :: value

     if (x .lt. 0.00001) then
         x = 0.00001
      end if

      value =((((EXP(x)-1.0)**2)*((x**4)*EXP(x)+4*(x**3)*EXP(x)))-((x**4)*EXP(x))*(2*EXP(x)*(EXP(x)-1.0))) /((EXP(x)-1.0)**4)
    end function derivada

    
  end program trapezoid0
  
