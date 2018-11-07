program main
               
  real ::  h, t, f, l
  real,dimension(2) :: w,x
  integer :: i
  
      print*,"Longitud pendulo"
      read*,l
      print*,"Angulo inicial << 1"
      read*,x(1)

      open(1,file='tabla.dat',status='unknown')
      t=0
      w(1)=0
      h=(2.0*3.1416*sqrt(l/9.81))/100
      do i = 1,100
         x(2)=x(1) + h*w(1)
         w(2)=w(1) + h*f(x(1),l)
         write(1,*) t,x(2)
         t = t + h
         x(1)=x(2)
         w(1)=w(2)
        
      end do
      
      
      
end program main

      function f(x,l)
      real, intent(in)::x, l
      f = -(9.81)*x/l
      end function f
