program euler
               
  real ::  h, t,tf, f,k,m,d,w,n_n,p
  real,dimension(2) :: x,v
  integer :: i,n
  

      print*,"X inicial"
      read*,x(1)

      open(1,file='tabla.dat',status='unknown')
      tf=20.0
      h=.01
      n_n=tf/h
      n=nint(n_n)
      
      k=.7
      m=.35
      w=sqrt(k/m)
      p=x(1)
      d=0.0
      t=0.0
      v(1)=0.0
 
      do i = 1,n
         x(2)=x(1) + h*v(1)
         v(2)=v(1) + h*f(x(1),w,d,v(1))
         write(1,*) t,x(2)
         t = t + h
         v(1)=v(2)
         x(1)=x(2)
        
      end do

      d=0.1
      t=0.0
      v(1)=0.0
      x(1)=p
       do i = 1,n
         x(2)=x(1) + h*v(1)
         v(2)=v(1) + h*f(x(1),w,d,v(1))
         write(1,*) t,x(2)
         t = t + h
         v(1)=v(2)
         x(1)=x(2) 
      end do

       d=0.2
      t=0.0
      v(1)=0.0
      x(1)=p
       do i = 1,n
         x(2)=x(1) + h*v(1)
         v(2)=v(1) + h*f(x(1),w,d,v(1))
         write(1,*) t,x(2)
         t = t + h
         v(1)=v(2)
         x(1)=x(2) 
      end do

       d=0.4
      t=0.0
      v(1)=0.0
      x(1)=p
       do i = 1,n
         x(2)=x(1) + h*v(1)
         v(2)=v(1) + h*f(x(1),w,d,v(1))
         write(1,*) t,x(2)
         t = t + h
         v(1)=v(2)
         x(1)=x(2) 
      end do

       d=0.7
      t=0.0
      v(1)=0.0
      x(1)=p
       do i = 1,n
         x(2)=x(1) + h*v(1)
         v(2)=v(1) + h*f(x(1),w,d,v(1))
         write(1,*) t,x(2)
         t = t + h
         v(1)=v(2)
         x(1)=x(2) 
      end do

       d=1.0
      t=0.0
      v(1)=0.0
      x(1)=p
       do i = 1,n
         x(2)=x(1) + h*v(1)
         v(2)=v(1) + h*f(x(1),w,d,v(1))
         write(1,*) t,x(2)
         t = t + h
         v(1)=v(2)
         x(1)=x(2) 
      end do

       d=2.0
      t=0.0
      v(1)=0.0
      x(1)=p
       do i = 1,n
         x(2)=x(1) + h*v(1)
         v(2)=v(1) + h*f(x(1),w,d,v(1))
         write(1,*) t,x(2)
         t = t + h
         v(1)=v(2)
         x(1)=x(2) 
      end do
      
end program euler

      function f(x,w,d,v)
      real, intent(in)::x,w,d,v
      f = (-2.0)*d*w*v - (w**2)*x
      end function f
