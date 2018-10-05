
!Funcion a resolver
FUNCTION F(i)
  real,intent(in)::i
  real::F
   f=(i**3)-i-2.0
 end function F
 



program  biseccion
   implicit none
   !a y b extremos del intervalo  n es el punto medio, fn,fa,fb funcion valuada en n,a,b
   real::F,fn,fa,fb,n,a,b
   print*,"Introduzca los extremos [a,b] para encontrar raiz de x^3 -x -2"
   read*,a
   read*,b
   fa=f(a)
   fb=f(b)
   if(fa*fb<0)then
      do
         n=(a+b)/2.0
         fn=f(n)
          !reasignacion de fronteras
         if(fn*fa<0) then
            b=n
            print*,n
            if(abs(b-a)<.0001)exit !criterio de paro
         else
            a=n
            fa=fn
            print*,n
            if(abs(b-a)<.0001)exit
         end if
      end do
             
      print *, "n=",n,"f(n)=",f(n)
   else
      print*,"Los extremos tienen el mismo signo favor de correr el programa con distintos valors"
   end if   
end program

      
