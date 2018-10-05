function F(n) !Funcion
  implicit none
  real,intent(in)::n
  real::F
  F=n**3 - n -2
end function F

function Fp(n) !Derivada
  implicit none
  real,intent(in)::n
  real::Fp
  Fp=3*n**2 - 1
end function Fp


program nr
  implicit none
  !x buscada, xi= x inicial, err error, F funcion valuada en xi, Fp derivada funcion valuada en xi
  real::x,xi,err
  real::F,Fp
 print*,"Indique su x inicial para aplicar newton-rhapsso a la funcion  x^3-x-2"

 read*,xi
Do
   !xi - F(xi)/F´(xi)
 x=xi-(F(xi)/Fp(xi))
 err=abs((x-xi)/x)*100
print*,"X= ",x,"err= ",err
 xi=x
 
 if(err<.00001)exit
end do
end program nr



