
program periodos

  implicit none
  !T oscilaciones arbitrarias, To oscilaciones pequeñas
  real::To,x,L,Serie,err,rerr,rerr0,pi=3.1416,g=9.81,f,T
  integer::i
  character::output*12,output1*12
  print*,"Programa calculo error periodo de un pendulo simple de Longitud= 10"
  L=10
  print*,"Nombre de archivo error"
  read*,output

 print*,"Nombre archivo error relativo"
 read*,output1
  To=2.0*pi*sqrt(L/g)

  open(1,file=output)

  open(2,file=output1)
  !i sera el angulo
  rerr0=To
  do i=0,90
     x=i*pi/180.0
     Serie=f(x)
     T=2*pi*sqrt(L/g)*(1+Serie)
     err=T/To
     rerr=abs((T-rerr0)/T)*100.0
     if(rerr<1)then
        write(2,*)i,rerr
     else
        rerr0=T
     end if
     
     write(1,*)i,err
  end do

endprogram periodos

function f(x)
  real,intent(in)::x

  f=(1.0/16.0)*(x**2)+(11.0/3072.0)*(x**4)+(173.0/737280.0)*(x**6)+(22931.0/1321205760.0)*(x**8)

  end function f
