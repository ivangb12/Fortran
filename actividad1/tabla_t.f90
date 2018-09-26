  ! nombre del programa
program temperatura
  
  implicit none

  !declaracion de variables
  Real::temp_f, temp_c, temp_k,ti, temp_cf, dt 
  Integer::n,i
  character(len=80):: format

 Open(unit=11, file="tabla.txt", status="unknown")
  
  write(*,*)"Por favor dame una temperatura inicial y final en Celsius, y el numero de brincos"
  
  read(*,*) ti
  read(*,*) temp_cf
  read(*,*) n
  
 
  dt=0
  temp_c=ti
  write(11,*)"I   ° C     ° F     ° K"
  
 format="(I3,3F8.2)"


do i=1,n+1
  temp_c=temp_c + dt
  temp_f=(temp_c*1.8)+32.0
  temp_k=temp_c + 273.15
  write(11,format)i, temp_c, temp_f, temp_k
  dt=(temp_cf-ti)/n
end do

close(unit=11)

end program temperatura
