  ! nombre del programa
program temperatura
  
  implicit none

  !declaracion de variables
  Real::temp_f, temp_c

  write(*,*)"Por favor dame una temperatura"
  read(*,*) temp_f
  
  !Conversion a �C
  temp_c=(temp_f-32.0)/1.8

  write(*,*)"Su temperatura en �C es de ",temp_c
end program temperatura

