program fibo
  implicit none
  integer:: first, second, temp, ix
  REAL:: start, finish, time
  first=0
  second=1
  write(*,*)first
  write(*,*) second

call cpu_time(start)
  
  do ix=1,45,1
     temp=first+second
     first=second
     second=temp
     write(*,*) temp
  end do
  
  call cpu_time(finish)
  time=finish-start
  write(*,*) time

end program fibo

