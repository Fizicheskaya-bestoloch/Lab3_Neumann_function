program	 Neumann_iterac
implicit none
real(8) :: x, Ni, N2, pi, N1
integer :: k, i

	
print*, 'enter x>0'
read*, x

print*, 'enter integer, k>=0'
read*, k
pi = atan(1.0)*4.0!подсчет числа пи 
N1=sqrt(2.0/(pi*x))*sin(x) !считаем начальные элементы
write(*,'(a9,f10.3)') 'N_-1/2 = ', N1
N2=-sqrt(2.0/(pi*x))*cos(x)
write(*,'(a8,f10.3)') 'N_1/2 = ', N2

do i=1,k
	Ni = (2.0/x)*(i-0.5)*N2 - N1
	N1=N2
	N2=Ni
	write(*,'(a2,i2,a7,f10.3)') 'N_', i, '+1/2 = ', Ni
end do

end program
