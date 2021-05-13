program	 Neumann_recurs
implicit none
real(8) :: x, Ni, N2
integer :: k, i

	
print*, 'enter x>0'
read*, x

print*, 'enter integer, k>=0'
read*, k
i=k

call Neumann(x, k, k, Ni, N2)

contains
	recursive subroutine Neumann(x, k, i, Ni, N2)
		real(8):: x, Ni  N1, N2, pi
		integer :: k, i
		pi = atan(1.0)*4.0
		
		if (i .EQ. 1) then
			N1=sqrt(2.0/(pi*x))*sin(x)
			write(*,'(a9,f10.3)') 'N_-1/2 = ', N1
			
			N2=-sqrt(2.0/(pi*x))*cos(x)
			write(*,'(a8,f10.3)') 'N_1/2 = ', N2
			
			Ni = (2.0/x)*(k-0.5)*N2-N1
			write(*,'(a10, f10.3)') 'N_1+1/2 = ', Ni
		else
			call Neumann(x, k, i-1, Ni, N2)
			N1=N2
			N2=Ni
			Ni = (2.0/x)*(k-0.5)*N2 - N1
			write(*,'(a2,i2,a7,f10.3)') 'N_', i, '+1/2 = ', Ni
		end if
		
	end subroutine
end program
