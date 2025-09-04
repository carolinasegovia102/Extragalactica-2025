program ejercicio1
	implicit none
	real :: res, f
	integer :: z
	external f
	
	open(11, file="salida.dat" ,status="new")
	do z = 1, 600
		call qromb(f, 0., z*0.001, res)
			write(11,*)z**0.001, res, res/(1+z*0.001), (1.+z*0.001)*res
	end do
	close(11)
end program ejercicio1

function f(s)
	implicit none
	real :: c, h0, r0, r, m, k, l
	real :: s
	real :: f
	c = 299792.458
	h0 = 70.
	r0 = 1.
	r = 0.
	m = 0.3
	k = 0.
	l = 0.7
	f= c/(h0*r0*sqrt((r*((1.+s)**4.))+(m*((1.+s)**3.))+(k*((1.+s)**2.))+l))
	return
end function f

include "qromb.f"
include "polint.f"
include "locate.f"
include "trapzd.f"

##Notas: res = coordenada comovil
