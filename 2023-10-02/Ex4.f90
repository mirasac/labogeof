PROGRAM arrotondamenti_4
! Programma che ............
! Variables Declaration
IMPLICIT NONE
INTEGER :: k
REAL :: x, y, r
!Initilizing
x=0.
y=0.
r=1./3
! Computing
DO k=1,10000
	x=x+r
	y=k*r
	WRITE (*,*) k,x,y
END DO
!end
END PROGRAM arrotondamenti_4
