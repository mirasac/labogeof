PROGRAM arrotondamenti_4
! Programma che ............
! Variables Declaration
IMPLICIT NONE
INTEGER, PARAMETER :: DBL = SELECTED_REAL_KIND(p=13)
INTEGER :: k
REAL(KIND=DBL) :: x, y, r
!Initilizing
x=0.0_DBL
y=0.0_DBL
r=1.0_DBL / 3.0_DBL
! Computing
DO k=1,10000
    x=x+r
    y=k*r
    WRITE (*,*) k,x,y
END DO
!End
END PROGRAM arrotondamenti_4
