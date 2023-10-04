PROGRAM ex2
IMPLICIT NONE
REAL, PARAMETER :: g = 9.80665
REAL :: v, h
WRITE (*,*) 'Insert height h / m: '
READ (*,*) h
v = SQRT(2.0 * g * h)
WRITE (*,*) 'v / (m / s) = ', v
END PROGRAM ex2
