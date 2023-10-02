PROGRAM Ex3
	IMPLICIT NONE
	REAL :: a, b, c, result_1, result_2, result_3
	a = 2.0e20
	b = 3.0e20
	c = 1.0e10
	result_1 = a * b
	result_2 = a * b / c
	result_3 = (a / c) * b
	WRITE (*,*) 'result_1 = ', result_1
	WRITE (*,*) 'result_2 = ', result_2
	WRITE (*,*) 'result_3 = ', result_3
END PROGRAM Ex3
