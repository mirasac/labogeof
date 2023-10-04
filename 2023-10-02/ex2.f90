PROGRAM Ex2
	IMPLICIT NONE
	REAL :: result_1, result_2
	result_1 = 1000001.0 + 0.1 - 1000000.0
	result_2 = 1000001.0 + 0.01 - 1000000.0
	WRITE (*,*) 'result_1 = ', result_1
	WRITE (*,*) 'result_2 = ', result_2
END PROGRAM Ex2
