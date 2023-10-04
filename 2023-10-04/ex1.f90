PROGRAM ex1
IMPLICIT NONE
REAL :: T_F, T_K
WRITE (*,*) 'Insert temperature T / F: '
READ (*,*) T_F
T_K = 5.0 / 9.0 * (T_F - 32.0) + 273.15
WRITE (*,*) 'T / K = ', T_K
END PROGRAM ex1
