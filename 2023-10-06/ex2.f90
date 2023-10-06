PROGRAM ex2
IMPLICIT NONE
REAL, PARAMETER :: PI = 3.14159265
REAL :: theta_1, n_1, n_2, theta_2, tmp_sin
WRITE (*,*) 'Write angle of incidence theta_1 / deg: '
READ (*,*) theta_1
WRITE (*,*) 'Write index of refraction of region 1 n_1: '
READ (*,*) n_1
WRITE (*,*) 'Write index of refraction of region 2 n_2: '
READ (*,*) n_2
theta_1 = theta_1 / 180.0 * PI
tmp_sin = n_1 / n_2 * SIN(theta_1)
IF (tmp_sin <= 1.0) THEN
    theta_2 = ASIN(tmp_sin)
    theta_2 = theta_2 / PI * 180.0
    WRITE (*,*) 'Transmission angle theta_2 / deg = ', theta_2
ELSE
    WRITE (*,*) 'Light is completely reflected with reflection angle theta_1'
END IF
END PROGRAM ex2
