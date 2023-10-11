PROGRAM ex1
IMPLICIT NONE
! Pseudocode:
! 1. Ask value of eccentricity.
! 2. Given an angle value, evaluate distance.
! 3. Print values of angle and distance.
! 4. If angle is less or equal to one turn, return to point 2.
REAL, PARAMETER :: p = 1.2e6 ! m
REAL, PARAMETER :: PI = 3.14159265
REAL :: e, r
INTEGER :: theta
WRITE (*,*) 'Insert eccentricity e: '
READ (*,*) e
! Output is formatted as shown in the example.
WRITE (*,*) 'epsilon: ', e
DO theta = 0, 259, 1
    r = p / (1.0 - e * COS(theta / 180.0 * PI))
    WRITE (*,*) 'theta: ', theta, ' r: ', r
END DO
END PROGRAM ex1
