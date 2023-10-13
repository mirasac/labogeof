PROGRAM ex1
IMPLICIT NONE
REAL, PARAMETER :: p = 1.2e6 ! m
REAL, PARAMETER :: PI = 3.14159265
REAL :: e, r
INTEGER :: i_e, theta
! Output is formatted as shown in the example using FORMAT.
DO i_e = 0, 2, 1
    e = 0.25 * i_e
    WRITE (*,'(A8, 1X, F4.2)') 'epsilon:', e
    WRITE (*,'(A, 2X, A)') 'teta(°)', 'raggio(km)'
    DO theta = 0, 259, 1
        r = p / (1.0 - e * COS(theta / 180.0 * PI))
        WRITE (*,100) theta, r / 1000.0
    END DO
    WRITE (*,'(A)') '------------------------------'
END DO
STOP
100 FORMAT (I3, 7X, F7.2)
END PROGRAM ex1
