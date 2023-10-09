PROGRAM ex2
IMPLICIT NONE
INTEGER :: n, c
c = 0
DO n = 1, 100, 1
    IF (MOD(n, 3) == 0) THEN
        WRITE (*,*) n
        c = c + 1
    END IF
END DO
WRITE (*,*) 'The number of divisible numbers is c = ', c
END PROGRAM ex2
