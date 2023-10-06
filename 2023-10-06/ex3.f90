PROGRAM ex3
IMPLICIT NONE
REAL :: x, y, f
WRITE (*,*) 'Insert value x: '
READ (*,*) x
WRITE (*,*) 'Insert value y: '
READ (*,*) y
IF (x >= 0.0) THEN
    IF (y >= 0.0) THEN
        f = x + y
    ELSE
        f = x + y**2
    END IF
ELSE
    IF (y >= 0.0) THEN
        f = x**2 + y
    ELSE
        f = x**2 + y**2
    END IF
END IF
WRITE (*,*) 'Result f(', x, ', ', y, ') = ', f
END PROGRAM ex3
