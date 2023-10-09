PROGRAM ex1
IMPLICIT NONE
INTEGER :: n, n_tmp, c
! Check input.
DO
    WRITE (*,*) 'Insert a number of 4 digits: '
    READ (*,*) n
    n_tmp = n
    c = 0
    DO
        n_tmp = n_tmp / 10
        c = c + 1
        IF (n_tmp == 0) EXIT
    END DO
    IF (c == 4) EXIT
    WRITE (*,*) 'The inserted number has not 4 digits, retry'
END DO
! Write digits in letters.
DO c = 3, 0, -1
    n_tmp = n / 10**c
    n = MOD(n, 10**c)
    SELECT CASE (n_tmp)
    CASE (1)
        WRITE (*,*) 'ONE'
    CASE (2)
        WRITE (*,*) 'TWO'
    CASE (3)
        WRITE (*,*) 'THREE'
    CASE (4)
        WRITE (*,*) 'FOUR'
    CASE (5)
        WRITE (*,*) 'FIVE'
    CASE (6)
        WRITE (*,*) 'SIX'
    CASE (7)
        WRITE (*,*) 'SEVEN'
    CASE (8)
        WRITE (*,*) 'EIGHT'
    CASE (9)
        WRITE (*,*) 'NINE'
    CASE (0)
        WRITE (*,*) 'ZERO'
    END SELECT
END DO
END PROGRAM ex1
