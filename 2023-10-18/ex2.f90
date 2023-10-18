PROGRAM ex2
IMPLICIT NONE
REAL, PARAMETER :: p = 1.2e6 ! m
REAL, PARAMETER :: PI = 3.14159265
REAL :: e, r
INTEGER :: i_e, theta
LOGICAL :: file_exists
INTEGER :: response
CHARACTER (LEN=32) :: filename
CHARACTER (LEN=7) :: status_tmp
! Check file existence.
response = -1
WRITE (*,*) 'Insert output file name: '
READ (*,'(A)') filename
DO
    INQUIRE(FILE=filename, EXIST=file_exists)
    IF (file_exists) THEN
        WRITE(*,*) 'Specified file exists already, how to proceed (1: overwrite, 2: interrupt, 3: choose new name, default: 2)? '
        READ(*,*) response
        SELECT CASE (response)
        CASE (1)
            status_tmp = 'REPLACE'
        CASE (3)
            WRITE(*,*) 'Insert new output file name:'
            READ(*,'(A)') filename
        CASE DEFAULT
            response = 2
        END SELECT
    ELSE
        response = 0
        status_tmp = 'NEW'
    END IF
    IF (response == 0 .OR. response == 1 .OR. response == 2) EXIT
END DO
! Write output.
IF (response == 0 .OR. response == 1 .OR. response == 3) THEN
    OPEN(UNIT=30, FILE=filename, STATUS=status_tmp, ACTION='WRITE')
    ! Output is formatted as shown in the example using FORMAT.
    DO i_e = 0, 2, 1
        e = 0.25 * i_e
        WRITE (30,'(A8, 1X, F4.2)') 'epsilon:', e
        WRITE (30,'(A, 2X, A)') 'teta(°)', 'raggio(km)'
        DO theta = 1, 360, 1
            r = p / (1.0 - e * COS(theta / 180.0 * PI))
            WRITE (30,100) theta, r / 1000.0
        END DO
        WRITE (30,'(A)') '------------------------------'
    END DO
    CLOSE(30)
END IF
STOP
100 FORMAT (I3, 7X, F7.2)
END PROGRAM ex2
