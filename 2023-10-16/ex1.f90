PROGRAM ex1
IMPLICIT NONE
! Variables declaration.
REAL, PARAMETER :: PI = 3.14159265
REAL :: r
CHARACTER(LEN=16) :: filename
INTEGER :: file_stat
! Input data.
DO
    WRITE (*,*) 'Insert radius r / m: '
    READ (*,*) r
    IF (r >= 0.0) EXIT
    WRITE (*,*) 'Radius must be non negative'
END DO
WRITE (*,*) 'Insert output file name: '
READ (*,*) filename
! Create output file.
OPEN (UNIT=20, FILE=filename, STATUS='NEW', ACCESS='SEQUENTIAL', FORM='FORMATTED', ACTION='WRITE', IOSTAT=file_stat)
IF (file_stat /= 0) THEN
    WRITE (*,*) 'Error in opening the specified file ', filename
END IF
! Write outputs.
WRITE (20,*) PI * r**2
WRITE (20,*) 4.0 * r**2
! Close file.
CLOSE (UNIT=20)
END PROGRAM ex1
