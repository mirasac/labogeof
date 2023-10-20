PROGRAM ex1
IMPLICIT NONE
INTEGER :: nvals, istatus, status_tmp
CHARACTER (LEN=32) :: filename
CHARACTER (LEN=128) :: value_tmp
LOGICAL file_exists
nvals = 0
WRITE(*,*) 'Insert input file name: '
READ(*,'(A)') filename
INQUIRE(FILE=filename, EXIST=file_exists)
IF (file_exists) THEN
    OPEN(UNIT=30, FILE=filename, STATUS='OLD', ACTION='READ', IOSTAT=istatus)
    IF (istatus == 0) THEN
        DO
            READ(30, *, IOSTAT=status_tmp) value_tmp
            IF (status_tmp /= 0) EXIT
            nvals = nvals + 1
            WRITE(*,*) nvals, value_tmp
        END DO
        IF (status_tmp > 0) THEN
            WRITE(*,*) 'Error reading line'
        ELSE
            WRITE(*,*) 'End of file', nvals
        END IF
    ELSE
        WRITE (*,*) 'Error opening file: IOSTAT =', istatus
    END IF
    CLOSE(30)
ELSE
    WRITE(*,*) 'Specified file does not exists'
END IF
END PROGRAM ex1
