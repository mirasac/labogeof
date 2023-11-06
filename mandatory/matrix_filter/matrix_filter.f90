PROGRAM matrix_filter
USE utilities
IMPLICIT NONE
! Declare variables.
CHARACTER(LEN=128) :: filename_input, filename_mean, filename_kernel
INTEGER :: iostat_input, iostat_mean, iostat_kernel
! Program body.
CALL sanitize_filename(filename_input, 'input')
OPEN(UNIT=30, FILE=filename_input, IOSTAT=iostat_input, ACTION='READ', STATUS='OLD')
IF (iostat_input /= 0) THEN
    WRITE(*, 100) filename_input
ELSE
    CALL sanitize_filename(filename_mean, 'latitudinal mean temperature')
    CALL sanitize_filename(filename_kernel, 'filtered matrix')
    ! MC continue.
END IF
CLOSE(30)
STOP
100 FORMAT('Error opening file ', A)
END PROGRAM matrix_filter
