PROGRAM sonico
USE utilities, ONLY : WK => SP, get_filename, count_lines
IMPLICIT NONE
! Declare variables.
CHARACTER(LEN=128) :: filename_input, field, filename_output
INTEGER :: iostat_input, n_data, n_analog, stat_analog
! Program body.
!CALL get_filename(filename_input, 'input', 'READ')
filename_input = 'sonico.dat' ! MC debug, to speed up testing.
OPEN(UNIT=30, FILE=filename_input, IOSTAT=iostat_input, ACTION='READ', STATUS='OLD')
IF (iostat_input /= 0) THEN
    WRITE(*, 100) filename_input
ELSE
    ! Read data about input file content.
    n_data = count_lines(30, skip=4)
    READ(30, *) field
    READ(30, *) field, field, field, n_analog
    READ(30, *) field
    ! Allocate arrays.
    ! MC continue with loop on data.
END IF
CLOSE(30)
STOP
100 FORMAT('Error opening file ', A)
END PROGRAM sonico
