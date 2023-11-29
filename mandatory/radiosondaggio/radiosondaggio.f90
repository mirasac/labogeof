PROGRAM radiosondaggio
USE utilities, ONLY : PATH_MAX, WK => SP, get_filename, count_lines
USE radiosondaggio_module, ONLY : add_altitude
IMPLICIT NONE
! Declare variables.
CHARACTER(LEN=10), PARAMETER :: FILENAME_CONFIG = 'config.nml'
INTEGER, PARAMETER :: UNIT_INPUT = 30
INTEGER, PARAMETER :: UNIT_OUTPUT = 31
CHARACTER(LEN=PATH_MAX) :: filename_A_input, filename_B_input, filename_A_output, filename_B_output, filename_output
NAMELIST /namelist_config/ filename_A_input, filename_B_input, filename_A_output, filename_B_output, filename_output
INTEGER :: iostat_input, iostat_output
! Read configurations.
OPEN(UNIT=UNIT_INPUT, FILE=FILENAME_CONFIG, IOSTAT=iostat_input, ACTION='READ', STATUS='OLD')
IF (iostat_input /= 0) THEN
    WRITE(*, 100) FILENAME_CONFIG
    CALL get_filename(filename_A_input, 'station A input', 'READ')
    CALL get_filename(filename_B_input, 'station B input', 'READ')
    CALL get_filename(filename_A_output, 'station A output', 'WRITE')
    CALL get_filename(filename_B_output, 'station B output', 'WRITE')
    CALL get_filename(filename_output, 'pressure difference values', 'WRITE')
ELSE
    READ(UNIT_INPUT, NML=namelist_config)
END IF
CLOSE(UNIT_INPUT)
! Operate on station A data.
OPEN(UNIT=UNIT_INPUT, FILE=filename_A_input, IOSTAT=iostat_input, ACTION='READ', STATUS='OLD')
OPEN(UNIT=UNIT_OUTPUT, FILE=filename_A_output, IOSTAT=iostat_output, ACTION='WRITE', STATUS='REPLACE')
IF (iostat_input /= 0) THEN
    WRITE(*, 100) filename_A_input
ELSE IF (iostat_output /= 0) THEN
    WRITE(*, 100) filename_A_output
ELSE
    CALL add_altitude(UNIT_INPUT, UNIT_OUTPUT)
END IF
CLOSE(UNIT_INPUT)
CLOSE(UNIT_OUTPUT)
! Operate on station B data.
OPEN(UNIT=UNIT_INPUT, FILE=filename_B_input, IOSTAT=iostat_input, ACTION='READ', STATUS='OLD')
OPEN(UNIT=UNIT_OUTPUT, FILE=filename_B_output, IOSTAT=iostat_output, ACTION='WRITE', STATUS='REPLACE')
IF (iostat_input /= 0) THEN
    WRITE(*, 100) filename_B_input
ELSE IF (iostat_output /= 0) THEN
    WRITE(*, 100) filename_B_output
ELSE
    CALL add_altitude(UNIT_INPUT, UNIT_OUTPUT)
END IF
CLOSE(UNIT_INPUT)
CLOSE(UNIT_OUTPUT)
! Evaluate pressure differences.
! MC continue, only if previous operations are successful.
STOP
100 FORMAT('Error opening file ', A)
101 FORMAT('Error allocating memory for ', A)
END PROGRAM radiosondaggio
