PROGRAM radiosondaggio
USE utilities, ONLY : PATH_MAX, WK => SP, get_filename, count_lines
USE radiosondaggio_module, ONLY : get_altitude, add_altitude
IMPLICIT NONE
! Declare variables.
CHARACTER(LEN=10), PARAMETER :: FILENAME_CONFIG = 'config.nml'
INTEGER, PARAMETER :: UNIT_A_INPUT = 31
INTEGER, PARAMETER :: UNIT_A_OUTPUT = 32
CHARACTER(LEN=PATH_MAX) :: filename_A_input, filename_B_input, filename_A_output, filename_B_output, filename_output
NAMELIST /namelist_config/ filename_A, filename_B, filename_A_output, filename_B_output, filename_output
INTEGER :: iostat_config, iostat_A_input, iostat_B_input, iostat_A_output, iostat_B_output, iostat_output
! Read configurations.
OPEN(UNIT=UNIT_A_INPUT, FILE=FILENAME_CONFIG, IOSTAT=iostat_config, ACTION='READ', STATUS='OLD')
IF (iostat_config /= 0) THEN
    WRITE(*, 100) FILENAME_CONFIG
    CALL get_filename(filename_A_input, 'station A input', 'READ')
    CALL get_filename(filename_B_input, 'station B input', 'READ')
    CALL get_filename(filename_A_output, 'station A output', 'WRITE')
    CALL get_filename(filename_B_output, 'station B output', 'WRITE')
    CALL get_filename(filename_output, 'pressure difference values', 'WRITE')
ELSE
    READ(UNIT_A_INPUT, NML=namelist_config)
END IF
CLOSE(UNIT_A_INPUT)
! Operate on station A data.
OPEN(UNIT=UNIT_A_INPUT, FILE=filename_A_input, IOSTAT=iostat_A_input, ACTION='READ', STATUS='OLD')
OPEN(UNIT=UNIT_A_OUTPUT, FILE=filename_A_output, IOSTAT=iostat_A_output, ACTION='WRITE', STATUS='REPLACE')
IF (iostat_A_input /= 0) THEN
    WRITE(*, 100) filename_A_input
ELSE IF (iostat_A_output /= 0) THEN
    WRITE(*, 100) filename_A_output
ELSE
    add_altitude(UNIT_A_INPUT, UNIT_A_OUTPUT)
END IF
CLOSE(UNIT_A_INPUT)
CLOSE(UNIT_A_OUTPUT)
! MC continue.
STOP
100 FORMAT('Error opening file ', A)
101 FORMAT('Error allocating memory for ', A)
END PROGRAM radiosondaggio
