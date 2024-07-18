PROGRAM main
USE utilities, ONLY : PATH_MAX, WK => SP, get_filename
IMPLICIT NONE
! Declare variables.
CHARACTER(LEN=10), PARAMETER :: FILENAME_CONFIG = 'config.nml'
INTEGER, PARAMETER :: UNIT_INPUT = 30
INTEGER, PARAMETER :: UNIT_OUTPUT = 31
CHARACTER(LEN=PATH_MAX) :: filename_constants, filename_radius, filename_positive_out, filename_negative_out, &
    filename_data, filename_data_out, filename_absolute_out, filename_relative_out
REAL(KIND=WK) :: T  ! K
REAL(KIND=WK) :: error_code_NA
NAMELIST /namelist_config/ filename_constants, filename_radius, T, filename_positive_out, filename_negative_out, &
    filename_data, error_code_NA, filename_data_out, filename_absolute_out, filename_relative_out
INTEGER :: iostat_input, iostat_output
! Read configurations.
OPEN(UNIT=UNIT_INPUT, FILE=FILENAME_CONFIG, IOSTAT=iostat_input, ACTION='READ', STATUS='OLD')
IF (iostat_input /= 0) THEN
    WRITE(*, 100) FILENAME_CONFIG
    CLOSE(UNIT_INPUT)
    CALL get_filename(filename_constants, 'physical constants', 'READ')
    CALL get_filename(filename_radius, 'surface radius', 'READ')
    WRITE(*, *) 'Insert value of temperature in K to evaluate the vapor pressure ratio:'
    READ(*, *) T
    CALL get_filename(filename_positive_out, 'vapor pressure ratio for convex surfaces', 'WRITE')
    CALL get_filename(filename_negative_out, 'vapor pressure ratio for concave surfaces', 'WRITE')
    CALL get_filename(filename_data, 'thermohygrometer data', 'READ')
    WRITE(*, *) 'Insert error code for missing values:'
    READ(*, *) error_code_NA
    CALL get_filename(filename_data_out, 'humidity and critical humidity', 'WRITE')
    CALL get_filename(filename_absolute_out, 'absolute count of days with various humidity values', 'WRITE')
    CALL get_filename(filename_relative_out, 'relative count of days with various humidity values', 'WRITE')
    OPEN(UNIT=UNIT_OUTPUT, FILE=FILENAME_CONFIG, IOSTAT=iostat_output, ACTION='WRITE', STATUS='REPLACE')
    IF (iostat_output /= 0) THEN
        WRITE(*, 100) FILENAME_CONFIG
        WRITE(*, *) 'Inserted configurations are not saved on disk'
    ELSE
        WRITE(UNIT_OUTPUT, NML=namelist_config)
    END IF
    CLOSE(UNIT_OUTPUT)
ELSE
    READ(UNIT_INPUT, NML=namelist_config)
END IF
CLOSE(UNIT_INPUT)
100 FORMAT('Error opening file ', A)
END PROGRAM main
