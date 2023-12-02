PROGRAM radiosondaggio
USE utilities, ONLY : PATH_MAX, WK => SP, get_filename, count_lines
USE radiosondaggio_module, ONLY : add_altitude
IMPLICIT NONE
! Declare variables.
CHARACTER(LEN=10), PARAMETER :: FILENAME_CONFIG = 'config.nml'
REAL(KIND=WK), PARAMETER :: z_res = 200.0_WK  ! m
INTEGER, PARAMETER :: UNIT_INPUT = 30
INTEGER, PARAMETER :: UNIT_OUTPUT = 31
CHARACTER(LEN=PATH_MAX) :: filename_A_input, filename_B_input, filename_A_output, filename_B_output, filename_output
NAMELIST /namelist_config/ filename_A_input, filename_B_input, filename_A_output, filename_B_output, filename_output
INTEGER :: iostat_input, iostat_output, n_lines, stat_p, stat_T, stat_z, stat_z_grid, n_layers
REAL(KIND=WK), ALLOCATABLE, DIMENSION(:) :: p, p_A, p_B  ! mbar
REAL(KIND=WK), ALLOCATABLE :: T(:)  ! °C
REAL(KIND=WK), ALLOCATABLE :: z(:), z_grid(:)  ! m
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
    n_lines = count_lines(UNIT_INPUT, skip=1)
    ALLOCATE(p(n_lines), STAT=stat_p)
    ALLOCATE(T(n_lines), STAT=stat_T)
    ALLOCATE(z(n_lines), STAT=stat_z)
    ALLOCATE(z_grid(n_lines), STAT=stat_z_grid)
    IF (stat_p > 0) THEN
        WRITE(*, 101) 'station A pressure'
    ELSE IF (stat_T > 0) THEN
        WRITE(*, 101) 'station A temperature'
    ELSE IF (stat_z > 0) THEN
        WRITE(*, 101) 'station A altitude'
    ELSE IF (stat_z_grid > 0) THEN
        WRITE(*, 101) 'station A vertical grid'
    ELSE
        ! Add altitude values.
        CALL add_altitude(UNIT_INPUT, UNIT_OUTPUT, p=p, T=T, z=z)
        ! Create pressure analyses.
        z_grid(1) = (INT(z(1) / z_res) + 1) * z_res
        z_grid(n_lines) = INT(z(n_lines) / z_res) * z_res
        n_layers = INT((z(n_lines) - z(1)) / z_res) + 1
        WRITE(*, *) z(1), z_grid(1), z(n_lines), z_grid(n_lines) ! MC debug.
        WRITE(*, *) INT((z_grid(n_lines) - z_grid(1)) / z_res) + 1, n_layers ! MC debug.
        ! MC continue.
    END IF
    IF (ALLOCATED(p)) THEN
        DEALLOCATE(p)
    END IF
    IF (ALLOCATED(T)) THEN
        DEALLOCATE(T)
    END IF
    IF (ALLOCATED(z)) THEN
        DEALLOCATE(z)
    END IF
    IF (ALLOCATED(z_grid)) THEN
        DEALLOCATE(z_grid)
    END IF
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
