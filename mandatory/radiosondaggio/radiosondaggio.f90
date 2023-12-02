PROGRAM radiosondaggio
USE utilities, ONLY : PATH_MAX, WK => SP, get_filename, count_lines
USE radiosondaggio_module, ONLY : add_altitude, get_pressure
IMPLICIT NONE
! Declare variables.
CHARACTER(LEN=10), PARAMETER :: FILENAME_CONFIG = 'config.nml'
REAL(KIND=WK), PARAMETER :: z_res = 200.0_WK  ! m
INTEGER, PARAMETER :: UNIT_INPUT = 30
INTEGER, PARAMETER :: UNIT_OUTPUT = 31
CHARACTER(LEN=PATH_MAX) :: filename_A_input, filename_B_input, filename_A_output, filename_B_output, filename_output
NAMELIST /namelist_config/ filename_A_input, filename_B_input, filename_A_output, filename_B_output, filename_output
INTEGER :: iostat_input, iostat_output, n_lines, stat_p, stat_T, stat_z, stat_z_A, stat_p_A, n_layers, i_layer, i_line, i_tmp
REAL(KIND=WK), ALLOCATABLE :: p(:), p_A(:)  ! mbar
REAL(KIND=WK), ALLOCATABLE :: T(:)  ! °C
REAL(KIND=WK) :: T_layer  ! °C
REAL(KIND=WK), ALLOCATABLE :: z(:), z_A(:)  ! m
REAL(KIND=WK) :: z_layer, z_0  ! m
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
    IF (stat_p > 0) THEN
        WRITE(*, 101) 'station A pressure'
    ELSE IF (stat_T > 0) THEN
        WRITE(*, 101) 'station A temperature'
    ELSE IF (stat_z > 0) THEN
        WRITE(*, 101) 'station A altitude'
    ELSE
        ! Add altitude values.
        CALL add_altitude(UNIT_INPUT, UNIT_OUTPUT, p=p, T=T, z=z)
        ! Create pressure analyses.
        n_layers = INT((z(n_lines) - z(1)) / z_res) + 1
        ALLOCATE(z_A(n_layers), STAT=stat_z_A)
        ALLOCATE(p_A(n_layers), STAT=stat_p_A)
        IF (stat_z_A > 0) THEN
            WRITE(*, 101) 'station A grid altitude'
        ELSE IF (stat_p_A > 0) THEN
            WRITE(*, 101) 'station A grid pressure'
        ELSE
            i_layer = 1
            DO i_line = 2, n_lines, 1
                WRITE(*, *) '---', z(i_line - 1), '---', p(i_line - 1), '---', i_line - 1 ! MC debug.
                T_layer = (T(i_line) + T(i_line - 1)) / 2.0_WK
                i_tmp = 1
                z_0 = (INT(z(i_line - 1) / z_res)) * z_res
                DO
                    z_layer = z_0 + i_tmp * z_res
                    IF (z_layer >= z(i_line)) EXIT
                    z_A(i_layer) = z_layer
                    p_A(i_layer) = get_pressure(T_layer, z(i_line - 1), z_A(i_layer), p(i_line - 1))
                    WRITE(*, *) '   ', z_A(i_layer), '   ', p_A(i_layer), '   ', i_layer ! MC debug.
                    i_tmp = i_tmp + 1
                    i_layer = i_layer + 1
                END DO
            END DO
            ! MC continue.
        END IF
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
IF (ALLOCATED(z_A)) THEN
    DEALLOCATE(z_A)
END IF
IF (ALLOCATED(p_A)) THEN
    DEALLOCATE(p_A)
END IF
STOP
100 FORMAT('Error opening file ', A)
101 FORMAT('Error allocating memory for ', A)
END PROGRAM radiosondaggio
