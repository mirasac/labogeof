PROGRAM radiosondaggio
USE utilities, ONLY : PATH_MAX, WK => SP, EPS, get_filename, count_lines
USE radiosondaggio_module, ONLY : add_altitude, get_pressure, get_analyses
IMPLICIT NONE
! Declare variables.
CHARACTER(LEN=10), PARAMETER :: FILENAME_CONFIG = 'config.nml'
REAL(KIND=WK), PARAMETER :: z_res = 200.0_WK  ! m
INTEGER, PARAMETER :: UNIT_INPUT = 30
INTEGER, PARAMETER :: UNIT_OUTPUT = 31
CHARACTER(LEN=PATH_MAX) :: filename_A_input, filename_B_input, filename_A_output, filename_B_output, filename_output
NAMELIST /namelist_config/ filename_A_input, filename_B_input, filename_A_output, filename_B_output, filename_output
INTEGER :: iostat_input, iostat_output, n_lines, stat_p, stat_T, stat_z, &
    stat_z_A, stat_p_A, n_A, n_B, stat_z_B, stat_p_B, i_A_min, i_B_min, i_A_max, i_B_max, i
REAL(KIND=WK), ALLOCATABLE :: z(:), z_A(:), z_B(:)  ! m
REAL(KIND=WK), ALLOCATABLE :: p(:), p_A(:), p_B(:), delta_p(:)  ! mbar
REAL(KIND=WK), ALLOCATABLE :: T(:)  ! °C
REAL(KIND=WK) :: z_min, z_max  ! m
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
stat_z_A = -1
stat_p_A = -1
IF (iostat_input /= 0) THEN
    WRITE(*, 100) filename_A_input
ELSE IF (iostat_output /= 0) THEN
    WRITE(*, 100) filename_A_output
ELSE
    n_lines = count_lines(UNIT_INPUT)
    ALLOCATE(z(n_lines), STAT=stat_z)
    ALLOCATE(p(n_lines), STAT=stat_p)
    ALLOCATE(T(n_lines), STAT=stat_T)
    IF (stat_z > 0) THEN
        WRITE(*, 101) 'station A altitude'
    ELSE IF (stat_p > 0) THEN
        WRITE(*, 101) 'station A pressure'
    ELSE IF (stat_T > 0) THEN
        WRITE(*, 101) 'station A temperature'
    ELSE
        ! Add altitude values.
        CALL add_altitude(UNIT_INPUT, UNIT_OUTPUT, store_zero=.TRUE., z=z, p=p, T=T)
        ! Create pressure analyses.
        n_A = INT(z(n_lines) / z_res) - INT(z(1) / z_res)
        ALLOCATE(z_A(n_A), STAT=stat_z_A)
        ALLOCATE(p_A(n_A), STAT=stat_p_A)
        IF (stat_z_A > 0) THEN
            WRITE(*, 101) 'station A grid altitude'
        ELSE IF (stat_p_A > 0) THEN
            WRITE(*, 101) 'station A grid pressure'
        ELSE
            CALL get_analyses(z, p, T, z_res, z_grid=z_A, p_grid=p_A)
        END IF
    END IF
    IF (ALLOCATED(z)) THEN
        DEALLOCATE(z)
    END IF
    IF (ALLOCATED(p)) THEN
        DEALLOCATE(p)
    END IF
    IF (ALLOCATED(T)) THEN
        DEALLOCATE(T)
    END IF
END IF
CLOSE(UNIT_INPUT)
CLOSE(UNIT_OUTPUT)
! Operate on station B data.
OPEN(UNIT=UNIT_INPUT, FILE=filename_B_input, IOSTAT=iostat_input, ACTION='READ', STATUS='OLD')
OPEN(UNIT=UNIT_OUTPUT, FILE=filename_B_output, IOSTAT=iostat_output, ACTION='WRITE', STATUS='REPLACE')
stat_z_B = -1
stat_p_B = -1
IF (iostat_input /= 0) THEN
    WRITE(*, 100) filename_B_input
ELSE IF (iostat_output /= 0) THEN
    WRITE(*, 100) filename_B_output
ELSE
    n_lines = count_lines(UNIT_INPUT)
    ALLOCATE(z(n_lines), STAT=stat_z)
    ALLOCATE(p(n_lines), STAT=stat_p)
    ALLOCATE(T(n_lines), STAT=stat_T)
    IF (stat_z > 0) THEN
        WRITE(*, 101) 'station B altitude'
    ELSE IF (stat_p > 0) THEN
        WRITE(*, 101) 'station B pressure'
    ELSE IF (stat_T > 0) THEN
        WRITE(*, 101) 'station B temperature'
    ELSE
        ! Add altitude values.
        CALL add_altitude(UNIT_INPUT, UNIT_OUTPUT, store_zero=.TRUE., z=z, p=p, T=T)
        ! Create pressure analyses.
        n_B = INT(z(n_lines) / z_res) - INT(z(1) / z_res)
        ALLOCATE(z_B(n_B), STAT=stat_z_B)
        ALLOCATE(p_B(n_B), STAT=stat_p_B)
        IF (stat_z_B > 0) THEN
            WRITE(*, 101) 'station A grid altitude'
        ELSE IF (stat_p_B > 0) THEN
            WRITE(*, 101) 'station A grid pressure'
        ELSE
            CALL get_analyses(z, p, T, z_res, z_grid=z_B, p_grid=p_B)
        END IF
    END IF
    IF (ALLOCATED(z)) THEN
        DEALLOCATE(z)
    END IF
    IF (ALLOCATED(p)) THEN
        DEALLOCATE(p)
    END IF
    IF (ALLOCATED(T)) THEN
        DEALLOCATE(T)
    END IF
END IF
CLOSE(UNIT_INPUT)
CLOSE(UNIT_OUTPUT)
! Evaluate pressure differences.
OPEN(UNIT=UNIT_OUTPUT, FILE=filename_output, IOSTAT=iostat_output, ACTION='WRITE', STATUS='REPLACE')
IF (iostat_output /= 0) THEN
    WRITE(*, 100) filename_output
ELSE IF (stat_z_A /= 0 .OR. stat_p_A /= 0 .OR. stat_z_B /= 0 .OR. stat_p_B /= 0) THEN
    WRITE(*, *) 'Pressure differences not evaluated'
ELSE
    ! Find index of minimum common altitude.
    z_min = MAX(z_A(1), z_B(1))
    i_A_min = MINLOC(z_A, 1, ABS(z_A - z_min) <= EPS)
    i_B_min = MINLOC(z_B, 1, ABS(z_B - z_min) <= EPS)
    ! Find index of maximum common altitude.
    z_max = MIN(z_A(n_A), z_B(n_B))
    i_A_max = MAXLOC(z_A, 1, ABS(z_A - z_max) <= EPS)
    i_B_max = MAXLOC(z_B, 1, ABS(z_B - z_max) <= EPS)
    ! Write pressure differences.
    delta_p =  p_A(i_A_min : i_A_max) - p_B(i_B_min : i_B_max)
    DO i = 1, SIZE(delta_p), 1
        WRITE(UNIT_OUTPUT, *) z_A(i + i_A_min - 1), delta_p(i)
    END DO
END IF
CLOSE(UNIT_OUTPUT)
IF (ALLOCATED(z_A)) THEN
    DEALLOCATE(z_A)
END IF
IF (ALLOCATED(p_A)) THEN
    DEALLOCATE(p_A)
END IF
IF (ALLOCATED(z_B)) THEN
    DEALLOCATE(z_B)
END IF
IF (ALLOCATED(p_B)) THEN
    DEALLOCATE(p_B)
END IF
IF (ALLOCATED(delta_p)) THEN
    DEALLOCATE(delta_p)
END IF
100 FORMAT('Error opening file ', A)
101 FORMAT('Error allocating memory for ', A)
END PROGRAM radiosondaggio
