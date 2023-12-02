PROGRAM sonico
USE utilities, ONLY : PATH_MAX, WK => SP, get_filename, count_lines, dd2rad, rotate_euler
USE sonico_module, ONLY : velocity_t
IMPLICIT NONE
! Declare variables.
CHARACTER(LEN=10), PARAMETER :: FILENAME_CONFIG = 'config.nml'
REAL(KIND=WK), PARAMETER :: NAN = 99.99_WK
CHARACTER(LEN=PATH_MAX) :: filename_input, filename_output
NAMELIST /namelist_config/ filename_input, filename_output
LOGICAL :: exist_output
CHARACTER(LEN=512) :: h1, h2_1, h2_2, h2_3, h3, h4
INTEGER :: iostat_config, iostat_input, iostat_output, n_data, n_analog, stat_air, stat_c, stat_analog, i_data, i_analog
TYPE(velocity_t), ALLOCATABLE :: air(:)
REAL(KIND=WK), ALLOCATABLE :: c(:)
INTEGER, ALLOCATABLE :: analog(:, :)
REAL(KIND=WK) :: phi, theta, psi
REAL(KIND=WK) :: original(3), rotated(3)
! Read configurations.
OPEN(UNIT=30, FILE=FILENAME_CONFIG, IOSTAT=iostat_config, ACTION='READ', STATUS='OLD')
IF (iostat_config /= 0) THEN
    WRITE(*, 100) FILENAME_CONFIG
    CALL get_filename(filename_input, 'input', 'READ')
    CALL get_filename(filename_output, 'output, note it is replaced if already existing a file wiht same name', 'WRITE')
ELSE
    READ(30, NML=namelist_config)
    INQUIRE(FILE=filename_output, EXIST=exist_output)
    WRITE(*, '(3A)') 'Warning: file ', TRIM(filename_output), ' is already present, it gets overwritten'
END IF
CLOSE(30)
! Open input and output files.
OPEN(UNIT=30, FILE=filename_input, IOSTAT=iostat_input, ACTION='READ', STATUS='OLD')
OPEN(UNIT=31, FILE=filename_output, IOSTAT=iostat_output, ACTION='WRITE', STATUS='REPLACE')
IF (iostat_input /= 0) THEN
    WRITE(*, 100) filename_input
ELSE IF (iostat_output /= 0) THEN
    WRITE(*, 100) filename_output
ELSE
    ! Read data about input file content.
    n_data = count_lines(30, skip=4)
    READ(30, '(A)') h1
    READ(30, *) h2_1, h2_2, h2_3, n_analog
    READ(30, '(A)') h3
    READ(30, '(A)') h4
    ! Allocate arrays.
    ALLOCATE(air(n_data), STAT=stat_air)
    ALLOCATE(c(n_data), STAT=stat_c)
    ALLOCATE(analog(n_data, n_analog), STAT=stat_analog)
    IF (stat_air > 0) THEN
        WRITE(*, 101) 'air velocity values'
    ELSE IF (stat_c > 0) THEN
        WRITE(*, 101) 'sound speed values'
    ELSE IF (stat_analog > 0) THEN
        WRITE(*, 101) 'analog values'
    ELSE
        ! Get rotation angles.
        DO
            WRITE(*, *) 'Insert phi: '
            READ(*, *) phi
            IF (phi >= 0.0_WK .AND. phi <= 360.0_WK) EXIT
            WRITE(*, *) 'Error: angle phi should be in interval [0°, 360°]'
        END DO
        phi = dd2rad(phi)
        DO
            WRITE(*, *) 'Insert theta: '
            READ(*, *) theta
            IF (theta >= 0.0_WK .AND. theta <= 90.0_WK) EXIT
            WRITE(*, *) 'Error: angle theta should be in interval [0°, 90°]'
        END DO
        theta = dd2rad(theta)
        DO
            WRITE(*, *) 'Insert psi: '
            READ(*, *) psi
            IF (psi >= 0.0_WK .AND. psi <= 360.0_WK) EXIT
            WRITE(*, *) 'Error: angle psi should be in interval [0°, 360°]'
        END DO
        psi = dd2rad(psi)
        ! Write header lines in output file.
        WRITE(31, '(A)') TRIM(h1)
        WRITE(31, '(3(A, 1X), I2)') TRIM(h2_1), TRIM(h2_2), TRIM(h2_3), n_analog
        WRITE(31, '(A)') TRIM(h3)
        WRITE(31, '(A)') TRIM(h4)
        ! Load data from input file and insert in output file.
        DO i_data = 1, n_data, 1
            READ(30, *) air(i_data)%u, air(i_data)%v, air(i_data)%w, c(i_data), &
                (analog(i_data, i_analog), i_analog = 1, n_analog, 1)
            ! Apply SDR rotation.
            original(1) = air(i_data)%u
            original(2) = air(i_data)%v
            original(3) = air(i_data)%w
            IF (original(1) >= NAN .OR. original(2) >= NAN .OR. original(3) >= NAN) THEN
                rotated = 0.0_WK
            ELSE
                CALL rotate_euler(original, phi, theta, psi, rotated)
            END IF
            air(i_data)%u = rotated(1)
            air(i_data)%v = rotated(2)
            air(i_data)%w = rotated(3)
            ! Write data in output file.
            WRITE(31, *) air(i_data)%u, air(i_data)%v, air(i_data)%w, c(i_data), &
                (analog(i_data, i_analog), i_analog = 1, n_analog, 1)
        END DO
    END IF
    ! Deallocate arrays.
    IF (ALLOCATED(air)) THEN
        DEALLOCATE(air)
    END IF
    IF (ALLOCATED(c)) THEN
        DEALLOCATE(c)
    END IF
    IF (ALLOCATED(analog)) THEN
        DEALLOCATE(analog)
    END IF
END IF
CLOSE(30)
STOP
100 FORMAT('Error opening file ', A)
101 FORMAT('Error allocating memory for ', A)
END PROGRAM sonico
