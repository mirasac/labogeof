PROGRAM sonico
USE utilities, ONLY : PATH_MAX, WK => SP, get_filename, count_lines, dd2rad, rotate_euler
USE sonico_module, ONLY : velocity_t
IMPLICIT NONE
! Declare variables.
CHARACTER(LEN=10), PARAMETER :: filename_config = 'config.nml'
CHARACTER(LEN=PATH_MAX) :: filename_input, filename_output
NAMELIST /namelist_config/ filename_input, filename_output
CHARACTER(LEN=512) :: field
INTEGER :: iostat_config, iostat_input, iostat_output, n_data, n_analog, stat_air, stat_c, stat_analog, i_data, i_analog
TYPE(velocity_t), ALLOCATABLE :: air(:)
REAL(KIND=WK), ALLOCATABLE :: c(:)
INTEGER, ALLOCATABLE :: analog(:, :)
REAL(KIND=WK) :: phi, theta, psi
REAL(KIND=WK) :: v(3)
! Read configurations.
OPEN(UNIT=30, FILE=filename_config, IOSTAT=iostat_config, ACTION='READ', STATUS='OLD')
IF (iostat_config /= 0) THEN
    WRITE(*, 100) filename_config
    !CALL get_filename(filename_input, 'input', 'WRITE')
    filename_input = 'sonico.dat' ! MC debug, to speed up testing.
    !CALL get_filename(filename_output, 'output', 'WRITE')
    filename_output = 'output.dat' ! MC debug, to speed up testing.
ELSE
    READ(30, NML=namelist_config)
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
    READ(30, *) field
    READ(30, *) field, field, field, n_analog
    READ(30, *) field
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
        WRITE(*, *) 'Insert phi: '
        READ(*, *) phi
        phi = dd2rad(phi)
        WRITE(*, *) 'Insert theta: '
        READ(*, *) theta
        theta = dd2rad(theta)
        WRITE(*, *) 'Insert psi: '
        READ(*, *) psi
        psi = dd2rad(psi)
        ! Load data from input file and insert in output file.
        DO i_data = 1, n_data, 1
            READ(30, *) air(i_data)%u, air(i_data)%v, air(i_data)%w, c(i_data), &
                (analog(i_data, i_analog), i_analog = 1, n_analog, 1)
            ! Apply SDR rotation.
            v(1) = air(i_data)%u
            v(2) = air(i_data)%v
            v(3) = air(i_data)%w
            CALL rotate_euler(v, phi, theta, psi, v)
            air(i_data)%u = v(1)
            air(i_data)%v = v(2)
            air(i_data)%w = v(3)
            ! Write data in output file.
            ! MC continue.
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
