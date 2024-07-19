PROGRAM main
USE utilities, ONLY : PATH_MAX, WK => SP, SIZE_BATCH, EPS, date_t, time_t, get_filename, count_lines
USE functions, ONLY : get_vapor_pressure_ratio, get_critical_relative_humidity
IMPLICIT NONE
! Declare variables.
CHARACTER(LEN=10), PARAMETER :: FILENAME_CONFIG = 'config.nml'
INTEGER, PARAMETER :: UNIT_INPUT = 30
INTEGER, PARAMETER :: UNIT_OUTPUT = 31
CHARACTER(LEN=PATH_MAX) :: filename_constants, filename_radius, filename_positive_out, filename_negative_out, &
    filename_data, filename_data_out, filename_absolute_out, filename_relative_out
REAL(KIND=WK) :: T, zero_celsius  ! K
REAL(KIND=WK) :: error_code_NA
INTEGER :: iostat_input, iostat_output, stat_radius, size_radius, iostat_radius, &
    stat_ratio_positive, stat_ratio_negative, i, &
    stat_date, stat_time, stat_T, stat_RH, stat_CRH, size_data, n_batch, i_batch, n_skipped_batch, iostat_read
REAL(KIND=WK) :: sigma  ! N m-1
REAL(KIND=WK) :: V_m  ! m3 mol-1
REAL(KIND=WK) :: R  ! J mol-1 K-1
REAL(KIND=WK), ALLOCATABLE :: radius(:)  ! m
REAL(KIND=WK), ALLOCATABLE :: ratio_positive(:), ratio_negative(:)  ! MC delete because I evaluate in DO loop.
TYPE(date_t), ALLOCATABLE :: batch_date(:)
TYPE(time_t), ALLOCATABLE :: batch_time(:)
REAL(KIND=WK), ALLOCATABLE :: batch_T(:)  ! K
REAL(KIND=WK), ALLOCATABLE :: batch_RH(:)
REAL(KIND=WK), ALLOCATABLE :: CRH(:)
CHARACTER(LEN=1024) :: dummy_field, format_string
NAMELIST /namelist_config/ filename_constants, filename_radius, T, filename_positive_out, filename_negative_out, &
    filename_data, error_code_NA, filename_data_out, filename_absolute_out, filename_relative_out
NAMELIST /namelist_constants/ sigma, V_m, R, zero_celsius
! Read configurations.
OPEN(UNIT=UNIT_INPUT, FILE=FILENAME_CONFIG, IOSTAT=iostat_input, ACTION='READ', STATUS='OLD')
IF (iostat_input /= 0) THEN
    WRITE(*, 100) FILENAME_CONFIG
    CLOSE(UNIT_INPUT)  ! Close here to allow repoening for writing.
    CALL get_filename(filename_constants, 'physical constants', 'READ')
    CALL get_filename(filename_radius, 'surface radius values', 'READ')
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
    CLOSE(UNIT_INPUT)
END IF
! Read physical constant.
sigma = error_code_NA
V_m = error_code_NA
R = error_code_NA
zero_celsius = error_code_NA
OPEN(UNIT=UNIT_INPUT, FILE=TRIM(filename_constants), IOSTAT=iostat_input, ACTION='READ', STATUS='OLD')
IF (iostat_input /= 0) THEN
    WRITE(*, 100) TRIM(filename_constants)
ELSE
    READ(UNIT_INPUT, NML=namelist_constants)
END IF
CLOSE(UNIT_INPUT)
! Point 1.
OPEN(UNIT=UNIT_INPUT, FILE=TRIM(filename_radius), IOSTAT=iostat_input, ACTION='READ', STATUS='OLD')
IF (iostat_input /= 0) THEN
    WRITE(*, 100) TRIM(filename_radius)
    CLOSE(UNIT_INPUT)
ELSE
    size_radius = count_lines(UNIT_INPUT)
    ALLOCATE(radius(size_radius), STAT=stat_radius)
    IF (stat_radius > 0) THEN
        WRITE(*, 101) 'surface radius values'
    ELSE
        READ(UNIT_INPUT, *, IOSTAT=iostat_radius) radius
        CLOSE(UNIT_INPUT)  ! Close here to free resource as soon as it is no more needed.
        IF (iostat_radius /= 0) THEN
            WRITE(*, 102) 'surface radius values'
        ELSE
            ! Work on convex surfaces.
            ALLOCATE(ratio_positive(size_radius), STAT=stat_ratio_positive)
            OPEN(UNIT=UNIT_OUTPUT, FILE=TRIM(filename_positive_out), IOSTAT=iostat_output, ACTION='WRITE', STATUS='REPLACE')
            IF (stat_ratio_positive > 0) THEN
                WRITE(*, 101) 'vapor pressure ratio for convex surfaces'
            ELSE IF (iostat_output /= 0) THEN
                WRITE(*, 100) TRIM(filename_positive_out)
            ELSE
                WRITE(UNIT_OUTPUT, 103) 'raggio', 'ew(r)/ew(inf)'
                WHERE (ABS(radius - 0.0_WK) .LT. EPS)
                    ratio_positive = error_code_NA
                ELSEWHERE
                    ratio_positive = get_vapor_pressure_ratio(radius, sigma, V_m, R, T)
                ENDWHERE
                DO i = 1, size_radius, 1
                    WRITE(UNIT_OUTPUT, 104) radius(i), ratio_positive(i)
                END DO
            END IF
            IF (ALLOCATED(ratio_positive)) THEN
                DEALLOCATE(ratio_positive)
            END IF
            CLOSE(UNIT_OUTPUT)
            ! Work on concave surfaces.
            ALLOCATE(ratio_negative(size_radius), STAT=stat_ratio_negative)
            OPEN(UNIT=UNIT_OUTPUT, FILE=TRIM(filename_negative_out), IOSTAT=iostat_output, ACTION='WRITE', STATUS='REPLACE')
            IF (stat_ratio_negative > 0) THEN
                WRITE(*, 101) 'vapor pressure ratio for convex surfaces'
            ELSE IF (iostat_output /= 0) THEN
                WRITE(*, 100) TRIM(filename_negative_out)
            ELSE
                WRITE(UNIT_OUTPUT, 103) 'raggio', 'ew(r)/ew(inf)'
                WHERE (ABS(radius - 0.0_WK) .LT. EPS)
                    ratio_negative = error_code_NA
                ELSEWHERE
                    ratio_negative = get_vapor_pressure_ratio(-radius, sigma, V_m, R, T)
                ENDWHERE
                DO i = 1, size_radius, 1
                    WRITE(UNIT_OUTPUT, 104) radius(i), ratio_negative(i)
                END DO
            END IF
            IF (ALLOCATED(ratio_negative)) THEN
                DEALLOCATE(ratio_negative)
            END IF
            CLOSE(UNIT_OUTPUT)
        END IF
    END IF
END IF
! Point 2.
OPEN(UNIT=UNIT_INPUT, FILE=TRIM(filename_data), IOSTAT=iostat_input, ACTION='READ', STATUS='OLD')
IF (iostat_input /= 0) THEN
    WRITE(*, 100) TRIM(filename_data)
ELSE
    ALLOCATE(batch_date(SIZE_BATCH), STAT=stat_date)
    ALLOCATE(batch_time(SIZE_BATCH), STAT=stat_time)
    ALLOCATE(batch_T(SIZE_BATCH), STAT=stat_T)
    ALLOCATE(batch_RH(SIZE_BATCH), STAT=stat_RH)
    ALLOCATE(CRH(size_radius), STAT=stat_CRH)
    IF (stat_date > 0 .OR. stat_time > 0 .OR. stat_T > 0 .OR. stat_RH > 0 .OR. stat_CRH > 0) THEN
        WRITE(*, 101) 'thermohygrometer data'
    ELSE
        OPEN(UNIT=UNIT_OUTPUT, FILE=TRIM(filename_data_out), IOSTAT=iostat_output, ACTION='WRITE', STATUS='REPLACE')
        IF (iostat_output /= 0) THEN
            WRITE(*, 100) TRIM(filename_data_out)
            WRITE(*, *) 'Values of critical relative humidity are not printed to file'
        ELSE
            ! Prepare format string for writing to output file.
            WRITE(format_string, '("(I2, 1X, I2, 1X, I4, 1X, I2, 1X, I2, 1X, F8.3, ", I4, "(1X, F8.3))")') size_radius  ! Maximum number of digits for array size is hard coded to keep the length of the format string under its declared size.
        END IF
        ! Header lines are skipped.
        size_data = count_lines(UNIT_INPUT, skip=2)
        READ(UNIT_INPUT, *) dummy_field
        READ(UNIT_INPUT, *) dummy_field
        n_batch = INT(size_data / SIZE_BATCH)  ! Equal to number of days due to data being always provided complete.
        n_skipped_batch = 0
        DO i_batch = 1, n_batch, 1
            DO i = 1, SIZE_BATCH, 1
                READ(UNIT_INPUT, *, IOSTAT=iostat_read) dummy_field, &
                    batch_date(i)%day, batch_date(i)%month, batch_date(i)%year, &
                    batch_time(i)%hour, batch_time(i)%minute, batch_time(i)%second, &
                    dummy_field, batch_T(i), dummy_field, batch_RH(i), dummy_field
                ! Skip batch on read error.
                IF (iostat_read /= 0) THEN
                    WRITE(*, 102) 'thermohygrometer data'
                    WRITE(*, *) 'Batch number ', i, ' is skipped'
                    n_skipped_batch = n_skipped_batch + 1
                    EXIT
                END IF
                IF (ABS(batch_T(i) - error_code_NA) .LT. EPS) THEN
                    CRH = error_code_NA
                ELSE
                    ! Convert temperature in input data to K.
                    batch_T(i) = batch_T(i) + zero_celsius
                    ! Evaluate critical relative humidity for concave surfaces.
                    WHERE (ABS(radius - 0.0_WK) .LT. EPS)
                        CRH = error_code_NA
                    ELSEWHERE
                        CRH = get_critical_relative_humidity(-radius, sigma, V_m, R, batch_T(i))
                    ENDWHERE
                END IF
                ! Write results to output file.
                IF (iostat_output == 0) THEN
                    WRITE(UNIT_OUTPUT, TRIM(format_string)) &
                        batch_date(i)%day, batch_date(i)%month, batch_date(i)%year, batch_time(i)%hour, batch_time(i)%minute, &
                        batch_RH(i), CRH
                END IF
            END DO
        END DO
        CLOSE(UNIT_OUTPUT)
    END IF
END IF
CLOSE(UNIT_INPUT)
IF (ALLOCATED(radius)) THEN
    DEALLOCATE(radius)
END IF
IF (ALLOCATED(batch_date)) THEN
    DEALLOCATE(batch_date)
END IF
IF (ALLOCATED(batch_time)) THEN
    DEALLOCATE(batch_time)
END IF
IF (ALLOCATED(batch_T)) THEN
    DEALLOCATE(batch_T)
END IF
IF (ALLOCATED(batch_RH)) THEN
    DEALLOCATE(batch_RH)
END IF
IF (ALLOCATED(CRH)) THEN
    DEALLOCATE(CRH)
END IF
100 FORMAT('Error opening file ', A)
101 FORMAT('Error allocating memory for ', A)
102 FORMAT('Error reading ', A)
103 FORMAT(A12, 1X, A)
104 FORMAT(E12.5E3, 1X, F5.3)
END PROGRAM main
