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
INTEGER :: iostat_input, iostat_output, iostat_read  ! Variables for IOSTAT.
INTEGER :: stat_allocate, stat_date, stat_time, stat_T, stat_RH, stat_CRH  ! Variables for STAT.
INTEGER :: size_radius, i, size_data, n_batch, i_batch, n_skipped_batch, i_day, c_always, c_sometimes, c_never
REAL(KIND=WK) :: sigma  ! N m-1
REAL(KIND=WK) :: V_m  ! m3 mol-1
REAL(KIND=WK) :: R  ! J mol-1 K-1
REAL(KIND=WK), ALLOCATABLE :: radius(:)  ! m
REAL(KIND=WK), ALLOCATABLE :: ratio_positive(:), ratio_negative(:)  ! MC delete because I evaluate in DO loop.
TYPE(date_t), ALLOCATABLE :: batch_date(:), daily_date(:)
TYPE(time_t), ALLOCATABLE :: batch_time(:)
REAL(KIND=WK), ALLOCATABLE :: batch_T(:)  ! K
REAL(KIND=WK), ALLOCATABLE :: batch_RH(:), CRH(:)
CHARACTER(LEN=1024) :: dummy_field, format_string
LOGICAL :: print_data_out, print_absolute_out, raised_warning
INTEGER, ALLOCATABLE :: daily_greater(:, :)
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
    ALLOCATE(radius(size_radius), STAT=stat_allocate)
    IF (stat_allocate > 0) THEN
        WRITE(*, 101) 'surface radius values'
    ELSE
        READ(UNIT_INPUT, *, IOSTAT=iostat_read) radius
        CLOSE(UNIT_INPUT)  ! Close here to free resource as soon as it is no more needed.
        IF (iostat_read /= 0) THEN
            WRITE(*, 102) 'surface radius values'
        ELSE
            ! Work on convex surfaces.
            ALLOCATE(ratio_positive(size_radius), STAT=stat_allocate)
            OPEN(UNIT=UNIT_OUTPUT, FILE=TRIM(filename_positive_out), IOSTAT=iostat_output, ACTION='WRITE', STATUS='REPLACE')
            IF (stat_allocate > 0) THEN
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
            ALLOCATE(ratio_negative(size_radius), STAT=stat_allocate)
            OPEN(UNIT=UNIT_OUTPUT, FILE=TRIM(filename_negative_out), IOSTAT=iostat_output, ACTION='WRITE', STATUS='REPLACE')
            IF (stat_allocate > 0) THEN
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
! Point 2, data for point 3 and point 4 are prepared in the same main loop for efficiency.
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
            print_data_out = .FALSE.
        ELSE
            print_data_out = .TRUE.
            ! Prepare format string for writing to output file, width of the integer field is hard coded for ease.
            WRITE(format_string, '("(I2, 1X, I2, 1X, I4, 1X, I2, 1X, I2, 1X, F8.3, ", I4, "(1X, F8.3))")') size_radius
        END IF
        ! Header lines are skipped.
        size_data = count_lines(UNIT_INPUT, skip=2)
        READ(UNIT_INPUT, *) dummy_field
        READ(UNIT_INPUT, *) dummy_field
        n_batch = INT(size_data / SIZE_BATCH)  ! Equal to number of days due to data being always provided complete.
        ! Prepare arrays for counting absolute and relative occurences of relative humidity over the critical value.
        ALLOCATE(daily_date(n_batch), STAT=stat_date)
        ALLOCATE(daily_greater(n_batch, size_radius), STAT=stat_allocate)
        IF (stat_date .GT. 0 .OR. stat_allocate > 0) THEN
            WRITE(*, 101) 'count of days with relative humidity greater than the critical value'
            WRITE(*, *) 'No operations are performed to evaluate related statistics'
            print_absolute_out = .FALSE.
        ELSE
            print_absolute_out = .TRUE.
            daily_greater = 0
        END IF
        ! Batched read of input data file.
        n_skipped_batch = 0
        DO i_batch = 1, n_batch, 1
            raised_warning = .FALSE.
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
                ! Evaluate critical relative humidity for concave surfaces.
                IF (ABS(batch_T(i) - error_code_NA) .LT. EPS) THEN
                    CRH = error_code_NA
                ELSE
                    batch_T(i) = batch_T(i) + zero_celsius  ! Input data has temperature in °C.
                    WHERE (ABS(radius - 0.0_WK) .LT. EPS)
                        CRH = error_code_NA
                    ELSEWHERE
                        CRH = get_critical_relative_humidity(-radius, sigma, V_m, R, batch_T(i))
                    ENDWHERE
                END IF
                ! Write results to output file.
                IF (print_data_out) THEN
                    WRITE(UNIT_OUTPUT, TRIM(format_string)) &
                        batch_date(i)%day, batch_date(i)%month, batch_date(i)%year, &
                        batch_time(i)%hour, batch_time(i)%minute, batch_RH(i), CRH
                END IF
                ! Count occurences of relative humidity over the critical value in the same day.
                IF (print_absolute_out) THEN
                    i_day = i_batch  ! Due to program design, no further operations are needed to identify a day.
                    daily_date(i_day) = batch_date(1)  ! Since a batch corresponds to a day, simply select any date.
                    IF (ANY(ABS(CRH - error_code_NA) .LT. EPS) .AND. .NOT. raised_warning) THEN
                        WRITE(*, '("Warning: ", A, I2, "/", I2, "/", I4)') &
                            'some error codes are present in critical relative humidity data for day ', &
                            daily_date(i_day)%day, daily_date(i_day)%month, daily_date(i_day)%year
                        WRITE(*, *) 'These values are skipped but the resulting statistics may be affected'
                        raised_warning = .TRUE.
                    END IF
                    WHERE (ABS(CRH - error_code_NA) .LT. EPS)
                        daily_greater(i_day, :) = 0
                    ELSEWHERE (batch_RH(i) .GT. CRH)
                        daily_greater(i_day, :) = daily_greater(i_day, :) + 1
                    ENDWHERE
                END IF
            END DO
        END DO
        CLOSE(UNIT_OUTPUT)
    END IF
END IF
CLOSE(UNIT_INPUT)
! Point 3.
IF (print_absolute_out) THEN
    OPEN(UNIT=UNIT_OUTPUT, FILE=TRIM(filename_absolute_out), IOSTAT=iostat_output, ACTION='WRITE', STATUS='REPLACE')
    IF (iostat_output /= 0) THEN
        WRITE(*, 100) TRIM(filename_absolute_out)
    ELSE
        WRITE(UNIT_OUTPUT, '(A9, 1X, A13, 1X, A15, 1X, A10, 1X, A10)') &
            'raggio', 'RH>RHc_sempre', 'RH>RHc_talvolta', 'RH>RHc_mai', 'RH>RHc_tot'
        DO i = 1, size_radius, 1
            c_always = COUNT(daily_greater(:, i) .EQ. SIZE_BATCH)  ! Due to regularity of data, day length is known a priori.
            c_sometimes = COUNT(0 .LT. daily_greater(:, i) .AND. daily_greater(:, i) .LT. SIZE_BATCH)
            c_never = COUNT(daily_greater(:, i) .EQ. 0)
            WRITE(UNIT_OUTPUT, '(E9.3E2, 1X, I4, 11X, I4, 11X, I4, 7X, I4)') &
                radius(i), c_always, c_sometimes, c_never, c_always + c_sometimes + c_never
        END DO
    END IF
    CLOSE(UNIT_OUTPUT)
END IF
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
