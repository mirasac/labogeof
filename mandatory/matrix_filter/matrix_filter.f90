PROGRAM matrix_filter
! Matrices are expressed in transposed form columns x rows to leverage the capabilities of Fortran.
USE utilities, ONLY : WK => SP, get_filename, mat_load, mat_write
USE filters, ONLY : conv2d
IMPLICIT NONE
! Declare variables.
CHARACTER(LEN=128) :: filename_input, field, filename_avg, filename_filtered, filename_average
INTEGER :: iostat_input, ncols, nrows, stat_T, stat_T_avg, iostat_avg, iostat_filtered, iostat_average
REAL(KIND=WK) :: xllcorner, yllcorner, cellsize, NODATA_value, norm_angle, norm_border, norm
REAL(KIND=WK), ALLOCATABLE :: T(:, :), T_avg(:), filtered(:, :), average(:, :)  ! C
REAL(KIND=WK) :: kernel_filtered(3, 3), kernel_average(9, 9)
! Program body.
CALL get_filename(filename_input, 'input', 'READ')
OPEN(UNIT=30, FILE=filename_input, IOSTAT=iostat_input, ACTION='READ', STATUS='OLD')
IF (iostat_input /= 0) THEN
    WRITE(*, 100) filename_input
ELSE
    ! Read data about input file content.
    READ(30, *) field, ncols
    READ(30, *) field, nrows
    READ(30, *) field, xllcorner
    READ(30, *) field, yllcorner
    READ(30, *) field, cellsize
    READ(30, *) field, NODATA_value
    ! Allocate arrays.
    ALLOCATE(T(nrows, ncols), STAT=stat_T)
    ! Load data from input file.
    IF (stat_T .GT. 0) THEN
        WRITE(*, *) 'Error allocating memory for input matrix'
    ELSE
        CALL mat_load(30, T)
        ! Evaluate latitudinal mean temperature.
        ALLOCATE(T_avg(nrows), STAT=stat_T_avg)
        IF (stat_T_avg .GT. 0) THEN
            WRITE(*, *) 'Error allocating memory for latitudinal mean temperature'
        ELSE
            CALL get_filename(filename_avg, 'latitudinal mean temperature', 'WRITE')
            OPEN(UNIT=31, FILE=filename_avg, IOSTAT=iostat_avg, ACTION='WRITE', STATUS='REPLACE')
            IF (iostat_avg /= 0) THEN
                WRITE(*, 100) filename_avg
            ELSE
                T_avg(:) = SUM(T, 2) / ncols
                WRITE(31, *) T_avg
            END IF
            CLOSE(31)
            DEALLOCATE(T_avg)
        END IF
        ! Evaluate filtered matrix.
        CALL get_filename(filename_filtered, 'filtered matrix', 'WRITE')
        OPEN(UNIT=31, FILE=filename_filtered, IOSTAT=iostat_filtered, ACTION='WRITE', STATUS='REPLACE')
        kernel_filtered = RESHAPE( &
            (/0.3_WK, 0.5_WK, 0.3_WK, 0.5_WK, 1.0_WK, 0.5_WK, 0.3_WK, 0.5_WK, 0.3_WK/), &
            SHAPE(kernel_filtered) &
        )
        filtered = conv2d(T, kernel_filtered, padding=1)
        IF (iostat_filtered /= 0) THEN
            WRITE(*, 100) filename_filtered
        ELSE IF (ALLOCATED(filtered)) THEN
            ! Apply normalization leveraging the kernel symmetry.
            norm_angle = SUM(kernel_filtered, RESHAPE( &
                (/.FALSE., .FALSE., .FALSE., .FALSE., .TRUE., .TRUE., .FALSE., .TRUE., .TRUE./), &
                SHAPE(kernel_filtered) &
            ))
            norm_border = SUM(kernel_filtered, RESHAPE( &
                (/.FALSE., .TRUE., .TRUE., .FALSE., .TRUE., .TRUE., .FALSE., .TRUE., .TRUE./), &
                SHAPE(kernel_filtered) &
            ))
            norm = SUM(kernel_filtered)
            filtered(1, 1) = filtered(1, 1) / norm_angle
            filtered(1, ncols) = filtered(1, ncols) / norm_angle
            filtered(nrows, 1) = filtered(nrows, 1) / norm_angle
            filtered(nrows, ncols) = filtered(nrows, ncols) / norm_angle
            filtered(1, 2 : ncols - 1) = filtered(1, 2 : ncols - 1) / norm_border
            filtered(nrows, 2 : ncols - 1) = filtered(nrows, 2 : ncols - 1) / norm_border
            filtered(2 : nrows - 1, 1) = filtered(2 : nrows - 1, 1) / norm_border
            filtered(2 : nrows - 1, ncols) = filtered(2 : nrows - 1, ncols) / norm_border
            filtered(2 : nrows - 1, 2 : ncols - 1) = filtered(2 : nrows - 1, 2 : ncols - 1) / norm
            CALL mat_write(filtered, 31)
            DEALLOCATE(filtered)
        END IF
        CLOSE(31)
        ! Evaluate average matrix.
        CALL get_filename(filename_average, 'average matrix', 'WRITE')
        OPEN(UNIT=31, FILE=filename_average, IOSTAT=iostat_average, ACTION='WRITE', STATUS='REPLACE')
        kernel_average = 1.0_WK / 81.0_WK
        average = conv2d(T, kernel_average, stride=9, padding=0)
        IF (iostat_average /= 0) THEN
            WRITE(*, 100) filename_average
        ELSE IF (ALLOCATED(average)) THEN
            CALL mat_write(average, 31)
            DEALLOCATE(average)
        END IF
        CLOSE(31)
        ! Deallocate arrays.
        DEALLOCATE(T)
    END IF
END IF
CLOSE(30)
STOP
100 FORMAT('Error opening file ', A)
END PROGRAM matrix_filter
