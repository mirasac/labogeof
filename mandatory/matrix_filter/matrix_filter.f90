PROGRAM matrix_filter
USE utilities
IMPLICIT NONE
! Declare variables.
CHARACTER(LEN=128) :: filename_input, field, filename_mean, filename_filtered_matrix, filename_average_matrix
INTEGER :: iostat_input, ncols, nrows, stat_T, stat_T_avg, stat_y, i, iostat_mean, iostat_filtered_matrix, iostat_average_matrix
REAL :: xllcorner, yllcorner, cellsize, NODATA_value
REAL, ALLOCATABLE :: T (:, :), T_avg (:)  ! C
REAL, ALLOCATABLE :: y (:)
! Program body.
!CALL get_filename(filename_input, 'input')
filename_input = 'monthly_temperature_sample.txt'  ! MC debug, for quick testing.
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
    ALLOCATE (T (nrows, ncols), STAT=stat_T)
    ALLOCATE (T_avg (nrows), STAT=stat_T_avg)
    ALLOCATE (y (nrows), STAT=stat_y)
    ! Load data from input file.
    IF (stat_T .GT. 0 .OR. stat_T_avg .GT. 0 .OR. stat_y .GT. 0) THEN
        WRITE(*, *) 'Error allocating memory'
    ELSE
        DO i = 1, nrows, 1
            READ(30, *) T(i, :)
        END DO
        ! MC continue.
        !CALL get_filename(filename_mean, 'latitudinal mean temperature')
        !CALL get_filename(filename_kernel, 'filtered matrix')
    END IF
    DEALLOCATE(T, T_avg, y)
END IF
CLOSE(30)
STOP
100 FORMAT('Error opening file ', A)
END PROGRAM matrix_filter
