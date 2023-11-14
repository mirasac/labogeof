PROGRAM matrix_filter
! Matrices are expressed in transposed form columns x rows to leverage the capabilities of Fortran.
USE utilities, ONLY : WP => DP, get_filename, mat_load, mat_write
IMPLICIT NONE
! Declare variables.
CHARACTER(LEN=128) :: filename_input, field, filename_avg, filename_filtered_matrix, filename_average_matrix
INTEGER :: iostat_input, ncols, nrows, stat_T, stat_T_avg, i_row, iostat_avg, iostat_filtered_matrix, iostat_average_matrix
REAL(KIND=WP) :: xllcorner, yllcorner, cellsize, NODATA_value
REAL(KIND=WP), ALLOCATABLE :: T(:, :), T_avg(:)  ! C
! Program body.
!CALL get_filename(filename_input, 'input', 'READ')
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
    ALLOCATE(T(nrows, ncols), STAT=stat_T)
    ALLOCATE(T_avg(nrows), STAT=stat_T_avg)
    ! Load data from input file.
    IF (stat_T .GT. 0 .OR. stat_T_avg .GT. 0) THEN
        WRITE(*, *) 'Error allocating memory for arrays'
    ELSE
        CALL mat_load(30, T)
        ! Evaluate latitudinal mean temperature.
        !CALL get_filename(filename_avg, 'latitudinal mean temperature', 'WRITE')
        filename_avg = 'latitudinal_mean_temperature.txt'  ! MC debug, for quick testing.
        OPEN(UNIT=31, FILE=filename_avg, IOSTAT=iostat_avg, ACTION='WRITE', STATUS='REPLACE')
        IF (iostat_avg /= 0) THEN
            WRITE(*, 100) filename_avg
        ELSE
            T_avg(:) = SUM(T, 2) / ncols
            WRITE(31, *) T_avg
        END IF
        CLOSE(31)
        ! MC continue.
        !CALL get_filename(filename_kernel, 'filtered matrix', 'WRITE')
        ! Deallocate arrays.
        DEALLOCATE(T, T_avg)
    END IF
END IF
CLOSE(30)
STOP
100 FORMAT('Error opening file ', A)
END PROGRAM matrix_filter
