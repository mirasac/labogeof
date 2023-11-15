PROGRAM matrix_filter
! Matrices are expressed in transposed form columns x rows to leverage the capabilities of Fortran.
USE utilities, ONLY : WK => DP, get_filename, mat_load, mat_write
IMPLICIT NONE
! Declare variables.
CHARACTER(LEN=128) :: filename_input, field, filename_avg, filename_filtered, filename_average
INTEGER :: iostat_input, ncols, nrows, stat_T, stat_T_avg, i_row, iostat_avg, iostat_filtered, iostat_average
REAL(KIND=WK) :: xllcorner, yllcorner, cellsize, NODATA_value
REAL(KIND=WK), ALLOCATABLE :: T(:, :), T_avg(:)  ! C
REAL(KIND=WK) :: kernel_filtered(3, 3), kernel_average(9, 9)
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
        ! Evaluate filtered matrix.
        !CALL get_filename(filename_filtered, 'filtered matrix', 'WRITE')
        filename_filtered = 'fltered_matrix.txt'  ! MC debug, for quick testing.
        kernel_filtered = RESHAPE((/0.3_WK, 0.5_WK, 0.3_WK, 0.5_WK, 1.0_WK, 0.5_WK, 0.3_WK, 0.5_WK, 0.3_WK/), (/3, 3/))
        ! MC continue.
        !CALL get_filename(filename_average, 'average matrix', 'WRITE')
        filename_average = 'average_matrix.txt'  ! MC debug, for quick testing.
        kernel_average = 1.0_WK / 81.0_WK
        ! Deallocate arrays.
        DEALLOCATE(T, T_avg)
    END IF
END IF
CLOSE(30)
STOP
100 FORMAT('Error opening file ', A)
END PROGRAM matrix_filter
