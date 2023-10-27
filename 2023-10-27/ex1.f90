PROGRAM ex1
IMPLICIT NONE
CHARACTER(LEN=30), PARAMETER :: FILENAME_INPUT = 'monthly_temperature_sample.txt'
CHARACTER(LEN=10), PARAMETER :: FILENAME_OUTPUT = 'output.txt'
INTEGER :: ncols, nrows, i, iostat_input, stat_T, stat_T_avg, stat_y, iostat_output
REAL :: xllcorner, yllcorner, cellsize, NODATA_value
REAL, ALLOCATABLE :: T (:, :), T_avg (:)  ! C
REAL, ALLOCATABLE :: y (:)
CHARACTER(LEN=128) :: field
OPEN(UNIT=30, FILE=FILENAME_INPUT, IOSTAT=iostat_input, ACTION='READ', STATUS='OLD')
IF (iostat_input /= 0) THEN
    WRITE(*,*) 'Error opening input file ', FILENAME_INPUT
ELSE
    READ(30,*) field, ncols
    READ(30,*) field, nrows
    READ(30,*) field, xllcorner
    READ(30,*) field, yllcorner
    READ(30,*) field, cellsize
    READ(30,*) field, NODATA_value
    ALLOCATE (T (nrows, ncols), STAT=stat_T)
    ALLOCATE (T_avg (nrows), STAT=stat_T_avg)
    ALLOCATE (y (nrows), STAT=stat_y)
    OPEN(UNIT=31, FILE=FILENAME_OUTPUT, IOSTAT=iostat_output, ACTION='WRITE', STATUS='REPLACE')
    IF (stat_T .GT. 0 .OR. stat_T_avg .GT. 0 .OR. stat_y .GT. 0) THEN
        WRITE(*,*) 'Error allocating memory'
    ELSE IF (iostat_output /= 0) THEN
        WRITE(*,*) 'Error opening output file ', FILENAME_OUTPUT
    ELSE
        DO i = 1, nrows, 1
            READ(30,*) T(i, :)
            ! Implicit loop in sum function is good only when there are no missing values, otherwise they must be managed using an explicit for loop.
            T_avg(i) = SUM(T(i, :)) / ncols
            y(i) = yllcorner + cellsize * (nrows - i)
            WRITE(31,*) y(i), T_avg(i)
        END DO
    END IF
    DEALLOCATE(T, T_avg, y)
    CLOSE(31)
END IF
CLOSE(30)
END PROGRAM ex1
