PROGRAM ex1
IMPLICIT NONE
! Declarations.
CHARACTER(LEN=14), PARAMETER :: FILENAME_INPUT = 'dati_tetto.txt'
INTEGER, PARAMETER :: N_DATE = 3
INTEGER, PARAMETER :: N_HOUR = 24
INTEGER, DIMENSION(3) :: day, month, year
REAL, DIMENSION(3) :: T_avg, T_sigma, T_sum, T_sum2
REAL, DIMENSION(24) :: T
INTEGER :: iostat_input, i_date, i_hour, hour
! Get input data.
OPEN(UNIT=30, FILE=FILENAME_INPUT, IOSTAT=iostat_input, ACTION='READ', STATUS='OLD')
IF (iostat_input == 0) THEN
    DO i_date = 1, N_DATE, 1
        T_sum(i_date) = 0.0
        T_sum2(i_date) = 0.0
        DO i_hour = 1, N_HOUR, 1
            READ(30,*) day(i_date), month(i_date), year(i_date), hour, T(i_hour)
            T_sum(i_date) = T_sum(i_date) + T(i_hour)
            T_sum2(i_date) = T_sum2(i_date) + T(i_hour)**2
        END DO
        T_avg(i_date) = T_sum(i_date) / N_HOUR
        T_sigma(i_date) = SQRT((T_sum2(i_date) - T_sum(i_date)**2 / N_HOUR) / (N_HOUR - 1.0))
        WRITE(*,100) i_date, T_avg(i_date), T_sigma(i_date)
    END DO
ELSE
    WRITE (*,*) 'Error opening file ', FILENAME_INPUT
END IF
CLOSE(30)
STOP
100 FORMAT ('media giorno ', I1, ' = ', F7.2, ' +- ', F7.2)
END PROGRAM ex1
