PROGRAM Exercise_5
IMPLICIT NONE
! Declare parameters.
REAL, PARAMETER :: E_NAN = -999.0  ! W / m^2
REAL, PARAMETER :: SECONDS_PER_DAY = 86400
INTEGER, PARAMETER :: N_NAN_MAX = 4
! Declare variables.
CHARACTER(LEN=128) :: filename
REAL :: E_shortwave, E_infrared, E, E_0, E_1, m, q  ! W / m^2
INTEGER :: iostat_open, iostat_read, DD, MM, YYYY, hh, minute, ss, t_0, c_NAN, i_NAN
! Program body.
WRITE(*, *) 'Insert filename: '
READ(*, '(A)') filename
filename = TRIM(filename)
OPEN(UNIT=30, FILE=filename, IOSTAT=iostat_open, ACTION='READ', STATUS='OLD')
IF (iostat_open /= 0) THEN
    WRITE(*, *) 'Error opening file ', filename
ELSE
    c_NAN = 0
    E_0 = 0.0
    t_0 = 0
    E_shortwave = 0.0
    E_infrared = 0.0
    ! Read header.
    READ(30, *)
    ! Read lines.
    DO
        READ(30, *, IOSTAT=iostat_read) DD, MM, YYYY, hh, minute, ss, E
        ! Manage edge cases.
        IF (iostat_read < 0) THEN
            EXIT
        ELSE IF (iostat_read > 0) THEN
            WRITE(*, *) 'Error reading file: ', iostat_read
            EXIT
        ELSE IF (c_NAN > N_NAN_MAX) THEN
            WRITE(*, *) 'Exceeded maximum number of missing values'
            EXIT
        END IF
        ! Manage read values.
        IF (E /= E_NAN) THEN
            ! Interpolate previous missing values.
            IF (c_NAN > 0) THEN
                E_1 = E
                m = (E_1 - E_0) / (c_NAN + 1)
                q = E_0 - m * t_0
                DO i_NAN = 1, c_NAN, 1
                    E_0 = q + m * (i_NAN + t_0)
                    IF (E_0 >= 0.0) THEN
                        E_shortwave = E_shortwave + E_0
                    ELSE
                        E_infrared = E_infrared - E_0
                    END IF
                END DO
                t_0 = t_0 + c_NAN
                c_NAN = 0
            END IF
            ! Add present value.
            E_0 = E
            t_0 = t_0 + 1
            IF (E_0 >= 0.0) THEN
                E_shortwave = E_shortwave + E_0
            ELSE
                E_infrared = E_infrared - E_0
            END IF
        ELSE
            c_NAN = c_NAN + 1
        END IF
    END DO
    ! Print output.
    WRITE(*, 100) 'shortwave', E_shortwave * SECONDS_PER_DAY / 1e6
    WRITE(*, 100) 'infrared', E_infrared * SECONDS_PER_DAY / 1e6
END IF
CLOSE(30)
STOP
100 FORMAT('Daily insolation in ', A, ' band is ', ES14.6, ' MJ / m^2')
END PROGRAM Exercise_5
