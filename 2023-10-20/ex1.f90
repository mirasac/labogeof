PROGRAM ex1
IMPLICIT NONE
! Declarations.
CHARACTER(LEN=14), PARAMETER :: FILENAME_1 = '10486917_4.txt'
CHARACTER(LEN=18), PARAMETER :: FILENAME_2 = '10512170_4_new.txt'
CHARACTER(LEN=10), PARAMETER :: FILENAME_OUTPUT = 'output.txt'
REAL, PARAMETER :: p = 1013.25 ! hPa
INTEGER :: iostat_1, iostat_2, code_1, code_2, c, n_1, n_2
CHARACTER(LEN=128) :: iomsg_1, iomsg_2, record_1, record_2
CHARACTER(LEN=10) :: date_1, date_2
CHARACTER(LEN=2) :: hh_1, hh_2, mm_1, mm_2
REAL :: T_1, T_2, T_1_avg, T_2_avg, RH_1, RH_2, RH_1_avg, RH_2_avg, dew_1, dew_2, e_w_1, e_w_2, e_1, e_2, q_1, q_2
! Body.
OPEN (UNIT=31, FILE=FILENAME_1, STATUS='OLD', IOSTAT=iostat_1, IOMSG=iomsg_1)
OPEN (UNIT=32, FILE=FILENAME_2, STATUS='OLD', IOSTAT=iostat_2, IOMSG=iomsg_2)
IF (iostat_1 == 0 .AND. iostat_2 == 0) THEN
    ! Read headers from input files.
    READ (31,100) code_1
    READ (32,100) code_2
    READ (31,*) record_1
    READ (32,*) record_2
    ! Create output file.
    OPEN (UNIT=30, FILE=FILENAME_OUTPUT, STATUS='REPLACE')
    WRITE (30,101) code_1, code_2
    WRITE (30,*) '#     Date   Time, GMT+01:00  T1      T2       RH1      RH2    q1     q2'
    ! Manipulate records.
    c = 1
    DO
        READ (31,'(A)', IOSTAT=iostat_1) record_1
        READ (32,'(A)', IOSTAT=iostat_2) record_2
        IF (iostat_1 /= 0 .OR. iostat_2 /= 0) EXIT
        ! Extract record number.
        SELECT CASE (c)
        CASE (1:9)
            READ (record_1,'(I1)') n_1
            READ (record_1,'(2X, A)') record_1
            READ (record_2,'(I1)') n_2
            READ (record_2,'(2X, A)') record_2
        CASE (10:99)
            READ (record_1,'(I2)') n_1
            READ (record_1,'(3X, A)') record_1
            READ (record_2,'(I2)') n_2
            READ (record_2,'(3X, A)') record_2
        CASE (100:999)
            READ (record_1,'(I3)') n_1
            READ (record_1,'(4X, A)') record_1
            READ (record_2,'(I3)') n_2
            READ (record_2,'(4X, A)') record_2
        CASE (1000:9999)
            READ (record_1,'(I4)') n_1
            READ (record_1,'(5X, A)') record_1
            READ (record_2,'(I4)') n_2
            READ (record_2,'(5X, A)') record_2
        CASE (10000:99999)
            READ (record_1,'(I5)') n_1
            READ (record_1,'(6X, A)') record_1
            READ (record_2,'(I5)') n_2
            READ (record_2,'(6X, A)') record_2
        CASE DEFAULT
            WRITE (*,*) 'Format for first field not available, exit program'
            STOP
        END SELECT
        ! Extract date.
        READ (record_1,'(A10)') date_1
        READ (record_1,'(11X, A)') record_1
        READ (record_2,'(A10)') date_2
        READ (record_2,'(11X, A)') record_2
        ! Extract time.
        READ (record_1,'(A2, 1X, A2)') hh_1, mm_1
        READ (record_1,'(9X, A)') record_1
        READ (record_2,'(A2, 1X, A2)') hh_2, mm_2
        READ (record_2,'(9X, A)') record_2
        ! Extract measures.
        READ (record_1,*) T_1, T_1_avg, RH_1, RH_1_avg, dew_1
        READ (record_2,*) T_2, T_2_avg, RH_2, RH_2_avg, dew_2
        ! Evaluate results.
        e_w_1 = 6.112 * EXP(17.67 * T_1 / (T_1 + 243.5))
        e_w_2 = 6.112 * EXP(17.67 * T_2 / (T_2 + 243.5))
        e_1 = 0.01 * RH_1 * e_w_1
        e_2 = 0.01 * RH_2 * e_w_2
        q_1 = 622.0 * e_1 / (p - e_1)
        q_2 = 622.0 * e_2 / (p - e_2)
        WRITE (30,102) n_1, date_1, hh_1, mm_1, T_1, T_2, RH_1, RH_2, q_1, q_2
        c = c + 1
    END DO
    CLOSE (30)
ELSE IF (iostat_1 /= 0) THEN
    WRITE (*,*) 'Error occured during opening file ', FILENAME_1, ', IOSTAT = ', iostat_1
    WRITE (*,*) iomsg_1
ELSE
    WRITE (*,*) 'Error occured during opening file ', FILENAME_2, ', IOSTAT = ', iostat_2
    WRITE (*,*) iomsg_2
END IF
CLOSE (31)
CLOSE (32)
STOP
100 FORMAT (12X, I8)
101 FORMAT ('Plot Title: ', I8, ', ', I8)
102 FORMAT (I5, 2X, A10, 4X, A2, ':', A2, 4(2X, F7.3), 2(2X, F5.2))
END PROGRAM ex1
