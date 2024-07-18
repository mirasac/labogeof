MODULE utilities
IMPLICIT NONE
PRIVATE
PUBLIC :: PATH_MAX, DP, SP, EPS
PUBLIC :: date_t, time_t
PUBLIC :: get_filename, count_lines
INTEGER, PARAMETER :: PATH_MAX = 259  ! Maximum number of characters in absolute paths.
INTEGER, PARAMETER :: CHAR_MAX = 80  ! Maximum number of characters displayed in terminal.
INTEGER, PARAMETER :: DP = SELECTED_REAL_KIND(15, 307)  ! Minimum precision and range of IEEE 754 double-precision floating-point format.
INTEGER, PARAMETER :: SP = SELECTED_REAL_KIND(6, 38)  ! Minimum precision and range of IEEE 754 single-precision floating-point format.
INTEGER, PARAMETER :: WK = SP  ! Working kind.
REAL(KIND=WK), PARAMETER :: EPS = TINY(1.0_WK)  ! Machine epsilon.
TYPE :: date_t
    INTEGER :: year
    INTEGER :: month
    INTEGER :: day
END TYPE
TYPE :: time_t
    INTEGER :: hour
    INTEGER :: minute
    INTEGER :: second
END TYPE
CONTAINS

! Get filename from standard input and store in specified variable.
! OUT arguments:
!   filename
! IN optional arguments:
!   description
!   action_file
!     String, specify file action, READ or WRITE, default READ.
SUBROUTINE get_filename(filename, description, action_file)
    ! Dummy arguments declaration.
    CHARACTER(LEN=*), INTENT(OUT) :: filename
    CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: description
    CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: action_file
    ! Variables declaration.
    CHARACTER(LEN=5) :: string_write, action_file_
    LOGICAL :: file_exists
    ! Build insert request message.
    IF (PRESENT(description)) THEN
        string_write = ' for '
    ELSE
        string_write = ''
    END IF
    ! Set default action for file.
    IF (PRESENT(action_file)) THEN
        action_file_ = action_file
    ELSE
        action_file_ = 'READ'
    END IF
    ! Loop insert request.
    DO
        WRITE(*, '(4A)') 'Insert filename', string_write, TRIM(description), ':'
        READ(*, '(A)') filename
        INQUIRE(FILE=TRIM(filename), EXIST=file_exists)
        IF (TRIM(action_file_) == 'READ' .OR. TRIM(action_file_) == 'read') THEN
            IF (file_exists) THEN
                EXIT
            ELSE
                WRITE(*, '(3A)') 'Error: file ', TRIM(filename), ' does not exist'
            END IF
        ELSE IF (TRIM(action_file_) == 'WRITE' .OR. TRIM(action_file_) == 'write') THEN
            IF (file_exists) THEN
                WRITE(*, '(3A)') 'Warning: file ', TRIM(filename), ' is already present'
            END IF
            EXIT
        END IF
    END DO
END SUBROUTINE

! Count lines from the beginning of the file.
! IN arguments:
!   unit_file
!     Scalar integer, unit of file opened with read action.
! IN optional arguments:
!   skip
!     Scalar, integer, number of lines to skip starting from the beginning, default 0.
INTEGER FUNCTION count_lines(unit_file, skip)
    ! Dummy arguments declaration.
    INTEGER, INTENT(IN) :: unit_file
    INTEGER, INTENT(IN), OPTIONAL :: skip
    ! Variables declaration.
    INTEGER :: skip_, iostat_file
    ! Set default values.
    IF (PRESENT(skip)) THEN
        skip_ = skip
    ELSE
        skip_ = 0
    END IF
    ! Count lines.
    REWIND(unit_file)
    count_lines = 0
    DO
        READ(unit_file, *, IOSTAT=iostat_file)
        IF (iostat_file < 0) EXIT
        count_lines = count_lines + 1
    END DO
    REWIND(unit_file)
    ! Skip lines from the beginning of unit.
    count_lines = count_lines - skip_
END FUNCTION

END MODULE utilities
