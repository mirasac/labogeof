MODULE utilities
IMPLICIT NONE
PRIVATE
PUBLIC :: PATH_MAX, DP, SP, PI, EPS
PUBLIC :: get_filename, mat_load, mat_write, count_lines, dd2rad, rotate_euler, character2real
INTEGER, PARAMETER :: PATH_MAX = 259  ! Maximum number of characters in absolute paths.
INTEGER, PARAMETER :: CHAR_MAX = 80  ! Maximum number of characters displayed in terminal.
INTEGER, PARAMETER :: DP = SELECTED_REAL_KIND(15, 307)  ! Minimum precision and range of IEEE 754 double-precision floating-point format.
INTEGER, PARAMETER :: SP = SELECTED_REAL_KIND(6, 38)  ! Minimum precision and range of IEEE 754 single-precision floating-point format.
INTEGER, PARAMETER :: WK = SP  ! Working kind.
REAL(KIND=WK), PARAMETER :: PI = 3.1415926535897932384626433832795028841971693993751058209749445923078164062862089986280348253421_WK
REAL(KIND=WK), PARAMETER :: EPS = 0.01_WK
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
        INQUIRE(FILE=filename, EXIST=file_exists)
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

! Load matrix of real values from specified unit.
! IN arguments:
!   unit_in
!     Scalar integer, unit to load the matrix from.
! OUT arguments:
!   matrix
!     Real rank 2 array.
SUBROUTINE mat_load(unit_in, matrix)
    ! Dummy arguments declaration.
    INTEGER, INTENT(IN) :: unit_in
    REAL(KIND=WK), INTENT(OUT) :: matrix(:, :)
    ! Variables declaration.
    INTEGER :: matrix_shape(2)
    REAL(KIND=WK), ALLOCATABLE :: matrix_tmp(:, :)
    INTEGER :: stat_matrix_tmp
    ! Read data from unit.
    matrix_shape = SHAPE(matrix)
    ALLOCATE(matrix_tmp(matrix_shape(2), matrix_shape(1)), STAT=stat_matrix_tmp)
    READ(unit_in, *) matrix_tmp
    ! Transpose read data.
    IF (stat_matrix_tmp .GT. 0) THEN
        WRITE(*, '(A)') 'Error allocating memory for read in mat_load, no change to input matrix occured'
    ELSE
        matrix = TRANSPOSE(matrix_tmp)
        DEALLOCATE(matrix_tmp)
    END IF
END SUBROUTINE

! Write matrix to unit in free form.
! IN arguments:
!   matrix
!     Real rank 2 array.
! IN optional arguments:
!   unit_out
!     Integer, unit to write the matrix, default standard output.
SUBROUTINE mat_write(matrix, unit_out)
    ! Dummy arguments declaration.
    REAL(KIND=WK), INTENT(IN) :: matrix(:, :)
    INTEGER, INTENT(IN), OPTIONAL :: unit_out
    ! Variables declaration.
    INTEGER :: i
    ! Write matrix to unit.
    IF (PRESENT(unit_out)) THEN
        DO i = 1, SIZE(matrix, 1), 1
            WRITE(unit_out, *) matrix(i, :)
        END DO
    ELSE
        DO i = 1, SIZE(matrix, 1), 1
            WRITE(*, *) matrix(i, :)
        END DO
    END IF
END SUBROUTINE

! Count lines from the beginning of the file.
! IN arguments:
!   unit_file
!     Scalar integer, unit of file opened with read action.
! IN optional arguments:
!   skip
!     Scalar, integer, number of lines to skip starting from the beginning,
!     default 0.
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

! Convert decimal degree to radians.
! IN arguments:
!   dd
REAL(KIND=WK) FUNCTION dd2rad(dd)
    ! Dummy arguments declaration.
    REAL(KIND=WK), INTENT(IN) :: dd
    ! Degree conversion.
    dd2rad = dd * PI / 180.0_WK
END FUNCTION

! Build 3-dimensional rotation matrix.
! IN arguments:
!   original
!     Real rank 1 array, vector to be rotated.
!   phi
!     Scalar real.
!   theta
!     Scalar real.
!   psi
!     Scalar real.
! OUT arguments:
!   rotated
!     Real rank 1 array, original vector rotated with specified angles.
SUBROUTINE rotate_euler(original, phi, theta, psi, rotated)
    ! Dummy arguments declaration.
    REAL(KIND=WK), INTENT(IN) :: original(3)
    REAL(KIND=WK), INTENT(IN) :: phi
    REAL(KIND=WK), INTENT(IN) :: theta
    REAL(KIND=WK), INTENT(IN) :: psi
    REAL(KIND=WK), INTENT(OUT) :: rotated(3)
    ! Variables declaration.
    REAL(KIND=WK) :: matrix(3, 3)
    ! Apply rotation.
    matrix(1, 1) = COS(psi) * COS(phi) - COS(theta) * SIN(phi) * SIN(psi)
    matrix(2, 1) = COS(psi) * SIN(phi) + COS(theta) * COS(phi) * SIN(psi)
    matrix(3, 1) = SIN(theta) * SIN(psi)
    matrix(1, 2) = - SIN(psi) * COS(phi) - COS(theta) * SIN(phi) * COS(psi)
    matrix(2, 2) = - SIN(psi) * SIN(phi) + COS(theta) * COS(phi) * COS(psi)
    matrix(3, 2) = SIN(theta) * COS(psi)
    matrix(1, 3) = SIN(theta) * SIN(phi)
    matrix(2, 3) = - SIN(theta) * COS(phi)
    matrix(3, 3) = COS(theta)
    rotated = MATMUL(matrix, original)
END SUBROUTINE

! Convert real number from character representation to numeric value.
! IN arguments:
!   str
!     Character, input string containing the character representation of
!     the number to convert.
! IN optional arguments:
!   sub_start
!     Character, substring of the input string starting from which the
!     number is written, default the conversion start from the beggining
!     of the input string.
!   sub_end
!     Character, substring of the input string which follows the number
!     to convert in the input string, default the conversion ends at the
!     end of the input string.
REAL(KIND=WK) FUNCTION character2real(str, sub_start, sub_end)
    ! Dummy arguments declaration.
    CHARACTER(LEN=*), INTENT(IN) :: str
    CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: sub_start
    CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: sub_end
    ! Variables declaration.
    INTEGER :: i_start, i_end
    ! Set default values.
    IF (PRESENT(sub_start)) THEN
        i_start = INDEX(str, sub_start) + LEN(sub_start)
    ELSE
        i_start = 1
    END IF
    IF (PRESENT(sub_end)) THEN
        i_end = INDEX(str, sub_end) - 1
    ELSE
        i_end = LEN(str)
    END IF
    READ(str(i_start : i_end), *) character2real
END FUNCTION

END MODULE utilities
