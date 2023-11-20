MODULE utilities
IMPLICIT NONE
PRIVATE
PUBLIC :: DP, SP, get_filename, mat_load, mat_write, count_lines
INTEGER, PARAMETER :: PATH_MAX = 4096  ! Maximum number of bytes in absolute paths.
INTEGER, PARAMETER :: CHAR_MAX = 80  ! Maximum number of characters displayed in terminal.
INTEGER, PARAMETER :: DP = SELECTED_REAL_KIND(15, 307)  ! Minimum precision and range of IEEE 754 double-precision floating-point format.
INTEGER, PARAMETER :: SP = SELECTED_REAL_KIND(6, 38)  ! Minimum precision and range of IEEE 754 single-precision floating-point format.
INTEGER, PARAMETER :: WK = SP  ! Working kind.
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

! Count lines present in the specified unit.
! IN arguments:
!   unit_file
!     Integer, unit opened with read action.
! IN optional arguments:
!   skip
!     Integer, number of lines to skip starting from the beginning,
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
    count_lines = 0
    DO
        READ(unit_file, *, IOSTAT=iostat_file)
        IF (iostat_file < 0) EXIT
        count_lines = count_lines + 1
    END DO
    ! Skip lines from the beginning of unit.
    count_lines = count_lines - skip_
END FUNCTION

END MODULE utilities
