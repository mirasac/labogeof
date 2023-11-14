MODULE utilities
IMPLICIT NONE
PRIVATE
PUBLIC :: DP, get_filename, mat_load, mat_write
INTEGER, PARAMETER :: PATH_MAX = 4096  ! Maximum number of bytes in absolute paths.
INTEGER, PARAMETER :: CHAR_MAX = 80  ! Maximum number of characters displayed in terminal.
INTEGER, PARAMETER :: DP = SELECTED_REAL_KIND(15, 307)  ! Minimum precision and range of IEEE 754 double-precision floating-point format.
INTEGER, PARAMETER :: WP = DP  ! Working precision.
CONTAINS

! Get filename from standard input and store in specified variable.
! OUT arguments:
!   filename
! IN optional arguments:
!   description
SUBROUTINE get_filename(filename, description)
    ! Dummy arguments declaration.
    CHARACTER(LEN=*), INTENT(OUT) :: filename
    CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: description
    ! Variables declaration.
    CHARACTER(LEN=5) :: string_write
    LOGICAL :: file_exists
    ! Build insert request message.
    IF (PRESENT(description)) THEN
        string_write = ' for '
    ELSE
        string_write = ''
    END IF
    ! Loop insert request.
    DO
        WRITE(*, '(4A)') 'Insert filename', string_write, TRIM(description), ':'
        READ(*, '(A)') filename
        filename = TRIM(filename)
        INQUIRE(FILE=filename, EXIST=file_exists)
        IF (file_exists) EXIT
        WRITE(*, '(2A)') 'Error opening file ', filename
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
    REAL(KIND=WP), INTENT(OUT) :: matrix(:, :)
    ! Variables declaration.
    INTEGER :: matrix_shape(2)
    REAL(KIND=WP), ALLOCATABLE :: matrix_tmp(:, :)
    INTEGER :: stat_matrix_tmp
    ! Read data from unit.
    matrix_shape = SHAPE(matrix)
    ALLOCATE(matrix_tmp (matrix_shape(2), matrix_shape(1)), STAT=stat_matrix_tmp)
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
!   unit_out
!     Integer, unit to write the matrix, default standard output.
SUBROUTINE mat_write(matrix, unit_out)
    ! Dummy arguments declaration.
    REAL(KIND=WP), INTENT(IN) :: matrix(:, :)
    INTEGER, INTENT(IN), OPTIONAL :: unit_out
    ! Write matrix to unit.
    IF (PRESENT(unit_out)) THEN
        WRITE(unit_out, *) TRANSPOSE(matrix)
    ELSE
        WRITE(*, *) TRANSPOSE(matrix)
    END IF
END SUBROUTINE

END MODULE utilities
