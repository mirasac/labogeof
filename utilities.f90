MODULE utilities
IMPLICIT NONE
CONTAINS

! Subroutine to sanitize the specified filename.
SUBROUTINE sanitize_filename(filename, description)
! Parameters declaration.
CHARACTER(LEN=128), INTENT(OUT) :: filename
CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: description
! Variables declaration.
CHARACTER(LEN=80) :: string_insert
LOGICAL :: file_exists
! Build insert prompt.
string_insert = 'Insert filename'
IF (PRESENT(description)) THEN
    string_insert = TRIM(string_insert) // ' for ' // TRIM(description) // ':'
ELSE
    string_insert = TRIM(string_insert) // ':'
END IF
! Loop insert prompt.
DO
    WRITE(*, '(A)') string_insert
    READ(*, '(A)') filename
    filename = TRIM(filename)
    INQUIRE(FILE=filename, EXIST=file_exists)
    IF (file_exists) EXIT
    WRITE(*, '(2A)') 'Error opening file ', filename
END DO
END SUBROUTINE

END MODULE utilities
