MODULE utilities
IMPLICIT NONE
PUBLIC
INTEGER, PARAMETER :: PATH_MAX = 4096  ! Maximum number of bytes in absolute paths.
INTEGER, PARAMETER :: CHAR_MAX = 80  ! Maximum number of characters displayed in terminal.
CONTAINS

! Subroutine to sanitize the specified filename.
! OUT arguments:
!   filename
! IN optional arguments:
!   description
SUBROUTINE sanitize_filename(filename, description)
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

END MODULE utilities
