PROGRAM ex1
IMPLICIT NONE
INTEGER :: grade
CHARACTER :: letter
WRITE (*,*) 'Write grade: '
READ (*,*) grade
! Solution with IF THEN ELSE.
IF (grade > 95 .AND. grade <= 100) THEN
    letter = 'A'
ELSE IF (grade > 86) THEN
    letter = 'B'
ELSE IF (grade > 76) THEN
    letter = 'C'
ELSE IF (grade > 66) THEN
    letter = 'D'
ELSE IF (grade >= 0) THEN
    letter = 'E'
ELSE
    letter = ' '
END IF
WRITE (*,*) 'Solution with IF THEN ELSE: ', letter
! Solution with SELECT CASE.
SELECT CASE (grade)
CASE (96:100)
    letter = 'A'
CASE (87:95)
    letter = 'B'
CASE (77:86)
    letter = 'C'
CASE (67:76)
    letter = 'D'
CASE (0:66)
    letter = 'E'
CASE DEFAULT
    letter = ' '
END SELECT
WRITE (*,*) 'Solution with SELECT CASE: ', letter
END PROGRAM ex1
