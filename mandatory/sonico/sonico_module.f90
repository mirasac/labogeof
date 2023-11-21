MODULE sonico_module
USE utilities, ONLY : WK => SP
IMPLICIT NONE
PRIVATE
PUBLIC :: velocity_t

TYPE :: velocity_t
    REAL(KIND=WK) :: u
    REAL(KIND=WK) :: v
    REAL(KIND=WK) :: w
END TYPE

END MODULE sonico_module
