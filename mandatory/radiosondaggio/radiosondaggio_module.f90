MODULE radiosondaggio_module
USE utilities, ONLY : WK => SP
IMPLICIT NONE
PRIVATE
PUBLIC :: R, g, zero_celsius
PUBLIC :: get_altitude, add_altitude
REAL(KIND=WK), PARAMETER :: R = 287.0_WK  ! J / (Kg K)
REAL(KIND=WK), PARAMETER :: g = 9.81_WK  ! m / s^2
REAL(KIND=WK), PARAMETER :: zero_celsius = 273.15  ! K
CONTAINS

! Evaluate altitude in m of final point using barometric formula.
! IN arguments:
!   T
!     Scalar real, temperature in °C.
!   p_1
!     Scalar real, pressure in mbar of initial point.
!   p_2
!     Scalar real, pressure in mbar of final point.
!   z_1
!     Scalar real, altitude in m of initial point.
REAL(KIND=WK) FUNCTION get_altitude(T, p_1, p_2, z_1) RESULT(z_2)
    ! Dummy arguments declaration.
    REAL(KIND=WK), INTENT(IN) :: T  ! °C
    REAL(KIND=WK), INTENT(IN) :: p_1  ! mbar
    REAL(KIND=WK), INTENT(IN) :: p_2  ! mbar
    REAL(KIND=WK), INTENT(IN) :: z_1  ! m
    ! Compute result.
    z_2 = z_1 + R / g * (T + zero_celsius) * LOG(p_1 / p_2)
END FUNCTION

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

! Add altitude values to and using data from the specified unit.
! IN arguments:
!   unit_input
!     Scalar integer.
!   unit_output
!     Scalar integer.
SUBROUTINE add_altitude(unit_input, unit_output)
    ! Dummy arguments declaration.
    INTEGER, INTENT(IN) :: unit_input
    INTEGER, INTENT(IN) :: unit_output
    ! Variables declaration.
    CHARACTER(LEN=512) :: header
    REAL(KIND=WK) :: h0  ! m
    REAL(KIND=WK) :: p0  ! mbar
    REAL(KIND=WK) :: T0  ! °C
    ! Read and write header.
    READ(unit_input, '(A)') header
    WRITE(unit_output, '(A)') header
    ! Extract data from header.
    h0 = character2real(header, sub_start='h0=', sub_end='p0=')
    p0 = character2real(header, sub_start='p0=', sub_end='T0=')
    T0 = character2real(header, sub_start='T0=')
    WRITE(*, *) h0, p0, T0 ! MC debug.
    ! MC continue.
END SUBROUTINE

END MODULE radiosondaggio_module
