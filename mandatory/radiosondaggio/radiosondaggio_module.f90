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
    INTEGER :: i_start, i_end
    ! Read and write header.
    READ(unit_input, '(A)') header
    WRITE(unit_output, '(A)') header
    ! Extract data from header.
    i_start = INDEX(header, 'h0=') + 3
    i_end = INDEX(header, 'p0=') - 1
    READ(header(i_start : i_end), *) h0
    i_start = INDEX(header, 'p0=') + 3
    i_end = INDEX(header, 'T0=') - 1
    READ(header(i_start : i_end), *) p0
    i_start = INDEX(header, 'T0=') + 3
    i_end = LEN(header)
    READ(header(i_start : i_end), *) T0
    ! MC continue.
END SUBROUTINE

END MODULE radiosondaggio_module
