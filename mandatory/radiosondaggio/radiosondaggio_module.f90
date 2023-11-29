MODULE radiosondaggio_module
USE utilities, ONLY : WK => SP, character2real
IMPLICIT NONE
PRIVATE
PUBLIC :: R, g, zero_celsius
PUBLIC :: get_altitude, get_pressure, get_temperature, add_altitude
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

! Evaluate pressure in mbar of final point using barometric formula.
! IN arguments:
!   T
!     Scalar real, temperature in °C.
!   z_1
!     Scalar real, altitude in m of initial point.
!   z_2
!     Scalar real, altitude in m of final point.
!   p_1
!     Scalar real, pressure in mbar of initial point.
REAL(KIND=WK) FUNCTION get_pressure(T, z_1, z_2, p_1) RESULT(p_2)
    ! Dummy arguments declaration.
    REAL(KIND=WK), INTENT(IN) :: T  ! °C
    REAL(KIND=WK), INTENT(IN) :: z_1  ! m
    REAL(KIND=WK), INTENT(IN) :: z_2  ! m
    REAL(KIND=WK), INTENT(IN) :: p_1  ! mbar
    ! Compute result.
    p_2 = p_1 * EXP(- (z_2 - z_1) * g / (R * (T + zero_celsius)))
END FUNCTION

! Evaluate temperature in °C using barometric formula.
! IN arguments:
!   p_1
!     Scalar real, pressure in mbar of initial point.
!   p_2
!     Scalar real, pressure in mbar of final point.
!   z_1
!     Scalar real, altitude in m of initial point.
!   z_2
!     Scalar real, altitude in m of final point.
REAL(KIND=WK) FUNCTION get_temperature(p_1, p_2, z_1, z_2) RESULT(T)
    ! Dummy arguments declaration.
    REAL(KIND=WK), INTENT(IN) :: p_1  ! mbar
    REAL(KIND=WK), INTENT(IN) :: p_2  ! mbar
    REAL(KIND=WK), INTENT(IN) :: z_1  ! m
    REAL(KIND=WK), INTENT(IN) :: z_2  ! m
    ! Compute result.
    T = (z_2 - z_1) * g / (R * LOG(p_1 / p_2)) - zero_celsius
END FUNCTION

! Add altitude values to and using data from the specified unit.
! IN arguments:
!   unit_input
!     Scalar integer.
!   unit_output
!     Scalar integer.
! OUT optional arguments:
!   p
!     Real rank 1 array, input pressure data are stored in this array,
!     its size must be equal or greater than the number of input
!     pressure data, by default no array is created.
!   T
!     Real rank 1 array, input temperature data are stored in this
!     array, its size must be equal or greater than the number of input
!     pressure data, by default no array is created.
!   z
!     Real rank 1 array, altitude values are stored in this array, its
!     size must be equal or greater than the number of input pressure
!     data, by default no array is created.
SUBROUTINE add_altitude(unit_input, unit_output, p, T, z)
    ! Dummy arguments declaration.
    INTEGER, INTENT(IN) :: unit_input
    INTEGER, INTENT(IN) :: unit_output
    REAL(KIND=WK), INTENT(OUT), OPTIONAL :: p(:)  ! mbar
    REAL(KIND=WK), INTENT(OUT), OPTIONAL :: T(:)  ! °C
    REAL(KIND=WK), INTENT(OUT), OPTIONAL :: z(:)  ! m
    ! Variables declaration.
    CHARACTER(LEN=512) :: header
    REAL(KIND=WK) :: h0, z_1, z_2  ! m
    REAL(KIND=WK) :: p0, p_1, p_2  ! mbar
    REAL(KIND=WK) :: T0, T_1, T_2  ! °C
    INTEGER :: iostat_input, i
    ! Read and write header.
    READ(unit_input, '(A)') header
    WRITE(unit_output, '(A)') TRIM(header)
    ! Extract data from header.
    h0 = character2real(header, sub_start='h0=', sub_end='p0=')
    p0 = character2real(header, sub_start='p0=', sub_end='T0=')
    T0 = character2real(header, sub_start='T0=')
    ! Write output.
    p_1 = p0
    T_1 = T0
    z_1 = h0
    i = 1
    DO
        READ(unit_input, *, IOSTAT=iostat_input) p_2, T_2
        IF (iostat_input < 0) EXIT
        z_2 = get_altitude((T_1 + T_2) / 2.0_WK, p_1, p_2, z_1)
        WRITE(unit_output, *) p_2, T_2, z_2
        IF (PRESENT(p)) THEN
            p(i) = p_2
        END IF
        IF (PRESENT(T)) THEN
            T(i) = T_2
        END IF
        IF (PRESENT(z)) THEN
            z(i) = z_2
        END IF
        p_1 = p_2
        T_1 = T_2
        z_1 = z_2
        i = i + 1
    END DO
END SUBROUTINE

END MODULE radiosondaggio_module
