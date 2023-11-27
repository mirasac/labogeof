MODULE radiosondaggio_module
USE utilities, ONLY : WK => SP
IMPLICIT NONE
PRIVATE
PUBLIC :: R, g
PUBLIC :: get_altitude
REAL(KIND=WK), PARAMETER :: R = 287.0_WK  ! J / (Kg K)
REAL(KIND=WK), PARAMETER :: g = 9.81_WK  ! m / s^2
CONTAINS

! Evaluate altitude in m of final point using barometric formula.
! IN arguments:
!   T
!     Scalar real, temperature in K.
!   p_1
!     Scalar real, pressure in Pa of initial point.
!   p_2
!     Scalar real, pressure in Pa of final point.
!   z_1
!     Scalar real, altitude in m of initial point.
REAL(KIND=WK) FUNCTION get_altitude(T, p_1, p_2, z_1) RESULT(z_2)
    ! Dummy arguments declaration.
    REAL(KIND=WK), INTENT(IN) :: T  ! K
    REAL(KIND=WK), INTENT(IN) :: p_1  ! Pa
    REAL(KIND=WK), INTENT(IN) :: p_2  ! Pa
    REAL(KIND=WK), INTENT(IN) :: z_1  ! m
    ! Compute result.
    z_2 = z_1 + R / g * T * LOG(p_1 / p_2)
END FUNCTION

END MODULE radiosondaggio_module