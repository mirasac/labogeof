MODULE functions
USE utilities, ONLY : WK => SP
IMPLICIT NONE
PRIVATE
PUBLIC :: get_vapor_pressure_ratio, get_critical_relative_humidity
CONTAINS

! Evaluate ratio between vapor pressure on a curved surface and vapor pressure on a planar surface of a gas in thermal equilibrium with the surface using Kelvin's formula.
! This function acts on arrays element-wise.
! IN arguments:
!   radius
!     Real, radius in m of the curved surface, positive for convex surface, negative for concave surface.
!   sigma
!     Real, surface tension in N m-1.
!   V_m
!     Real, molar volume in m3 mol-1 of the gas.
!   R
!     Real, universal gas constant in J mol-1 K-1.
!   T
!     Real, temperature in K of gas and surface.
REAL(KIND=WK) ELEMENTAL FUNCTION get_vapor_pressure_ratio(radius, sigma, V_m, R, T) RESULT(ratio)
    ! Dummy arguments declaration.
    REAL(KIND=WK), INTENT(IN) :: radius  ! m
    REAL(KIND=WK), INTENT(IN) :: sigma  ! N m-1
    REAL(KIND=WK), INTENT(IN) :: V_m  ! m3 mol-1
    REAL(KIND=WK), INTENT(IN) :: R  ! J mol-1 K-1
    REAL(KIND=WK), INTENT(IN) :: T  ! K
    ! Compute result.
    ratio = EXP(2.0_WK * sigma * V_m / (radius * R * T))
END FUNCTION

! Evaluate Critical Relative Humidity (CRH) needed to start condensation inside micropores. Kelvin's formula is used for concave surfaces.
! This function acts on arrays element-wise.
! IN arguments:
!   radius
!     Real, radius in m of the concave surface, must be negative.
!   sigma
!     Real, surface tension in N m-1.
!   V_m
!     Real, molar volume in m3 mol-1 of the gas.
!   R
!     Real, universal gas constant in J mol-1 K-1.
!   T
!     Real, temperature in K of gas and surface.
REAL(KIND=WK) ELEMENTAL FUNCTION get_critical_relative_humidity(radius, sigma, V_m, R, T) RESULT(CRH)
    ! Dummy arguments declaration.
    REAL(KIND=WK), INTENT(IN) :: radius  ! m
    REAL(KIND=WK), INTENT(IN) :: sigma  ! N m-1
    REAL(KIND=WK), INTENT(IN) :: V_m  ! m3 mol-1
    REAL(KIND=WK), INTENT(IN) :: R  ! J mol-1 K-1
    REAL(KIND=WK), INTENT(IN) :: T  ! K
    ! Compute result.
    CRH = 100.0_WK * get_vapor_pressure_ratio(radius, sigma, V_m, R, T)
END FUNCTION

END MODULE functions
