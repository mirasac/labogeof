PROGRAM ex3
IMPLICIT NONE
REAL, PARAMETER :: G = 6.67430e-11
REAL, PARAMETER :: M_earth = 6.0e12
REAL, PARAMETER :: R_earth = 6.4e6
REAL, PARAMETER :: M_moon = 7.4e22
REAL, PARAMETER :: R_moon = 1.7e6
REAL, PARAMETER :: M_ceres = 8.7e20
REAL, PARAMETER :: R_ceres = 4.7e5
REAL, PARAMETER :: M_jupiter = 1.9e27
REAL, PARAMETER :: R_jupiter = 7.1e7
REAL :: v_earth, v_moon, v_ceres, v_jupiter
v_earth = SQRT(2.0 * G * M_earth / R_earth)
v_moon = SQRT(2.0 * G * M_moon / R_moon)
v_ceres = SQRT(2.0 * G * M_ceres / R_ceres)
v_jupiter = SQRT(2.0 * G * M_jupiter / R_jupiter)
WRITE (*,*) 'v_earth / (m / s) = ', v_earth
WRITE (*,*) 'v_moon / (m / s) = ', v_moon
WRITE (*,*) 'v_ceres / (m / s) = ', v_ceres
WRITE (*,*) 'v_jupiter / (m / s) = ', v_jupiter
END PROGRAM ex3
