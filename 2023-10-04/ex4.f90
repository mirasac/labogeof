PROGRAM ex4
IMPLICIT NONE
REAL, PARAMETER :: g = 9.80665
REAL, PARAMETER :: PI = 3.14159265
REAL :: alpha, t, u, x, y, v_x, v_y, v, theta, y_max
WRITE (*,*) 'Insert initial angle between trajectory and horizontal surface alpha / deg: '
READ (*,*) alpha
WRITE (*,*) 'Insert instant t / s: '
READ (*,*) t
WRITE (*,*) 'Insert initial speed u / (m / s): '
READ (*,*) u
alpha = alpha / 180.0 * PI
x = u * t * COS(alpha)
y = u * t * SIN(alpha) - g * t**2 / 2.0
v_x = u * COS(alpha)
v_y = u * SIN(alpha) - g * t
v = SQRT(v_x**2 + v_y**2)
theta = ATAN2(v_y, v_x)
y_max = (u * SIN(alpha))**2 / (2.0 * g)
theta = theta / PI * 180.0
WRITE (*,*) 'x / m = ', x
WRITE (*,*) 'y / m = ', y
WRITE (*,*) 'v / (m / s) = ', v
WRITE (*,*) 'theta / deg = ', theta
WRITE (*,*) 'y_max / m = ', y_max
END PROGRAM ex4
