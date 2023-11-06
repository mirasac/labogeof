MAIN='Exercise_5'
FLAG='-ffree-form -Wextra -Wall -Wconversion -fimplicit-none -pedantic -fcheck=all -fbacktrace'
gfortran ${FLAG} -o ${MAIN}.exe ${MAIN}.f90
