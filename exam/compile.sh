MAIN='main'
FLAG='-ffree-form -Wextra -Wall -Wconversion -fimplicit-none -pedantic -fcheck=all -fbacktrace'
gfortran ${FLAG} -c utilities.f90
gfortran ${FLAG} -c functions.f90
gfortran ${FLAG} -o ${MAIN}.exe utilities.o functions.o ${MAIN}.f90
