MAIN='matrix_filter'
FLAG='-ffree-form -Wextra -Wall -Wconversion -fimplicit-none -pedantic -fcheck=all -fbacktrace'
gfortran ${FLAG} -c ../../utilities.f90
gfortran ${FLAG} -c filters.f90
gfortran ${FLAG} -o ${MAIN}.exe utilities.o filters.o ${MAIN}.f90
