MAIN=main

all:
	gfortran -ffree-form -Wextra -Wall –Wconversion –fimplicit-none -pedantic –fcheck=all -fbacktrace –o ${MAIN}.exe ${MAIN}.f90
