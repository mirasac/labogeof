MAIN='sonico'
FLAG='-ffree-form -Wextra -Wall -Wconversion -fimplicit-none -pedantic -fcheck=all -fbacktrace'
gfortran ${FLAG} -c ../../utilities.f90
gfortran ${FLAG} -c sonico_module.f90
gfortran ${FLAG} -o ${MAIN}.exe utilities.o sonico_module.o ${MAIN}.f90
