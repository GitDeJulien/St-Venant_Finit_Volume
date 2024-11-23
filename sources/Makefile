FC = gfortran
FFLAGS = -g -ffpe-trap=invalid -ffpe-trap=zero -ffpe-trap=overflow -Wall -fcheck=all -fbounds-check 
# ajouter les flags
SRC = mod_precision.f90 mod_parser_toml.f90 main.f90
OBJ = ${SRC:.f90=.o}

%.o : %.f90
	$(FC) $(FFLAGS) -o $@ -c $<


run: $(OBJ)
	$(FC) $(FFLAGS) -o $@ $(OBJ)

clean :
	@rm -f *.mod *.o run

clear :
	@rm -f ../output/*/*.dat