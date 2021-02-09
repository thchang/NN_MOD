FORT = gfortran -fcheck=all -std=f2008
CFLAGS = -c
LIBS = -llapack -lblas

all : test
	./test

test : main.f90 neuralnet.o optimizers.o real_precision.mod
	$(FORT) main.f90 neuralnet.o optimizers.o $(LIBS) -o test

neuralnet.o : neuralnet.f90 real_precision.mod optimizers.o
	$(FORT) $(CFLAGS) neuralnet.f90 -o neuralnet.o

optimizers.o : optimizers.f90 real_precision.mod
	$(FORT) $(CFLAGS) optimizers.f90 -o optimizers.o

real_precision.mod : real_precision.f90
	$(FORT) $(CFLAGS) real_precision.f90

clean :
	rm -f *.o *.mod test
