FORT = gfortran -fcheck=all -std=f2008
CFLAGS = -c
LIBS = -llapack -lblas

all : test
	./test

test : main.f08 neuralnet.o optimizers.o real_precision.mod
	$(FORT) main.f08 neuralnet.o optimizers.o $(LIBS) -o test

neuralnet.o : neuralnet.f08 real_precision.mod optimizers.o
	$(FORT) $(CFLAGS) neuralnet.f08 -o neuralnet.o

optimizers.o : optimizers.f08 real_precision.mod
	$(FORT) $(CFLAGS) optimizers.f08 -o optimizers.o

real_precision.mod : real_precision.f08
	$(FORT) $(CFLAGS) real_precision.f08

clean :
	rm -f *.o *.mod test
