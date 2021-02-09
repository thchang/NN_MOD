FORT = gfortran -fcheck=all -std=f2008
CFLAGS = -c
LIBS = -llapack -lblas

all : test_reg_ls test_activation
	./test_activation
	./test_reg_ls

test_activation : test_activation.f90 neuralnet.o optimizers.o real_precision.mod
	$(FORT) test_activation.f90 neuralnet.o optimizers.o $(LIBS) -o test_activation

test_reg_ls : test_reg_ls.f90 neuralnet.o optimizers.o real_precision.mod
	$(FORT) test_reg_ls.f90 neuralnet.o optimizers.o $(LIBS) -o test_reg_ls

neuralnet.o : ../src/neuralnet.f90 real_precision.mod optimizers.o
	$(FORT) $(CFLAGS) ../src/neuralnet.f90 -o neuralnet.o

optimizers.o : ../src/optimizers.f90 real_precision.mod
	$(FORT) $(CFLAGS) ../src/optimizers.f90 -o optimizers.o

real_precision.mod : ../src/real_precision.f90
	$(FORT) $(CFLAGS) ../src/real_precision.f90

clean :
	rm -f *.o *.mod test_reg_ls test_activation
