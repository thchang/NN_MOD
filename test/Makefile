FORT = gfortran -fcheck=all -std=f2008
CFLAGS = -c
LIBS = -llapack -lblas

all : test_reg_ls test_activation
	./test_activation
	./test_reg_ls

test_activation : test_activation.f08 neuralnet.o optimizers.o real_precision.mod
	$(FORT) test_activation.f08 neuralnet.o optimizers.o $(LIBS) -o test_activation

test_reg_ls : test_reg_ls.f08 neuralnet.o optimizers.o real_precision.mod
	$(FORT) test_reg_ls.f08 neuralnet.o optimizers.o $(LIBS) -o test_reg_ls

neuralnet.o : ../src/neuralnet.f08 real_precision.mod optimizers.o
	$(FORT) $(CFLAGS) ../src/neuralnet.f08 -o neuralnet.o

optimizers.o : ../src/optimizers.f08 real_precision.mod
	$(FORT) $(CFLAGS) ../src/optimizers.f08 -o optimizers.o

real_precision.mod : ../src/real_precision.f08
	$(FORT) $(CFLAGS) ../src/real_precision.f08

clean :
	rm -f *.o *.mod test_reg_ls test_activation
