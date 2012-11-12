#for debugging:
OPT = -Wall -Wextra -g -O0 -ffpe-trap=invalid,zero,overflow

F90=gfortran

.SUFFIXES: .a .o .mod .f90 .F90 .F .f

SRC=precision.f90 constants.f90 utils.f90 operators.f90 boundary.f90

LIBOBJ := $(patsubst %.f,%.o,$(filter %.f,$(SRC))) \
          $(patsubst %.F,%.o,$(filter %.F,$(SRC))) \
          $(patsubst %.f90,%.o,$(filter %.f90,$(SRC))) \
          $(patsubst %.F90,%.o,$(filter %.F90,$(SRC))) \
          $(patsubst %.c,%.o,$(filter %.c,$(SRC)))

%.o : %.mod

.f90.o:
	$(F90) $(F90FLAGS) -c $<

all: model

model:  atm_model.f90 $(LIBOBJ)
	$(F90) $(F90FLAGS) -o $@ $^ $(LIBS)

clean: 
	$(RM) *.o rm *.mod

realclean: clean
	$(RM) *.o *.mod *.lib *~