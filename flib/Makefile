# fortran compiler
FC	= gfortran
FFLAGS	= -O2

# library generation
AR	= ar
ARFLAGS	= cru
RANLIB	= ranlib

LIB	= liblbfgs.a

# default behavior is to:
#  (1) download the reference code
#  (2) extract and compile the routine of interest
#  (3) create the library
#  (4) clean up, but leave the tar-archived reference code
all: $(LIB) clean

# downloads the L-BFGS reference implementation
lbfgs_um.tar.gz:
	@echo
	@echo "Note: see http://users.eecs.northwestern.edu/~nocedal/lbfgs.html for"
	@echo "      conditions of use set forth for the L-BFGS code you are about"
	@echo "      to download."
	@echo
	@sleep 2
	@curl -O http://users.eecs.northwestern.edu/~nocedal/Software/lbfgs_um.tar.gz

lbfgs_um/lbfgs.f: lbfgs_um.tar.gz
	@tar xzf $<

lbfgs.o: lbfgs_um/lbfgs.f
	$(FC) $(FFLAGS) -c $< -o $@

$(LIB): lbfgs.o
	$(AR) $(ARFLAGS) $@ lbfgs.o
	$(RANLIB) $@

.PHONY: clean
clean:
	rm -rf lbfgs_um lbfgs.o

.PHONY: distclean
distclean: clean
	rm -rf liblbfgs.a lbfgs_um.tar.gz
