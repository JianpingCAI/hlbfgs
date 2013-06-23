FC	= gfortran
FFLAGS	= -O2

AR	= ar
ARFLAGS	= cru
RANLIB	= ranlib

LIB	= liblbfgs.a

all : $(LIB)

lbfgs_um.tar.gz:
	@echo
	@echo "Note: see http://users.eecs.northwestern.edu/~nocedal/lbfgs.html for"
	@echo "      conditions of use set forth for the L-BFGS library you are about"
	@echo "      to download"
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
	rm -rf lbfgs_um lbfgs_um.tar.gz lbfgs.o

.PHONY: distclean
distclean: clean
	rm -rf liblbfgs.a
