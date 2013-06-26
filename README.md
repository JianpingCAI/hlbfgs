# Haskell interface for the reference implementation of L-BFGS

A high-level interface to the L-BFGS reference implementation (Fortran), which
is available from [here](http://users.eecs.northwestern.edu/~nocedal/lbfgs.html)
and is distributed under a BSD license.

# compilation

The package expects the L-BFGS reference code to be available as a library
names, surprisingly enough, `lbfgs`. In the `flib/` subdirectory, there is a
`Makefile` provided to do just that (download the reference implementation,
compile it, and package it appropriately). If you this route, you will still
need to supply the requisite library paths to cabal - possibly including that
for `gfortran`.

For example, after compiling the `lbfgs` library in `flib/` the homebrew
version of `gfortran` on OS X, this process will likely look something like:

	> cabal configure \
		--extra-lib-dirs=$(pwd)/flib \
		--extra-lib-dirs=/usr/local/opt/gfortran/gfortran/lib \
		--enable-tests
	Resolving dependencies...
	Configuring hlbfgs-0.0.1.0...
	> cabal build
	Building hlbfgs-0.0.1.0...
	Preprocessing library hlbfgs-0.0.1.0...
	[1 of 1] Compiling Math.HLBFGS      ( src/Math/HLBFGS.hs, dist/build/Math/HLBFGS.o )
	[1 of 1] Compiling Math.HLBFGS      ( src/Math/HLBFGS.hs, dist/build/Math/HLBFGS.p_o )
	In-place registering hlbfgs-0.0.1.0...
	Preprocessing test suite 'tests' for hlbfgs-0.0.1.0...
	[1 of 1] Compiling Main             ( tests/Test.hs, dist/build/tests/tests-tmp/Main.o )
	Linking dist/build/tests/tests ...
	> cabal test
	Running 1 test suites...
	Test suite tests: RUNNING...
	Test suite tests: PASS
	Test suite logged to: dist/test/hlbfgs-0.0.1.0-tests.log
	1 of 1 test suites (1 of 1 test cases) passed.
	>
