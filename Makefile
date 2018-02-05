#!/bin/sh

MAKEDIR = $(INFRASTRUCTURE_DIR)/..

# Build all tests
all:
	rake -f $(MAKEDIR)/src/functions/tests/rakefile_linear_fcn_class all 
	make -f $(MAKEDIR)/src/functions/tests/make_testLinearFunction all
	rake -f $(MAKEDIR)/src/general/tests/rakefile_modulo_mod all

test:
	./test_all.py

# Do not include testing framework code in coverage analysis
lcov:
	/bin/rm -f $(shell find $(MAKEDIR) -name 'test*.gcno')
	/bin/rm -f $(shell find $(MAKEDIR) -name 'test*.gcda')
	/bin/rm -f $(shell find $(MAKEDIR) -name 'fruit*.gcno')
	/bin/rm -f $(shell find $(MAKEDIR) -name 'fruit*.gcda')
	lcov -o $(MAKEDIR)/Personal_lcov.info -c -d .
	genhtml -o lcov $(MAKEDIR)/Personal_lcov.info

# Build documentation webpage content
docs:
	doxygen $(MAKEDIR)/documentation/Doxyfile

clean:
	/bin/rm -rf $(MAKEDIR)/html/*
	/bin/rm -rf $(MAKEDIR)/lcov
	/bin/rm -f  $(MAKEDIR)/Personal_lcov.info

	rake -f $(MAKEDIR)/src/functions/tests/rakefile_linear_fcn_class clean 
	make -f $(MAKEDIR)/src/functions/tests/make_testLinearFunction clean 
	rake -f $(MAKEDIR)/src/general/tests/rakefile_modulo_mod clean 

