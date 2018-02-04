#!/bin/sh

MAKEDIR = $(INFRASTRUCTURE_DIR)/..

# Build documentation webpage content
docs:
	doxygen $(MAKEDIR)/documentation/Doxyfile

clean:
	/bin/rm -rf $(MAKEDIR)/html/*

