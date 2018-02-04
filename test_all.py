#!/usr/bin/env python

import os
import subprocess as sbp

PATH = os.getenv('INFRASTRUCTURE_DIR')

TESTS_ALL = [os.path.join(PATH, 'functions', 'test_functions.py'), \
             os.path.join(PATH, 'general',   'test_general.py')]

n_failed = 0
for test in TESTS_ALL:
    try:
        sbp.check_call([test])
    except sbp.CalledProcessError as err:
        n_failed += abs(err.returncode)

exit(n_failed)
