#!/usr/bin/env python

import os
import subprocess as sbp

PATH = os.getenv('INFRASTRUCTURE_DIR')

TESTS_ALL = [os.path.join(PATH, 'general', 'test_modulo_mod.x')]

n_failed = 0
for test in TESTS_ALL:
    print('-' * 80)
    print('Test - {}'.format(test))
    print('-' * 80)
    try:
        sbp.check_call([test])
    except sbp.CalledProcessError as err:
        n_failed += abs(err.returncode)
    print('-' * 80)
    print

exit(n_failed)
