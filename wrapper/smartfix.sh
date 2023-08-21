#!/bin/bash

# on + off + diff (Final)
docker run --rm -it -v `pwd`/fix_result:/home/opam/fix_result smartfix python3 fix_experiment/scripts/do_all_smartfix.py --process $1
