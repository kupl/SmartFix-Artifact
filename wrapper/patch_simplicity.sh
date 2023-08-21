#!/bin/bash

docker run --rm -it -v `pwd`/fix_result:/home/opam/fix_result smartfix python3 fix_experiment/scripts/compare_simplicity.py --smartfix $1 --sguard $2
