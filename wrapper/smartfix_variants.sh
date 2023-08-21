#!/bin/bash

cores=$1

# baseline
docker run --rm -it -v `pwd`/fix_result:/home/opam/fix_result smartfix python3 fix_experiment/scripts/do_all_smartfix.py --no_learn --no_diff --process $cores
sleep 60

# online (No-offline)
docker run --rm -it -v `pwd`/fix_result:/home/opam/fix_result smartfix python3 fix_experiment/scripts/do_all_smartfix.py --no_offline --process $cores
