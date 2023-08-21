#!/bin/bash

docker run --rm -it -v `pwd`/fix_result:/home/opam/fix_result smartfix python3 fix_experiment/scripts/draw_cactus_plot.py --base $1/all_summary.csv --on $2/all_summary.csv --final $3/all_summary.csv
