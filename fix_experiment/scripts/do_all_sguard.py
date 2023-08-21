import sys
import os
import tempfile
import time
import csv
import subprocess
from datetime import datetime
import argparse
from os.path import expanduser

HOME = expanduser("~")
BENCH = os.path.join(HOME, 'benchmarks')
PROC = 0

WORK_SPACE = os.path.join(HOME, 'fix_experiment')
OUT_SPACE = os.path.join(HOME, 'fix_result')

KILL_TIMEOUT = '6000'

IO = 'io'
LS = 'ls'
RE = 're'
TX = 'tx'

def run(dataset, outdir_root,  meta_csv):
    cmd = ['python3', os.path.join(WORK_SPACE, 'scripts', 'run_sguard.py'),
           '--root_info', meta_csv,
           '--dataset', ','.join(dataset),
           '--outdir_root', outdir_root,
           '--kill_timeout', KILL_TIMEOUT,
           '--process', str(PROC)]

    print (' '.join(cmd))
    print(cmd)
    assert(os.system (' '.join(cmd)) == 0)


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument ('--dataset', type=str, help='{io,ls,re,tx}')
    parser.add_argument ('--process', type=int, default=40)
    args = parser.parse_args ()

    dataset = [IO, RE, TX] if args.dataset==None else [d for d in args.dataset.split(',')]
    global PROC
    PROC = args.process
    assert(PROC > 0)

    outdir_root = 'sguard_' + datetime.now().strftime('%m%d_%H%M')
    outdir_root = os.path.join(OUT_SPACE, outdir_root)

    if not (dataset == [IO, RE, TX]):
      if IO in dataset:
        outdir_root = outdir_root + '_io'
      if LS in dataset:
        outdir_root = outdir_root + '_ls'
      if RE in dataset:
        outdir_root = outdir_root + '_re'
      if TX in dataset:
        outdir_root = outdir_root + '_tx'

    os.mkdir(outdir_root)

    pgmdir_io = os.path.join(BENCH, 'cve')
    pgmdir_ls = os.path.join(BENCH, 'leaking_suicidal')
    pgmdir_re = os.path.join(BENCH, 'reentrancy')
    pgmdir_tx = os.path.join(BENCH, 'txorigin')

    outdir_io = os.path.join(outdir_root, 'io')
    outdir_ls = os.path.join(outdir_root, 'ls')
    outdir_re = os.path.join(outdir_root, 're')
    outdir_tx = os.path.join(outdir_root, 'tx')

    meta_csv = os.path.join(BENCH, 'mix-meta.csv')

    BEFORE = time.time ()
    run(dataset, outdir_root, meta_csv)
    AFTER = time.time()

    s = "Entire Time Took    : " + str(AFTER - BEFORE)
    print ('')
    print ('========================================')
    print (s)
    print ('========================================')
    print ('')

    with open (os.path.join (outdir_root, 'took.txt'), 'w') as fp:
        fp.write(s + '\n')

    # generate result summaries
    if IO in dataset:
        assert(os.system(f'python3 {WORK_SPACE}/scripts/mark_sguard.py --dataset io --directory {outdir_io}') == 0)
        print('')
    if RE in dataset:
        assert(os.system(f'python3 {WORK_SPACE}/scripts/mark_sguard.py --dataset re --directory {outdir_re}') == 0)
        print('')
    if TX in dataset:
        assert(os.system(f'python3 {WORK_SPACE}/scripts/mark_sguard.py --dataset tx --directory {outdir_tx}') == 0)
    if LS in dataset:
        assert(False)


if __name__ == "__main__":
    main ()
