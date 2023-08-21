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
VERISMART = os.path.join(HOME, 'SmartFix')
BENCH = os.path.join(HOME, 'benchmarks')
PROC = 0

WORK_SPACE = os.path.join(HOME, 'fix_experiment')
OUT_SPACE = os.path.join(HOME, 'fix_result')

KILL_TIMEOUT = '6000'
REPAIR_LOOP_TIMEOUT = '5400'
REPAIR_TOOL_TIMEOUT = '150'
Z3_TIMEOUT = '20000'

IO = 'io'
LS = 'ls'
RE = 're'
TX = 'tx'

def run(dataset, outdir_root, meta_csv, include, no_learn, no_offline, no_diff, pre_ksrc):
    cmd = ['python3', os.path.join(WORK_SPACE, 'scripts', 'run_smartfix.py'),
           '--root_info', meta_csv,
           '--dataset', ','.join(dataset),
           '--outdir_root', outdir_root,
           '--repair_loop_timeout', REPAIR_LOOP_TIMEOUT,
           '--repair_tool_timeout', REPAIR_TOOL_TIMEOUT,
           '--kill_timeout', KILL_TIMEOUT,
           '--z3_timeout', Z3_TIMEOUT,
           '--process', str(PROC)]

    if include != None:
        cmd += ['--include', include]

    if no_learn:
        cmd += ['--no_learn']

    if no_offline:
        cmd += ['--no_offline']

    if no_diff:
        cmd += ['--no_diff']

    if pre_ksrc != None:
        cmd += ['--pre_ksrc', pre_ksrc]

    print(' '.join(cmd))
    assert(os.system (' '.join(cmd)) == 0)


def main ():
    parser = argparse.ArgumentParser()
    parser.add_argument ('--dataset', type=str, help='{io,ls,re,tx}')
    parser.add_argument ('--include', type=str, help='program lists to be included')
    parser.add_argument ('--no_learn', default=False, action='store_true', help='search in increasing order')
    parser.add_argument ('--no_offline', default=False, action='store_true', help='no offline learning if specified')
    parser.add_argument ('--no_diff', default=False, action='store_true', help='no differential verification if specified')
    parser.add_argument ('--pre_ksrc', type=str, help='pre-specified knowledge source (debugging purpose')
    parser.add_argument ('--meta_csv', type=str, help='{mix-meta.csv, ...}. default: mix-meta.csv')
    parser.add_argument ('--process', type=int, default=40)

    args = parser.parse_args ()

    os.chdir(VERISMART)

    global PROC
    PROC = args.process
    assert(PROC > 0)

    dataset = [IO, LS, RE, TX] if args.dataset==None else [d for d in args.dataset.split(',')]
    include = args.include
    no_learn = args.no_learn
    no_offline = args.no_offline
    no_diff = args.no_diff
    pre_ksrc = args.pre_ksrc

    assert (not (no_learn==True and no_offline==True))

    meta_csv = args.meta_csv
    if meta_csv == None or meta_csv == 'mix-meta.csv':
        meta_csv = os.path.join(BENCH, 'mix-meta.csv')
    # else:
    #    assert(False)

    # setup directories
    # outdir_root = 'all_' + datetime.now().strftime('%m%d_%H%M')
    outdir_root = datetime.now().strftime('%m%d_%H%M')
    if no_learn and no_diff:
        assert (no_offline == False)
        outdir_root = 'basic_' + outdir_root
    elif (not no_learn) and no_offline and (not no_diff):
        outdir_root = 'on_' + outdir_root
    elif (not no_learn) and (not no_offline) and (not no_diff):
        outdir_root = 'smartfix_' + outdir_root
    else:
        assert(False)

    # if no_learn:
    #   outdir_root = outdir_root + '_inc'
    # if no_offline:
    #   outdir_root = outdir_root + '_no_offline'
    # if no_diff:
    #   outdir_root = outdir_root + '_no_diff'

    if not (dataset == [IO, LS, RE, TX]):
      if IO in dataset:
        outdir_root = outdir_root + '_io'
      if LS in dataset:
        outdir_root = outdir_root + '_ls'
      if RE in dataset:
        outdir_root = outdir_root + '_re'
      if TX in dataset:
        outdir_root = outdir_root + '_tx'

    outdir_root = os.path.join(OUT_SPACE, outdir_root)
    os.mkdir (outdir_root)

    pgmdir_io = os.path.join(BENCH, 'cve')
    pgmdir_ls = os.path.join(BENCH, 'leaking_suicidal')
    pgmdir_re = os.path.join(BENCH, 'reentrancy')
    pgmdir_tx = os.path.join(BENCH, 'txorigin')

    outdir_io = os.path.join(outdir_root, 'io')
    outdir_ls = os.path.join(outdir_root, 'ls')
    outdir_re = os.path.join(outdir_root, 're')
    outdir_tx = os.path.join(outdir_root, 'tx')

    # run experiments
    BEFORE = time.time ()
    run(dataset, outdir_root, meta_csv, include, no_learn, no_offline, no_diff, pre_ksrc)
    AFTER = time.time ()

    time_str = "Entire Time Took   : " + str(AFTER - BEFORE)
    print ('')
    print ('========================================')
    print (time_str)
    print ('========================================')
    print ('')
   
    with open (os.path.join (outdir_root, 'took.txt'), 'w') as fp:
        fp.write(time_str + '\n')
        fp.write('* meta csv file : ' + meta_csv + '\n')

    # generate result summaries
    dataset_s = ','.join(dataset)
    assert(os.system(f'python3 {WORK_SPACE}/scripts/mark_smartfix.py --dataset {dataset_s} --directory {outdir_root}') == 0)
    print('')

if __name__ == '__main__':
    main ()
