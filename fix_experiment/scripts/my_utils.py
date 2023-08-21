import csv
import os
import sys
import json
from os.path import expanduser
from datetime import timedelta
import subprocess

solc_lst = ["0.4.16", "0.4.17", "0.4.18", "0.4.19", "0.4.20", "0.4.21", "0.4.23", "0.4.24", "0.4.25", "0.4.26",
            "0.5.0", "0.5.1", "0.5.2", "0.5.3", "0.5.4", "0.5.6", "0.5.7", "0.5.8", "0.5.9", "0.5.10",
            "0.5.11", "0.5.12", "0.5.13", "0.5.14", "0.5.15", "0.5.16", "0.5.17",
            "0.6.0", "0.6.1", "0.6.2", "0.6.3", "0.6.4", "0.6.5", "0.6.6", "0.6.7", "0.6.8", "0.6.9", "0.6.10", "0.6.11", "0.6.12",
            "0.7.0", "0.7.1", "0.7.2", "0.7.3", "0.7.4", "0.7.5", "0.7.6",
            "0.8.0", "0.8.1", "0.8.2", "0.8.3", "0.8.4", "0.8.5", "0.8.6", "0.8.7", "0.8.8", "0.8.9", "0.8.10"]

IO = 'io'
RE = 're'
TX = 'tx'
LS = 'ls'

EL = 'el'
SU = 'su'

HOME = expanduser("~")
BENCH = os.path.join(HOME, 'benchmarks')
META = os.path.join(BENCH, 'mix-meta.csv')

def get_main_name(fid):
    fp = open (META, 'r')
    rows = list(csv.DictReader(fp))
    fp.close()

    rows = list(filter(lambda r: r['id'] == fid, rows))
    assert (len(rows) == 1)
    row = rows[0]
    main_name = row['main_name']

    return main_name


def get_lineno(fid):
    fp = open (META, 'r')
    rows = list(csv.DictReader(fp))
    fp.close()

    rows = list(filter(lambda r: r['id'] == fid, rows))
    assert (len(rows) == 1)
    row = rows[0]
    main_name = row['loc']

    return main_name


def check_bytesize(full_fname, solv, main_name):

    cmd = ['solc_' + solv, full_fname, '--bin']
    out = subprocess.Popen (cmd, stdout= subprocess.PIPE, stderr=subprocess.STDOUT)
    cmdout = out.stdout.read()
    cmdout = cmdout.decode ("utf-8")


    bar7 = "======="
    idx = cmdout.index (bar7) # Find the initial signal
    cmdout = cmdout[idx:] # preprocessing for handling warning messages
    lst = cmdout.split("\n\n")
    # print(lst)
    # assert(False)
    start = bar7 + " " + full_fname + ":" + main_name + " " + bar7
    lst = list (map (lambda x : x[1:] if x.startswith('\n=') else x, lst)) # may start with '\n=' due to prepositive abstract contracts
    lst = list (filter (lambda x : x.startswith(start), lst))
    assert (len (lst) == 1)
    target = lst[0]
    target = target[:-1] if target[-1] == '\n' else target # remove '\n' at the end

    bin_just_before = target.rfind('\n')
    final = target[bin_just_before+1:]
    size = len(final)
    assert(size > 0)

    return len(final)


def from_sec_to_hm (sec):
    x = str(timedelta(seconds=sec)).split(':')
    assert(len(x) == 3)
    m = str(int(x[1]) + 1).zfill(2) if int(x[2]) >= 30 else x[1]
    hm = x[0] + 'h ' + m + 'm'
    return hm


def check_time(directory, fname):
    with open(os.path.join(directory,'time.csv'), 'r') as fp:
        rows = list(csv.DictReader(fp))
        rows = list(filter (lambda row: row['file'] == fname, rows))
        assert (len(rows) == 1)
    row = rows[0]
    assert (row['timeout'] == '' or row['timeout'] == 'O')
    if row['timeout']=='O':
        return 'TO'
    else:
        return row['time']


def is_compilable(solv, full_fname):
    ret = os.system(f'solc_{solv} {full_fname} 2>/dev/null')
    compiled = (ret == 0)
    return compiled


def get_solc_version (row):
    for solv in list(reversed(solc_lst)): # To avoid mismatch, iterate in reversed order (e.g., mismatch 0.6.11 to 0.6.1 )
        if solv in row['compiler_version']:
            return solv
    return "0.4.25" # NotImplemented

def get_solc_version_fid (fid):
    rows = []
    with open (META, 'r') as fp:
        rows = list(csv.DictReader(fp))
    rows = list(filter(lambda r: r['id'] == fid, rows))
    assert (len(rows) == 1)

    row = rows[0]
    solv = get_solc_version(row)
    return solv

# Get annotated vulnerability lines from source code
# NOTE: do not obtain answer lines from answer sheet,
#       because line numbers in patched contracts may have been chagned.
def get_ans_lines(dirname, bugtype):
    if bugtype == IO:
        lines = []
        with open (dirname, 'r') as fp: lines = fp.readlines()
        assert(len(lines) > 0)

        ans_lines = set()
        for (i,line) in enumerate(lines):
          if '<IO_VUL>' in line:
            ans_lines.add(i+1)
        ans = set(list(map(lambda l: ('IO', l), ans_lines)))
        return ans

    elif bugtype == RE:
        lines = []
        with open (dirname, 'r') as fp: lines = fp.readlines()
        assert(len(lines) > 0)

        ans_lines = set()
        for (i,line) in enumerate(lines):
          # if ('<RE_VUL>' in line) and ('.call' in line or ('.transfer' in line)):
          if ('<RE_VUL>' in line):
            ans_lines.add(i+1)
        ans = set(list(map(lambda l: ('RE', l), ans_lines))) # to address labels related to state-change (i.e., uniformly RE rather than RE_EL)
        return ans

    elif bugtype == TX:
        lines = []
        with open (dirname, 'r') as fp: lines = fp.readlines()
        assert(len(lines) > 0)

        ans_lines = set()
        for (i,line) in enumerate(lines):
          if '<TX_VUL>' in line:
            ans_lines.add(i+1)
        ans = set(list(map(lambda l: ('TX_ORG', l), ans_lines)))
        return ans

    elif bugtype == EL:
        lines = []
        with open (dirname, 'r') as fp: lines = fp.readlines()
        assert(len(lines) > 0)

        ans_lines = set()
        for (i,line) in enumerate(lines):
          if '<LEAKING_VUL>' in line:
            ans_lines.add(i+1)
        ans = set(list(map(lambda l: ('ETH_LEAK', l), ans_lines)))
        return ans

    elif bugtype == SU:
        lines = []
        with open (dirname, 'r') as fp: lines = fp.readlines()
        assert(len(lines) > 0)

        ans_lines = set()
        for (i,line) in enumerate(lines):
          if '<SUICIDAL_VUL>' in line:
            ans_lines.add(i+1)
        ans = set(list(map(lambda l: ('KA', l), ans_lines)))
        return ans

    else:
        assert (False)
