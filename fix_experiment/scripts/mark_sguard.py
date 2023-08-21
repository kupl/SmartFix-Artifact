import sys
import os
import tempfile
import time
import csv
import subprocess
from datetime import datetime
import argparse
from multiprocessing import Pool, Lock, Value
from os.path import expanduser
import run_sguard
import my_utils

HOME = expanduser("~")
BENCH = os.path.join(HOME, 'benchmarks')

IO = 'io'
RE = 're'
TX = 'tx'
LS = 'ls'

fieldnames = ["file", "solv", "time", "compiled",
              "failmsg",
              "bug_run", "bug_gen",
              "dz_fix", # "O" if bound check for DZ is inserted
              "bound_check", "non_reentrant", "bytesize"]


def empty_fieldnames():
    return dict.fromkeys(fieldnames, '')


def check_io(full_fname, dataset):
    lines = []
    with open(full_fname, 'r') as fp: lines = fp.readlines()
    assert(len(lines) > 0)

    labels = my_utils.get_ans_lines (full_fname, dataset)
    run,gen = 0,0
    run = len(labels)
    for (kind,line) in labels:
      assert(kind == "IO")
      idx = line - 1
      patched = "add_uint256" in lines[idx] or "sub_uint256" in lines[idx] or "mul_uint256" in lines[idx]
      if patched:
        gen = gen + 1
        
    return (run,gen)


def check_re(full_fname, dataset):
    src = ""
    with open(full_fname, 'r') as fp: src = fp.read()
    assert(len(src) > 0)

    nr_def = src.count("modifier nonReentrant_()")
    nr = src.count("nonReentrant_") - nr_def

    labels = my_utils.get_ans_lines (full_fname, dataset)
    run,gen = 0,0
    run = len(labels)
    gen = run if nr > 0 else 0 # if nr exists, favorably assume all patches are generated.

    return (run,gen)


def check_tx(full_fname, dataset):
    basename = os.path.basename(full_fname).replace('fixed_', '')
    org_full_fname = os.path.join(BENCH,'txorigin',basename)

    lines_fix = []
    with open(full_fname, 'r') as fp: lines_fix = fp.readlines()
    assert(len(lines_fix) > 0)

    lines_org = []
    with open(org_full_fname, 'r') as fp: lines_org = fp.readlines()
    assert(len(lines_org) > 0)

    labels_org = my_utils.get_ans_lines (org_full_fname, dataset)
    labels_fix = my_utils.get_ans_lines (full_fname, dataset)
    assert(len(labels_org) == len(labels_fix))

    run,gen = 0,0
    run = len(labels_fix)
    for ((k1,l_org), (k2,l_fix)) in zip(labels_org, labels_fix):
      s_org = lines_org[l_org - 1]
      s_fix = lines_fix[l_fix - 1]
      gen = gen + 1 if not(s_org == s_fix) else gen

    return (run,gen)


def check_labels (full_fname, dataset):
    if dataset == IO:
        run,gen = check_io(full_fname, dataset)
        return (run,gen)
    elif dataset == RE:
        run,gen = check_re(full_fname, dataset)
        return (run,gen)
    elif dataset == TX:
        run,gen = check_tx(full_fname, dataset)
        return (run,gen)
    else:
        assert (False)

def check_dz_fix (full_fname):
    src = ""
    with open (full_fname, 'r') as fp: src = fp.read()
    assert(len(src) > 0)

    div_def = src.count("function div_uint256(uint256 a, uint256 b)")
    div = src.count("div_uint256") - div_def

    dz_fix = "O" if div > 0 else ""
    return dz_fix

def check_bc_nr(full_fname):
    src = ""
    with open (full_fname, 'r') as fp: src = fp.read()
    assert(len(src) > 0)

    nr_def = src.count("modifier nonReentrant_()")
    add_def = src.count("function add_uint256(uint256 a, uint256 b)")
    sub_def = src.count("function sub_uint256(uint256 a, uint256 b)")
    mul_def = src.count("function mul_uint256(uint256 a, uint256 b)")

    add = src.count("add_uint256") - add_def
    sub = src.count("sub_uint256") - sub_def
    mul = src.count("mul_uint256") - mul_def

    nr = src.count("nonReentrant_") - nr_def
    bc = add + sub + mul

    
    fp = open(full_fname, 'r')
    lines = fp.readlines()
    fp.close()

    # Check if there are unknown BC patterns
    for line in lines:
        if (not "add_uint256" in line) and ("add_uint" in line):
            print('* Warning!! Unknown pattern appeared')
            print (full_fname + ':' + line)
            assert(False)
        if (not "sub_uint256" in line) and ("sub_uint" in line):
            print('* Warning!! Unknown pattern appeared')
            print (full_fname + ':' + line)
            assert(False)
        if (not "mul_uint256" in line) and ("mul_uint" in line):
            print('* Warning!! Unknown pattern appeared')
            print (full_fname + ':' + line)
            assert(False)

    return (bc,nr)


def mark(directory, dataset, rows):
    fixfiledir = os.path.join(directory, 'fixed')
    res = []
    for row in rows:
      fname = 'fixed_' + row['file']
      full_fname = os.path.join(fixfiledir, fname)
      time = my_utils.check_time (os.path.dirname(directory), row['file'])

      timeout = True if time == 'TO' else False
      no_patch_file_exist = not (os.path.exists(full_fname))
      compiled1 = my_utils.is_compilable(row['solv'], full_fname)
      compiled2 = my_utils.is_compilable('0.4.25', full_fname) # 'constructor' keyword is not compiled with old solc, e.g., 0.4.16
      compiled = compiled1 or compiled2

      assert(type(timeout)==bool and type(no_patch_file_exist)==bool and type(compiled)==bool)
      fail = timeout or no_patch_file_exist or (not compiled)

      if fail:
        msg = ""
        msg = msg + "TO/" if timeout else msg
        msg = msg + "no_file/" if no_patch_file_exist else msg
        msg = msg + "uncompiled" if not compiled else msg

        res.append({
          'file': row['file'],
          'solv': row['solv'],
          'time': time,
          'compiled': compiled,
          'failmsg': msg,
          'bug_run': 'n/a',
          'bug_gen': 'n/a',
          'dz_fix': 'n/a',
          'bound_check': 'n/a',
          'non_reentrant': 'n/a',
          'bytesize': 'n/a'
        })

      else:
        assert(compiled)
        assert(not(time == 'TO'))

        bug_run, bug_gen = check_labels(full_fname, dataset)
        dz_fix = check_dz_fix(full_fname)
        (bc, nr) = check_bc_nr(full_fname)

        solv = row['solv'] if compiled1 else '0.4.25'
        main_name = my_utils.get_main_name(row['file'].replace('.sol', ''))
        bytesize = my_utils.check_bytesize(full_fname, solv, main_name)

        res.append({
          'file': row['file'],
          'solv': row['solv'],
          'time': time,
          'compiled': compiled,
          'failmsg': '',
          'bug_run': bug_run, # mark_io(compiled, dataset, full_fname),
          'bug_gen': bug_gen,
          'dz_fix': dz_fix,
          'bound_check': bc,
          'non_reentrant': nr,
          'bytesize': bytesize
        })

    return res


def make_table(rows):
    contract = len(rows)
    timeout = len(list(filter(lambda r: r['time']=='TO', rows)))
    fail = len(list(filter(lambda r: r['failmsg']!='', rows)))
    bug_run = sum(list(map(lambda r: r['bug_run'] if type(r['bug_run'])==int else 0, rows)))
    bug_gen = sum(list(map(lambda r: r['bug_gen'] if type(r['bug_gen'])==int else 0, rows)))
    bound_check = sum(list(map(lambda r: r['bound_check'] if type(r['bound_check'])==int else 0, rows)))
    non_reentrant = sum(list(map(lambda r: r['non_reentrant'] if type(r['non_reentrant'])==int else 0, rows)))

    s = '\n'
    s = s + "#Contract      : " + str(contract) + '\n'
    s = s + "#Timeout       : " + str(timeout)  + '\n'
    s = s + "#Fail Contract : " + str(fail)     + '\n'
    s = s + "#Bug Run       : " + str(bug_run)  + '\n'
    s = s + "#Bug Fix Gen   : " + str(bug_gen)  + '\n'
    s = s + "#Bound Check   : " + str(bound_check) + '\n'
    s = s + "#Non-Reentrant : " + str(non_reentrant) + '\n'
    return s

def get_rows (dataset, directory):
    rows = []
    with open (os.path.join(os.path.dirname(directory),'cmd_history.csv'), 'r') as fp:
        rows = list (csv.DictReader(fp))

    assert(len(rows) > 0)
    rows = list(filter(lambda r: r['dataset']==dataset, rows))

    return rows


def main():
    parser = argparse.ArgumentParser(epilog='python3 mark_sguard.py --directory dir')
 
    parser.add_argument ('--directory', type=str, help='E.g., XXX/io, YYY/re')
    parser.add_argument ('--dataset', type=str, help='{io,ls,re,tx}')

    args = parser.parse_args()

    directory = args.directory
    dataset = args.dataset

    assert (dataset == IO or dataset == LS or dataset == RE or dataset == TX)

    rows = get_rows(dataset, directory)
    result = mark(directory, dataset, rows)

    stat = make_table(result)

    # newline : https://stackoverflow.com/questions/3191528/csv-in-python-adding-an-extra-carriage-return-on-windows
    # lineterminator : https://stackoverflow.com/questions/9845681/does-python-csv-writer-always-use-dos-end-of-line-characters#comment76482943_29976091
    summary_dir = os.path.join(os.path.dirname(directory), dataset + '_summary.csv')
    stat_dir = os.path.join(os.path.dirname(directory), dataset + '_stat.txt')

    with open(summary_dir, 'w', newline='') as fp:
        writer = csv.DictWriter(fp, fieldnames=fieldnames, lineterminator=os.linesep)
        writer.writeheader()
        writer.writerows(result)

    with open(stat_dir, 'w') as fp:
        fp.write(stat)

    print('Result file generated : ' + summary_dir)
    print('Stat file generated   : ' + stat_dir)
    print(stat)

if __name__ == "__main__":
    main ()
