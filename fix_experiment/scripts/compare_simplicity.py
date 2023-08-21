import csv
import os
import argparse
import sys
import json
from os.path import expanduser
import statistics
import my_utils

HOME = expanduser("~")
BENCH = os.path.join(HOME, 'benchmarks')
OUT_SPACE = os.path.join(HOME, 'fix_result')

IO = 'io'
RE = 're'
TX = 'tx'
LS = 'ls'

EL = 'el'
SU = 'su'

fieldnames = ["file", "common_gen", "sguard_dz_fix", "org_byte",
              "smartfix_run", "smartfix_gen", "smartfix_bc", "smartfix_nr", "smartfix_byte",
              "sguard_run", "sguard_gen", "sguard_bc", "sguard_nr", "sguard_byte"]

def read_csv(summary):
    rows = []
    with open(summary, 'r') as fp:
        rows = list(csv.DictReader(fp))
    assert(len(rows) > 0)
    return rows


def str_to_int(s):
    try:
      return int(s)
    except ValueError:
      return 0


def check_bytesize(dirname,fname):
    fid = fname.replace('.sol', '')
    main_name = my_utils.get_main_name (fid)
    full_fname = os.path.join(dirname,fname)
    solv = my_utils.get_solc_version_fid (fid)
    compiled1 = my_utils.is_compilable(solv, full_fname)
    compiled2 = my_utils.is_compilable('0.4.25', full_fname)
    assert (compiled1 or compiled2)
    solv = solv if compiled1 else '0.4.25'
    org_byte = my_utils.check_bytesize(full_fname, solv, main_name)
    return org_byte


def compare(dataset, dirname, smartfix_summary, sguard_summary):
    smartfix_rows = read_csv(smartfix_summary)
    smartfix_rows = list (filter (lambda row: row['dataset'] == dataset, smartfix_rows))
    sguard_rows = read_csv(sguard_summary)

    smartfix_fnames = list (map (lambda row: row['file'], smartfix_rows))
    sguard_rows = list (filter (lambda row: row['file'] in smartfix_fnames, sguard_rows))

    res = []

    for smartfix_row, sguard_row in zip(smartfix_rows, sguard_rows):
      assert(smartfix_row['file'] == sguard_row['file'])
      fname = smartfix_row['file']

      # Step 1. SmartFix result
      smartfix_run = True if smartfix_row['errmsg'] == "" else False
      assert(type(smartfix_row['alarm_pat']) == str)
      smartfix_gen = True if smartfix_row['alarm_pat'] == "0" else False
      smartfix_bc = str_to_int(smartfix_row['bound_check'])
      smartfix_nr = str_to_int(smartfix_row['non_reentrant'])
      smartfix_bytesize = str_to_int(smartfix_row['bytesize'])

      # print(fname + ' : ' + smartfix_row['non_reentrant'])
      # print(smartfix_nr) 

      # Step 2. sGuard result
      # print(sguard_row)
      sguard_run = True if sguard_row['failmsg'] == "" else False
      sguard_gen = ""
      if sguard_row['bug_gen'] == "n/a":
        sguard_gen = False
      else:
        sguard_gen = True if int(sguard_row['bug_gen']) > 0 else False

      assert(type(sguard_gen) == bool)
      sguard_bc = str_to_int(sguard_row['bound_check'])
      sguard_nr = str_to_int(sguard_row['non_reentrant'])
      sguard_bytesize = str_to_int(sguard_row['bytesize'])

      common_gen = smartfix_gen and sguard_gen
      assert(sguard_row['dz_fix'] == 'O' or sguard_row['dz_fix'] == 'n/a' or sguard_row['dz_fix'] == '')
      sguard_dz_fix = True if sguard_row['dz_fix'] == 'O' else False

      # Commented as this information is unused.
      # org_byte = check_bytesize(dirname,fname)
      org_byte = 0

      res.append({
        'file': fname,
        'common_gen': common_gen,
        'sguard_dz_fix': sguard_dz_fix,

        'org_byte': org_byte,

        'smartfix_run': smartfix_run,
        'smartfix_gen': smartfix_gen,
        'smartfix_bc': smartfix_bc,
        'smartfix_nr': smartfix_nr,
        'smartfix_byte': smartfix_bytesize,

        'sguard_run': sguard_run,
        'sguard_gen': sguard_gen,
        'sguard_bc': sguard_bc,
        'sguard_nr': sguard_nr,
        'sguard_byte': sguard_bytesize
      })

    return res



def make_table(rows, mode):
    sep = 20
    sep2 = 25

    assert(mode == 1 or  mode == 2)
    col = 'Common_Gen'.ljust(sep) if mode == 1 else 'Common_Gen & No_DZ_Fix'.ljust(sep2)
    col = col + 'SmartFix (BC)'.ljust(sep) + 'SmartFix (NR)'.ljust(sep)
    col = col + 'sGuard (BC)'.ljust(sep) + 'sGuard (NR)'.ljust(sep)
    col = col + 'SmartFix/sGuard (BC+NR)'.ljust(sep2)
 
    common_gen_rows = list(filter(lambda r: r['common_gen']==True, rows)) if mode == 1 else list(filter(lambda r: r['common_gen']==True and r['sguard_dz_fix']==False, rows))
    common_gen_contract = len(common_gen_rows)
    # org_byte = sum(list(map(lambda r: r['org_byte'], common_gen_rows)))

    smartfix_gen = len(list(filter(lambda r: r['smartfix_gen']==True, common_gen_rows)))
    smartfix_bc = sum(list(map(lambda r: r['smartfix_bc'], common_gen_rows)))
    smartfix_nr = sum(list(map(lambda r: r['smartfix_nr'], common_gen_rows)))

    sguard_gen = len(list(filter(lambda r: r['sguard_gen']==True, common_gen_rows)))
    sguard_bc = sum(list(map(lambda r: r['sguard_bc'], common_gen_rows)))
    sguard_nr = sum(list(map(lambda r: r['sguard_nr'], common_gen_rows)))

    assert (common_gen_contract == smartfix_gen and smartfix_gen == sguard_gen)
    bcnr_ratio = round (((smartfix_bc + smartfix_nr) / (sguard_bc + sguard_nr)) * 100, 1) if (sguard_bc + sguard_nr) > 0 else 'n/a'

    s = str(common_gen_contract).ljust(sep) if mode == 1 else str(common_gen_contract).ljust(sep2)
    s = s + str(smartfix_bc).ljust(sep) + str(smartfix_nr).ljust(sep)
    s = s + str(sguard_bc).ljust(sep) + str(sguard_nr).ljust(sep)
    s = s + str(bcnr_ratio).ljust(sep2)

    bar = '=' * 130
    title = '* Results on commonly patched contracts'
    final = bar + '\n' + col + '\n' + s

    return final


def main():
    parser = argparse.ArgumentParser(epilog='python3 compare_simplicity.py --smartfix dir1 --sguard dir2 --dataset {io,re,tx}')
    parser.add_argument ('--smartfix', type=str, help='root directory of smartfix output')
    parser.add_argument ('--sguard', type=str, help='root directory of sguard output')
    parser.add_argument ('--dataset', type=str, help='{io,re,tx}')

    args = parser.parse_args()

    smartfix_dir = args.smartfix
    sguard_dir = args.sguard

    dataset = [IO, RE, TX] if args.dataset==None else [d for d in args.dataset.split(',')]

    res = ''

    if IO in dataset:
        smartfix_summary = os.path.join(smartfix_dir, 'all_summary.csv')
        sguard_summary = os.path.join(sguard_dir, 'io_summary.csv')
        result = compare(IO, os.path.join(BENCH,'cve'), smartfix_summary, sguard_summary)
        stat1 = make_table(result, 1)
        # stat2 = make_table(result, 2)
        iobench = '=== IO Bench ==='
        print(iobench)
        print(stat1)
        # print(stat2)
        print('')

        res += iobench + '\n' + stat1 + '\n\n'


    if RE in dataset:
        smartfix_summary = os.path.join(smartfix_dir, 'all_summary.csv')
        sguard_summary = os.path.join(sguard_dir, 're_summary.csv')
        result = compare(RE, os.path.join(BENCH,'reentrancy'), smartfix_summary, sguard_summary)
        stat1 = make_table(result, 1)
        # stat2 = make_table(result, 2)
        rebench = '=== RE Bench ==='
        print(rebench)
        print(stat1)
        # print(stat2)
        print('')

        res += rebench + '\n' + stat1 + '\n\n'

    if TX in dataset:
        smartfix_summary = os.path.join(smartfix_dir, 'all_summary.csv')
        sguard_summary = os.path.join(sguard_dir, 'tx_summary.csv')
        result = compare(TX, os.path.join(BENCH,'txorigin'), smartfix_summary, sguard_summary)
        stat1 = make_table(result, 1)
        # stat2 = make_table(result, 2)
        txbench = '=== TX Bench ==='
        print(txbench)
        print(stat1)
        # print(stat2)
        print('')

        res += txbench + '\n' + stat1 + '\n\n'

    table_path = os.path.join(OUT_SPACE, 'patch_simplicity.txt')
    with open (table_path, 'w') as fp:
        fp.write(res)

    print('[INFO] table generated : ' + table_path)

if __name__ == "__main__":
    main ()
