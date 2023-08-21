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
import my_utils

# python3 scripts/run_sguard.py --root_info ~/benchmarks/meta/sb-reentrancy-meta.csv --pgmdir ~/benchmarks/reentrancy/ --outdir sGuard_result/ --process 2

HOME = expanduser("~")
sGUARD = os.path.join (HOME, "sGuard")
BENCH = os.path.join(HOME, 'benchmarks')

LOCK = Lock ()
cnt = Value ('i', 0)

KILL_TIMEOUT = ""
OUTDIR_ROOT = ""
TOTAL_NUM = 0

fieldnames_time = ["dataset", "file", "timeout", "time", "before", "after"]
fieldnames_hist = ["dataset", "file", "solv", "cmd", "when"]

IO = 'io'
LS = 'ls'
RE = 're'
TX = 'tx'

def get_pgmdir(dataset):
    pgmdir = ""
    if dataset == IO:
        pgmdir = os.path.join(BENCH, 'cve')
    elif dataset == LS:
        pgmdir = os.path.join(BENCH, 'leaking_suicidal')
    elif dataset == RE:
        pgmdir = os.path.join(BENCH, 'reentrancy')
    elif dataset == TX:
        pgmdir = os.path.join(BENCH, 'txorigin')
    assert(pgmdir != "")
    return pgmdir

def run (row):
    dataset = row['dataset']
    fid = row['id']
    fname = fid + ".sol"
    solv = my_utils.get_solc_version (row)
    main_name = row['main_name']
    pgmdir = get_pgmdir(dataset)

    cmd = ["timeout", str(KILL_TIMEOUT),
           "npm", "--prefix", sGUARD, "run", "dev",
           os.path.join(pgmdir,fname),
           solv,
           os.path.join(OUTDIR_ROOT, dataset),
           main_name]
 
    LOCK.acquire()
    cnt.value += 1
    print ("processing " + str(cnt.value) + "/" + str(TOTAL_NUM))
    print (" ".join (cmd))
    LOCK.release()

    NOW = str (datetime.now())
    BEFORE = time.time ()
    p = subprocess.Popen (cmd, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
    stdout = p.stdout.read()
    AFTER = time.time () # MUST be placed after the previous statement.
    p.wait() # somtimes processes are not immediately killed by 'timeout', so explicitly wait to terminate, in order to get proper return code.
    p.poll () # update p.returncode.

    elapsed = AFTER-BEFORE
    timeout = ""
    if elapsed >= KILL_TIMEOUT:
        timeout = "O"
        assert (p.returncode == 124)

    # Record related information
    LOCK.acquire ()

    f_res = open (os.path.join (OUTDIR_ROOT, dataset, 'log', fid + ".txt"), 'w')
    f_time = open (os.path.join (OUTDIR_ROOT, "time.csv"), 'a')
    f_history = open (os.path.join (OUTDIR_ROOT, "cmd_history.csv"), 'a')

    writer_t = csv.DictWriter (f_time, fieldnames=fieldnames_time)
    writer_h = csv.DictWriter (f_history, fieldnames=fieldnames_hist)

    f_res.write (stdout.decode("utf-8"))
    writer_t.writerow ({"dataset": dataset, "file": fname, "timeout": timeout, "time": str(elapsed), "before": str(BEFORE), "after": str(AFTER)})
    writer_h.writerow ({"dataset": dataset, "file": fname, "solv":solv, "cmd": " ".join(cmd), "when": NOW})

    f_res.close()
    f_time.close()
    f_history.close()
    LOCK.release()


def write_csv_header ():
    with open (os.path.join (OUTDIR_ROOT, "time.csv"), 'w') as fp:
        writer = csv.DictWriter(fp, fieldnames=fieldnames_time)
        writer.writeheader()
    with open (os.path.join (OUTDIR_ROOT, "cmd_history.csv"), 'w') as fp:
        writer = csv.DictWriter(fp, fieldnames=fieldnames_hist)
        writer.writeheader()


# Order results according to names of input files
def postprocess ():
    with open(os.path.join (OUTDIR_ROOT, "time.csv"), 'r') as f_input:
        csv_input = csv.DictReader(f_input)
        data = sorted(csv_input, key=lambda row: row['file'])

    with open(os.path.join (OUTDIR_ROOT, "time.csv"), 'w') as f_output:
        csv_output = csv.DictWriter(f_output, fieldnames=fieldnames_time)
        csv_output.writeheader()
        csv_output.writerows(data)

    with open(os.path.join (OUTDIR_ROOT, "cmd_history.csv"), 'r') as f_input:
        csv_input = csv.DictReader(f_input)
        data = sorted(csv_input, key=lambda row: row['file'])

    with open(os.path.join (OUTDIR_ROOT, "cmd_history.csv"), 'w') as f_output:
        csv_output = csv.DictWriter(f_output, fieldnames=fieldnames_hist)
        csv_output.writeheader()
        csv_output.writerows(data)

def get_tasks(root_info, dataset, include, exclude, pgmnum):
    rows = []
    with open (root_info, 'r') as fp:
        rows = list(csv.DictReader(fp))
    assert (all(map (lambda row: not (row['actual_order'] == ""), rows)))
    assert (len(rows)>=1)

    include_lst = []
    if include != "":
        include_lst = [line.replace("\r\n","").replace ("\n","") for line in open(include)]
        rows = list(filter(lambda row: row['id'] in include_lst, rows))

    exclude_lst = []
    if exclude != "":
        exclude_lst = [line.replace("\r\n","").replace ("\n","") for line in open(exclude)]
        rows = list(filter(lambda row: not (row['id'] in exclude_lst), rows))

    if pgmnum==None:
        pass
    elif pgmnum>0:
        rows = rows[:pgmnum]
    else:
        assert(False)

    rows = list(filter(lambda row: row['dataset'] in dataset, rows))

    global TOTAL_NUM
    TOTAL_NUM = len(rows)

    return rows


def setup_globals(kill_timeout, outdir_root):
    global KILL_TIMEOUT
    global OUTDIR_ROOT

    KILL_TIMEOUT = kill_timeout
    OUTDIR_ROOT = outdir_root

    b = KILL_TIMEOUT != "" and OUTDIR_ROOT != ""
    assert(b)

def mk_dataset_dirs(dataset, outroot_dir):
    if IO in dataset:
        os.mkdir(os.path.join(outroot_dir, IO))
        os.mkdir(os.path.join(outroot_dir, IO, 'log'))
        os.mkdir(os.path.join(outroot_dir, IO, 'json'))
        os.mkdir(os.path.join(outroot_dir, IO, 'fixed'))
    if RE in dataset:
        os.mkdir(os.path.join(outroot_dir, RE))
        os.mkdir(os.path.join(outroot_dir, RE, 'log'))
        os.mkdir(os.path.join(outroot_dir, RE, 'json'))
        os.mkdir(os.path.join(outroot_dir, RE, 'fixed'))
    if TX in dataset:
        os.mkdir(os.path.join(outroot_dir, TX))
        os.mkdir(os.path.join(outroot_dir, TX, 'log'))
        os.mkdir(os.path.join(outroot_dir, TX, 'json'))
        os.mkdir(os.path.join(outroot_dir, TX, 'fixed'))
    if LS in dataset:
        assert(False)

def main ():
    parser = argparse.ArgumentParser()
 
    parser.add_argument ('--root_info', type=str)
    parser.add_argument ('--dataset', type=str, help='{io,ls,re,tx}')
    parser.add_argument ('--outdir_root', type=str)
    parser.add_argument ('--kill_timeout', type=int, default=7800)
    parser.add_argument ('--process', type=int)
    parser.add_argument ('--include', type=str, default='', help='program lists to be included')
    parser.add_argument ('--exclude', type=str, default='', help='program lists to be excluded')
    parser.add_argument ('--pgmnum', type=int)

    args = parser.parse_args ()

    assert(args.pgmnum == None or args.pgmnum > 0)

    setup_globals (args.kill_timeout, args.outdir_root)

    dataset = [IO, RE, TX] if args.dataset==None else [d for d in args.dataset.split(',')]
    mk_dataset_dirs(dataset, args.outdir_root)

    rows = get_tasks (args.root_info, dataset, args.include, args.exclude, args.pgmnum)
    write_csv_header()

    BEFORE = time.time()
    pool = Pool (args.process)
    pool.map (run, rows)
    # https://stackoverflow.com/questions/35708371/purpose-of-pool-join-pool-close-in-multiprocessing
    pool.close () # pool.close tells the pool not to accept any new job.
    pool.join () # pool.join tells the pool to wait until all jobs finished then exit, effectively cleaning up the pool.
    AFTER = time.time ()

    print ("Took: " + str(AFTER-BEFORE))
    with open (os.path.join (OUTDIR_ROOT, 'took.txt'), 'w') as fp:
        fp.write (str (AFTER-BEFORE))

    postprocess ()

if __name__ == "__main__":
    main ()
