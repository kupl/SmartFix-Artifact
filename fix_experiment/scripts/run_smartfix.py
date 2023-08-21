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
import my_utils2
import json

HOME = expanduser("~")
VERISMART = os.path.join(HOME, 'SmartFix')
BENCH = os.path.join(HOME, 'benchmarks')

LOCK = Lock ()
cnt = Value ('i', 0)

REPAIR_LOOP_TIMEOUT = ""
REPAIR_TOOL_TIMEOUT = ""

Z3_TIMEOUT = ""
KILL_TIMEOUT = ""
OUTDIR_ROOT = ""
NO_LEARN = False
NO_OFFLINE = False
NO_DIFF = False

TOTAL_NUM = 0

KNOWLEDGE_DIR = ""

fieldnames_time = ["dataset", "file", "timeout", "time", "before", "after"]
fieldnames_hist = ["dataset", "file", "cmd", "when"]

IO = 'io'
LS = 'ls'
RE = 're'
TX = 'tx'

def copy_knowledges (dataset, fid):
    assert(KNOWLEDGE_DIR != "")
    assert(os.system(f'cp {KNOWLEDGE_DIR}/knowledge_{fid}.json {OUTDIR_ROOT}/{dataset}/knowledge_{fid}.json') == 0)

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

    # Make execution command
    cmd = ["timeout", str(KILL_TIMEOUT),  os.path.join(VERISMART, "main.native"),
           "-mode", "repair", "-solv", solv,
           "-repair_loop_timeout", str(REPAIR_LOOP_TIMEOUT),
           "-repair_tool_timeout", str(REPAIR_TOOL_TIMEOUT),
           "-z3timeout", str(Z3_TIMEOUT),
           "-input", os.path.join(pgmdir,fname), "-main", main_name,
           "-refined_vcgen",
           "-report",
           "-debug", "repair"]

    if NO_LEARN:
        cmd.append("-repair_no_learn")
    if not NO_OFFLINE:
        cmd += ["-repair_ksrc", os.path.join(OUTDIR_ROOT, dataset, f'knowledge_{fid}.json')]
    if NO_DIFF:
        cmd.append('-repair_no_diff')

    cmd += ["-outdir", os.path.join(OUTDIR_ROOT, dataset, fid)] # outdir and its childeren will be made by SmartFix

    LOCK.acquire()
    cnt.value += 1
    print ("processing " + str(cnt.value) + "/" + str(TOTAL_NUM))
    print(" ".join (cmd))
    LOCK.release()

    if KNOWLEDGE_DIR == "":
      my_utils2.merge_knowledges(os.path.join(OUTDIR_ROOT,dataset), fid)
    else:
      copy_knowledges(dataset, fid)

    # Run tool
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

    if not (os.path.exists(os.path.join(OUTDIR_ROOT, dataset, fid))): # failed to produce dir, e.g., stuck in path generation step
        assert(os.system('mkdir ' + os.path.join(OUTDIR_ROOT, dataset, fid)) == 0)

    if not (os.path.exists(os.path.join(OUTDIR_ROOT, dataset, fid, 'summary.json'))): # When summary file is not generated.
        d = {
          'fileName': os.path.join(pgmdir,fname),
          'baseName': fname,
          'time': elapsed,
          'errMsg': 'No summary file',
          'result': []
        }
        with open(os.path.join(OUTDIR_ROOT, dataset, fid, 'summary.json'), 'w') as fp:
            json.dump(d,fp,indent=4)

    f_res = open (os.path.join (OUTDIR_ROOT, dataset, fid, fid + ".txt"), 'w')
    f_time = open (os.path.join (OUTDIR_ROOT, "time.csv"), 'a')
    f_history = open (os.path.join (OUTDIR_ROOT, "cmd_history.csv"), 'a')

    writer_t = csv.DictWriter (f_time, fieldnames=fieldnames_time)
    writer_h = csv.DictWriter (f_history, fieldnames=fieldnames_hist)
    f_res.write (stdout.decode("utf-8"))
    writer_t.writerow ({"dataset": dataset, "file": fname, "timeout": timeout, "time": str(elapsed), "before": str(BEFORE), "after": str(AFTER)})
    writer_h.writerow ({"dataset": dataset, "file": fname, "cmd": " ".join(cmd), "when": NOW})

    f_res.close()
    f_time.close()
    f_history.close()

    LOCK.release ()

    assert(os.system ('rm ' +  os.path.join(OUTDIR_ROOT, dataset, f'knowledge_{fid}.json')) == 0)


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
    rows = list (filter (lambda row: not (row['actual_order'] == ""), rows))
    assert (len(rows)>=1)

    include_lst = []
    if include != "":
        include_lst = [line.replace("\r\n","").replace ("\n","") for line in open(include)]
        rows = list(filter(lambda row: row['id'] in include_lst, rows))
        # for r in rows:
        #    print(r)
        # assert(False)

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


def setup_globals(repair_loop_timeout, repair_tool_timeout, z3_timeout, kill_timeout, outdir_root, no_learn, no_offline, no_diff, pre_ksrc):
    global REPAIR_LOOP_TIMEOUT
    global REPAIR_TOOL_TIMEOUT
    global Z3_TIMEOUT
    global KILL_TIMEOUT
    global OUTDIR_ROOT
    global NO_LEARN
    global NO_OFFLINE
    global NO_DIFF
    global KNOWLEDGE_DIR

    REPAIR_LOOP_TIMEOUT = repair_loop_timeout
    REPAIR_TOOL_TIMEOUT = repair_tool_timeout
    Z3_TIMEOUT = z3_timeout
    KILL_TIMEOUT = kill_timeout
    OUTDIR_ROOT = outdir_root
    NO_LEARN = no_learn
    NO_OFFLINE = no_offline
    NO_DIFF = no_diff
    KNOWLEDGE_DIR = pre_ksrc

    b = REPAIR_LOOP_TIMEOUT !="" and REPAIR_TOOL_TIMEOUT !="" and Z3_TIMEOUT !="" and KILL_TIMEOUT !=""
    assert(b)


def mk_dataset_dirs(dataset, outroot_dir):
    if IO in dataset:
        os.mkdir(os.path.join(outroot_dir, IO))
    if LS in dataset:
        os.mkdir(os.path.join(outroot_dir, LS))
    if RE in dataset:
        os.mkdir(os.path.join(outroot_dir, RE))
    if TX in dataset:
        os.mkdir(os.path.join(outroot_dir, TX))


def write_csv_header ():
    with open (os.path.join (OUTDIR_ROOT, "time.csv"), 'w') as fp:
        writer = csv.DictWriter(fp, fieldnames=fieldnames_time)
        writer.writeheader()
    with open (os.path.join (OUTDIR_ROOT, "cmd_history.csv"), 'w') as fp:
        writer = csv.DictWriter(fp, fieldnames=fieldnames_hist)
        writer.writeheader()


def main ():
    parser = argparse.ArgumentParser()
 
    parser.add_argument ('--root_info', type=str)
    parser.add_argument ('--dataset', type=str, help='{io,ls,re,tx}')
    parser.add_argument ('--outdir_root', type=str)
    parser.add_argument ('--repair_loop_timeout', type=int, default=3600)
    parser.add_argument ('--repair_tool_timeout', type=int, default=180)
    parser.add_argument ('--kill_timeout', type=int, default=4000)
    parser.add_argument ('--z3_timeout', type=int, default=60000)
    parser.add_argument ('--process', type=int)
    parser.add_argument ('--include', type=str, default='', help='program lists to be included')
    parser.add_argument ('--exclude', type=str, default='', help='program lists to be excluded')
    parser.add_argument ('--pgmnum', type=int)
    parser.add_argument ('--no_learn', default=False, action='store_true')
    parser.add_argument ('--no_offline', default=False, action='store_true')
    parser.add_argument ('--no_diff', default=False, action='store_true')
    parser.add_argument ('--pre_ksrc', default="", type=str, help='pre-specified knowledge source (debugging purpose')

    args = parser.parse_args ()

    assert(args.pgmnum == None or args.pgmnum > 0)

    setup_globals (args.repair_loop_timeout, args.repair_tool_timeout, args.z3_timeout, args.kill_timeout,
                   args.outdir_root, args.no_learn, args.no_offline, args.no_diff, args.pre_ksrc)

    dataset = [IO, LS, RE, TX] if args.dataset==None else [d for d in args.dataset.split(',')]
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
