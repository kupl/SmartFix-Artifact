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

IO = 'io'
RE = 're'
TX = 'tx'
LS = 'ls'

EL = 'el'
SU = 'su'

BTYPS = [IO, RE, TX, EL, SU]

fieldnames = ['dataset', 'file', 'lineno', 'errmsg', 'time', 'iter',
              'skip_exist',
              'alarm_org', 'alarm_pat',
              'correct_cont',

              'io_lab', 'io_run', 'io_gen', 'io_cor',
              're_lab', 're_run', 're_gen', 're_cor',
              'tx_lab', 'tx_run', 'tx_gen', 'tx_cor',
              'el_lab', 'el_run', 'el_gen', 'el_cor',
              'su_lab', 'su_run', 'su_gen', 'su_cor',

              'bound_check', 'non_reentrant', 'bytesize',
              'first_sol_iter', 'first_sol_time',
              'time_rank1', 'iter_rank1', 'edit_rank1',
              'edit_min', 'edit_max', 'patch_found',
              'uncompiled', 'patch_err', 'verify_err']


def empty_fieldnames():
    return dict.fromkeys(fieldnames, '')

def init_fieldnames_with(value):
    return dict.fromkeys(fieldnames, value)

def na_if_err(summary_j, key):
    if summary_j['errMsg']!=None:
      return 'err'
    elif summary_j['alarm_org']== 0: # original contract is safe
      return 'safe'
    elif summary_j['alarm_pat']==None: # failed to find solutions
      return 'no patch'
    else:
      try:
        return summary_j[key]
      except KeyError: # for compatibility with previous versions of fieldnames
        return 'n/a'
    # return 'n/a' if summary_j['errMsg']!=None else summary_j[key]


def check_time(directory, fname):
    with open(os.path.join(os.path.dirname(directory),'time.csv'), 'r') as fp:
        rows = list(csv.DictReader(fp))
        rows = list(filter (lambda row: row['file'] == fname, rows))
        assert (len(rows) == 1)
    row = rows[0]
    assert (row['timeout'] == '' or row['timeout'] == 'O')
    if row['timeout']=='O':
        return 'TO'
    else:
        return float(row['time'])


def check_alarm_skip_exist (summary_j):

    if summary_j['errMsg']!=None:
        return ""

    patch_dirs = list(map(lambda j: j['dir'], summary_j['detail']))
    if len(patch_dirs) == 0:
        return ""

    pdir = patch_dirs[0]

    rootdir = os.path.dirname(pdir)

    contents = ""
    with open(os.path.join(rootdir,'log','verismart.txt'), 'r') as fp:
        contents = fp.read()
    assert(len(contents) > 0)

    return "O" if "Warning : conditional safety checking" in contents else ""


def get_alarm_lines (pdir, bugtype):
    rootdir = os.path.dirname(pdir)
    # with open (os.path.join(rootdir,'reports/verismart.json'), 'r') as fp:
    #    json_patch = json.load(fp)

    # report = json_patch['vul_result']
    # alarm_lines = list(filter(lambda q: q['status']=='unproven', report))

    alarm_lines = []
    with open (os.path.join(rootdir,'reports/vulnerability_report.csv'), 'r') as fp:
        alarm_lines = list(csv.DictReader(fp))

    kind = ""
    try: # for compatibility with previous column names
      _ = list(filter(lambda q: q['kind']=='IO', alarm_lines))
      kind = 'kind'
    except KeyError:
      kind = 'vultyp'
    assert(kind != "")

    if bugtype == IO:
        alarm_lines = list(filter(lambda q: q[kind]=='IO', alarm_lines))
        alarm_lines = set(map(lambda q: (q[kind], int(q['line'])), alarm_lines))
        return alarm_lines

    elif bugtype == EL:
        alarm_lines = list(filter(lambda q: q[kind]=='ETH_LEAK', alarm_lines))
        alarm_lines = set(map(lambda q: (q[kind], int(q['line'])), alarm_lines))
        return alarm_lines

    elif bugtype == SU:
        alarm_lines = list(filter(lambda q: q[kind]=='KA', alarm_lines))
        alarm_lines = set(map(lambda q: (q[kind], int(q['line'])), alarm_lines))
        return alarm_lines

    elif bugtype == RE:
        alarm_lines = list(filter(lambda q: q[kind]=='RE_EL' or q[kind]=='RE', alarm_lines))
        re_kind = set(map(lambda q: q[kind], alarm_lines))
        assert(len(re_kind) < 2)
        alarm_lines = set(map(lambda q: (q[kind], int(q['line'])), alarm_lines))
        return alarm_lines

    elif bugtype == TX:
        alarm_lines = list(filter(lambda q: q[kind]=='TX_ORG', alarm_lines))
        alarm_lines = set(map(lambda q: (q[kind], int(q['line'])), alarm_lines))
        return alarm_lines

    else:
        assert(False)


def check_time_iter_edit (summary_j):
    if summary_j['errMsg']!=None:
        return ("n/a", "n/a", "n/a")

    patches = summary_j['detail']
    if len(patches) == 0:
        return ("n/a", "n/a", "n/a")

    gen_patch = patches[0] # only consider top-ranked patch
    time = gen_patch['found_time']
    iteration = gen_patch['found_iter']
    edit = gen_patch['edit']
    return (time, iteration, edit)


def check_bc_nr(summary_j):
    if summary_j['errMsg']!=None:
        return ("n/a", "n/a")

    patch_dirs = list(map(lambda j: j['dir'], summary_j['detail']))
    if len(patch_dirs) == 0:
        return ("n/a", "n/a")

    gen_patch_dir = patch_dirs[0] # only consider top-ranked patch
    fp = open(gen_patch_dir, 'r')
    src = fp.read()
    fp.close()
    bc = src.count("/* <FIX> Insert:BC */")
    nr = src.count("_nonReentrant /* <FIX> Add Modifier:NR */")
    return (bc,nr)

def check_bytesize(summary_j):
    if summary_j['errMsg']!=None:
        return "n/a"

    patch_dirs = list(map(lambda j: j['dir'], summary_j['detail']))
    if len(patch_dirs) == 0:
        return "n/a"

    gen_patch_dir = patch_dirs[0] # only consider top-ranked patch
    fid = summary_j['baseName'].replace('.sol', '')
    main_name = my_utils.get_main_name (fid)
    solv = my_utils.get_solc_version_fid (fid)
    compiled1 = my_utils.is_compilable(solv, summary_j['fileName'])
    compiled2 = my_utils.is_compilable('0.4.25', summary_j['fileName'])
    assert (compiled1 or compiled2)
    solv = solv if compiled1 else '0.4.25'

    bytesize = my_utils.check_bytesize(gen_patch_dir, solv, main_name)
    # print(solv)
    # print(main_name)
    # print(bytesize)
    # print('')
    # assert(False)

    return bytesize


def check_labels_helper (summary_j, bugtype):
    label_num = len(my_utils.get_ans_lines (summary_j['fileName'], bugtype))

    if summary_j['errMsg']!=None:
        return (label_num, "n/a", "n/a", "n/a")

    run, gen, cor = 0, 0, 'n/a'

    patch_dirs = list(map(lambda j: j['dir'], summary_j['detail']))
    # No patches are generated
    if len(patch_dirs) == 0:
        run = label_num
        gen = 0
        cor = "n/a"
        return (label_num, run, gen, cor)

    gen_patch_dir = patch_dirs[0] # only consider top-ranked patch
    alarms = get_alarm_lines (gen_patch_dir, bugtype)
    labels = my_utils.get_ans_lines (gen_patch_dir, bugtype)
    # print(gen_patch_dir)
    # print(label_num)
    # print(len(labels))
    # print('')
    assert(label_num == len(labels))

    # record 'run' and 'gen'
    run = len(labels)

    for (kind,line) in labels:
      lab = (kind,line)
      if kind == "RE": # to address statement-level annotations
        if len(alarms) == 0: # +1 only when all RE alarms do not exist
          gen = gen + 1
      elif not (lab in alarms): # check if alarms at label are not raised
        gen = gen + 1

    return (label_num, run, gen, cor)


def check_labels(summary_j, dataset):
    dummy = ['','','','']

    if dataset == IO:
        (io_lab, io_run, io_gen, io_cor) = check_labels_helper(summary_j, IO)
        lst = [io_lab, io_run, io_gen, io_cor]
        res = lst + dummy + dummy + dummy + dummy
        return res

    elif dataset == RE:
        re_lab, re_run, re_gen, re_cor = check_labels_helper(summary_j, RE)
        lst = [re_lab, re_run, re_gen, re_cor]
        res = dummy + lst + dummy + dummy + dummy
        return res

    elif dataset == TX:
        tx_lab, tx_run, tx_gen, tx_cor = check_labels_helper(summary_j, TX)
        lst = [tx_lab, tx_run, tx_gen, tx_cor]
        res = dummy +  dummy + lst + dummy + dummy
        return res

    elif dataset == LS:
        el_lab, el_run, el_gen, el_cor = check_labels_helper(summary_j, EL)
        su_lab, su_run, su_gen, su_cor = check_labels_helper(summary_j, SU)
        lst_el = [el_lab, el_run, el_gen, el_cor]
        lst_su = [su_lab, su_run, su_gen, su_cor]
        res = dummy + dummy + dummy + lst_el + lst_su
        return res

    else:
        assert(False)

def aggregate(directory, dataset):
    res = []
    fids = sorted([d for d in os.listdir(directory) if os.path.isdir(os.path.join(directory,d))]) # 'isdir' requires full/relative path

    for fid in fids:
        summary = os.path.join(directory, fid, 'summary.json')
        if not(os.path.isfile(summary)):
            assert(False)
            # tmp = init_fieldnames_with('n/a')
            # tmp['file'] = fid + '.sol'
            # res.append(tmp)
            # continue


        summary_j = json.load(open(summary,'r'))
        # lab_fix_top1, lab_fix_top2 = check_label_fix_generated (summary_j, dataset)
   
        lab_res = check_labels(summary_j, dataset)
        io_lab, io_run, io_gen, io_cor = lab_res[0], lab_res[1], lab_res[2], lab_res[3]
        re_lab, re_run, re_gen, re_cor = lab_res[4], lab_res[5], lab_res[6], lab_res[7]
        tx_lab, tx_run, tx_gen, tx_cor = lab_res[8], lab_res[9], lab_res[10], lab_res[11]
        el_lab, el_run, el_gen, el_cor = lab_res[12], lab_res[13], lab_res[14], lab_res[15]
        su_lab, su_run, su_gen, su_cor = lab_res[16], lab_res[17], lab_res[18], lab_res[19]
        # bug2_lab, bug2_run, bug2_gen, bug2_cor = lab_res[4], lab_res[5], lab_res[6], lab_res[7]

        # TODO: handle error case at once
        time_rank1, iter_rank1, edit_rank1 = check_time_iter_edit (summary_j)
        bc, nr = check_bc_nr (summary_j)
        bytesize = 'n/a' # check_bytesize (summary_j)

        assert(fid + '.sol' == summary_j['baseName'])
        res.append({
            'dataset': dataset,
            'file': summary_j['baseName'],
            'lineno': my_utils.get_lineno (fid),
            'errmsg': summary_j['errMsg'],

            'alarm_org': na_if_err(summary_j, 'alarm_org'),
            # 'alarm_org_wo_leak': na_if_err(summary_j, 'alarm_org_wo_leak'),
            'alarm_pat': na_if_err(summary_j, 'alarm_pat'),
            # 'alarm_pat_wo_leak': na_if_err(summary_j, 'alarm_pat_wo_leak'),
            'correct_cont':'n/a', #correct_cont,
            'skip_exist': check_alarm_skip_exist(summary_j),

            'io_lab': io_lab, 'io_run': io_run, 'io_gen': io_gen, 'io_cor': io_cor,
            're_lab': re_lab, 're_run': re_run, 're_gen': re_gen, 're_cor': re_cor,
            'tx_lab': tx_lab, 'tx_run': tx_run, 'tx_gen': tx_gen, 'tx_cor': tx_cor,
            'el_lab': el_lab, 'el_run': el_run, 'el_gen': el_gen, 'el_cor': el_cor,
            'su_lab': su_lab, 'su_run': su_run, 'su_gen': su_gen, 'su_cor': su_cor,
            # 'bug1_lab': bug1_lab, 'bug1_run': bug1_run, 'bug1_gen': bug1_gen, 'bug1_cor': bug1_cor,
            # 'bug2_lab': bug2_lab, 'bug2_run': bug2_run, 'bug2_gen': bug2_gen, 'bug2_cor': bug2_cor,

            'bound_check': bc,
            'non_reentrant': nr,
            'bytesize': bytesize,

            'time_rank1': time_rank1,
            'iter_rank1': iter_rank1,
            'edit_rank1': edit_rank1,

            'edit_min': na_if_err(summary_j, 'edit_min'),
            'edit_max': na_if_err(summary_j, 'edit_max'),
            'patch_found': na_if_err(summary_j, 'patch_found'),

            'time': check_time(directory, summary_j['baseName']), # summary_j['time'],
            'iter': na_if_err(summary_j, 'iter'),
            'first_sol_iter': na_if_err(summary_j, 'first_sol_iter'),
            'first_sol_time': na_if_err(summary_j, 'first_sol_time'),

            'uncompiled': na_if_err(summary_j, 'uncompiled'),
            'patch_err': na_if_err(summary_j, 'patch_err'),
            'verify_err': na_if_err(summary_j, 'verify_err')
        })

    return res


def make_table_bug(rows):
    sep = 13

    res = ''
    all_lab, all_run, all_gen, all_cor = 0, 0, 0, 'n/a'
    for btyp in BTYPS:
        cap = btyp.upper()
        col = f'{cap}_Lab'.ljust(sep) + f'{cap}_Run'.ljust(sep) + f'{cap}_Gen'.ljust(sep) + f'{cap}_Cor'.ljust(sep)

        bug_lab = sum(list(map(lambda r: r[f'{btyp}_lab'] if type(r[f'{btyp}_lab'])==int else 0, rows)))
        bug_run = sum(list(map(lambda r: r[f'{btyp}_run'] if type(r[f'{btyp}_run'])==int else 0, rows)))
        bug_gen = sum(list(map(lambda r: r[f'{btyp}_gen'] if type(r[f'{btyp}_gen'])==int else 0, rows)))
        bug_cor = 'n/a' # sum(list(map(lambda r: r[f'{btyp}_cor'] if type(r[f'{btyp}_cor'])==int else 0, rows)))


        all_lab += bug_lab
        all_run += bug_run
        all_gen += bug_gen
        # all_cor += bug_cor

        vals = str(bug_lab).ljust(sep) + str(bug_run).ljust(sep) + str(bug_gen).ljust(sep) + str(bug_cor).ljust(sep)
        bar = '=' * 50
        s = bar + '\n' + col + '\n' + vals + '\n' + bar + '\n\n'
        res += s

    col = 'ALL_Lab'.ljust(sep) + 'ALL_Run'.ljust(sep) + 'ALL_Gen'.ljust(sep) + 'ALL_Cor'.ljust(sep)
    bar = '=' * 50
    vals = str(all_lab).ljust(sep) + str(all_run).ljust(sep) + str(all_gen).ljust(sep) + str(all_cor).ljust(sep)
    s = bar + '\n' + col + '\n' + vals + '\n' + bar + '\n\n'
    res += s

    return res


def make_table_sol_cont (rows):
    sep = 13

    contract = len(rows)
    col = 'Contract'.ljust(sep) + 'Alarm_Org'.ljust(sep) + 'Alarm_Pat'.ljust(sep) + 'Run_Cont'.ljust(sep) + 'Gen_Cont'.ljust(sep) + 'Cor_Cont'.ljust(sep)

    alarm_org = sum(list(map(lambda r: r['alarm_org'] if type(r['alarm_org']) == int else 0, rows)))
    alarm_pat = sum(list(map(lambda r: r['alarm_pat'] if type(r['alarm_pat']) == int else 0, rows)))

    run_cont = len(list(filter(lambda r: r['errmsg'] == None, rows)))
    gen_cont = len(list(filter(lambda r: r['alarm_pat'] == 0, rows)))
    correct_cont = 'n/a'

    s = str(contract).ljust(sep) + str(alarm_org).ljust(sep) + str(alarm_pat).ljust(sep)
    s = s + str(run_cont).ljust(sep) + str(gen_cont).ljust(sep) + str(correct_cont).ljust(sep)

    bar = '=' * 75
    s = bar + '\n' + col + '\n' + s + '\n' + bar + '\n'
    return s


def make_table_patch_size (rows):
    sep = 13
    sep2 = 8

    col = 'BC'.ljust(sep2) + 'NR'.ljust(sep2)
    bound_check = sum( list(map(lambda r: r['bound_check'] if type(r['bound_check']) == int else 0, rows)))
    non_reentrant = sum( list(map(lambda r: r['non_reentrant'] if type(r['non_reentrant']) == int else 0, rows)))

    s = str(bound_check).ljust(sep2) + str(non_reentrant).ljust(sep2)

    bar = '=' * 15
    s = bar + '\n' + col + '\n' + s + '\n' + bar + '\n'
    return s

def make_table_time_all (rows):
    sep = 17
    sep2 = 8

    col = 'Contract'.ljust(sep) + 'Avg_EndTime'.ljust(sep) + 'Med_EndTime'.ljust(sep)

    contract = len(rows)

    fin_time_rows =  list(filter(lambda r: isinstance(r['time'], float), rows))
    fin_time_rows = list(map(lambda r: r['time'], fin_time_rows))
    avg_finish_time = round(statistics.mean(fin_time_rows)) if len(fin_time_rows) > 0 else 'n/a'
    med_finish_time = round(statistics.median(fin_time_rows)) if len(fin_time_rows) > 0 else 'n/a'
    avg_finish_time_hm = my_utils.from_sec_to_hm(avg_finish_time) if len(fin_time_rows) > 0 else 'n/a'
    med_finish_time_hm = my_utils.from_sec_to_hm(med_finish_time) if len(fin_time_rows) > 0 else 'n/a'

    s = str(contract).ljust(sep)
    s = s + (avg_finish_time_hm + ' (' + str(avg_finish_time) + ')').ljust(sep) + (med_finish_time_hm + ' (' + str(med_finish_time) + ')').ljust(sep)

    bar = '=' * 50
    s = bar + '\n' + col + '\n' + s + '\n' + bar + '\n'
    return s


def make_table_time_sol_cont (rows):
    sep = 17
    sep2 = 8

    col = 'SolCont'.ljust(sep) + 'Avg_EndTime'.ljust(sep) + 'Med_EndTime'.ljust(sep)

    # choose solution contracts
    rows = list(filter(lambda r: r['alarm_pat'] == 0, rows))
    contract = len(rows)

    fin_time_rows =  list(filter(lambda r: isinstance(r['time'], float), rows))
    fin_time_rows = list(map(lambda r: r['time'], fin_time_rows))
    avg_finish_time = round(statistics.mean(fin_time_rows)) if len(fin_time_rows) > 0 else 'n/a'
    med_finish_time = round(statistics.median(fin_time_rows)) if len(fin_time_rows) > 0 else 'n/a'
    avg_finish_time_hm = my_utils.from_sec_to_hm(avg_finish_time) if len(fin_time_rows) > 0 else 'n/a'
    med_finish_time_hm = my_utils.from_sec_to_hm(med_finish_time) if len(fin_time_rows) > 0 else 'n/a'

    s = str(contract).ljust(sep)
    s = s + (avg_finish_time_hm + ' (' + str(avg_finish_time) + ')').ljust(sep) + (med_finish_time_hm + ' (' + str(med_finish_time) + ')').ljust(sep)

    bar = '=' * 50
    s = bar + '\n' + col + '\n' + s + '\n' + bar + '\n'
    return s


def filter_lineno(rows, min_line, max_line):
    res = []
    for row in rows:
        line = my_utils.get_lineno(row['file'].replace('.sol',''))
        if int(line) >= min_line and int(line) <= max_line:
            res.append(row)
    return res


def print_store_result(directory, rows, prefix):
    rows_io = list(filter (lambda row: row['dataset'] == IO, rows))
    rows_re = list(filter (lambda row: row['dataset'] == RE, rows))
    rows_tx = list(filter (lambda row: row['dataset'] == TX, rows))
    rows_ls = list(filter (lambda row: row['dataset'] == LS, rows))

    title = "* Fix Result (Annotated Bugs) \n"
    stat = title + make_table_bug (rows)

    title = "* Fix Result (Solution Contract - ALL) \n"
    stat = stat + '\n' + title + make_table_sol_cont (rows)

    title = "* Fix Result (Solution Contract - IO) \n"
    stat = stat + '\n' + title + '\n' + make_table_sol_cont (rows_io)

    title = "* Fix Result (Solution Contract - RE) \n"
    stat = stat + '\n' + title + make_table_sol_cont (rows_re)

    title = "* Fix Result (Solution Contract - TX) \n"
    stat = stat + '\n' + title + make_table_sol_cont (rows_tx)

    title = "* Fix Result (Solution Contract - LS) \n"
    stat = stat + '\n' + title + make_table_sol_cont (rows_ls)

    title = "* Time (ALL) \n"
    stat = stat + '\n' + title + make_table_time_all (rows)

    title = "* Time (IO) \n"
    stat = stat + '\n' + title + make_table_time_all (rows_io)

    title = "* Time (RE) \n"
    stat = stat + '\n' + title + make_table_time_all (rows_re)

    title = "* Time (TX) \n"
    stat = stat + '\n' + title + make_table_time_all (rows_tx)

    title = "* Time (LS) \n"
    stat = stat + '\n' + title + make_table_time_all (rows_ls)

    title = "* Time (Solution Contract) \n"
    stat = stat + '\n' + title + make_table_time_sol_cont (rows)

    title = "* Patch Size \n"
    stat = stat + '\n' + title + make_table_patch_size (rows)

    summary_dir = os.path.join(directory, prefix + '_summary.csv')
    stat_dir = os.path.join(directory, prefix + '_stat.txt')

    with open(summary_dir, 'w') as fp:
        writer = csv.DictWriter(fp, fieldnames=fieldnames)
        writer.writeheader()
        writer.writerows(rows)

    with open(stat_dir, 'w') as fp:
        fp.write(stat + '\n')

    print("########## " + prefix.upper() + " ##########")
    print('Result file generated : ' + summary_dir)
    print('Stat file generated   : ' + stat_dir)
    print('')
    print(stat)
    print('\n\n')

def main():
    parser = argparse.ArgumentParser(epilog='python3 mark_smartfix.py --directory dir')
    parser.add_argument ('--directory', type=str, help='root output dir')
    parser.add_argument ('--dataset', type=str, help='{io,ls,re,tx}')

    args = parser.parse_args()

    directory = args.directory
    dataset = args.dataset
    dataset = [IO,LS,RE,TX] if dataset == None else dataset.split(',')

    rows = []
    for d in dataset:
        # print(d)
        assert (d == IO or d == LS or d == RE or d == TX)
        rootdir = os.path.join(directory,d)
        rows += aggregate (rootdir, d)

    large = filter_lineno (rows, 501, 999999)

    print_store_result (directory, rows, 'all')
    print_store_result (directory, large, 'large')


if __name__ == "__main__":
    main ()
