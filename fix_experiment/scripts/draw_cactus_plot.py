from matplotlib import pyplot as plt
import csv
import sys
import os
import argparse
from os.path import expanduser
from datetime import datetime
import statistics
import my_utils

# https://tex.stackexchange.com/a/508961
plt.rcParams['pdf.fonttype'] = 42

HOME = expanduser("~")
WORK_SPACE = os.path.join(HOME, 'fix_experiment')
OUT_DIR = os.path.join(HOME, 'fix_result', 'variant_comparison')

IO = 'io'
LS = 'ls'
RE = 're'
TX = 'tx'

ALL = 'all'

fieldnames = ['dataset', 'id', 'base', 'on', 'final']

def get_rows_from_csvfile (csvfile):
    if csvfile == None:
        print(f'[INFO] No input given : {csvfile}')
        return None
    if not os.path.exists(csvfile):
        print(f'[INFO] No file exists : {csvfile}')
        return None
    fp = open(csvfile, 'r')
    rows = list(csv.DictReader(fp))
    fp.close()
    return rows

def patch_generated_for_all_annotated_bugs(row):
    dataset = row['dataset']
    if dataset == IO:
        b = row['io_lab'] == row['io_gen']
        return b
    elif dataset == LS:
        b1 = row['el_lab'] == row['el_gen']
        b2 = row['su_lab'] == row['su_gen']
        return b1 and b2
    elif dataset == RE:
        b = row['re_lab'] == row['re_gen']
        return b
    elif dataset == TX:
        b = row['tx_lab'] == row['tx_gen']
        return b
    else:
        assert(False)

def is_solution_generated (row):
    alarm_patch = None
    try:
        alarm_patch = row['alarm_pat']
    except Exception as e: # old column name
        alarm_patch = row['alarm_sol']
    assert(alarm_patch != None)
    return alarm_patch == str (0)

def draw_cactus_plot(rows_base, rows_on, rows_final, timekind, timeout_1h):
    def make_dots(rows):
        if rows == None:
            return (None,None)

        rows = list(filter(lambda r: is_solution_generated (r), rows))
        rows = sorted(rows, key=lambda row: float(row[timekind]))
        xs, ys = [], []
        total_time = 0.0
        for (i,row) in enumerate(rows):
            # fname = row['file']
            total_time += float(row[timekind])
            if timeout_1h and float(row[timekind]) > 3600.0:
                continue
            xs.append(i+1) # fixed contracts
            unit = 3600.0
            ys.append(total_time / unit)
        assert(len(xs) > 0 and len(ys) > 0)
        return (xs,ys)

    (xs_base, ys_base) = make_dots(rows_base)
    (xs_on, ys_on) = make_dots(rows_on)
    (xs_final, ys_final) = make_dots(rows_final)

    # line style
    # '-': solid, '--': dashed, '-.': dashdot, ':', dotted
    linewidth = 4
    if xs_base!=None and ys_base!=None:
        plt.plot (xs_base, ys_base, linestyle=':', linewidth=linewidth, label='Basic')
    if xs_on!=None and ys_on!=None:
        plt.plot (xs_on, ys_on, linestyle='--', linewidth=linewidth, label='Online')
    if xs_final!=None and ys_final!=None:
        plt.plot (xs_final, ys_final, linestyle='-', linewidth=linewidth, label='Online+Offline')

    x_label = '# generated solution contracts'
    y_label = 'cumulative runtime (h)'

    plt.xlabel (x_label, fontsize = 18)
    plt.ylabel (y_label, fontsize = 18)

    def mymax (lst):
        return 0 if lst == None else max(lst)
    # print(ys_on)
    # assert(False)

    x_end = max(mymax(xs_base), mymax(xs_on), mymax(xs_final))
    y_end = max(mymax(ys_base), mymax(ys_on), mymax(ys_final))
    plt.xlim (0, x_end)
    plt.ylim (0, y_end)

    plt.grid()
    plt.legend(fontsize = 15, loc = 'upper left')

    pdfname = f'cactus-{timekind}-1h.pdf' if timeout_1h else f'cactus.pdf'
    fig_path = os.path.join(OUT_DIR, pdfname)
    plt.savefig(fig_path)
    plt.clf()

    print('[INFO] file generated : ' + fig_path)


def make_sol_gen_time_rows(rows_base, rows_on, rows_final):

    res = []
    for (r_base, r_on, r_final) in zip(rows_base, rows_on, rows_final):
        assert(r_base['dataset'] == r_on['dataset'] and r_on['dataset'] == r_final['dataset'])
        assert(r_base['file'] == r_on['file'] and r_on['file'] == r_final['file'])
        dataset = r_base['dataset']
        fid = (r_base['file'])[:-4]
        time_base = float(r_base['time']) if is_solution_generated(r_base) else 'n/a'
        time_on = float(r_on['time']) if is_solution_generated(r_on) else 'n/a'
        time_final = float(r_final['time']) if is_solution_generated(r_final) else 'n/a'

        res.append({
          'dataset': dataset,
          'id': fid,
          'base': time_base,
          'on': time_on,
          'final': time_final 
        })

    return res


def make_str_gen_sol (rows, timeout_30m):
    sep = 15

    col = 'Contract'.ljust(sep) + 'Base_Sol'.ljust(sep) + 'Online_Sol'.ljust(sep) + 'Final_Sol'.ljust(sep)

    contract = len(rows)

    base_sol = list(filter (lambda r: isinstance(r['base'], float), rows))
    on_sol = list(filter (lambda r: isinstance(r['on'], float), rows))
    final_sol = list(filter (lambda r: isinstance(r['final'], float), rows))

    if timeout_30m:
        base_sol = list(filter (lambda r: r['base'] <= 1800.0, base_sol))
        on_sol = list(filter (lambda r: r['on'] <= 1800.0, on_sol))
        final_sol = list(filter (lambda r: r['final'] <= 1800.0, final_sol))

    bar = '=' * 60
    s = str(contract).ljust(sep) + str(len(base_sol)).ljust(sep) + str(len(on_sol)).ljust(sep) + str(len(final_sol)).ljust(sep)
    s = bar + '\n' + col + '\n' + s + '\n' + bar

    if timeout_30m:
        return s

    # get diff
    diff_on_final = list(filter(lambda r: isinstance(r['on'], float) and r['final'] == 'n/a', rows))
    diff_on_final = list(map(lambda r: '- ' + r['dataset'] + ', ' + r['id'], diff_on_final))

    diff_final_on = list(filter(lambda r: r['on'] == 'n/a' and isinstance(r['final'], float), rows))
    diff_final_on = list(map(lambda r: '- ' + r['dataset'] + ', ' + r['id'], diff_final_on))

    diff1 = '# Online - Final : ' + str(len(diff_on_final))
    diff1_detail = '\n'.join(diff_on_final)
    diff2 = '# Final - Online : ' + str(len(diff_final_on))
    diff2_detail = '\n'.join(diff_final_on)
    s2 = diff1 + '\n' + diff1_detail + '\n' + diff2 + '\n' + diff2_detail
    
    return s + '\n' + s2

def make_str_common_sol_gen_time (rows):
    sep = 15

    col = 'Common_Sol'.ljust(sep) + 'Base'.ljust(sep) + 'Online'.ljust(sep) + 'Final'.ljust(sep)

    common_rows = list(filter (lambda r: isinstance(r['base'], float) and isinstance(r['on'], float) and isinstance(r['final'], float), rows))
    common = len(common_rows)

    base_times = list(map(lambda r: r['base'], common_rows))
    on_times = list(map(lambda r: r['on'], common_rows))
    final_times = list(map(lambda r: r['final'], common_rows))

    base_avg = statistics.mean(base_times)
    on_avg = statistics.mean(on_times)
    final_avg = statistics.mean(final_times)

    base_avg_m = my_utils.from_sec_to_hm(round(base_avg))
    on_avg_m = my_utils.from_sec_to_hm(round(on_avg))
    final_avg_m = my_utils.from_sec_to_hm(round(final_avg))

    bar = '=' * 60
    s = str(common).ljust(sep) + str(base_avg_m).ljust(sep) + str(on_avg_m).ljust(sep) + str(final_avg_m).ljust(sep)
    s = bar + '\n' + col + '\n' + s + '\n' + bar

    return s


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('--base', type=str, help='csv result of baseline smartfix')
    parser.add_argument('--on', type=str, help='csv result of smartfix without offline learning')
    parser.add_argument('--final', type=str, help='csv result of final smartfix')
    # parser.add_argument('--dataset', type=str, help='{io,ls,re,tx}')
    parser.add_argument('--outdir', type=str, help='output directory for graphs')

    args = parser.parse_args()

    # mydate = datetime.now().strftime('%m%d_%H%M')
    csv_base = args.base
    csv_on = args.on # on with diff
    csv_final = args.final

    rows_base = get_rows_from_csvfile(csv_base)
    rows_on = get_rows_from_csvfile(csv_on)
    rows_final = get_rows_from_csvfile(csv_final)

    draw_cactus_plot(rows_base, rows_on, rows_final, 'time', False)

    time_rows = make_sol_gen_time_rows(rows_base, rows_on, rows_final)
    s1 = make_str_common_sol_gen_time(time_rows)
    s2 = make_str_gen_sol(time_rows, False)
    s3 = make_str_gen_sol(time_rows, True)

    src_info_path = f'{OUT_DIR}/src_info.txt'
    with open (src_info_path, 'w') as fp:
        fp.write(csv_base + '\n')
        fp.write(csv_on + '\n')
        fp.write(csv_final + '\n')
    print('[INFO] file generated : ' + src_info_path)

    per_file_path = f'{OUT_DIR}/per_file.csv'
    with open(per_file_path, 'w') as fp:
        writer = csv.DictWriter(fp, fieldnames=fieldnames)
        writer.writeheader()
        writer.writerows(time_rows)
    print('[INFO] file generated : ' + per_file_path)

    stat_path = f'{OUT_DIR}/stat.txt'
    with open(stat_path, 'w') as fp:
        fp.write(s1 + '\n\n')
        fp.write(s2 + '\n')
    print('[INFO] file generated : ' + stat_path)

    stat_30m_path = f'{OUT_DIR}/stat_timeout_30m.txt'
    with open(stat_30m_path, 'w') as fp:
        fp.write(s3 + '\n')
    print('[INFO] file generated : ' + stat_30m_path)

if __name__ == '__main__':
    main ()
