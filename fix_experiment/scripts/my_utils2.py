import csv
import os
import sys
import json
from os.path import expanduser
from datetime import timedelta


def merge_knowledges (outdir, fid):
    dirs = [os.path.join(outdir,d)  for d in os.listdir(outdir) if os.path.isdir(os.path.join(outdir,d))]

    acc = {}

    for d in dirs:
      if not os.path.exists(os.path.join(d,'new_knowledge.json')):
        continue
      with open (os.path.join(d,'new_knowledge.json'),'r') as fp:
        j = json.load(fp)
        # print('===========')
        # print(d)
        # for k in j['knowledge']:
        #    print(k['abs_patch'])
        #    print(',\n'.join(k['abs_patch']))
        #    print('')
        # assert(False)
        acc['src'] = list( set(acc.get('src',[]) + j['src']) )
        acc['knowledge'] = acc.get('knowledge',[]) + j['knowledge']

    res = {}
    if len(acc) == 0: # empty dictionary check
      res = {
        'src': [],
        'knowledge': []
      }

    else:
      res = {
        'src': acc['src'],
        'knowledge': acc['knowledge'] #  list(acc.values())
      }

    with open(os.path.join(outdir, f'knowledge_{fid}.json'), 'w') as fp:
      json.dump(res, fp, indent=4) # sort_keys=True)

# def main():
#    merge_knowledges('out', 'test')

# if __name__ == "__main__":
#    main ()
