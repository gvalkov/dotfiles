#!/usr/bin/env python3

from itertools import chain
from subprocess import check_output, Popen, PIPE


run = lambda x: check_output(x, shell=True)

def revs():
    for rev in run('git rev-list --all').splitlines():
        yield rev.decode('utf8')

def blobs(rev):
    cmd = 'git diff-tree -r -c -M -C --no-commit-id %s'
    for line in run(cmd % rev).splitlines():
        line = line.split()
        blob = line[2] if len(line[2]) == 40 else line[3]
        if blob == b'0' * 40:
            continue
        yield blob, line[-1].decode('utf8')

def catfile():
    cmd = 'git cat-file --batch-check="%(objectname) %(objectsize)"'
    proc = Popen(cmd, stdin=PIPE, stdout=PIPE, shell=True)
    while True:
        blob = yield
        if blob == 0:
            break

        proc.stdin.write(blob + b'\n')
        proc.stdin.flush()

        line = proc.stdout.readline().decode('utf8').rstrip()
        if line.endswith('missing'):
            continue

        _, size = line.rstrip().split()
        yield int(size)

    proc.stdin.close()
    proc.wait()


seen = set()
sizes = []

cf = catfile(); next(cf)
for blob, fn in chain(*(blobs(rev) for rev in revs())):
    if blob in seen:
        continue
    seen.add(blob)

    size = cf.send(blob)
    if size:
        sizes.append((size, blob.decode('utf8'), fn))

try:
    cf.send(0)
except StopIteration:
    pass

for size, blob, fn in reversed(sorted(sizes)):
    res = '{blob!s} {size}\n  - {fn}'.format_map(vars())
    print(res)
