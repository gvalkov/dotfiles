#!/usr/bin/env python
# encoding: utf-8

from re import compile
from subprocess import check_output
from collections import Counter

authors = Counter()
cmd = ('git', 'log', '--patch', '--find-renames', '--find-copies-harder')

re_diffpm = compile(r'^[\+\-]')
re_commit = compile(r'^commit [0-9A-Fa-f]{40}$')

def itercommits(log):
  commit = []
  for line in log.splitlines():
    if re_commit.match(line) and commit:
      yield commit
      commit = []
    commit.append(line)

def index(l, s):
  for n,line in enumerate(l):
    if line.startswith(s):
      return n

def rank(commit):
  author = commit[index(commit, 'Author: ')]
  author = author.split('Author: ', 1)[-1]
  rank = 0

  for line in commit[index(commit, '+++'):]:
    if re_diffpm.match(line):
      rank += 1

    return author, rank

commits = itercommits(check_output(cmd))
ranks = map(rank, commits)

for author, rank in ranks:
  authors[author] += rank

for author, rank in authors.most_common():
  print('{} {}'.format(rank, author))
