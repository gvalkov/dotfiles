#!/usr/bin/env python
# encoding: utf-8


from os  import getenv, unlink, system
from sys import exit, argv, stdout
from operator import itemgetter

from cPickle import dump, load
from subprocess import check_output, Popen, PIPE

cache_fn = getenv('HOME') + '/.cache/dmenu-launch.pickle'

def create_cache():
    # all executables in PATH
    cmd = getenv('PATH').split(':')
    cmd.insert(0, 'lsx')

    cmd_list = check_output(cmd).splitlines()

    return dict.fromkeys(cmd_list, 0)

def write_cache(cache):
    fh = open(cache_fn, 'w')
    dump(cache, fh)

def read_cache():
    try:
        fh = open(cache_fn, 'rb+')
        cache = load(fh)
    except:
        cache = create_cache()
        write_cache(cache)

    return cache

def update_cache(cache):
    new_cache = create_cache()
    new_cache.update(cache)
    write_cache(new_cache)
    return new_cache

def dmenu(l):
    cmd = ['dmenu']
    cmd.extend(argv[1:])
    p = Popen(cmd, stdin=PIPE, stdout=PIPE)

    out, err = p.communicate('\n'.join(l))
    return out

cache = read_cache()
cache = update_cache(cache)
cache_items = cache.items()

cache_items.sort(key=itemgetter(1))
cache_items.reverse()
cache_items = map(itemgetter(0), cache_items)

commands = {
        'dmenu-cache-clear'  : lambda : unlink(cache_fn),
        'dmenu-cache-update' : lambda : update_cache(cache),
    }

cache_items.extend(commands.keys())

choice = dmenu(cache_items).rstrip()

if len(choice) == 0:
    exit(1)

if choice in commands:
    commands[choice]()
    exit(0)

if choice in cache:
    cache[choice] += 1

write_cache(cache)

system(choice)
#stdout.write(choice)
#stdout.flush()
