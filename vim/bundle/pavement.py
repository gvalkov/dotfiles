#!/usr/bin/env python

from sys import stdout
from shutil import rmtree
from subprocess import call
from paver.easy import *

# submodules/subtrees, you have failed me (history isn't that important)

bundles = {
'fugitive.git'         : 'https://github.com/tpope/vim-fugitive.git',
'nerdtree.git'         : 'https://github.com/scrooloose/nerdtree.git',
'surround.git'         : 'https://github.com/tpope/vim-surround.git',
'pyflakes.git'         : 'https://github.com/kevinw/pyflakes-vim.git',
'easytags.git'         : 'https://github.com/xolox/vim-easytags.git',
'quickrun.git'         : 'https://github.com/thinca/vim-quickrun',
'pathogen.git'         : 'https://github.com/tpope/vim-pathogen.git',
'UltiSnips.git'        : 'https://github.com/vim-scripts/UltiSnips',
'FuzzyFinder.git'      : 'https://github.com/vim-scripts/FuzzyFinder.git',
'L9.git'               : 'https://github.com/vim-scripts/L9.git',
'delimitMate.git'      : 'https://github.com/Raimondi/delimitMate.git',
'scss-syntax.git'      : 'https://github.com/cakebaker/scss-syntax.vim.git',
'nerdcommenter.git'    : 'https://github.com/scrooloose/nerdcommenter.git',
'blackboard.git'       : 'https://github.com/nelstrom/vim-blackboard.git',
'colors-solarized.git' : 'https://github.com/altercation/vim-colors-solarized.git',
'vim-git.git'          : 'https://github.com/tpope/vim-git.git',
'unimpaired.git'       : 'https://github.com/tpope/vim-unimpaired.git',
'YankRing.vim.git'     : 'https://github.com/vim-scripts/YankRing.vim.git',
'speeddating.git'      : 'https://github.com/tpope/vim-speeddating.git',
'NrrwRgn.git'          : 'https://github.com/chrisbra/NrrwRgn.git',
'tagbar.git'           : 'https://github.com/majutsushi/tagbar.git',
'repeat.git'           : 'https://github.com/tpope/vim-repeat.git',
'notes.git'            : 'https://github.com/xolox/vim-notes.git',
'ropevim.hg'           : 'https://bitbucket.org/agr/ropevim',
'ropemode.hg'          : 'https://bitbucket.org/agr/ropemode',
'eunuch.git'           : 'https://github.com/tpope/vim-eunuch.git',
'syntastic.git'        : 'https://github.com/scrooloose/syntastic',
'indent-guides.git'    : 'https://github.com/nathanaelkane/vim-indent-guides.git',
'indent-object.git'    : 'https://github.com/michaeljsmith/vim-indent-object.git',
'easymotion.git'       : 'https://github.com/Lokaltog/vim-easymotion.git',
'gundo.git'            : 'https://github.com/sjl/gundo.vim.git',
'zoomwin.git'          : 'https://github.com/vim-scripts/ZoomWin.git',
'ack.git'              : 'https://github.com/mileszs/ack.vim.git',
'coffee-script.git'    : 'https://github.com/kchmck/vim-coffee-script.git',
}



def subtree(prefix, url):
    cmd = 'git subtree add --squash --prefix "%(prefix)s" "%(url)s" master' % locals()
    call(cmd, shell=True)

def export_git(prefix, url):
    #cmd = 'git archive --format=tar --remote "%(url)s master"'
    cmd = 'git clone %(url)s %(prefix)s && rm -rf %(prefix)s/.git' % locals()
    call(cmd, shell=True)

def export_hg(prefix, url):
    cmd = 'hg clone %(url)s %(prefix)s && rm -rf %(prefix)s/.hg' % locals()
    call(cmd, shell=True)

@task
def status(options, info):
    'show bundle status'

    #dirs = path('./').dirs()
    #missing = [i for i in bundles if i not in [j.name for j in dirs]]

    for name, remote in bundles.iteritems():
        out = [name]
        p = path(name)

        out.append('installed') if p.exists() else out.append('missing')

        if any([p.joinpath(i).exists() for i in ('.git', '.hg')]):
            out.append('clone')
        else:
            out.append('export')

        print '%-23s %-10s %-10s' % tuple(out)

@task
def remove(options, info):
    'remove bundles'

    for name in bundles:
        p = path(name)
        if p.exists():
            print 'removed %s' % name
            rmtree(name)

@task
def clone(options, info):
    'clone bundles'

@task
def export(options, info):
    'export bundles'
    for name, remote in bundles.iteritems():
        p = path(name)

        if  p.exists():
            stdout.write('%s ... skipped\n' % name)
            stdout.flush()
            continue

        if p.ext == '.git':
            export_git(name, remote)

        elif p.ext == '.hg':
            export_hg(name, remote)
