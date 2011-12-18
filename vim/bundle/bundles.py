#!/usr/bin/env python

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
}


def subtree(prefix, url):
    cmd = 'git subtree add --squash --prefix "%(prefix)s" "%(url)s" master' % locals()
    call(cmd, shell=True)

def export(prefix, url):
    #cmd = 'git archive --format=tar --remote "%(url)s master"'
    cmd = 'git clone %(url)s %(prefix)s && rm -rf %(prefix)s/.git' % locals()
    call(cmd, shell=True)

#if __name__ == '__main__':
    #from os.path import join
    #from os import chdir
    #from subprocess import call, check_output

    #root = check_output(['git', 'rev-parse', '--show-cdup']).rstrip()
    #bundle_dir = 'vim/bundle'

    #chdir(root if root else '.')
    #for d, url in bundles.iteritems():
        #prefix = join(bundle_dir, d)
        ##subtree(prefix, url)
        #export(prefix, url)

