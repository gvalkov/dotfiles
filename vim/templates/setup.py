#!/usr/bin/env python
# encoding: utf-8

import os

from os.path import abspath, dirname
from distutils import sysconfig
from setuptools import setup, find_packages, Extension


here = abspath(dirname(__file__))


requires = []
test_requires = []

classifiers = [
    'Development Status :: 1 - Planning'
    'Programming Language :: Python',
    'License :: OSI Approved :: BSD License',
    'Operating System :: Linux',
]

scripts = [
]

# Extension modules
#module_c = Extension('proj._module',  sources = ['proj/source.c'],  extra_compile_args=['-O0'])
#ext_modules = [module_c]

kw = {
    'name'                 : '',
    #'version'              : version(),

    'description'          : 'tbd',
    'long_description'     : 'tbd',

    'author'               : 'Georgi Valkov',
    'author_email'         : 'georgi.t.valkov@gmail.com',

    'license'              : 'New BSD License',

    'keywords'             : '',
    'classifiers'          : classifiers,

    'url'                  : 'tbd',

    'packages'             : find_packages(),

    #'ext_modules'          : ext_modules,
    #'install_requires'     : requires,
    #'test_requires'        : test_requires,

    'entry_points'         : {
        'console_scripts'  : ['project = project.module:method']
    },

    'scripts'              : scripts,

    'include_package_data' : True,
    'zip_safe'             : False,
}


setup(**kw)
