#!/usr/bin/env python
# encoding: utf-8

"""
https://raw.github.com/kennethreitz/spark.py/master/spark.py
A port of @holman's spark project for Python.
"""

import sys

ticks = u'▁▂▃▅▆▇'


def spark_string(ints):
    """Returns a spark string from given iterable of ints."""
    step = ((max(ints)) / float(len(ticks) - 1)) or 1
    return u''.join(ticks[int(round(i / step))] for i in ints)


def spark_print(ints, stream=None):
    """Prints spark to given stream."""
    if stream is None:
        stream = sys.stdout
    stream.write(spark_string(ints).encode('utf-8'))

if __name__ == '__main__':
    if len(sys.argv) > 1:
        sparks = map(int, sys.argv[1:])
        spark_print(sparks)
        print
    else:
        print "spark\n"
        print "USAGE:"
        print "  spark.py [space separated values]\n"
        print "EXAMPLES:"
        print "  spark.py 1 5 22 13 53"
        spark_print([1, 5, 22, 13, 53])
        print
        print "  spark.py 0 30 55 80 33 150"
        spark_print([0, 30, 55, 80, 33, 150])
        print
