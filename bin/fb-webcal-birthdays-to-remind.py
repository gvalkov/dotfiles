#!/usr/bin/env python

from sys import stdin


last = lambda x: x.split(':')[-1].rstrip('\r\n')
m = (None, 'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',
           'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')

# fmt = 'REM {} {} +7 MSG {} [since()] birthday is %b%'
fmt = 'REM {} {} +7 MSG {} birthday is %b%'

def parse():
    line, summary = None, None
    for line in stdin.readlines():
        if line.startswith('SUMMARY'):
            summary = last(line)
        if line.startswith('DTSTART'):
            dtstart = last(line)

        if line and summary:
            yield dtstart, summary
            dtstart, summary = None, None

for dtstart, summary in parse():
    name = ''.join(summary.split('Birthday')[0])
    month = m[int(dtstart[4:6])]
    day = dtstart[6:8]
    print(fmt.format(day, month, name))
