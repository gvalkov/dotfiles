#!/usr/bin/env python3
# -*- coding: utf-8; -*-

import os

from subprocess import run
from i3pystatus import Status

is_notebook = os.path.isdir('/proc/acpi/battery/')

status = Status(interval=1)
status.register('clock', format='%X')
status.register('clock', format='%a %-d %b W%V', interval=30)
status.register('temp', format='{temp:.0f}°C')

if is_notebook:
	batfmt = 'Bat: {status}{percentage:.2f}% {remaining:%E%hh:%Mm}'
	status.register('battery', format=batfmt, alert=True, alert_percentage=10, status={'DIS': '↓', 'CHR': '↑', 'FULL': ''})

status.register('pulseaudio', format='♪{volume}')
status.register('load', format='Load: {avg1} {avg5} {avg15}')
status.register('mem', format='Mem: {percent_used_mem:.2f}%')
status.run()
