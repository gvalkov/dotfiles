#!/usr/bin/env python3
# -*- coding: utf-8; -*-

import os
from subprocess import run

import netifaces
from i3pystatus import Status

terminal = 'gnome-terminal'

is_notebook = os.path.isdir('/proc/acpi/battery/')
ifaces = netifaces.interfaces()

lan1 = next(( i for i in ifaces if i.startswith('enp')), None)

status = Status(interval=1)
status.register('clock', format='%X')
status.register('clock', format='%a %-d %b W%V', interval=30)
status.register('temp', format='{temp:.0f}°C')

if is_notebook:
	batfmt = 'Bat: {status}{percentage:.2f}% {remaining:%E%hh:%Mm}'
	status.register('battery', format=batfmt, alert=True, alert_percentage=10, status={'DIS': '↓', 'CHR': '↑', 'FULL': ''})

status.register('pulseaudio', format='♪{volume}')
status.register('cpu_usage_graph', format='{cpu_graph}', start_color='green', end_color='red', graph_width=7, graph_style='braille-fill')
status.register('load', format='Load: {avg1} {avg5} {avg15}', on_leftclick=f'{terminal} -e "htop --sort-key=PERCENT_CPU"')
status.register('mem', format='Mem: {percent_used_mem:.2f}%', on_leftclick=f'{terminal} -e "htop --sort-key=PERCENT_MEM"')

status.register('disk', path='/', format=' {free}/{total}', interval=300)
status.register('disk', path='/home/gv/', format=' {free}/{total}', on_leftclick=f'{terminal} -e "ncdu $HOME"', interval=300)

if lan1:
    status.register('network', interface=lan1,
                    format_up=' {interface} {kbs}KB/s',
                    format_down='wired is down',
                    dynamic_color=False, graph_width=5,
    )

status.run()
