#!/usr/bin/env python3

import os
from subprocess import run
from glob import glob

import netifaces
from i3pystatus import Status

terminal = 'gnome-terminal'

is_notebook = os.path.isdir('/proc/acpi/battery/') or glob('/sys/class/power_supply/BAT*')

status = Status(interval=1)
status.register('clock', format='%X')
status.register('clock', format='%a %-d %b W%V', interval=30)
#status.register('temp', format='{temp:.0f}°C')

if is_notebook:
	batfmt = 'Bat: {status}{percentage:.2f}% {remaining:%E%hh:%Mm}'
	status.register('battery', format=batfmt, alert=True, alert_percentage=10, status={'DIS': '↓', 'CHR': '↑', 'FULL': ''})

status.register('pulseaudio', format='♪{volume}')
status.register('load', format='{avg1} {avg5} {avg15}', on_leftclick=f'{terminal} -e "htop --sort-key=PERCENT_CPU"')
status.register('mem', format='Mem: {percent_used_mem:.2f}%', on_leftclick=f'{terminal} -e "htop --sort-key=PERCENT_MEM"')

ifaces = netifaces.interfaces()
ifaces = sorted(ifaces, key=len)
lan1 = next(( i for i in ifaces if i.startswith('enp')), None)
if lan1:
    status.register('network', interface=lan1,
                    format_up=u'{interface} {kbs}KB/s',
                    format_down='wired is down',
                    dynamic_color=False, graph_width=5,
    )

status.run()
