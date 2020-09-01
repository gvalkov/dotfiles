#!/usr/bin/env python3

import os
from glob import glob

import netifaces
from i3pystatus import Status

terminal = 'kitty'

is_notebook = os.path.isdir('/proc/acpi/battery/') or glob('/sys/class/power_supply/BAT*')

status = Status(interval=1)
status.register('clock', format='%X', interval=1)
status.register('clock', format='%a %-d %b W%V', interval=30)
#status.register('temp', format='{temp:.0f}°C')

if is_notebook:
	status.register(
        'battery',
        # format='{bar} {percentage_design:.2f}%{remaining:%E %hh:%Mm}',
        format='Bat: {status}{percentage:.2f}% {remaining:%E%hh:%Mm}',
        alert=True, alert_percentage=15,
        interval=10,
        status={'DIS': '↓', 'CHR': '↑', 'FULL': ''}
    )

status.register(
    'pulseaudio',
    format = '♪{volume}',
    bar_type = 'vertical',
    vertical_bar_width = 1,
)

status.register('load', format='{avg1} {avg5} {avg15}', on_leftclick=f'{terminal} -- htop --sort-key=PERCENT_CPU')
status.register('mem', format='Mem: {percent_used_mem:.2f}%', on_leftclick=f'{terminal} -- htop --sort-key=PERCENT_MEM')

ifaces = netifaces.interfaces()
ifaces = sorted(ifaces, key=len)
lan1 = next(( i for i in ifaces if i.startswith('enp')), None)
wlan1 = next(( i for i in ifaces if i.startswith('wlp')), None)

netkw = dict(dynamic_color=False, graph_width=5, format_up=u'{interface} {kbs}KB/s')
if lan1:
    status.register('network', interface=lan1, format_down='wired is down', **netkw)
if wlan1:
    status.register('network', interface=wlan1, format_down='wifi is down', **netkw)

status.run()
