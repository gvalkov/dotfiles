#!/usr/bin/env python3

import sys
import i3ipc


mark_name, match, cmd = sys.argv[1:]
match_type, match_value = match.split(':', 1)

i3 = i3ipc.Connection()
wn = i3.get_tree().find_marked(mark_name)

if wn:
	wn[-1].command('[con_mark="%s"] scratchpad show' % mark_name)
	exit(0)

def on_window_new(self, event):
	con = event.container

	if match_type == 'role' and con.window_role != match_value:
		return
	elif match_type == 'name' and con.name != match_value:
		return

	con.command('mark %s' % mark_name)
	con.command('move scratchpad')
	con.command('scratchpad show')
	exit(0)

i3.on('window::new', on_window_new)
i3.command('exec %s' % cmd)
i3.main()
