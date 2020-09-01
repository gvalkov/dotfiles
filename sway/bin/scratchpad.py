#!/usr/bin/env python3

import sys
import i3ipc
import shlex

ratio_w, ratio_h, mark_name, match, *cmd = sys.argv[1:]
match_type, match_value = match.split(':', 1)


i3 = i3ipc.Connection()
wn = i3.get_tree().find_marked(mark_name)
ws = i3.get_tree().find_focused().workspace()

# Calculate window position
ratio_w, ratio_h = float(ratio_w), float(ratio_h)

ws_x, ws_y = ws.rect.x, ws.rect.y
ws_w, ws_h = ws.rect.width, ws.rect.height

win_w = int(ws_w * ratio_w)
win_h = int(ws_h * ratio_h)
win_x = ws_x
win_y = ws_y

if wn:
    wn = wn[-1]
    wn.command(
        f'[con_mark="{mark_name}"],'
        f'scratchpad show'
    )
    exit(0)

def on_window_new(self, event):
    con = event.container

    if match_type == 'role' and con.window_role != match_value:
        return
    elif match_type == 'app_id' and event.ipc_data['container']['app_id'] == match_value:
        return
    elif match_type == 'name' and con.name != match_value:
        return
    elif match_type == 'class' and con.window_class != match_value:
        return

    con.command(
        f'mark {mark_name},'
        f'floating enable,'
        f'move scratchpad,'
        f'resize set {win_w} px {win_h} px,'
        f'scratchpad show,'
        f'move absolute position {win_x}px {win_y}px'
    )
    exit(0)

i3.on('window', on_window_new)

quoted_cmd = ' '.join(shlex.quote(i) for i in cmd)
i3.command('exec %s' % quoted_cmd)

i3.main()
