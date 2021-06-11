#!/usr/bin/env python3

import io
import os
import datetime
import argparse
import subprocess

from PIL import Image, ImageFont, ImageDraw


p = argparse.ArgumentParser()
p.add_argument("-i", "--input", default=os.path.expanduser("~/Sync/images/signature.png"), type=argparse.FileType())
p.add_argument("-f", "--font", default="/usr/share/fonts/google-droid-sans-fonts/DroidSans-Bold.ttf", type=argparse.FileType())
a = p.parse_args()

img_a = Image.open(a.input.name)

w = img_a.size[0] + 120
h = img_a.size[1]

img_b = Image.new(img_a.mode, (w, h))
img_b.paste(img_a, (60, 0))

font = ImageFont.truetype(a.font.name, 50)

now = datetime.datetime.now()
text = now.strftime("%D")

draw = ImageDraw.Draw(img_b)
text_w, text_h = draw.textsize(text, font=font)
text_pos = (w//2 - text_w//2, h//2 - text_h//2 - 20)
draw.text(text_pos, text, (0, 0, 0, 255), font=font)

mem = io.BytesIO()
img_b.save(mem, format="png")

cmd = ["xclip", "-selection", "clipboard", "-target", "image/png", "-i"]
output = subprocess.Popen(cmd, stdin=subprocess.PIPE)
output.stdin.write(mem.getvalue())
output.stdin.close()
