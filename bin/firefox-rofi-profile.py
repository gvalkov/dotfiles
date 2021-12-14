#!/usr/bin/env python3

import os
import re
import sys
import subprocess

PROFILES_DIR = os.path.expanduser("~/.mozilla/firefox/")
FIREFOX_BIN = "/usr/bin/firefox"

text = open(PROFILES_DIR + "profiles.ini").read()
profiles = dict(
    zip(
        re.findall("Name=(.*)", text, re.MULTILINE),
        re.findall("Path=(.*)", text, re.MULTILINE),
    )
)

choices = b"\n".join(i.encode("utf8") for i in profiles)
profile_name = (subprocess.check_output(("rofi", "-dmenu"), input=choices).strip().decode("utf8"))
profile_path = profiles[profile_name]

os.execl(FIREFOX_BIN, FIREFOX_BIN, "--profile", PROFILES_DIR + profile_path)
