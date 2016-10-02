#!/usr/bin/python

# Source: https://github.com/gregmccoy/GPMDP-PlayerInfo

#The MIT License (MIT)
#
#Copyright (c) 2016 Greg McCoy
#
#Permission is hereby granted, free of charge, to any person obtaining a copy
#of this software and associated documentation files (the "Software"), to deal
#in the Software without restriction, including without limitation the rights
#to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
#copies of the Software, and to permit persons to whom the Software is
#furnished to do so, subject to the following conditions:
#
#    The above copyright notice and this permission notice shall be included in all
#    copies or substantial portions of the Software.
#
#    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
#    IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
#    FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
#    AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
#    LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
#    OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
#    SOFTWARE.

import json
from os.path import expanduser
import sys
import argparse

home = expanduser("~")
parser = argparse.ArgumentParser(formatter_class=argparse.RawTextHelpFormatter, description="Parses and print Google Play Music Desktop Player song info")

def parseJson():
    try:
        with open(home + '/.config/Google Play Music Desktop Player/json_store/playback.json') as f:
            data = f.read()
    except:
        with open(home + '/GPMDP_STORE/playback.json') as f:
            data = f.read()
    return json.loads(data)

def getSong(data):
    return data['song']['title']

def getAlbum(data):
    return data['song']['album']

def getArtist(data):
    return data['song']['artist']

def convert_time(ms):
    x = ms / 1000
    x % 60
    m, s = divmod(x, 60)
    return "%d:%02d" % (m, s)
def getProgress(data):
    cur = data['time']['current']
    total = data['time']['total']
    return convert_time(cur) + "/" + convert_time(total)

def parseLayout(layout):
    displaystr = ""
    for i in layout:
        if i == 't':
            displaystr += getSong(data)
        elif i == 'a':
            displaystr += getAlbum(data)
        elif i == 'A':
            displaystr += getArtist(data)
        elif i == 'p':
            displaystr += getProgress(data)
        elif i == '-':
            displaystr += " - "
    return displaystr

def run(data, layout):
    displaystr = ""
    if data['playing']:
        displaystr = parseLayout(layout)
    else:
        sys.stdout.write("   ")
    print(displaystr)

parser.add_argument("--layout",
        action="store",
        dest="layout",
        help="t = Song Title\na = Song Album\nA = Artist Name\np = Track time progess\n- = Spacer\nExample: t-a-A-p",
    )
args = parser.parse_args()
data = parseJson()
try:
    run(data, args.layout)
except:
    run(data, "t-a-A-p")
