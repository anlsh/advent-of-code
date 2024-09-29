#!/usr/bin/python3
import sys

inp = sys.stdin.readlines()

def sa_and_smallest(l, w, h):
    lw = l*w
    lh = l*h
    wh = w*h

    return 2 * (lw + lh + wh), min(lw, lh, wh)

def ribbonlength(l, w, h):
    k = sorted([l, w, h])
    return 2 * (k[0] + k[1]) + (l * w * h)

s = 0
r = 0
for box in inp:
    sides = [int(x) for x in box.split("x")]
    sa, smallest = sa_and_smallest(*sides)
    s += sa + smallest
    r += ribbonlength(*sides)

print(s)
print(r)
