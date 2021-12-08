import re
import sys
import math

def get_input(fname):
    lines = None
    with open(fname, "r") as f:
        lines = f.readlines()

    outs = []
    for line in lines:
        s = line.split(" | ")[1][:-1]
        outs.append(s.split(" "))
    return outs

if __name__ == "__main__":
    inp = get_input(sys.argv[1])

    s = 0
    for outs in inp:
        for w in outs:
            if len(w) in [2, 4, 3, 7]:
                s += 1

    print(s)
