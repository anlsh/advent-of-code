#!/usr/bin/python3
import sys
inp = sys.stdin.readlines()[0]

def get_offset(c):
    if c == "v":
        return (0, -1)
    elif c == "^":
        return (0, 1)
    elif c == ">":
        return (1, 0)
    elif c == "<":
        return (-1, 0)
    else:
        raise RuntimeError("FUCK")

def simulate_santa(offsets):
    pos = (0, 0)
    num_visits = {pos: 1}

    for o in offsets:
        pos = (pos[0] + o[0], pos[1] + o[1])
        num_visits[pos] = num_visits.get(pos, 0) + 1

    return num_visits

counts = simulate_santa(map(get_offset, inp))

print(len(counts))

santa_inputs = [c for i, c in enumerate(inp) if i % 2 == 0]
robo_inputs = [c for i, c in enumerate(inp) if i % 2 == 1]

santa_counts = simulate_santa(map(get_offset, santa_inputs))
robo_counts = simulate_santa(map(get_offset, robo_inputs))

def sum_dicts(d1, d2):
    s = {}
    for d in [d1, d2]:
        for key in d:
            s[key] = s.get(key, 0) + d[key]
    return s

print(len(sum_dicts(santa_counts, robo_counts)))
