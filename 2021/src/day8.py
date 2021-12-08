import re
import sys
import math

def get_input(fname):
    lines = None
    with open(fname, "r") as f:
        lines = f.readlines()

    ins = []
    outs = []
    for line in lines:
        s = line.split(" | ")
        s0 = s[0].split(" ")
        s1 = s[1][:-1].split(" ")
        ins.append(s0)
        outs.append(s1)
    return ins, outs

num_to_segments = {
    1: "cf",
    7: "acf",
    4: "bcdf",
    2: "acdeg",
    3: "acdfg",
    5: "abdfg",
    6: "abdefg",
    0: "abcefg",
    9: "abcdfg",
    8: "abcdefg",
}

segments_to_num = {}
for key in num_to_segments:
    segments_to_num[num_to_segments[key]] = key

alphabet = "abcdefg"

def make_mapping(patterns):
    f2real = {}
    for letter in alphabet:
        f2real[letter] = set(alphabet)

    for p in patterns:
        if len(p) == 2:
            for letter in p:
                f2real[letter] = set.intersection(f2real[letter], set(num_to_segments[1]))
        elif len(p) == 3:
            for letter in p:
                f2real[letter] = set.intersection(f2real[letter], set(num_to_segments[7]))
        elif len(p) == 4:
            for letter in p:
                f2real[letter] = set.intersection(f2real[letter], set(num_to_segments[4]))
        elif len(p) == 5:
            s = set()
            for num in [2, 3, 5]:
                s = s.union(set(num_to_segments[num]))
            for letter in p:
                f2real[letter] = set.intersection(f2real[letter], s)
        elif len(p) == 6:
            s = set()
            for num in [6, 0, 9]:
                s = s.union(set(num_to_segments[num]))
            for letter in p:
                f2real[letter] = set.intersection(f2real[letter], s)
        elif len(p) == 7:
            for letter in p:
                f2real[letter] = set.intersection(f2real[letter], set(num_to_segments[8]))

    print(min(len(f2real[k]) for k in f2real))

    def make_backtracking_map():
        curr_mapping = {}
        for a


if __name__ == "__main__":
    inp = get_input(sys.argv[1])

    patterns = inp[0]

    for pline in patterns:
        make_mapping(pline)
