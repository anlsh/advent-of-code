import sys
import re

def get_cubes(fname):
    with open(fname) as f:
        inp = [l.strip() for l in f.readlines()]

    cubes = []
    for l in inp:
        r = re.search("(.*) x=(.*)\.\.(.*),y=(.*)\.\.(.*),z=(.*)\.\.(.*)", l)
        cubes.append((
            r.group(1) == "on",
            (
                (int(r.group(2)), int(r.group(3))),
                (int(r.group(4)), int(r.group(5))),
                (int(r.group(6)), int(r.group(7)))
            )
        ))

    return cubes

def part_a(cubes):
    state = [[[False for _ in range(101)] for _ in range(101)] for _ in range(101)]
    for turn_on, dims in cubes:
        if (-50 <= dims[0]) and (dims[1] <= 50) \
           and (-50 <= dims[2]) and (dims[3] <= 50) \
           and (-50 <= dims[4]) and (dims[5] <= 50):
            for x in range(dims[0], dims[1] + 1):
                for y in range(dims[2], dims[3] + 1):
                    for z in range(dims[4], dims[5] + 1):
                        state[x + 50][y + 50][z + 50] = turn_on


    count = 0
    for x in range(0, 101):
        for y in range(0, 101):
            for z in range(0, 101):
                if state[x][y][z]:
                    count += 1
    return count

def ivals_int(xs, ys):
    if ys[0] < xs[0]:
        xs, ys = ys, xs
    if ys[0] <= xs[1]:
        return ys[1], xs[1]
    else:
        return None

def do_boxes_intersect(b1, b2):
    return invals_int((b1[0], b2[]))


def refine_boxes(boxlist, new_box):


if __name__ == "__main__":
    cubes = get_cubes(sys.argv[1])
    print(f"Part A: {part_a(cubes)}")
