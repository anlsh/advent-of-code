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

def ivals_int(i1, i2):
    if i2[0] < i1[0]:
        i1, i2 = i2, i1

    if i2[0] <= i1[1]:
        ranges = (
            (i1[0], i2[0] - 1),
            (i2[0], min(i1[1], i2[1])),
            (min(i1[1], i2[1]) + 1, max(i1[1], i2[1]))
        )
        return True, tuple((r for r in ranges if r[0] <= r[1]))
    else:
        return False, None

def box_contains(container, b):
    return (container[0][0] <= b[0][0] <= b[0][1] <= container[0][1]) \
        and (container[1][0] <= b[1][0] <= b[1][1] <= container[1][1]) \
        and (container[2][0] <= b[2][0] <= b[2][1] <= container[2][1])

def refine_boxes(nondisj_boxes):

    disj_boxes = []

    while len(nondisj_boxes) > 0:
        new_box = nondisj_boxes[0]
        i = 0

        new_disj_boxes = []

        while i < len(disj_boxes):
            box = disj_boxes[i]
            i += 1
            xint, xrs = ivals_int(box[0], new_box[0])
            yint, yrs = ivals_int(box[1], new_box[1])
            zint, zrs = ivals_int(box[2], new_box[2])

            if not (xint and yint and zint):
                new_disj_boxes.append(box)
            else:
                for xr in xrs:
                    for yr in yrs:
                        for zr in zrs:
                            rs = (xr, yr, zr)
                            if box_contains(new_box, rs) or box_contains(box, rs):
                                new_disj_boxes.append(rs)

                break

        if len(new_disj_boxes) == len(disj_boxes):
            disj_boxes = disj_boxes + [new_box]
            nondisj_boxes = nondisj_boxes[1:]
        else:
            unconsidered_disjs = disj_boxes[i:]
            disj_boxes = new_disj_boxes
            nondisj_boxes = unconsidered_disjs + nondisj_boxes[1:]

    return disj_boxes


if __name__ == "__main__":
    ops = get_cubes(sys.argv[1])
    # print(f"Part A: {part_a(cubes)}")
    print(refine_boxes([op[1] for op in ops]))
