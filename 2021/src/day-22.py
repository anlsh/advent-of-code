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

def ref_two_bs(b1, b2):

    # print(f"Refining boxes {b1}, {b2} => ", end="")
    xint, xrs = ivals_int(b1[0], b2[0])
    yint, yrs = ivals_int(b1[1], b2[1])
    zint, zrs = ivals_int(b1[2], b2[2])

    if (xint and yint and zint):
        refinements = []
        for xr in xrs:
            for yr in yrs:
                for zr in zrs:
                    rs = (xr, yr, zr)
                    if box_contains(b1, rs) or box_contains(b2, rs):
                        refinements.append(rs)
        # print(f"Refined {len(refinements)} from {b1}, {b2}")
        # print(f"From intervals {xrs, yrs, zrs}")
        return refinements
    else:
        # print("no intersection")
        return []

def refine_boxes(boxes):
    cands = []
    boxes_with_inters = set()

    for i in range(len(boxes)):
        for j in range(i + 1, len(boxes)):
            b1 = boxes[i]
            b2 = boxes[j]
            refs = ref_two_bs(b1, b2)
            if len(refs) > 0:
                boxes_with_inters.add(i)
                boxes_with_inters.add(j)
            cands.extend(refs)

    for i in range(len(boxes)):
        if i not in boxes_with_inters:
            cands.append(boxes[i])
            # print(f"Appending {boxes[i]} to candidates due to no-inter")

    # print(f"Going into containment step, len(cads) == {len(cands)}")
    real_cands = []
    for i in range(len(cands)):
        c1 = cands[i]
        for j in range(i + 1, len(cands)):
            c2 = cands[j]
            contains_another = False
            if c1 == c2:
                continue
            if box_contains(c1, c2):
                contains_another = True
                break
        if not contains_another:
            real_cands.append(c1)
            # print(f"Appended {c1} to real_cands b/c no-contain")

    return real_cands, len(boxes_with_inters) > 0

if __name__ == "__main__":
    ops = get_cubes(sys.argv[1])
    boxes = [op[1] for op in ops]

    cands = boxes
    found_inter = True
    i = 0
    while found_inter:
        print(f"Iteration {i}: Have {len(cands)} boxes to handle")
        i += 1
        cands, found_inter = refine_boxes(cands)

    print(len(cands))
