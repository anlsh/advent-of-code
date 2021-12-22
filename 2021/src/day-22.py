import sys
import re
from functools import reduce

def get_ops(fname):
    with open(fname) as f:
        inp = [l.strip() for l in f.readlines()]

    cubes = []
    for l in inp:
        r = re.search("(.*) x=(.*)\.\.(.*),y=(.*)\.\.(.*),z=(.*)\.\.(.*)", l)
        cubes.append((
            r.group(1) == "on",
            (
                (int(r.group(2)), int(r.group(3)) + 1),
                (int(r.group(4)), int(r.group(5)) + 1),
                (int(r.group(6)), int(r.group(7)) + 1)
            )
        ))

    return cubes

def count_lit_points(ops):
    cubes = [op[1] for op in ops]
    xs = sorted(reduce(lambda a, b: a + b, [list(c[0]) for c in cubes]))
    ys = sorted(reduce(lambda a, b: a + b, [list(c[1]) for c in cubes]))
    zs = sorted(reduce(lambda a, b: a + b, [list(c[2]) for c in cubes]))
    print(f"The state size is {len(xs) * len(ys) * len(zs)}")
    state = [[[False
               for _ in range(len(zs))]
              for _ in range(len(ys))]
             for _ in range(len(xs))]
    print(f"Made the state!")

    x2i = {x: i for i, x in enumerate(xs)}
    y2i = {y: i for i, y in enumerate(ys)}
    z2i = {z: i for i, z in enumerate(zs)}

    n_lit = 0
    for turn_on, box in ops:
        for xi in range(x2i[box[0][0]], x2i[box[0][1]]):
            for yi in range(y2i[box[1][0]], y2i[box[1][1]]):
                for zi in range(z2i[box[2][0]], z2i[box[2][1]]):
                    region_size = (xs[xi + 1] - xs[xi]) \
                        * (ys[yi + 1] - ys[yi]) \
                        * (zs[zi + 1] - zs[zi])
                    if turn_on and not state[xi][yi][zi]:
                        n_lit += region_size
                        state[xi][yi][zi] = True
                    elif (not turn_on) and state[xi][yi][zi]:
                        n_lit -= region_size
                        state[xi][yi][zi] = False

    return n_lit

if __name__ == "__main__":
    ops = get_ops(sys.argv[1])
    boxes = [op[1] for op in ops]

    print(count_lit_points(ops))
