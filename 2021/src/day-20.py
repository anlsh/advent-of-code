import sys
import itertools

nbor_offs = ((-1, 1), (0, 1), (1, 1),
             (-1, 0), (0, 0), (1, 0),
             (-1, -1), (0, -1), (1, -1))

def tuple_add(x, y):
    return (x[0] + y[0], x[1] + y[1])

def get_nborhood(set_points):
    nbor_sets = [set([tuple_add(root, off) for off in nbor_offs]) for root in set_points]
    return set().union(*nbor_sets)

def step_picture(set_pts, key):
    def get_val(loc):
        if loc in set_pts:
            return "1"
        else:
            return "0"

    new_pts = set()
    for pt in get_nborhood(set_pts):
        bstr = "".join([get_val(tuple_add(pt, off)) for off in nbor_offs])
        bnum = int(bstr, 2)
        if key[bnum]:
            new_pts.add(pt)

    return new_pts

if __name__ == "__main__":
    fname = sys.argv[1]
    key = []
    set_pts = set()

    with open(fname) as f:
        lines = f.readlines()
        for c in lines[0].strip():
            key.append(c == "#")

        for rnum, line in enumerate(lines[2:][::-1]):
            for cnum in range(len(line)):
                if line[cnum] == "#":
                    set_pts.add((cnum, rnum))

    for _ in range(2):
        set_pts = step_picture(set_pts, key)

    print(f"Part A: {len(set_pts)}")
