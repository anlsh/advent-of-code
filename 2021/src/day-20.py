import sys
import itertools

nbor_offs = ((-1, 1), (0, 1), (1, 1),
             (-1, 0), (0, 0), (1, 0),
             (-1, -1), (0, -1), (1, -1))

PADDING_SIZE = 5

def tuple_add(x, y):
    return (x[0] + y[0], x[1] + y[1])

# def get_nborhood(set_points):
#     nbor_sets = [set([tuple_add(root, off) for off in nbor_offs]) for root in set_points]
#     return set().union(*nbor_sets)

# def step_picture(set_pts, key, fringe_zero):

#     close_nborhood = get_nborhood(set_pts)
#     far_nborhood = get_nborhood(close_nborhood)

#     def get_val(pos):
#         if pos in set_pts:
#             return "1"
#         elif pos in close_nborhood:
#             return "0"
#         else:
#             return "0" if fringe_zero else "1"

#     new_pts = set()
#     for pt in far_nborhood:
#         s = "".join([get_val(tuple_add(pt, off)) for off in nbor_offs])

#         if key[int(s, 2)]:
#             new_pts.add(pt)

#     return new_pts

if __name__ == "__main__":
    fname = sys.argv[1]
    key = []
    set_pts = set()

    with open(fname) as f:
        lines = [l.strip() for l in f.readlines()]
        for c in lines[0].strip():
            key.append(c == "#")

        pic = []
        for line in enumerate(lines[2:][::-1]):
            pic.append(("." * PADDING_SIZE) + line.strip() + ("." * PADDING_SIZE))

        line_size = len(pic[0])
        for _ in PADDING_SIZE:
            pic = ["." * line_size] + pic + ["." * line_size]

    for i in range(2):
        pic = step_picture(pic, key, (i % 2 == 0) and key[0])
