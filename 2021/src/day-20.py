import sys
import itertools

nbor_offs = ((-1, 1), (0, 1), (1, 1),
             (-1, 0), (0, 0), (1, 0),
             (-1, -1), (0, -1), (1, -1))

N_STEPS = 50
PADDING_SIZE = N_STEPS + 5

def tuple_add(x, y):
    return (x[0] + y[0], x[1] + y[1])

def step_picture(picture, key, fringe_zero):
    new_picture = [[0] * len(picture[0]) for _ in range(len(picture))]

    def get_numeric_val(pos):
        if (0 <= pos[0] < len(picture[0])) and (0 <= pos[1] < len(picture)):
            return "0" if picture[pos[1]][pos[0]] == "." else "1"
        else:
            return "0" if fringe_zero else "1"

    for x in range(len(picture[0])):
        for y in range(len(picture)):
            s = "".join([get_numeric_val(tuple_add((x, y), off)) for off in nbor_offs])
            new_picture[y][x] = "#" if key[int(s, 2)] else "."

    return new_picture

def count_set_picture(picture):
    c = 0
    for row in picture:
        for char in row:
            if char == "#":
                c += 1
    return c

if __name__ == "__main__":
    fname = sys.argv[1]
    key = []
    set_pts = set()

    with open(fname) as f:
        lines = [l.strip() for l in f.readlines()]
        for c in lines[0].strip():
            key.append(c == "#")

        pic = []
        for line in lines[2:][::-1]:
            pic.append(("." * PADDING_SIZE) + line.strip() + ("." * PADDING_SIZE))

        line_size = len(pic[0])
        for _ in range(PADDING_SIZE):
            pic = ["." * line_size] + pic + ["." * line_size]

    print(f"Beginning count {count_set_picture(pic)}")
    for i in range(N_STEPS):
        pic = step_picture(pic, key, (not key[0]) or (i % 2 == 0))
        print(f"Then {count_set_picture(pic)}")
