import sys
import itertools
from math import floor, ceil

LETS = "xyzw"

ADD_KEY = "+"
MUL_KEY = "*"
DIV_KEY = "/"
MOD_KEY = "%"
EQL_KEY = "=="

NUMS = (1,2,3,4,5,6,7,8,9)

def get_reduced_ops(fname):
    reduced_ops = []
    f = open(fname)
    for _ in range(14):
        op = []
        for _ in range(4):
            f.readline()
        op.append(int(f.readline().rstrip().split(" ")[-1]))
        op.append(int(f.readline().rstrip().split(" ")[-1]))
        for _ in range(9):
            f.readline()
        op.append(int(f.readline().rstrip().split(" ")[-1]))
        reduced_ops.append(tuple(op))
        for _ in range(2):
            f.readline()

    return tuple(reduced_ops)

def run_reduced(reduced_ops, in_num):
    x = y = z = w = 0
    for i, op in enumerate(reduced_ops):
        w = in_num[i]
        x = 0
        x = x + z
        x = x % 26
        zint = z / op[0]
        z = floor(zint) if zint >= 0 else ceil(zint)
        x = x + op[1]
        x = int(x != w)
        y = 0
        y = y + 25
        y = y & x
        y += 1
        z = z * y
        y = 0
        y = w
        y = y + op[2]
        y = y * x
        z = z + y

    return {"x": x, "y": y, "z": z, "w": w}

if __name__ == "__main__":
    reduced_ops = get_reduced_ops(sys.argv[1])

    iterable = None
    if len(sys.argv) == 4:
        nlen = len(sys.argv[3])
        if nlen != int(sys.argv[2]):
            raise RuntimeError(f"Incorrect in num length {nlen}")
        num =  [int(c) for c in sys.argv[3]]
        iterable = (num,)
    else:
        iterable = itertools.product(NUMS[::1], repeat=int(sys.argv[2]))

    i = 0
    for in_number in iterable:
        n = int("".join([str(i) for i in in_number]))
        i += 1
        state = run_reduced(reduced_ops, in_number)
        z = state["z"]
        print(f"{n} => {z}")
        if z == 0:
            print(f'Accepted {n}!')
            exit()
