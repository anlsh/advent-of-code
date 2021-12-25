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

def truncate(n):
    return ceil(n) if n <= 0 else floor(n)

def run_reduced(reduced_ops, in_num):
    x = y = z = 0
    for i, op in enumerate(reduced_ops):
        # Finals
        x_f = int(z % 26 + op[1] != in_num[i])
        y_f = (in_num[i] + op[2]) * x_f
        z_f = truncate(z / op[0]) * (25 * x_f + 1) + y_f

        # Set out vars
        x, y, z = x_f, y_f, z_f

    return {"x": x, "y": y, "z": z}

if __name__ == "__main__":
    reduced_ops = get_reduced_ops(sys.argv[1])
    # print(reduced_ops)
    # exit()

    iterable = None
    if len(sys.argv) == 4:
        nlen = len(sys.argv[3])
        if nlen != int(sys.argv[2]):
            raise RuntimeError(f"Incorrect in num length {nlen}")
        num =  [int(c) for c in sys.argv[3]]
        iterable = (num,)
    else:
        iterable = itertools.product(NUMS[::-1], repeat=int(sys.argv[2]))

    for in_number in iterable:
        n = int("".join([str(i) for i in in_number]))
        state = run_reduced(reduced_ops, in_number)
        z = state["z"]
        if z == 0:
            print(f'Accepted {n}!')
            exit()
