import sys
import itertools
from math import floor, ceil

LETS = "xyzw"

ADD_KEY = "+"
MUL_KEY = "*"
DIV_KEY = "/"
MOD_KEY = "%"
EQL_KEY = "=="

# TODO Adding the reverse will solve part b in ~5min
# Figure out a much smarter way to do that :/
NUMS = (9,8,7,6,5,4,3,2,1)#[::-1]

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

def solve(reduced_ops):
    n_ops = len(reduced_ops)
    spot_26s = [1 if op[0] == 26 else 0 for op in reduced_ops]
    n_26s = sum(spot_26s)
    n_26s_remaining_inclusive = [sum(spot_26s[i+1:]) for i in range(n_ops)]

    def solve_helper(z0, i, curr_state,):
        if i == 14:
            if z0 == 0:
                print(f"Holy shit we accepted something")
                print(curr_state)
                print("".join([str(c) for c in curr_state]))
                exit()
            else:
                return
        # TODO Might need a + 1 to get the right anser :/
        elif z0 >= 26 ** (n_26s_remaining_inclusive[i] + 2):
            return
        else:
            for n in NUMS:
                op = reduced_ops[i]
                x = int(z0 % 26 + op[1] != n)
                y = x * (n + op[2])
                znew = truncate(z0 / op[0]) * (25 * x + 1) + y
                solve_helper(znew, i + 1, curr_state + (n,))

    solve_helper(0, 0, tuple())

if __name__ == "__main__":
    reduced_ops = get_reduced_ops(sys.argv[1])
    solve(reduced_ops)
