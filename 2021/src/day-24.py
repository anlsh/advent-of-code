import sys

LETS = "xyzw"

def get_ops(fname):
    with open(fname) as f:
        ops = [line.rstrip().split(" ") for line in f.readlines()]



if __name__ == "__main__":
    state = {"x": x, "y": y, "z": z, "w": w}
    i = 0
    for op in ops:
        if op[0] == "inp":
            state[ops[1]] = "d" + ops[1]
            continue
        place = ops[1]
        arg1 = state[ops[1]]
        arg2 = state[ops[2]] if ops[2] in LETS else int(ops[2])
        if ops[0] == "add":
            if arg1 == 0:
                state[place] = arg2
            elif arg2 == 0:
                state[place] = arg1
            else:
                state[place] = arg1 + arg2 if type(arg1) == type(arg2) == int \
                    else ["+", arg1, arg2]
        elif ops[0] == "mul":
            if arg1 == 0 or arg2 == 0:
                state[place] = arg2
            else:
                state[place] = arg1 * arg2 if type(arg1) == type(arg2) == int \
                    else ["*", arg1, arg2]
        elif ops[0] == "div":
            if arg2 == 0:
                raise RuntimeError("Dividing by zero!")
            elif arg2 == 1:
                state[place] = arg1
            else:
                state[place] = arg1 // arg2 if type(arg1) == type(arg2) == int \
                    else ["/", arg1, arg2]
        elif ops[0] == "mod":
            if arg2 == 0:
                raise RuntimeError("Modding by zero!")
            elif arg2 == 1:
                state[place] = 0
            else:
                state[place] = arg1 % arg2 if type(arg1) == type(arg2) == int \
                    else ["%", arg1, arg2]
        elif ops[0] == "eql":
            state[place] = (1 if arg1 == arg2 else -1) if type(arg1) == type(arg2) == int \
                else ["%", arg1, arg2]
        state[ops[1]] = ("+")
