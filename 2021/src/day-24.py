import sys
import itertools

LETS = "xyzw"

ADD_KEY = "+"
MUL_KEY = "*"
DIV_KEY = "/"
MOD_KEY = "%"
EQL_KEY = "=="

NUMS = (1,2,3,4,5,6,7,8,9)

def get_ops(fname):
    with open(fname) as f:
        ops = [line.rstrip().split(" ") for line in f.readlines()]
    return ops

def eval_tree(tree, inp):
    def eval_tree_helper(tree):
        if type(tree) == int:
            return tree
        elif type(tree) == str:
            return inp[int(tree)]
        else:
            if tree[0] == ADD_KEY:
                return eval_tree_helper(tree[1]) + eval_tree_helper(tree[2])
            elif tree[0] == MUL_KEY:
                return eval_tree_helper(tree[1]) * eval_tree_helper(tree[2])
            elif tree[0] == DIV_KEY:
                return eval_tree_helper(tree[1]) // eval_tree_helper(tree[2])
            elif tree[0] == MOD_KEY:
                return eval_tree_helper(tree[1]) % eval_tree_helper(tree[2])
            elif tree[0] == EQL_KEY:
                return int(eval_tree_helper(tree[1]) == eval_tree_helper(tree[2]))
    return eval_tree_helper(tree)

def state_from_ops(ops):
    state = {"x": 0, "y": 0, "z": 0, "w": 0}
    i = 0
    for op in ops:
        if op[0] == "inp":
            state[op[1]] = str(i)
            i += 1
            continue
        place = op[1]
        arg1 = state[op[1]]
        arg2 = state[op[2]] if op[2] in LETS else int(op[2])
        if op[0] == "add":
            if arg1 == 0:
                state[place] = arg2
            elif arg2 == 0:
                state[place] = arg1
            else:
                state[place] = arg1 + arg2 if type(arg1) == type(arg2) == int \
                    else [ADD_KEY, arg1, arg2]
        elif op[0] == "mul":
            if arg1 == 0 or arg2 == 0:
                state[place] = arg2
            else:
                state[place] = arg1 * arg2 if type(arg1) == type(arg2) == int \
                    else [MUL_KEY, arg1, arg2]
        elif op[0] == "div":
            if arg2 == 0:
                raise RuntimeError("Dividing by zero!")
            elif arg2 == 1:
                state[place] = arg1
            else:
                state[place] = arg1 // arg2 if type(arg1) == type(arg2) == int \
                    else [DIV_KEY, arg1, arg2]
        elif op[0] == "mod":
            if arg2 == 0:
                raise RuntimeError("Modding by zero!")
            elif arg2 == 1:
                state[place] = 0
            else:
                state[place] = arg1 % arg2 if type(arg1) == type(arg2) == int \
                    else [MOD_KEY, arg1, arg2]
        elif op[0] == "eql":
            state[place] = int(arg1 == arg2) if type(arg1) == type(arg2) == int \
                else [EQL_KEY, arg1, arg2]

    return state

def run_program(ops, in_num_tuple):
    state = {"x": 0, "y": 0, "z": 0, "w": 0}
    i = 0
    for op in ops:
        if op[0] == "inp":
            state[op[1]] = in_num_tuple[i]
            i += 1
            continue
        place = op[1]
        arg1 = state[op[1]]
        arg2 = state[op[2]] if op[2] in LETS else int(op[2])
        if op[0] == "add":
            state[place] = arg1 + arg2
        elif op[0] == "mul":
            state[place] = arg1 * arg2
        elif op[0] == "div":
            if arg2 == 0:
                raise RuntimeError("Dividing by zero!")
            state[place] = arg1 // arg2
        elif op[0] == "mod":
            if arg2 == 0:
                raise RuntimeError("Modding by zero!")
            state[place] = arg1 % arg2
        elif op[0] == "eql":
            state[place] = int(arg1 == arg2)

    return state

if __name__ == "__main__":
    # trees = state_from_ops(get_ops(sys.argv[1]))
    # inp = [int(c) for c in sys.argv[2]]
    # final_state = {t: eval_tree(trees[t], inp) for t in trees}
    # print(final_state)
    ops = get_ops(sys.argv[1])

    i = 0
    for in_number in itertools.product(NUMS[::-1], repeat=int(sys.argv[2])):
        z_state = run_program(ops, in_number)["z"]
        nstr = "".join([str(i) for i in in_number])
        i += 1
        if z_state == 0:
            print(f"Accepted {nstr}!")
            exit()
    print(i)
