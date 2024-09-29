#!/usr/bin/python3
import sys
import re

def solve(inp: list[str], initial_element, on_action, off_action, toggle_action):
    state = [initial_element for _ in range(1000 * 1000)]
    def pair_to_idx(r, c):
        return 1000 * r + c

    for ins in inp:
        regex = r"(.*) ([0-9]+),([0-9]+) through ([0-9]+),([0-9]+)"
        result = re.search(regex, ins)
        action_code = result.groups()[0]
        a0, a1, b0, b1 = map(int, re.search(regex, ins).groups()[1:])

        action = {"turn on": on_action, "turn off": off_action, "toggle": toggle_action}[action_code]

        for r in range(a0, b0 + 1):
            for c in range(a1, b1 + 1):
                idx = pair_to_idx(r, c)
                state[idx] = action(state[idx])
    return state

if __name__ == "__main__":
    inp = sys.stdin.readlines()

    a_state = solve(inp, False, lambda _: True, lambda _: False, lambda x: not x)
    print(sum(bool(c) for c in a_state))
    b_state = solve(inp, 0, lambda x: x + 1, lambda x: max(x - 1, 0), lambda x: x + 2)
    print(sum(b_state))
