import sys
import itertools
from functools import lru_cache

def part_a(p1p, p2p):

    MAX_SCORE = 1000

    def step_game(p1st, p2st, dice_state, nrolls):
        p1p, p1s = p1st
        p2p, p2s = p2st

        p1p = ((p1p + (3 * dice_state) + 3 - 1) % 10) + 1
        p1s += p1p
        dice_state = ((dice_state + 3 - 1) % 100) + 1
        nrolls += 3
        if p1s >= MAX_SCORE:
            return (p1p, p1s), (p2p, p2s), dice_state, nrolls, True

        p2p = ((p2p + (3 * dice_state) + 3 - 1) % 10) + 1
        p2s += p2p
        dice_state = ((dice_state + 3 - 1) % 100) + 1
        nrolls += 3
        return (p1p, p1s), (p2p, p2s), dice_state, nrolls, p2s >= MAX_SCORE

    p1st = (p1p, 0)
    p2st = (p2p, 0)
    ds = 1
    nrolls = 0
    done = False

    while not done:
        p1st, p2st, ds, nrolls, done = step_game(p1st, p2st, ds, nrolls)

    return (p1st if p1st[1] < p2st[1] else p2st)[1] * nrolls

def part_b(p1p, p2p):
    jump_list = [sum(x) for x in itertools.product((1,2,3), repeat=3)]
    SCORE_LIMIT = 21
    # Assuming a key of ((p1p, p1s), (p2p, p2s)) return
    # number of universes where p1 wins, total_universes
    @lru_cache(maxsize=None)
    def memo(p1st, p2st):
        p1p, p1s = p1st
        p2p, p2s = p2st

        if p1s >= SCORE_LIMIT:
            return 1, 1
        elif p2s >= SCORE_LIMIT:
            return 0, 1
        else:
            p1wins = 0
            total = 0
            for jump in jump_list:
                c_p1p = ((p1p + jump - 1) % 10) + 1
                c_p1s = p1s + c_p1p
                p2wins, subtotal = memo(p2st, (c_p1p, c_p1s))
                total += subtotal
                p1wins += (subtotal - p2wins)

            return (p1wins, total)

    p1wins, total = memo((p1p, 0), (p2p, 0))
    return max(p1wins, total - p1wins)

if __name__ == "__main__":
    with open(sys.argv[1]) as f:
        flines = [line.strip() for line in f.readlines()]

    p1p = int(flines[0][-1])
    p2p = int(flines[1][-1])

    print(f"Answer to A: {part_a(p1p, p2p)}")
    print(f"Answer to B: {part_b(p1p, p2p)}")
