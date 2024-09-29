#!/usr/bin/python3
import sys

def for_iperm(fn, n):
    # For each permutation of the first 'n' integers.

    state = list(range(n))

    while True:
        reversed_tl_start = n - 1
        while reversed_tl_start > 0:
            if state[reversed_tl_start - 1] > state[reversed_tl_start]:
                reversed_tl_start -= 1
            else:
                break

        if reversed_tl_start == 0:
            break
        else:
            fn(state)
            k = n - 1
            while state[k] < state[reversed_tl_start - 1]:
                k -= 1
            state[reversed_tl_start - 1], state[k] = state[k], state[reversed_tl_start - 1]
            state[reversed_tl_start:] = state[reversed_tl_start:][::-1]
    # WOOHOOO I FINALLY UNDERSTAND THE NEXT PERMUTATION FUNCTION AFTER FOUR
    # FUCKING YEARS LET"S GOOOOOOOO
    # Easy enough once you think of the example 6, 9, 8, 7
    # Make sure not to just naively swap 6 with the reverse-sorted tail!
    # Consider what happens with [2, 3, 1] to see why that's a bad idea

def for_perm(fn, iterable):
    iterable = list(iterable)
    def helper(iperm):
        k = [iterable[i] for i in iperm]
        fn(k)
    for_iperm(helper, len(iterable))

if __name__ == "__main__":
    inp = sys.stdin.readlines()

    graph = {}
    for edge in inp:
        c1, _, c2, _, dist = edge.split(" ")
        dist = int(dist)
        if c1 not in graph:
            graph[c1] = {}
        if c2 not in graph:
            graph[c2] = {}
        graph[c1][c2] = dist
        graph[c2][c1] = dist

    global best_s
    global worst_s
    best_s = None
    worst_s = None
    def perm_fn(perm):
        # You actually can't close over the variables above i FUCKING HATE
        # how closures work in this language
        global best_s
        global worst_s
        s = 0
        for i in range(len(graph) - 1):
            s += graph[perm[i]][perm[(i + 1) % len(graph)]]
            # Makes part A faster
            # if best_s is not None and s >= best_s:
            #     continue

        best_s = s if best_s is None else min(s, best_s)
        worst_s = s if worst_s is None else max(s, worst_s)

    for_perm(perm_fn, graph.keys())
    print(best_s)
    print(worst_s)
