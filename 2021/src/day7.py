import re
import sys
import math

def get_input(fname):
    lines = None
    with open(fname, "r") as f:
        lines = f.readlines()

    return [int(x) for x in lines[0].split(",")]

if __name__ == "__main__":
    inp = get_input(sys.argv[1])

    best_pos = None
    min_fuel = 300000000000

    for target in range(min(inp), max(inp) + 1):
        fuelsum = 0
        for cpos in inp:
            n = abs(cpos - target)
            cost = n * (n + 1) / 2
            fuelsum += cost

        if fuelsum  <= min_fuel:
            best_pos = target
            min_fuel = fuelsum

    print(min_fuel, target)
