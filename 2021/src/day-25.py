import sys
from copy import deepcopy

def get_grid(fname):
    with open(fname) as f:
        return [[c for c in l.strip()] for l in f.readlines()]

def print_grid(grid):
    print("\n".join(["".join(l) for l in grid]))

def step_grid(grid):
    done = True

    nrows = len(grid)
    ncols = len(grid[0])

    new_grid = deepcopy(grid)

    for (incfs, char) in (((0, 1), ">"), ((1, 0), "v")):
        grid, new_grid = new_grid, deepcopy(new_grid)
        for row in range(nrows):
            for col in range(ncols):
                nrow = (row + incfs[0]) % nrows
                ncol = (col + incfs[1]) % ncols
                if grid[row][col] == char and grid[nrow][ncol] == ".":
                    done = False
                    new_grid[row][col] = "."
                    new_grid[nrow][ncol] = char

    return new_grid, done

if __name__ == "__main__":

    grid = get_grid(sys.argv[1])
    done = False
    n = 0

    while not done:
        grid, done = step_grid(grid)
        n += 1

    print(f"Frozen after {n}")
