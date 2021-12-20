import fileinput
import numpy as np
from functools import reduce
import itertools
import sys

NDIMS = 3
MIN_INTER = 12
DTYPE = int
PRINT_PROGRESS = False

def get_points(fname):
    radar_scans = []
    curr_scan = set()
    scan_num = 0
    with open(fname) as f:
        for line in f.readlines():
            if line == "\n":
                radar_scans.append((scan_num, curr_scan))
                scan_num += 1
                curr_scan = set()
            elif line[1] == "-":
                pass
            else:
                curr_scan.add(tuple(int(x) for x in line.strip().split(",")))

    radar_scans.append((scan_num, curr_scan))
    return radar_scans

def vecs2tupleset(vecs):
    return set((tuple(vec) for vec in vecs))


all_axes = [vecs2tupleset([vec, -1 * vec]) for vec in np.eye(NDIMS, dtype=DTYPE)]
all_axes = set().union(*all_axes)

def t2v(tup):
    return np.array(tup, dtype=DTYPE)

def orthogonal_fn(x):
    dot = np.dot(t2v(x[0]), t2v(x[1]))
    return dot == 0

def tform_from_xy(x, y):
    x = t2v(x)
    y = t2v(y)
    return np.asarray([x, y, np.cross(x, y)], dtype=DTYPE)

if NDIMS == 3:
    valid_tforms = list(tform_from_xy(x, y) for x, y in
                        filter(orthogonal_fn, itertools.permutations(all_axes, 2)))
elif NDIMS == 2:
    k = list(filter(orthogonal_fn, itertools.permutations(all_axes, 2)))
    valid_tforms = list(np.array([t2v(x), t2v(y)]) for x, y in k)

incompat_scanners = set()

def add_another_set(fixed_psets, floating_psets):
    """
    fixed_psets is a list of (scanner_index, point-set, transformation)
    tuples where the transformation brought the scanner's points to point_set
    (ie point-set is already transformed)
    """

    for fixed_idx, fixed_points, _, _ in fixed_psets:
        root_fixed_point = t2v(next(iter(fixed_points)))
        for floating_idx, raw_points in  floating_psets:
            key = tuple(sorted((fixed_idx, floating_idx)))
            if key in incompat_scanners:
                continue

            for root_fixed_point in fixed_points:
                root_fixed_point = t2v(root_fixed_point)
                for axes_tform in valid_tforms:
                    axes_tformed_pts = [np.dot(x, axes_tform).flatten() for x in raw_points]
                    for tformed_pt in axes_tformed_pts:
                        mb_root = root_fixed_point - tformed_pt
                        fully_tformed_pts = set([tuple((pt + mb_root).flatten())
                                                for pt in axes_tformed_pts])

                        if len(set.intersection(fixed_points, fully_tformed_pts)) >= MIN_INTER:
                            if PRINT_PROGRESS:
                                print(f"Pset {floating_idx} is at {mb_root} by comparision with {fixed_idx}")
                            return (
                                fixed_psets + [(floating_idx, fully_tformed_pts, tuple(mb_root), axes_tform)],
                                [r for r in  floating_psets if r[0] != floating_idx]
                            )

            # We're at this point -> fixed_idx & floating_idx cannot be aligned
            incompat_scanners.add(key)

if __name__ == "__main__":
    psets = get_points(sys.argv[1])
    fixed_psets = [(psets[0][0], psets[0][1], np.zeros(NDIMS), np.eye(NDIMS, dtype=DTYPE))]
    floating_psets = psets[1:]

    while len(floating_psets) > 0:
        fixed_psets, floating_psets = add_another_set(fixed_psets, floating_psets)

    print("Part A: ", end="")
    print(len(set().union(*[sol[1] for sol in fixed_psets])))

    max_dist = 0
    offsets = [fix[2] for fix in fixed_psets]
    for m1, m2 in itertools.combinations(offsets, 2):
        print(m1, m2)
        max_dist = max(max_dist, int(np.linalg.norm(t2v(m1) - t2v(m2), ord=1)))
    print(f"Part B: {max_dist}")
