import fileinput
import numpy as np
from functools import reduce
import itertools

NDIMS = 2
DTYPE = int
MIN_INTER = 3

def get_points():
    radar_scans = []
    curr_scan = set()
    scan_num = 0
    for line in fileinput.input():
        if len(line.strip()) == 0:
            scan_num += 1
            radar_scans.append((scan_num, curr_scan))
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

def orthogonal_fn(x):
    dot = np.dot(np.array(x[0], dtype=DTYPE), np.array(x[1], dtype=DTYPE))
    return dot == 0

def tform_from_xy(x, y):
    x = np.array(x, dtype=DTYPE)
    y = np.array(y, dtype=DTYPE)
    return np.asarray([x, y, np.cross(x, y)], dtype=DTYPE)

if NDIMS == 3:
    valid_tforms = list(tform_from_xy(x, y) for x, y in
                        filter(orthogonal_fn, itertools.permutations(all_axes, 2)))
elif NDIMS == 2:
    k = list(filter(orthogonal_fn, itertools.permutations(all_axes, 2)))
    valid_tforms = list(np.array([np.array(x), np.array(y)]) for x, y in k)

def add_another_set(fixed_scanner_sets, remaining_scanner_sets):
    """
    fixed_scanner_sets is a list of (scanner_index, point-set, transformation)
    tuples where the transformation brought the scanner's points to point_set
    (ie point-set is already transformed)
    """
    for _, fixed_points, _ in fixed_scanner_sets:
        root_fixed_point = np.array(next(iter(fixed_points)), dtype=DTYPE)
        for raw_idx, raw_points in remaining_scanner_sets:
            for axes_tform in valid_tforms:
                axes_tformed_pts = [np.dot(x, axes_tform).flatten() for x in raw_points]
                for tformed_pt in axes_tformed_pts:
                    mb_root = root_fixed_point - tformed_pt
                    fully_tformed_pts = set([tuple((pt + mb_root).flatten())
                                             for pt in axes_tformed_pts])

                    iset_inters = set.intersection(fixed_points, fully_tformed_pts)
                    if len(iset_inters) >= MIN_INTER:
                        return (
                            fixed_scanner_sets + [(raw_idx, fully_tformed_pts, axes_tform)],
                            [r for r in remaining_scanner_sets if r[0] != raw_idx]
                        )



if __name__ == "__main__":
    psets = get_points()
    fixed_psets = [(psets[0][0], psets[0][1], np.eye(NDIMS, dtype=DTYPE))]
    floating_psets = psets[1:]

    while len(floating_psets) > 0:
        fixed_psets, floating_psets = add_another_set(fixed_psets, floating_psets)
