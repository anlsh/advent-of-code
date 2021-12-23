import sys
import heapq

# Below is a map of the positions
#
# 0 1 2 3 4 5 6 7 8 9 10
#     1   3   5   7
#     0   2   4   6
#
# 0 is an empty space
# A, B, C, D are represented as 1, 2, 3, 4
# Amphipods which are frozen in the hallway are incremented by 5
# By the rules, the doorway positions should actually always be empty


HALL_START = 8
HALLWAY_SIZE = 11
STATE_SIZE = HALL_START + HALLWAY_SIZE

def state_from_file(fname):
    def let2n(let):
        if let == "A":
            return 1
        elif let == "B":
            return 2
        elif let == "C":
            return 3
        elif let == "D":
            return 4
        elif let == ".":
            return 0

    with open(fname) as f:
        lines = [l.rstrip() for l in f.readlines()]

    rooms = [lines[3][3], lines[2][3], lines[3][5], lines[2][5],
            lines[3][7], lines[2][7], lines[3][9], lines[2][9]]
    rooms = [let2n(let) for let in rooms]
    return tuple(rooms + [let2n(l) for l in lines[1][1:-1]])

TARGET_STATE = tuple([1, 1, 2, 2, 3, 3, 4, 4] + ([0] * HALLWAY_SIZE))

TYPES = (1, 2, 3, 4)
COSTS = (None,) + (1, 10, 100, 1000)
BOTS = (None,) + (0, 2, 4, 6)
TOPS = (None,) + tuple((r + 1 for r in BOTS[1:]))
DOORS = (None,) + tuple((r + 2 + HALL_START for r in BOTS[1:]))
CORNERS_FOR_ROOM = (None,) + tuple(((d - 1, d + 1) for d in DOORS[1:]))
CORNER_LIST = tuple((c[0] for c in CORNERS_FOR_ROOM[1:])) + CORNERS_FOR_ROOM[-1]
VALID_LEN1_HALLMOVES = (
    (HALL_START, HALL_START + 1),
    (HALL_START + 1, HALL_START),
    (STATE_SIZE - 2, STATE_SIZE - 1),
    (STATE_SIZE - 1, STATE_SIZE - 2)
)

def print_state(state):
    def n2str(n):
       return str(n) if n > 0 else "."
    print(" ".join([n2str(state[i]) for i in range(HALL_START, STATE_SIZE)]))
    print("    " + "   ".join([n2str(state[i]) for i in TOPS[1:]]))
    print("    " + "   ".join([n2str(state[i]) for i in BOTS[1:]]))

FROZEN_OFFSET = 4

def freeze(n):
    return n + FROZEN_OFFSET if 1 <= n <= FROZEN_OFFSET else n
def unfreeze(n):
    return n - FROZEN_OFFSET if n > FROZEN_OFFSET else n
def is_frozen(n):
    return n != unfreeze(n) if n != 0 else False

def room_accepts(state, rtype, atype):
    return rtype == atype \
        and (state[BOTS[rtype]] == state[TOPS[rtype]] == 0) \
             or (state[BOTS[rtype]] == rtype and state[TOPS[rtype]] == 0)

def unfreeze_pathable_hallway(state):
    state = list(state)
    for rtype in TYPES:
        if not room_accepts(state, rtype, rtype):
            continue
        for idcs in (range(DOORS[rtype], STATE_SIZE), range(HALL_START, DOORS[rtype])[::-1]):
            for i in idcs:
                if state[i] == 0:
                    continue
                else:
                    if unfreeze(state[i]) == rtype:
                        state[i] = unfreeze(state[i])
                        break
    return tuple(state)

def ridx_to_rtype(ind):
    return ind // 2 + 1

def state_from_move(state, from_idx, to_idx):
    state = list(state)

    # Checks to make sure we're doing things sanely #
    if state[from_idx] == 0:
        raise RuntimeError("Trying to move from empty pos!")
    elif state[to_idx] != 0:
        raise RuntimeError("Trying to move into occupied pos!!")
    elif is_frozen(state[from_idx]):
        raise RuntimeError(f"Trying to move from frozen pos! {from_idx}")
    elif to_idx in DOORS:
        raise RuntimeError("Stopped in doorway!")
    elif to_idx < HALL_START and (from_idx >= HALL_START) \
         and not room_accepts(state, ridx_to_rtype(to_idx), state[from_idx]):
        raise RuntimeError("Trying to move apod into non-accepting hallway")

    state[to_idx] = state[from_idx]
    state[from_idx] = 0

    def freeze_hallway():
        for i in range(HALL_START, STATE_SIZE):
            state[i] = freeze(state[i])

    freeze_hallway()
    if from_idx < HALL_START and to_idx < HALL_START:
        pass
    elif from_idx >= HALL_START and to_idx < HALL_START:
        pass
    elif from_idx < HALL_START and to_idx >= HALL_START:
        state[to_idx] = unfreeze(state[to_idx])
    elif from_idx >= HALL_START and to_idx >= HALL_START:
        state[to_idx] = unfreeze(state[to_idx])

    return unfreeze_pathable_hallway(state)

def get_nbors(state):
    nbors = []

    def append_with_cost(state, cost):
        nbors.append((state, cost))

    for rtype in TYPES:
        # If the room is happy and full, skip moving anything in/from it
        if state[BOTS[rtype]] == state[TOPS[rtype]] == rtype:
            continue

        # Moving from bottom of room to top
        if state[BOTS[rtype]] != 0 and state[BOTS[rtype]] != rtype and state[TOPS[rtype]] == 0:
            # print("k6")
            append_with_cost(
                state_from_move(state, BOTS[rtype], TOPS[rtype]),
                COSTS[state[BOTS[rtype]]]
            )

        # Moving from top of room to bottom
        if state[TOPS[rtype]] != 0 and state[BOTS[rtype]] == 0 and state[TOPS[rtype]] == rtype:
            # print("k5")
            append_with_cost(
                state_from_move(state, TOPS[rtype], BOTS[rtype]),
                COSTS[state[TOPS[rtype]]]
            )

        # Moving from top of room to corner
        if state[TOPS[rtype]] != 0:
            for corner_idx in CORNERS_FOR_ROOM[rtype]:
                if state[corner_idx] == 0:
                    # print("k4")
                    append_with_cost(
                        state_from_move(state, TOPS[rtype], corner_idx),
                        2 * COSTS[state[TOPS[rtype]]]
                    )

    # Move across hallway: length 1 moves
    for from_idx, to_idx in VALID_LEN1_HALLMOVES:
        if state[from_idx] != 0 and not is_frozen(state[from_idx]) and state[to_idx] == 0:
            # print("k3")
            append_with_cost(
                state_from_move(state, from_idx, to_idx),
                COSTS[state[from_idx]]
            )

    # Move across hallway: skipping doors
    for left_i in range(len(CORNER_LIST) - 1):
        lcorner = CORNER_LIST[left_i]
        rcorner = CORNER_LIST[left_i + 1]
        for from_idx, to_idx in ((lcorner, rcorner), (rcorner, lcorner)):
            if state[from_idx] != 0 and not is_frozen(state[from_idx]) and state[to_idx] == 0:
                # print("k2")
                append_with_cost(
                    state_from_move(state, from_idx, to_idx),
                    2 * COSTS[state[from_idx]]
                )

    for rtype in range(1, len(CORNERS_FOR_ROOM)):
        for from_idx in CORNERS_FOR_ROOM[rtype]:
            if state[from_idx] != 0 and state[from_idx] == rtype \
               and not is_frozen(state[from_idx]) \
               and room_accepts(state, ridx_to_rtype(BOTS[rtype]), state[from_idx]):
                # print(f"k1: from_idx {from_idx}")
                append_with_cost(
                    state_from_move(state, from_idx, TOPS[rtype]),
                    2 * COSTS[state[from_idx]]
                )

    return nbors

def astar(starting_state, target_state, nbors_with_costs_fn, heuristic):
    visited = {}
    heap = []
    heapq.heappush(heap, (0, 0, starting_state))

    while len(heap) > 0:
        _, total_cost, state = heapq.heappop(heap)
        if state in visited:
            continue
        visited[state] = total_cost

        if state == target_state:
            print(f"Found target state with total cost {total_cost}")
            break

        nexts = nbors_with_costs_fn(state)
        for nbor, transition_cost in nexts:
            heapq.heappush(
                heap,
                (
                    total_cost + transition_cost + heuristic(nbor),
                    total_cost + transition_cost,
                    nbor
                )
            )

if __name__ == "__main__":
    starting_state = state_from_file(sys.argv[1])
    target_state = TARGET_STATE if len(sys.argv) == 2 else state_from_file(sys.argv[2])

    next_states = get_nbors(starting_state)
    astar(starting_state, target_state, get_nbors, lambda x: 0)
