import abc
import sys
import heapq

# Will have to change ROOM_SIZE depending on what file we're running with
# There is some bounds checking done so that if you run w/ an
# incorrectly-sized file, the script will error
ROOM_SIZE = 4

# Fundamental parameters, should never have to touch these
ROOMS = (1, 2, 3, 4)
APODS = ROOMS
HALLWAY_SIZE = 11
FROZEN_OFFSET = 4

def cost(apod):
    return 10 ** (apod - 1)
def freeze(n):
    return n + FROZEN_OFFSET if 1 <= n <= FROZEN_OFFSET else n
def unfreeze(n):
    return n - FROZEN_OFFSET if n > FROZEN_OFFSET else n
def is_frozen(n):
    return n != unfreeze(n) if n != 0 else False

# Calculated constants used for a lot of stuff
DOORS = {room: 2 * room for room in ROOMS}
CORNERS_FOR_ROOM = {room: (DOORS[room] - 1, DOORS[room] + 1) for room in ROOMS}
CORNER_LIST = tuple(
    [CORNERS_FOR_ROOM[room][0] for room in ROOMS]
    + [CORNERS_FOR_ROOM[ROOMS[-1]][1]]
)
VALID_LEN1_HALLMOVES = (
    (0, 1), (1, 0),
    (HALLWAY_SIZE - 2, HALLWAY_SIZE - 1),
    (HALLWAY_SIZE - 1, HALLWAY_SIZE - 2)
)
POSITIONS =[(0, i) for i in range(HALLWAY_SIZE)]
for room in ROOMS:
    for room_i in range(ROOM_SIZE):
        POSITIONS.append((room, room_i))
POSITIONS = tuple(POSITIONS)

class State(abc.ABC):
    def __getitem__(self, key):
        raise NotImplementedError()
    def __str__(self,):
        def n2str(n):
            return str(n) if n > 0 else "."
        lines = []
        lines.append(" ".join([n2str(self[(0, hall_i)]) for hall_i in range(HALLWAY_SIZE)]))
        for room_i in range(ROOM_SIZE):
            lines.append("    " + "   ".join([n2str(self[room, room_i]) for room in ROOMS]))
        return "\n" + "\n".join(lines)

class MutableState(State):
    def __setitem__(self, key, val):
        raise NotImplementedError()
    def __freeze__(self, key, val):
        raise NotImplementedError()
    @classmethod
    def solution_state(cls,):
        mut = cls()
        for i in range(HALLWAY_SIZE):
            mut[0, i] = 0
        for room in ROOMS:
            for room_i in range(ROOM_SIZE):
                mut[room, room_i] = room
        return mut.freeze()
    @classmethod
    def from_file(cls, fname):
        def let2n(let):
            return 0 if let == "." else ord(let) - ord("A") + 1
        with open(fname) as f:
            lines = [l.rstrip() for l in f.readlines()]

        room_size = len(lines) - 3
        if room_size != ROOM_SIZE:
            raise RuntimeError("Mismatch between hardcoded and parsed room sizes")

        mut = cls()
        for hall_i, let in enumerate(lines[1][1:-1]):
            mut[0, hall_i] = let2n(let)
        for room_i in range(room_size):
            str_line = 2 + room_i
            for room in ROOMS:
                mut[room, room_i] = let2n(lines[str_line][3 + 2*(room - 1)])

        return mut.freeze()
    def __eq__(self, o):
        raise RuntimeError("Dont compare these things!")
    def __hash__(self, o):
        raise RuntimeError("Dont hash a mutable!")

class SingleArrayBackedState(MutableState):
    @staticmethod
    def pos2key(key):
        if key[0] == 0:
            return key[1]
        else:
            return HALLWAY_SIZE + ((key[0] - 1) * ROOM_SIZE) + key[1]
    def __init__(self, frozen=None):
        if not frozen:
            self.state = [0] * (HALLWAY_SIZE + 4*ROOM_SIZE)
        else:
            self.state = [c for c in frozen]
    def __getitem__(self, key):
        return self.state[self.pos2key(key)]
    def __setitem__(self, key, val):
        self.state[self.pos2key(key)] = val
    def freeze(self,):
        return tuple(self.state)

class MultipleArrayBackedState(MutableState):
    def __init__(self, frozen=None):
        if frozen is None:
            self.state = [[0] * HALLWAY_SIZE] \
                + [[0] * ROOM_SIZE for _ in range(len(ROOMS))]
        else:
            self.state = [list(place) for place in frozen]
    def __getitem__(self, key):
        return self.state[key[0]][key[1]]
    def __setitem__(self, key, val):
        self.state[key[0]][key[1]] = val
    def freeze(self,):
        return tuple((tuple(place) for place in self.state))

def room_accepts(state, room, apod):
    if room != apod:
        return False
    elif state[room, 0] != 0:
        return 0
    else:
        for room_i in range(ROOM_SIZE):
            if state[room, room_i] != room and state[room, room_i] != 0:
                return False
        return True

def mutating_unfreeze_hallway(mut_state):
    for room in ROOMS:
        if not room_accepts(mut_state, room, room):
            continue
        for hall_is in (
                range(DOORS[room], HALLWAY_SIZE),
                range(0, DOORS[room])[::-1]
        ):
            for hall_i in hall_is:
                if mut_state[0, hall_i] == 0:
                    continue
                else:
                    if unfreeze(mut_state[0, hall_i]) == room:
                        mut_state[0, hall_i] = unfreeze(mut_state[0, hall_i])
                    break

def get_transition_fn(stype):

    def after_move(frozen_state, fromloc, toloc):
        # Create mutable copy of backing state
        mutable = stype(frozen_state)

        # Checks to make sure we're doing things: should NEVER be hit #
        if mutable[fromloc] == 0:
            raise RuntimeError("Trying to move from empty pos!")
        elif is_frozen(mutable[fromloc]):
            raise RuntimeError(f"Trying to move from frozen pos! {from_idx}")
        elif mutable[toloc] != 0:
            raise RuntimeError("Trying to move into occupied pos!!")
        elif (toloc[0] == 0) and (toloc[1] in DOORS.values()):
            raise RuntimeError("Trying to stop in doorway!")
        elif (fromloc[0] == 0) and (toloc[0] > 0) \
            and not room_accepts(mutable, toloc[0], mutable[fromloc]):
            raise RuntimeError("Trying to move apod into non-accepting hallway")

        # Perform the move
        mutable[toloc] = mutable[fromloc]
        mutable[fromloc] = 0

        # freeze the stuff in the hallway except for the pod we just moved
        # (if we in fact moved it into the hallway)
        for i in range(HALLWAY_SIZE):
            if toloc == (0, i):
                continue
            mutable[0, i] = freeze(mutable[0, i])
        mutating_unfreeze_hallway(mutable)
        return mutable.freeze()

    def get_costs_and_nbors(frozen_state,):

        state = stype(frozen_state)

        costs_and_nbors = []
        def consider_move(from_loc, to_loc, cost):
            costs_and_nbors.append((
                cost,
                after_move(frozen_state, from_loc, to_loc)
            ))

        # Consider possible moves to/from rooms first
        for room in ROOMS:
            has_incorrect_apods = False
            bottommost_incorrect_apod_pos = None
            bottommost_correct_apod_pos = None
            topmost_apod_pos = None
            bottommost_empty_pos = None
            full = True
            for room_i in range(ROOM_SIZE):
                if state[room, room_i] == 0:
                    full = False
                    bottommost_empty_pos = room_i
                else:
                    if state[room, room_i] != room:
                        bottommost_incorrect_apod_pos = room_i
                        has_incorrect_apods = True
                    else:
                        bottommost_correct_apod_pos = room_i
                    topmost_apod_pos = topmost_apod_pos or room_i

            if full and not has_incorrect_apods:
                continue
            if state[room, 0] == 0 and has_incorrect_apods:
                consider_move(
                    (room, topmost_apod_pos), (room, 0),
                    topmost_apod_pos * cost(state[room, topmost_apod_pos])
                )
            # Move from top of room to corners
            if state[room, 0] != 0 and has_incorrect_apods:
                for corner_i in CORNERS_FOR_ROOM[room]:
                    if state[0, corner_i] == 0:
                        consider_move(
                            (room, 0), (0, corner_i),
                            2 * cost(state[room, 0])
                        )
            if not has_incorrect_apods:
                for room_i in range(bottommost_empty_pos)[::-1]:
                    if state[room, room_i] == room:
                        consider_move(
                            (room, room_i), (room, bottommost_empty_pos),
                            (bottommost_empty_pos - room_i) * cost(state[room, room_i])
                        )

            # Move from corners to bottommost spot in room
            if (not has_incorrect_apods) \
               and (bottommost_empty_pos < (topmost_apod_pos or ROOM_SIZE)):
                for corner_i in CORNERS_FOR_ROOM[room]:
                    if state[0, corner_i] != room:
                        continue
                    consider_move(
                        (0, corner_i), (room, bottommost_empty_pos),
                        (2 + bottommost_empty_pos) * cost(state[0, corner_i])
                    )

        # Move across hallway: length 1 moves
        for from_idx, to_idx in VALID_LEN1_HALLMOVES:
            if state[0, from_idx] != 0 and not is_frozen(state[0, from_idx]) \
                and state[0, to_idx] == 0:
                consider_move(
                    (0, from_idx), (0, to_idx),
                    cost(state[0, from_idx])
                )

        # Move across hallway between corners
        for left_i in range(len(CORNER_LIST) - 1):
            lcorner = CORNER_LIST[left_i]
            rcorner = CORNER_LIST[left_i + 1]
            for from_idx, to_idx in ((lcorner, rcorner), (rcorner, lcorner)):
                if state[0, from_idx] != 0 and not is_frozen(state[0, from_idx]) \
                    and state[0, to_idx] == 0:
                    consider_move(
                        (0, from_idx), (0, to_idx),
                        2 * cost(state[0, from_idx])
                    )
        return costs_and_nbors

    return get_costs_and_nbors

def underestimate_cost_to_solve(state,):
    est = 0
    # Look at the apods in room they don't belong in.
    # Assuming they can phase through other apods to the top of the room
    # they belong in, add the cost of that path
    for room in ROOMS:
        for room_i in range(ROOM_SIZE):
            if state[room, room_i] == 0:
                continue
            elif state[room, room_i] == room:
                continue
            else:
                plen = (room_i + 1) + 1 + abs(DOORS[state[room, room_i]] - DOORS[room])
                est += plen * cost(state[room, room_i])

    # Look at things in hallways. Again assuming they can phase to the mouth
    # of the correct room, calculate that path's cost
    for hall_i in range(HALLWAY_SIZE):
        if state[0, hall_i] == 0:
            continue
        else:
            apod = unfreeze(state[0, hall_i])
            plen = 1 + abs(DOORS[apod] - hall_i)
            est += plen * cost(apod)

    return est

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
        for transition_cost, nbor in nexts:
            heapq.heappush(
                heap,
                (
                    total_cost + transition_cost + heuristic(nbor),
                    total_cost + transition_cost,
                    nbor
                )
            )

if __name__ == "__main__":

    stype = SingleArrayBackedState

    starting_state = stype.from_file(sys.argv[1])
    target_state = stype.solution_state() \
        if len(sys.argv) == 2 else stype.from_file(sys.argv[2])

    astar(
        starting_state, target_state,
        get_transition_fn(stype),
        lambda x: underestimate_cost_to_solve(stype(x))
    )
