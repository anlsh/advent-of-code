#!/usr/bin/python3
private = input()

import hashlib

def hashstr(private, i):
    return hashlib.md5((private + str(i)).encode('ascii')).hexdigest()

def answer(NUM_LEADING_ZEROS):
    hsh_mask = ((16** NUM_LEADING_ZEROS) - 1) << ((32 - NUM_LEADING_ZEROS) * 4)
    i = 0
    while True:
        hshstr = hashstr(private, i)
        if int(hshstr, 16) & hsh_mask == 0:
            break
        i += 1
    return i

print(answer(5))
print(answer(6))
