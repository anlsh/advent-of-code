#!/usr/bin/python3
import sys

def count_chars(s: str):
    code_chars = len(s)

    mem_chars = 0
    s = s[1:-1]

    i = 0
    while i < len(s):
        if s[i] == "\\":
            if s[i + 1] == "\\":
                mem_chars += 1
                i += 2
            elif s[i + 1] == '"':
                mem_chars += 1
                i += 2
            else:
                if s[i + 1] != "x":
                    raise RuntimeError("NAKUPUKUUUUU")
                mem_chars += 1
                i += 4
        else:
            mem_chars += 1
            i += 1

    return code_chars, mem_chars

def encode_chars(s: str):
    encoded_len = 0

    for char in s:
        if char == "\\":
            encoded_len += 2
        elif char == '"':
            encoded_len += 2
        else:
            encoded_len += 1

    return encoded_len + 2, len(s)

if __name__ == "__main__":
    inp = sys.stdin.readlines()

    print(sum(a - b for (a, b) in map(count_chars, inp)))
    print(sum(a - b for (a, b) in map(encode_chars, inp)))
