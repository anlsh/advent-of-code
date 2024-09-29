#!/usr/bin/python3
import sys

lines = list(sys.stdin.readlines())

def nice(s):
    num_vowels = 0

    double_detected = False
    bad_bigrams = ["ab", "cd", "pq", "xy"]

    for i in range(len(s)):
        if s[i] in "aeiou":
            num_vowels += 1

        if i != len(s) - 1:
            bigram = s[i:i + 2]
            if bigram[0] == bigram[1]:
                double_detected = True
            if bigram in bad_bigrams:
                return False

    return num_vowels >= 3 and double_detected

def better_nice(s):
    triple_detected = False
    for i in range(len(s) - 2):
        if s[i] == s[i + 2]:
            triple_detected = True
            break

    if not triple_detected:
        return False

    seen_bigrams = {}
    for i in range(len(s) - 1):
        bigram = s[i:i + 2]
        if i == 0:
            seen_bigrams[bigram] = 0
            continue

        if bigram in seen_bigrams:
            if i != seen_bigrams[bigram] + 1:
                return True
            continue
        else:
            seen_bigrams[bigram] = i

    return False

print(sum([int(nice(s)) for s in lines]))
print(sum([int(better_nice(s)) for s in lines]))
