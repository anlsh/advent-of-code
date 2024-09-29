#!/usr/bin/python3
inp = input()

f = 0
for i in inp:
    if i == "(":
        f += 1
    else:
        f -= 1

print(f)

f = 0
k = 1
for i in inp:
    if i == "(":
        f += 1
    else:
        f -= 1

    if f < 0:
        print(k)
        break
    k += 1
