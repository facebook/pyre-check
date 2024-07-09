import random
import math

a = input()
b = ''
for _ in range(9):
        if _ == 4:
            continue
        b += a
c = ''
for _ in range(3):
    d = ''
    for _ in range(4):
        e = ''
        for _ in range(4):
            e += d
            d += c
        c += b
f = f'string {e}'
g = ''
counterg = 0
while counterg < 5:
    g += f
    counterg += 1
h = ''
for _ in range(10):
        if _ == 1:
            continue
        h += g
i = h + '2'
j = ''
counterj = 0
while counterj < 4:
    k = ''
    counterk = 0
    while counterk < 4:
        l = ''
        counterl = 0
        while counterl < 3:
            l += k
            counterl += 1
            k += j
            counterk += 1
        j += i
        counterj += 1
m = l + '2'
n = m + '3'
o = f'string {n}'
p_dict = {88: o, 86: o, 77: o, 72: o}
q = random.choice(list(p_dict.values()))
r = f'string {q}'
s_set = {r, r, r}
s = random.choice(list(s_set))
t = ''
for _ in range(5):
        if _ == 3:
            break
        t += s
u_list = [t for _ in range(9)]
v_list = [u_list for _ in range(6)]
w = random.choice(v_list)
x = random.choice(w)
y = ''
for _ in range(3):
    for __ in range(3):
                y += x
z = ''
counterz = 0
while counterz < 4:
    z += y
    counterz += 1
aa = z + '2'
ab = aa + '4'
ac = ab[0:]
print(ac)