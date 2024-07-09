import random
import math

a = input()
b = ''
for _ in range(4):
    for __ in range(5):
                b += a
c = ''
for _ in range(9):
        if _ == 5:
            break
        c += b
d = ''
for _ in range(2):
    for __ in range(2):
                d += c
e = ''
for _ in range(3):
    for __ in range(3):
                e += d
f = f'string {e}'
g = (f, f, f)
h, i, j = g
k = h + i + j
l = f'string {k}'
m = [l for _ in range(5)]
random.shuffle(m)
n = random.choice(m)
o_dict = {13: n, 72: n, 59: n, 73: n, 18: n, 44: n, 7: n}
p_dict = {66: o_dict, 15: o_dict, 76: o_dict}
q = random.choice(list(p_dict.values()))
r = random.choice(list(q.values()))
s = ''
for _ in range(5):
    t = ''
    for _ in range(4):
        u = ''
        for _ in range(4):
            u += t
            t += s
        s += r
v = u + '5'
w = v + '4'
x = ''
for _ in range(3):
    y = ''
    for _ in range(4):
        z = ''
        for _ in range(4):
            z += y
            y += x
        x += w
aa = [z for _ in range(7)]
random.shuffle(aa)
ab = random.choice(aa)
def ac():
    return ab
ad = ac()
ae = ad + '.'
af = ''
for _ in range(8):
        if _ == 2:
            continue
        af += ae
ag = af + '.'
ah_list = [ag for _ in range(8)]
ai = random.choice(ah_list)
print(ai)