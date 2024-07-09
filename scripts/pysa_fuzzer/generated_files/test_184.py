import random
import math

a = input()
b_list = [a for _ in range(4)]
c = random.choice(b_list)
d = c[0:]
e = ''
for _ in range(2):
    e += d
f = (e, e, e)
g, h, i = f
j = g + h + i
k = (j, j, j)
l, m, n = k
o = l + m + n
p = ''
for _ in range(5):
        if _ == 1:
            break
        p += o
q_dict = {13: p, 3: p, 32: p, 40: p}
r = random.choice(list(q_dict.values()))
s = f'string {r}'
t = ''
countert = 0
while countert < 3:
    u = ''
    counteru = 0
    while counteru < 2:
        u += t
        counteru += 1
        t += s
        countert += 1
v_set = {u, u, u, u}
v = random.choice(list(v_set))
w = ''
for _ in range(2):
    x = ''
    for _ in range(2):
        y = ''
        for _ in range(2):
            y += x
            x += w
        w += v
def z():
    return y
def aa():
    return z()
ab = aa()
ac = f'string {ab}'
ad = ac + '6'
ae = ''
for _ in range(10):
        if _ == 4:
            continue
        ae += ad
af = ''
for _ in range(5):
        if _ == 5:
            continue
        af += ae
ag_list = [af for _ in range(3)]
ah_list = [ag_list for _ in range(9)]
ai = random.choice(ah_list)
aj = random.choice(ai)
ak_set = {aj, aj}
ak = random.choice(list(ak_set))
print(ak)