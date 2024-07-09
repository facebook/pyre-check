import random
import math

a = input()
b = [a for _ in range(9)]
random.shuffle(b)
c = random.choice(b)
d = ''
for _ in range(5):
        if _ == 3:
            break
        d += c
e = d[0:]
f = ''
counterf = 0
while counterf < 5:
    g = ''
    counterg = 0
    while counterg < 5:
        g += f
        counterg += 1
        f += e
        counterf += 1
h = ''
for _ in range(2):
    i = ''
    for _ in range(3):
        j = ''
        for _ in range(5):
            j += i
            i += h
        h += g
k_list = [j for _ in range(10)]
l_list = [k_list for _ in range(8)]
m_list = [l_list for _ in range(3)]
n = random.choice(m_list)
o = random.choice(n)
p = random.choice(o)
q = ''
for _ in range(4):
    r = ''
    for _ in range(4):
        s = ''
        for _ in range(3):
            s += r
            r += q
        q += p
t = ''
countert = 0
while countert < 5:
    u = ''
    counteru = 0
    while counteru < 2:
        v = ''
        counterv = 0
        while counterv < 5:
            v += u
            counterv += 1
            u += t
            counteru += 1
        t += s
        countert += 1
w = v + '.'
x = f'string {w}'
y = x[0:]
z = (y, y, y)
aa, ab, ac = z
ad = aa + ab + ac
ae = ''
for _ in range(9):
        if _ == 4:
            break
        ae += ad
af = ae[0:]
ag = [af for _ in range(5)]
random.shuffle(ag)
ah = random.choice(ag)
ai = ''
for _ in range(3):
    aj = ''
    for _ in range(4):
        aj += ai
        ai += ah
ak_dict = {3: aj, 94: aj, 51: aj, 56: aj, 2: aj, 96: aj}
al = random.choice(list(ak_dict.values()))
am_list = [al for _ in range(10)]
an = random.choice(am_list)
print(an)