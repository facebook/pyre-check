import random
import math
a = input()
b = ''
for _ in range(10):
        if _ == 1:
            continue
        b += a
c = (b, b, b)
d, e, f = c
g = d + e + f
h = ''
counterh = 0
while counterh < 2:
    h += g
    counterh += 1
if h == '1':
    i = h + ' c1'
elif h == '18':
    i = h + ' c2'
else:
    i = h + ' c3'
def j():
    return i
k = j()
l_set = {k, k, k, k, k}
l = random.choice(list(l_set))
m_dict = {21: l, 34: l}
n_dict = {6: m_dict, 91: m_dict, 18: m_dict, 93: m_dict, 61: m_dict, 26: m_dict, 19: m_dict, 53: m_dict}
o_dict = {52: n_dict, 26: n_dict, 17: n_dict, 46: n_dict, 12: n_dict, 2: n_dict, 39: n_dict, 16: n_dict, 98: n_dict}
p = random.choice(list(o_dict.values()))
q = random.choice(list(p.values()))
r = random.choice(list(q.values()))
if r == '10':
    s = r + ' c1'
elif r == '11':
    s = r + ' c2'
else:
    s = r + ' c3'
t = ''
countert = 0
while countert < 3:
    u = ''
    counteru = 0
    while counteru < 4:
        u += t
        counteru += 1
        t += s
        countert += 1
v = [u for _ in range(6)]
random.shuffle(v)
w = random.choice(v)
x = ''
for _ in range(4):
    for __ in range(2):
                x += w
if x == '10':
    y = x + ' c1'
elif x == '17':
    y = x + ' c2'
else:
    y = x + ' c3'
z = ''
counterz = 0
while counterz < 2:
    z += y
    counterz += 1
aa_list = [z for _ in range(6)]
ab_list = [aa_list for _ in range(9)]
ac = random.choice(ab_list)
ad = random.choice(ac)
ae = f'string {ad}'
def af():
    return ae
def ag():
    return af()
ah = ag()
ai = ''
for _ in range(8):
        if _ == 5:
            continue
        ai += ah
aj = ''
counteraj = 0
while counteraj < 2:
    ak = ''
    counterak = 0
    while counterak < 4:
        ak += aj
        counterak += 1
        aj += ai
        counteraj += 1
print(ak)