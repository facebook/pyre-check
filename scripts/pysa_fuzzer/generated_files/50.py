import random
import math
a = input()
b_dict = {78: a, 44: a, 36: a, 73: a, 65: a, 18: a, 90: a, 35: a, 72: a, 68: a}
c_dict = {56: b_dict, 3: b_dict}
d = random.choice(list(c_dict.values()))
e = random.choice(list(d.values()))
f = ''
for _ in range(5):
    f += e
g = f[0:]
def h():
    return g
def i():
    return h()
def j():
    return i()
k = j()
l = k + '9'
m = l + '5'
n = m + '1'
o = [n for _ in range(10)]
random.shuffle(o)
p = random.choice(o)
q = ''
for _ in range(6):
        if _ == 2:
            break
        q += p
r_list = [q for _ in range(9)]
s_list = [r_list for _ in range(2)]
t_list = [s_list for _ in range(9)]
u = random.choice(t_list)
v = random.choice(u)
w = random.choice(v)
if w == '2':
    x = w + ' c1'
elif w == '20':
    x = w + ' c2'
else:
    x = w + ' c3'
y = ''
for _ in range(2):
    z = ''
    for _ in range(2):
        aa = ''
        for _ in range(4):
            aa += z
            z += y
        y += x
ab = f'string {aa}'
ac = [ab for _ in range(7)]
random.shuffle(ac)
ad = random.choice(ac)
ae_list = [ad for _ in range(4)]
af_list = [ae_list for _ in range(5)]
ag_list = [af_list for _ in range(7)]
ah = random.choice(ag_list)
ai = random.choice(ah)
aj = random.choice(ai)
ak_list = [aj for _ in range(8)]
al = random.choice(ak_list)
am_dict = {76: al, 96: al, 92: al, 49: al, 12: al, 78: al, 53: al, 92: al}
an_dict = {55: am_dict, 56: am_dict, 48: am_dict}
ao = random.choice(list(an_dict.values()))
ap = random.choice(list(ao.values()))
if ap == '4':
    aq = ap + ' c1'
elif ap == '11':
    aq = ap + ' c2'
else:
    aq = ap + ' c3'
ar = ''
for _ in range(3):
    for __ in range(5):
                ar += aq
def at():
    return ar
def au():
    return at()
def av():
    return au()
aw = av()
print(aw)