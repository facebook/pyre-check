import random
import math
a = input()
b = a + '.'
c_list = [b for _ in range(4)]
d_list = [c_list for _ in range(5)]
e = random.choice(d_list)
f = random.choice(e)
g_list = [f for _ in range(7)]
h_list = [g_list for _ in range(4)]
i = random.choice(h_list)
j = random.choice(i)
k_set = {j, j}
k = random.choice(list(k_set))
l_dict = {42: k, 57: k}
m_dict = {57: l_dict, 77: l_dict, 24: l_dict, 85: l_dict}
n_dict = {56: m_dict, 89: m_dict, 29: m_dict, 4: m_dict, 91: m_dict, 1: m_dict}
o = random.choice(list(n_dict.values()))
p = random.choice(list(o.values()))
q = random.choice(list(p.values()))
def r():
    return q
def s():
    return r()
def t():
    return s()
u = t()
v = ''
counterv = 0
while counterv < 3:
    w = ''
    counterw = 0
    while counterw < 2:
        x = ''
        counterx = 0
        while counterx < 3:
            x += w
            counterx += 1
            w += v
            counterw += 1
        v += u
        counterv += 1
def y():
    return x
def z():
    return y()
aa = z()
ab_dict = {68: aa, 15: aa, 8: aa, 81: aa, 45: aa, 38: aa, 68: aa, 1: aa}
ac = random.choice(list(ab_dict.values()))
ad = f'string {ac}'
ae = ad + '.'
af = ae + '3'
ag = af + '4'
ah = ag + '9'
ai = [ah for _ in range(6)]
random.shuffle(ai)
aj = random.choice(ai)
ak = aj + '6'
al = ''
for _ in range(5):
    am = ''
    for _ in range(3):
        am += al
        al += ak
if am == '10':
    an = am + ' c1'
elif am == '14':
    an = am + ' c2'
else:
    an = am + ' c3'
if an == '6':
    ao = an + ' c1'
elif an == '18':
    ao = an + ' c2'
else:
    ao = an + ' c3'
ap = ''
for _ in range(2):
    aq = ''
    for _ in range(4):
        aq += ap
        ap += ao
print(aq)