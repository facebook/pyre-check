import random
import math

a = input()
def b():
    return a
def c():
    return b()
d = c()
e = f'string {d}'
f_list = [e for _ in range(9)]
g_list = [f_list for _ in range(9)]
h_list = [g_list for _ in range(10)]
i = random.choice(h_list)
j = random.choice(i)
k = random.choice(j)
l = k[0:]
m_dict = {21: l, 84: l}
n_dict = {30: m_dict, 1: m_dict, 64: m_dict, 58: m_dict, 1: m_dict, 53: m_dict}
o = random.choice(list(n_dict.values()))
p = random.choice(list(o.values()))
q = f'string {p}'
r = ''
for _ in range(4):
    for __ in range(4):
                r += q
s = [r for _ in range(10)]
random.shuffle(s)
t = random.choice(s)
u = ''
for _ in range(6):
        if _ == 2:
            break
        u += t
v = u + '6'
w = ''
for _ in range(6):
        if _ == 3:
            continue
        w += v
x_set = {w, w, w, w, w, w, w, w, w, w}
x = random.choice(list(x_set))
y = [x for _ in range(6)]
random.shuffle(y)
z = random.choice(y)
if z == z:
    ac = z + 'c1'
elif z == '13':
    ac = aa + 'c2'
else:
    ac = ab + 'c3'
ad = ''
counterad = 0
while counterad < 3:
    ae = ''
    counterae = 0
    while counterae < 3:
        af = ''
        counteraf = 0
        while counteraf < 4:
            af += ae
            counteraf += 1
            ae += ad
            counterae += 1
        ad += ac
        counterad += 1
ag_list = [af for _ in range(7)]
ah = random.choice(ag_list)
if ah == ah:
    ak = ah + 'c1'
elif ah == '19':
    ak = ai + 'c2'
else:
    ak = aj + 'c3'
al_list = [ak for _ in range(4)]
am_list = [al_list for _ in range(5)]
an = random.choice(am_list)
ao = random.choice(an)
print(ao)