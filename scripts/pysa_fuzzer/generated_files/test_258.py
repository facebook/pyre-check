import random
import math

a = input()
b_dict = {8: a, 55: a, 78: a, 57: a, 50: a, 2: a, 89: a, 42: a}
c = random.choice(list(b_dict.values()))
d = ''
for _ in range(2):
    for __ in range(4):
                d += c
e_list = [d for _ in range(6)]
f = random.choice(e_list)
g_list = [f for _ in range(8)]
h_list = [g_list for _ in range(6)]
i_list = [h_list for _ in range(7)]
j = random.choice(i_list)
k = random.choice(j)
l = random.choice(k)
m = ''
for _ in range(2):
    for __ in range(4):
                m += l
def n():
    return m
o = n()
p = ''
for _ in range(6):
        if _ == 3:
            break
        p += o
q = ''
for _ in range(4):
    r = ''
    for _ in range(2):
        s = ''
        for _ in range(2):
            s += r
            r += q
        q += p
t = [s for _ in range(8)]
random.shuffle(t)
u = random.choice(t)
v = u[0:]
def w():
    return v
x = w()
y = [x for _ in range(5)]
random.shuffle(y)
z = random.choice(y)
aa = f'string {z}'
ab = ''
for _ in range(2):
    ac = ''
    for _ in range(5):
        ad = ''
        for _ in range(3):
            ad += ac
            ac += ab
        ab += aa
ae_list = [ad for _ in range(3)]
af = random.choice(ae_list)
ag = af[0:]
ah = (ag, ag, ag)
ai, aj, ak = ah
al = ai + aj + ak
am_dict = {83: al, 89: al, 53: al, 82: al, 94: al}
an_dict = {34: am_dict, 86: am_dict, 65: am_dict, 16: am_dict, 85: am_dict, 13: am_dict, 91: am_dict, 67: am_dict, 1: am_dict}
ao = random.choice(list(an_dict.values()))
ap = random.choice(list(ao.values()))
print(ap)