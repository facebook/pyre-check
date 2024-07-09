import random
import math

a = input()
b = a + '.'
c_dict = {18: b, 75: b, 66: b, 32: b, 12: b, 43: b, 32: b, 22: b}
d_dict = {99: c_dict, 1: c_dict, 86: c_dict, 62: c_dict, 29: c_dict, 16: c_dict}
e = random.choice(list(d_dict.values()))
f = random.choice(list(e.values()))
g = ''
counterg = 0
while counterg < 4:
    h = ''
    counterh = 0
    while counterh < 4:
        h += g
        counterh += 1
        g += f
        counterg += 1
i_list = [h for _ in range(2)]
j_list = [i_list for _ in range(6)]
k_list = [j_list for _ in range(3)]
l = random.choice(k_list)
m = random.choice(l)
n = random.choice(m)
o = n + '.'
p_dict = {65: o, 14: o, 65: o, 91: o, 29: o}
q_dict = {62: p_dict, 35: p_dict, 64: p_dict, 93: p_dict, 22: p_dict}
r = random.choice(list(q_dict.values()))
s = random.choice(list(r.values()))
t = s + '7'
u = t + '6'
v = u + '2'
def w():
    return v
def x():
    return w()
y = x()
z = y[0:]
aa = ''
for _ in range(4):
    for __ in range(3):
                aa += z
ab_list = [aa for _ in range(6)]
ac_list = [ab_list for _ in range(7)]
ad = random.choice(ac_list)
ae = random.choice(ad)
af_dict = {26: ae, 62: ae, 68: ae}
ag = random.choice(list(af_dict.values()))
ah = ag + '8'
ai = ah + '9'
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
al = ak + '9'
am = al + '3'
an = am + '8'
def ao():
    return an
def ap():
    return ao()
def aq():
    return ap()
ar = aq()
def at():
    return ar
au = at()
av_dict = {13: au, 44: au, 13: au, 41: au, 83: au, 41: au, 40: au}
aw = random.choice(list(av_dict.values()))
print(aw)