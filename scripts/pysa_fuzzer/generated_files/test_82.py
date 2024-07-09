import random
import math

a = input()
b = [a for _ in range(8)]
random.shuffle(b)
c = random.choice(b)
d_dict = {14: c, 2: c, 27: c}
e_dict = {34: d_dict, 36: d_dict, 9: d_dict, 54: d_dict, 65: d_dict, 77: d_dict, 49: d_dict, 86: d_dict}
f = random.choice(list(e_dict.values()))
g = random.choice(list(f.values()))
def h():
    return g
i = h()
j = i[0:]
k = [j for _ in range(9)]
random.shuffle(k)
l = random.choice(k)
m_dict = {61: l, 1: l, 50: l, 68: l, 16: l, 56: l, 17: l}
n_dict = {75: m_dict, 60: m_dict, 45: m_dict}
o_dict = {31: n_dict, 10: n_dict, 29: n_dict, 72: n_dict, 59: n_dict, 19: n_dict, 98: n_dict, 25: n_dict, 85: n_dict}
p = random.choice(list(o_dict.values()))
q = random.choice(list(p.values()))
r = random.choice(list(q.values()))
s = ''
for _ in range(2):
    s += r
def t():
    return s
def u():
    return t()
v = u()
w_set = {v, v, v, v}
w = random.choice(list(w_set))
x = ''
for _ in range(2):
    y = ''
    for _ in range(3):
        z = ''
        for _ in range(3):
            z += y
            y += x
        x += w
aa = ''
for _ in range(5):
        if _ == 5:
            continue
        aa += z
ab_list = [aa for _ in range(3)]
ac_list = [ab_list for _ in range(9)]
ad = random.choice(ac_list)
ae = random.choice(ad)
af = ''
for _ in range(4):
    for __ in range(4):
                af += ae
ag = ''
for _ in range(2):
    for __ in range(3):
                ag += af
ah = ''
for _ in range(4):
    for __ in range(5):
                ah += ag
ai = [ah for _ in range(8)]
random.shuffle(ai)
aj = random.choice(ai)
ak = ''
for _ in range(4):
    ak += aj
al = ''
for _ in range(2):
    for __ in range(2):
                al += ak
print(al)