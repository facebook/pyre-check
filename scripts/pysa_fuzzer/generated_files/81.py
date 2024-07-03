import random
import math
a = input()
b = ''
for _ in range(5):
    for __ in range(5):
                b += a
c = ''
counterc = 0
while counterc < 2:
    c += b
    counterc += 1
d_dict = {43: c, 32: c, 83: c, 18: c}
e_dict = {82: d_dict, 49: d_dict, 40: d_dict}
f_dict = {10: e_dict, 78: e_dict, 43: e_dict, 25: e_dict, 19: e_dict, 58: e_dict}
g = random.choice(list(f_dict.values()))
h = random.choice(list(g.values()))
i = random.choice(list(h.values()))
def j():
    return i
def k():
    return j()
l = k()
m = ''
for _ in range(10):
        if _ == 2:
            continue
        m += l
n = ''
for _ in range(5):
    n += m
o = [n for _ in range(7)]
random.shuffle(o)
p = random.choice(o)
q = f'string {p}'
r = q + '8'
s = r + '6'
t = s + '8'
u_set = {t, t, t, t}
u = random.choice(list(u_set))
def v():
    return u
w = v()
x_list = [w for _ in range(9)]
y_list = [x_list for _ in range(8)]
z = random.choice(y_list)
aa = random.choice(z)
ab = aa + '2'
ac = ab + '9'
ad_set = {ac, ac, ac, ac, ac, ac, ac, ac, ac}
ad = random.choice(list(ad_set))
ae = [ad for _ in range(10)]
random.shuffle(ae)
af = random.choice(ae)
ag = ''
for _ in range(3):
    ah = ''
    for _ in range(4):
        ai = ''
        for _ in range(5):
            ai += ah
            ah += ag
        ag += af
aj = ''
counteraj = 0
while counteraj < 2:
    ak = ''
    counterak = 0
    while counterak < 2:
        al = ''
        counteral = 0
        while counteral < 4:
            al += ak
            counteral += 1
            ak += aj
            counterak += 1
        aj += ai
        counteraj += 1
am = ''
for _ in range(3):
    for __ in range(3):
                am += al
print(am)