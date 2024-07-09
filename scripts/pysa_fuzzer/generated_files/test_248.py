import random
import math

a = input()
b = ''
counterb = 0
while counterb < 3:
    c = ''
    counterc = 0
    while counterc < 2:
        c += b
        counterc += 1
        b += a
        counterb += 1
d = [c for _ in range(7)]
random.shuffle(d)
e = random.choice(d)
f = e + '6'
g = f + '8'
h_dict = {57: g, 43: g, 50: g, 58: g, 69: g, 58: g, 6: g, 25: g}
i = random.choice(list(h_dict.values()))
j = f'string {i}'
k = ''
for _ in range(3):
    l = ''
    for _ in range(2):
        l += k
        k += j
m = ''
for _ in range(5):
        if _ == 4:
            continue
        m += l
n_dict = {63: m, 96: m}
o_dict = {62: n_dict, 26: n_dict, 80: n_dict, 67: n_dict, 42: n_dict, 51: n_dict, 58: n_dict}
p_dict = {54: o_dict, 84: o_dict, 22: o_dict, 61: o_dict}
q = random.choice(list(p_dict.values()))
r = random.choice(list(q.values()))
s = random.choice(list(r.values()))
t = s + '7'
u = t + '8'
v = u + '6'
def w():
    return v
def x():
    return w()
def y():
    return x()
z = y()
aa = (z, z, z)
ab, ac, ad = aa
ae = ab + ac + ad
af = ae + '.'
ag = ''
counterag = 0
while counterag < 4:
    ah = ''
    counterah = 0
    while counterah < 3:
        ah += ag
        counterah += 1
        ag += af
        counterag += 1
ai = ''
counterai = 0
while counterai < 2:
    aj = ''
    counteraj = 0
    while counteraj < 3:
        aj += ai
        counteraj += 1
        ai += ah
        counterai += 1
ak = [aj for _ in range(10)]
random.shuffle(ak)
al = random.choice(ak)
def am():
    return al
def an():
    return am()
ao = an()
ap_set = {ao, ao, ao, ao, ao, ao, ao, ao, ao, ao}
ap = random.choice(list(ap_set))
aq_dict = {1: ap, 77: ap, 16: ap, 2: ap, 52: ap, 89: ap, 94: ap}
ar_dict = {94: aq_dict, 81: aq_dict, 53: aq_dict, 69: aq_dict, 88: aq_dict, 50: aq_dict}
at_dict = {53: ar_dict, 53: ar_dict, 73: ar_dict, 38: ar_dict, 50: ar_dict, 24: ar_dict, 18: ar_dict, 96: ar_dict}
au = random.choice(list(at_dict.values()))
av = random.choice(list(au.values()))
aw = random.choice(list(av.values()))
print(aw)