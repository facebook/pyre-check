import random
import math
a = input()
b_dict = {39: a, 60: a}
c_dict = {17: b_dict, 27: b_dict, 89: b_dict, 24: b_dict, 66: b_dict, 76: b_dict, 9: b_dict, 69: b_dict}
d = random.choice(list(c_dict.values()))
e = random.choice(list(d.values()))
f_set = {e, e, e, e, e, e, e, e, e}
f = random.choice(list(f_set))
g = ''
for _ in range(10):
        if _ == 3:
            break
        g += f
h = ''
for _ in range(7):
        if _ == 5:
            continue
        h += g
i_dict = {49: h, 97: h}
j_dict = {44: i_dict, 53: i_dict, 85: i_dict, 48: i_dict, 64: i_dict, 36: i_dict, 21: i_dict, 74: i_dict}
k = random.choice(list(j_dict.values()))
l = random.choice(list(k.values()))
def m():
    return l
def n():
    return m()
o = n()
p_dict = {21: o, 89: o, 91: o, 94: o, 83: o, 60: o, 66: o, 96: o, 21: o, 1: o}
q_dict = {90: p_dict, 11: p_dict, 42: p_dict, 64: p_dict, 69: p_dict, 94: p_dict, 90: p_dict, 11: p_dict}
r = random.choice(list(q_dict.values()))
s = random.choice(list(r.values()))
t = ''
for _ in range(3):
    u = ''
    for _ in range(5):
        v = ''
        for _ in range(4):
            v += u
            u += t
        t += s
w = ''
counterw = 0
while counterw < 5:
    x = ''
    counterx = 0
    while counterx < 4:
        y = ''
        countery = 0
        while countery < 4:
            y += x
            countery += 1
            x += w
            counterx += 1
        w += v
        counterw += 1
z = ''
for _ in range(3):
    aa = ''
    for _ in range(5):
        aa += z
        z += y
ab_set = {aa, aa, aa, aa, aa, aa}
ab = random.choice(list(ab_set))
def ac():
    return ab
ad = ac()
ae = ad + '1'
af = ae + '9'
ag = af + '9'
ah = (ag, ag, ag)
ai, aj, ak = ah
al = ai + aj + ak
am_list = [al for _ in range(10)]
an = random.choice(am_list)
ao = [an for _ in range(8)]
random.shuffle(ao)
ap = random.choice(ao)
aq_list = [ap for _ in range(2)]
ar_list = [aq_list for _ in range(2)]
at = random.choice(ar_list)
au = random.choice(at)
def av():
    return au
aw = av()
print(aw)