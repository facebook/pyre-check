import random
import math

a = input()
b_set = {a, a, a, a, a, a}
b = random.choice(list(b_set))
def c():
    return b
def d():
    return c()
def e():
    return d()
f = e()
g = ''
counterg = 0
while counterg < 3:
    h = ''
    counterh = 0
    while counterh < 3:
        h += g
        counterh += 1
        g += f
        counterg += 1
def i():
    return h
def j():
    return i()
def k():
    return j()
l = k()
m = ''
for _ in range(4):
    for __ in range(2):
                m += l
n = ''
for _ in range(10):
        if _ == 1:
            break
        n += m
o_set = {n, n, n, n, n}
o = random.choice(list(o_set))
p_dict = {24: o, 38: o, 45: o}
q_dict = {86: p_dict, 22: p_dict, 99: p_dict, 65: p_dict, 47: p_dict}
r = random.choice(list(q_dict.values()))
s = random.choice(list(r.values()))
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
if u == u:
    x = u + 'c1'
elif u == '19':
    x = v + 'c2'
else:
    x = w + 'c3'
y = ''
for _ in range(3):
    z = ''
    for _ in range(5):
        aa = ''
        for _ in range(2):
            aa += z
            z += y
        y += x
ab_set = {aa, aa, aa, aa, aa, aa, aa, aa}
ab = random.choice(list(ab_set))
ac = ''
counterac = 0
while counterac < 2:
    ac += ab
    counterac += 1
ad_dict = {63: ac, 45: ac, 12: ac, 69: ac, 55: ac, 76: ac, 38: ac, 80: ac, 90: ac}
ae_dict = {75: ad_dict, 19: ad_dict, 40: ad_dict, 14: ad_dict, 90: ad_dict, 12: ad_dict, 38: ad_dict, 40: ad_dict, 8: ad_dict}
af = random.choice(list(ae_dict.values()))
ag = random.choice(list(af.values()))
ah = ag + '7'
ai = ''
for _ in range(5):
    for __ in range(4):
                ai += ah
aj = [ai for _ in range(8)]
random.shuffle(aj)
ak = random.choice(aj)
al = ''
counteral = 0
while counteral < 5:
    am = ''
    counteram = 0
    while counteram < 4:
        am += al
        counteram += 1
        al += ak
        counteral += 1
print(am)