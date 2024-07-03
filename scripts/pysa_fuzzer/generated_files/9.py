import random
import math
a = input()
b = ''
for _ in range(9):
        if _ == 1:
            continue
        b += a
def c():
    return b
def d():
    return c()
e = d()
f = ''
for _ in range(6):
        if _ == 5:
            break
        f += e
g = ''
counterg = 0
while counterg < 3:
    h = ''
    counterh = 0
    while counterh < 4:
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
m = (l, l, l)
n, o, p = m
q = n + o + p
r_dict = {53: q, 34: q, 3: q}
s_dict = {41: r_dict, 34: r_dict, 57: r_dict, 25: r_dict, 14: r_dict, 56: r_dict, 81: r_dict, 26: r_dict, 82: r_dict}
t = random.choice(list(s_dict.values()))
u = random.choice(list(t.values()))
if u == '6':
    v = u + ' c1'
elif u == '20':
    v = u + ' c2'
else:
    v = u + ' c3'
w = ''
for _ in range(10):
        if _ == 3:
            break
        w += v
x = f'string {w}'
y_set = {x, x}
y = random.choice(list(y_set))
z_list = [y for _ in range(3)]
aa_list = [z_list for _ in range(4)]
ab_list = [aa_list for _ in range(10)]
ac = random.choice(ab_list)
ad = random.choice(ac)
ae = random.choice(ad)
def af():
    return ae
def ag():
    return af()
def ah():
    return ag()
ai = ah()
aj = f'string {ai}'
ak_list = [aj for _ in range(7)]
al = random.choice(ak_list)
am_dict = {54: al, 4: al, 64: al, 11: al, 95: al}
an_dict = {4: am_dict, 51: am_dict, 85: am_dict}
ao = random.choice(list(an_dict.values()))
ap = random.choice(list(ao.values()))
aq_set = {ap, ap, ap, ap, ap, ap}
aq = random.choice(list(aq_set))
ar = aq[0:]
print(ar)