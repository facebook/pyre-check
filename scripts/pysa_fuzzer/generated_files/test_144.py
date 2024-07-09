import random
import math

a = input()
b = ''
for _ in range(5):
        if _ == 5:
            break
        b += a
c = (b, b, b)
d, e, f = c
g = d + e + f
h = (g, g, g)
i, j, k = h
l = i + j + k
m_dict = {63: l, 21: l, 60: l, 26: l, 39: l, 21: l}
n = random.choice(list(m_dict.values()))
def o():
    return n
p = o()
q = (p, p, p)
r, s, t = q
u = r + s + t
v = u[0:]
w = (v, v, v)
x, y, z = w
aa = x + y + z
ab = ''
for _ in range(5):
    for __ in range(4):
                ab += aa
ac_list = [ab for _ in range(5)]
ad = random.choice(ac_list)
ae_list = [ad for _ in range(9)]
af = random.choice(ae_list)
ag = ''
for _ in range(2):
    ah = ''
    for _ in range(4):
        ai = ''
        for _ in range(3):
            ai += ah
            ah += ag
        ag += af
aj = f'string {ai}'
def ak():
    return aj
def al():
    return ak()
am = al()
an = (am, am, am)
ao, ap, aq = an
ar = ao + ap + aq
at = (ar, ar, ar)
au, av, aw = at
ax = au + av + aw
ay_dict = {81: ax, 55: ax, 65: ax, 67: ax, 12: ax, 61: ax, 37: ax, 13: ax}
az_dict = {57: ay_dict, 82: ay_dict}
ba_dict = {78: az_dict, 74: az_dict, 13: az_dict, 13: az_dict}
bb = random.choice(list(ba_dict.values()))
bc = random.choice(list(bb.values()))
bd = random.choice(list(bc.values()))
be = (bd, bd, bd)
bf, bg, bh = be
bi = bf + bg + bh
print(bi)