import random
import math

a = input()
b = a[0:]
c = b + '.'
if c == c:
    f = c + 'c1'
elif c == '12':
    f = d + 'c2'
else:
    f = e + 'c3'
g_dict = {9: f, 78: f, 15: f, 53: f, 34: f, 99: f, 63: f, 46: f}
h_dict = {90: g_dict, 82: g_dict}
i_dict = {90: h_dict, 96: h_dict, 95: h_dict, 56: h_dict, 79: h_dict}
j = random.choice(list(i_dict.values()))
k = random.choice(list(j.values()))
l = random.choice(list(k.values()))
m = ''
for _ in range(4):
    n = ''
    for _ in range(4):
        o = ''
        for _ in range(5):
            o += n
            n += m
        m += l
p = o + '8'
q = p + '1'
r = q + '2'
if r == r:
    u = r + 'c1'
elif r == '14':
    u = s + 'c2'
else:
    u = t + 'c3'
v = ''
for _ in range(3):
    v += u
w = (v, v, v)
x, y, z = w
aa = x + y + z
def ab():
    return aa
ac = ab()
ad_list = [ac for _ in range(5)]
ae = random.choice(ad_list)
af = ''
for _ in range(3):
    ag = ''
    for _ in range(3):
        ah = ''
        for _ in range(4):
            ah += ag
            ag += af
        af += ae
ai = ''
counterai = 0
while counterai < 4:
    aj = ''
    counteraj = 0
    while counteraj < 3:
        aj += ai
        counteraj += 1
        ai += ah
        counterai += 1
ak_dict = {88: aj, 53: aj, 96: aj, 13: aj}
al_dict = {60: ak_dict, 31: ak_dict, 6: ak_dict, 21: ak_dict, 28: ak_dict, 61: ak_dict, 35: ak_dict, 92: ak_dict, 84: ak_dict}
am_dict = {67: al_dict, 60: al_dict, 13: al_dict, 71: al_dict}
an = random.choice(list(am_dict.values()))
ao = random.choice(list(an.values()))
ap = random.choice(list(ao.values()))
aq = ap + '3'
ar = (aq, aq, aq)
at, au, av = ar
aw = at + au + av
ax_list = [aw for _ in range(3)]
ay_list = [ax_list for _ in range(10)]
az_list = [ay_list for _ in range(6)]
ba = random.choice(az_list)
bb = random.choice(ba)
bc = random.choice(bb)
bd = ''
for _ in range(9):
        if _ == 5:
            continue
        bd += bc
print(bd)