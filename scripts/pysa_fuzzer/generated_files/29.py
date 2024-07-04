import random
import math
a = input()
b = f'string {a}'
c = ''
for _ in range(3):
    d = ''
    for _ in range(4):
        d += c
        c += b
e = d[0:]
if e == '8':
    f = e + ' c1'
elif e == '18':
    f = e + ' c2'
else:
    f = e + ' c3'
g = ''
for _ in range(2):
    for __ in range(5):
                g += f
def h():
    return g
def i():
    return h()
def j():
    return i()
k = j()
def l():
    return k
m = l()
n_dict = {10: m, 40: m, 93: m, 61: m}
o_dict = {92: n_dict, 20: n_dict, 15: n_dict, 26: n_dict, 19: n_dict}
p_dict = {57: o_dict, 50: o_dict, 57: o_dict, 84: o_dict, 86: o_dict}
q = random.choice(list(p_dict.values()))
r = random.choice(list(q.values()))
s = random.choice(list(r.values()))
t = [s for _ in range(8)]
random.shuffle(t)
u = random.choice(t)
v = [u for _ in range(8)]
random.shuffle(v)
w = random.choice(v)
x = ''
for _ in range(4):
    y = ''
    for _ in range(2):
        y += x
        x += w
z_list = [y for _ in range(8)]
aa_list = [z_list for _ in range(7)]
ab = random.choice(aa_list)
ac = random.choice(ab)
ad = [ac for _ in range(7)]
random.shuffle(ad)
ae = random.choice(ad)
af = ''
for _ in range(10):
        if _ == 3:
            continue
        af += ae
ag = [af for _ in range(8)]
random.shuffle(ag)
ah = random.choice(ag)
ai = ah + '.'
if ai == '1':
    aj = ai + ' c1'
elif ai == '19':
    aj = ai + ' c2'
else:
    aj = ai + ' c3'
ak_dict = {27: aj, 9: aj, 90: aj, 26: aj, 82: aj, 27: aj}
al_dict = {59: ak_dict, 100: ak_dict, 46: ak_dict, 67: ak_dict, 42: ak_dict, 40: ak_dict, 35: ak_dict, 65: ak_dict, 3: ak_dict, 22: ak_dict}
am = random.choice(list(al_dict.values()))
an = random.choice(list(am.values()))
print(an)