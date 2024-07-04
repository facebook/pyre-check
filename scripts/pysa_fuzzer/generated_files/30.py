import random
import math
a = input()
b = ''
for _ in range(4):
    c = ''
    for _ in range(5):
        d = ''
        for _ in range(4):
            d += c
            c += b
        b += a
e = d + '.'
f = ''
counterf = 0
while counterf < 3:
    f += e
    counterf += 1
g_dict = {9: f, 71: f, 80: f, 6: f, 18: f, 70: f, 7: f}
h = random.choice(list(g_dict.values()))
i = h + '1'
j = i + '5'
if j == '4':
    k = j + ' c1'
elif j == '16':
    k = j + ' c2'
else:
    k = j + ' c3'
l = (k, k, k)
m, n, o = l
p = m + n + o
if p == '8':
    q = p + ' c1'
elif p == '12':
    q = p + ' c2'
else:
    q = p + ' c3'
r_dict = {42: q, 71: q, 3: q, 90: q, 48: q}
s_dict = {93: r_dict, 83: r_dict, 55: r_dict, 24: r_dict, 3: r_dict, 78: r_dict, 11: r_dict, 60: r_dict}
t_dict = {5: s_dict, 68: s_dict, 37: s_dict, 100: s_dict, 35: s_dict, 86: s_dict}
u = random.choice(list(t_dict.values()))
v = random.choice(list(u.values()))
w = random.choice(list(v.values()))
def x():
    return w
y = x()
z = ''
counterz = 0
while counterz < 4:
    aa = ''
    counteraa = 0
    while counteraa < 4:
        aa += z
        counteraa += 1
        z += y
        counterz += 1
ab = (aa, aa, aa)
ac, ad, ae = ab
af = ac + ad + ae
ag = [af for _ in range(10)]
random.shuffle(ag)
ah = random.choice(ag)
if ah == '1':
    ai = ah + ' c1'
elif ah == '18':
    ai = ah + ' c2'
else:
    ai = ah + ' c3'
if ai == '10':
    aj = ai + ' c1'
elif ai == '16':
    aj = ai + ' c2'
else:
    aj = ai + ' c3'
ak_set = {aj, aj}
ak = random.choice(list(ak_set))
al = (ak, ak, ak)
am, an, ao = al
ap = am + an + ao
def aq():
    return ap
def ar():
    return aq()
def at():
    return ar()
au = at()
print(au)