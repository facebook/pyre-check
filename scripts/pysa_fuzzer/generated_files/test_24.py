import random
import math

a = input()
if a == a:
    d = a + 'c1'
elif a == '11':
    d = b + 'c2'
else:
    d = c + 'c3'
e = (d, d, d)
f, g, h = e
i = f + g + h
j_dict = {51: i, 50: i, 1: i, 9: i, 23: i, 64: i, 35: i, 84: i, 86: i}
k_dict = {20: j_dict, 99: j_dict, 80: j_dict, 88: j_dict, 68: j_dict, 59: j_dict, 74: j_dict, 8: j_dict}
l = random.choice(list(k_dict.values()))
m = random.choice(list(l.values()))
n = m[0:]
o = ''
for _ in range(5):
        if _ == 1:
            break
        o += n
p_set = {o, o, o, o, o, o, o, o, o, o}
p = random.choice(list(p_set))
q = ''
counterq = 0
while counterq < 3:
    q += p
    counterq += 1
if q == q:
    t = q + 'c1'
elif q == '14':
    t = r + 'c2'
else:
    t = s + 'c3'
u = f'string {t}'
v_set = {u, u, u, u, u, u, u}
v = random.choice(list(v_set))
w = (v, v, v)
x, y, z = w
aa = x + y + z
ab = aa[0:]
ac = ''
for _ in range(4):
    ac += ab
ad_dict = {15: ac, 4: ac, 16: ac, 40: ac, 24: ac, 62: ac, 90: ac}
ae_dict = {50: ad_dict, 26: ad_dict, 59: ad_dict}
af = random.choice(list(ae_dict.values()))
ag = random.choice(list(af.values()))
ah = ''
for _ in range(2):
    ai = ''
    for _ in range(3):
        aj = ''
        for _ in range(3):
            aj += ai
            ai += ah
        ah += ag
ak_set = {aj, aj}
ak = random.choice(list(ak_set))
al = (ak, ak, ak)
am, an, ao = al
ap = am + an + ao
aq = ''
counteraq = 0
while counteraq < 5:
    ar = ''
    counterar = 0
    while counterar < 4:
        at = ''
        counterat = 0
        while counterat < 5:
            at += ar
            counterat += 1
            ar += aq
            counterar += 1
        aq += ap
        counteraq += 1
print(at)