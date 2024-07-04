import random
import math
a = input()
b = a + '6'
c = b + '5'
d = ''
for _ in range(4):
    e = ''
    for _ in range(2):
        e += d
        d += c
f_dict = {73: e, 51: e, 95: e, 85: e, 72: e}
g_dict = {68: f_dict, 89: f_dict, 32: f_dict}
h = random.choice(list(g_dict.values()))
i = random.choice(list(h.values()))
j = ''
for _ in range(2):
    k = ''
    for _ in range(5):
        k += j
        j += i
l = k[0:]
m = (l, l, l)
n, o, p = m
q = n + o + p
r_set = {q, q, q, q, q, q, q, q, q, q}
r = random.choice(list(r_set))
s = r + '.'
t = [s for _ in range(6)]
random.shuffle(t)
u = random.choice(t)
v_set = {u, u, u}
v = random.choice(list(v_set))
w = (v, v, v)
x, y, z = w
aa = x + y + z
ab = aa + '.'
if ab == '6':
    ac = ab + ' c1'
elif ab == '19':
    ac = ab + ' c2'
else:
    ac = ab + ' c3'
ad_list = [ac for _ in range(10)]
ae_list = [ad_list for _ in range(9)]
af_list = [ae_list for _ in range(5)]
ag = random.choice(af_list)
ah = random.choice(ag)
ai = random.choice(ah)
aj_list = [ai for _ in range(5)]
ak = random.choice(aj_list)
al = [ak for _ in range(5)]
random.shuffle(al)
am = random.choice(al)
an_list = [am for _ in range(10)]
ao = random.choice(an_list)
ap = ao + '.'
print(ap)