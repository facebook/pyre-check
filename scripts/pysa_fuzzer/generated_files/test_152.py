import random
import math

a = input()
b = ''
for _ in range(9):
        if _ == 5:
            continue
        b += a
c = b[0:]
d = c + '.'
e = (d, d, d)
f, g, h = e
i = f + g + h
def j():
    return i
def k():
    return j()
l = k()
m = ''
for _ in range(3):
    n = ''
    for _ in range(4):
        o = ''
        for _ in range(3):
            o += n
            n += m
        m += l
if o == o:
    r = o + 'c1'
elif o == '13':
    r = p + 'c2'
else:
    r = q + 'c3'
s = ''
for _ in range(4):
    s += r
t_set = {s, s, s, s}
t = random.choice(list(t_set))
u = t + '8'
v_set = {u, u, u, u, u, u, u}
v = random.choice(list(v_set))
if v == v:
    y = v + 'c1'
elif v == '18':
    y = w + 'c2'
else:
    y = x + 'c3'
z_set = {y, y, y, y}
z = random.choice(list(z_set))
aa_set = {z, z, z, z, z, z, z, z, z}
aa = random.choice(list(aa_set))
ab = (aa, aa, aa)
ac, ad, ae = ab
af = ac + ad + ae
ag_dict = {100: af, 9: af, 15: af, 51: af, 53: af, 8: af}
ah_dict = {61: ag_dict, 10: ag_dict, 50: ag_dict}
ai = random.choice(list(ah_dict.values()))
aj = random.choice(list(ai.values()))
ak_list = [aj for _ in range(9)]
al = random.choice(ak_list)
am_set = {al, al, al, al, al}
am = random.choice(list(am_set))
print(am)