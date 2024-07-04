import random
import math
a = input()
b = a + '.'
if b == '10':
    c = b + ' c1'
elif b == '19':
    c = b + ' c2'
else:
    c = b + ' c3'
d = c[0:]
e_set = {d, d, d, d, d, d, d, d}
e = random.choice(list(e_set))
f = ''
for _ in range(4):
    for __ in range(5):
                f += e
g_set = {f, f, f, f, f, f, f, f, f}
g = random.choice(list(g_set))
if g == '6':
    h = g + ' c1'
elif g == '15':
    h = g + ' c2'
else:
    h = g + ' c3'
i_dict = {85: h, 37: h, 28: h, 41: h, 87: h, 15: h, 92: h, 9: h, 72: h, 56: h}
j_dict = {61: i_dict, 94: i_dict, 83: i_dict, 27: i_dict, 21: i_dict, 11: i_dict, 10: i_dict, 43: i_dict, 65: i_dict, 90: i_dict}
k = random.choice(list(j_dict.values()))
l = random.choice(list(k.values()))
if l == '2':
    m = l + ' c1'
elif l == '18':
    m = l + ' c2'
else:
    m = l + ' c3'
n = ''
for _ in range(10):
        if _ == 1:
            continue
        n += m
o = [n for _ in range(6)]
random.shuffle(o)
p = random.choice(o)
q = ''
for _ in range(3):
    r = ''
    for _ in range(2):
        s = ''
        for _ in range(5):
            s += r
            r += q
        q += p
if s == '10':
    t = s + ' c1'
elif s == '19':
    t = s + ' c2'
else:
    t = s + ' c3'
u = (t, t, t)
v, w, x = u
y = v + w + x
z = y[0:]
aa = f'string {z}'
ab = [aa for _ in range(7)]
random.shuffle(ab)
ac = random.choice(ab)
def ad():
    return ac
def ae():
    return ad()
af = ae()
print(af)