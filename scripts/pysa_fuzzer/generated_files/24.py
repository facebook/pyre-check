import random
import math
a = input()
def b():
    return a
def c():
    return b()
d = c()
e = ''
for _ in range(10):
        if _ == 1:
            continue
        e += d
f = ''
for _ in range(5):
    f += e
if f == '10':
    g = f + ' c1'
elif f == '13':
    g = f + ' c2'
else:
    g = f + ' c3'
h = g + '9'
i = h + '8'
j = (i, i, i)
k, l, m = j
n = k + l + m
o = n[0:]
p = ''
for _ in range(10):
        if _ == 4:
            continue
        p += o
q = ''
for _ in range(4):
    for __ in range(4):
                q += p
r_dict = {32: q, 28: q}
s_dict = {79: r_dict, 2: r_dict, 74: r_dict}
t_dict = {74: s_dict, 89: s_dict, 82: s_dict, 25: s_dict}
u = random.choice(list(t_dict.values()))
v = random.choice(list(u.values()))
w = random.choice(list(v.values()))
x = ''
for _ in range(5):
        if _ == 1:
            continue
        x += w
y = (x, x, x)
z, aa, ab = y
ac = z + aa + ab
if ac == '7':
    ad = ac + ' c1'
elif ac == '11':
    ad = ac + ' c2'
else:
    ad = ac + ' c3'
ae = ad[0:]
af = [ae for _ in range(8)]
random.shuffle(af)
ag = random.choice(af)
ah_list = [ag for _ in range(6)]
ai = random.choice(ah_list)
aj = [ai for _ in range(9)]
random.shuffle(aj)
ak = random.choice(aj)
al = ak[0:]
print(al)