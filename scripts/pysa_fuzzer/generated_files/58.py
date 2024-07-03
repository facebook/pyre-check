import random
import math
a = input()
b = ''
for _ in range(6):
        if _ == 5:
            continue
        b += a
c = b[0:]
d = c + '.'
if d == '6':
    e = d + ' c1'
elif d == '11':
    e = d + ' c2'
else:
    e = d + ' c3'
if e == '5':
    f = e + ' c1'
elif e == '16':
    f = e + ' c2'
else:
    f = e + ' c3'
if f == '7':
    g = f + ' c1'
elif f == '17':
    g = f + ' c2'
else:
    g = f + ' c3'
h = ''
for _ in range(7):
        if _ == 2:
            continue
        h += g
i = h[0:]
j = i[0:]
k = [j for _ in range(5)]
random.shuffle(k)
l = random.choice(k)
def m():
    return l
n = m()
o = ''
for _ in range(3):
    p = ''
    for _ in range(2):
        p += o
        o += n
q_dict = {8: p, 36: p, 27: p, 91: p, 87: p, 79: p, 11: p, 26: p, 28: p}
r = random.choice(list(q_dict.values()))
s_dict = {37: r, 76: r, 88: r, 51: r, 6: r, 4: r, 95: r}
t_dict = {64: s_dict, 2: s_dict, 16: s_dict, 66: s_dict}
u_dict = {100: t_dict, 44: t_dict}
v = random.choice(list(u_dict.values()))
w = random.choice(list(v.values()))
x = random.choice(list(w.values()))
y = x[0:]
z = y[0:]
aa_set = {z, z, z, z, z, z, z, z}
aa = random.choice(list(aa_set))
def ab():
    return aa
def ac():
    return ab()
ad = ac()
print(ad)