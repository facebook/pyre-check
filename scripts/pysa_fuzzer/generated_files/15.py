import random
import math
a = input()
b = a[0:]
c = [b for _ in range(5)]
random.shuffle(c)
d = random.choice(c)
e = ''
for _ in range(10):
        if _ == 2:
            continue
        e += d
f_set = {e, e, e, e, e, e, e, e}
f = random.choice(list(f_set))
g = f + '4'
h = g + '9'
i = ''
for _ in range(4):
    j = ''
    for _ in range(4):
        j += i
        i += h
k = (j, j, j)
l, m, n = k
o = l + m + n
p = [o for _ in range(7)]
random.shuffle(p)
q = random.choice(p)
r_dict = {47: q, 63: q, 85: q, 29: q, 98: q, 99: q, 30: q}
s_dict = {41: r_dict, 52: r_dict, 93: r_dict}
t_dict = {20: s_dict, 28: s_dict, 20: s_dict, 9: s_dict, 44: s_dict}
u = random.choice(list(t_dict.values()))
v = random.choice(list(u.values()))
w = random.choice(list(v.values()))
x = [w for _ in range(10)]
random.shuffle(x)
y = random.choice(x)
z_set = {y, y, y, y, y, y, y, y, y}
z = random.choice(list(z_set))
aa = [z for _ in range(10)]
random.shuffle(aa)
ab = random.choice(aa)
ac_set = {ab, ab}
ac = random.choice(list(ac_set))
ad = f'string {ac}'
ae = ''
for _ in range(10):
        if _ == 5:
            break
        ae += ad
af = f'string {ae}'
ag = (af, af, af)
ah, ai, aj = ag
ak = ah + ai + aj
al = (ak, ak, ak)
am, an, ao = al
ap = am + an + ao
print(ap)