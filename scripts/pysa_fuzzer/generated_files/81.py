import random
import math
a = input()
b = ''
for _ in range(9):
        if _ == 2:
            break
        b += a
c = b[0:]
d_dict = {26: c, 44: c, 90: c, 1: c, 91: c, 87: c, 91: c, 85: c, 66: c}
e = random.choice(list(d_dict.values()))
if e == '5':
    f = e + ' c1'
elif e == '12':
    f = e + ' c2'
else:
    f = e + ' c3'
g_set = {f, f, f}
g = random.choice(list(g_set))
h = [g for _ in range(10)]
random.shuffle(h)
i = random.choice(h)
j_dict = {90: i, 27: i, 94: i, 42: i}
k = random.choice(list(j_dict.values()))
l = k[0:]
def m():
    return l
def n():
    return m()
o = n()
p = o + '9'
q = p + '4'
r_set = {q, q, q, q, q, q, q, q}
r = random.choice(list(r_set))
s = r[0:]
t_set = {s, s, s, s, s, s, s, s}
t = random.choice(list(t_set))
u = [t for _ in range(10)]
random.shuffle(u)
v = random.choice(u)
w = [v for _ in range(8)]
random.shuffle(w)
x = random.choice(w)
y = f'string {x}'
z_dict = {31: y, 80: y, 46: y, 59: y, 30: y, 27: y, 11: y, 68: y, 67: y, 100: y}
aa = random.choice(list(z_dict.values()))
if aa == '10':
    ab = aa + ' c1'
elif aa == '19':
    ab = aa + ' c2'
else:
    ab = aa + ' c3'
print(ab)