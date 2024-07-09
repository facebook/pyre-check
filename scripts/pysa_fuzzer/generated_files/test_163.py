import random
import math

a = input()
b = ''
for _ in range(2):
    c = ''
    for _ in range(4):
        c += b
        b += a
d = c + '9'
e = d + '4'
f = e[0:]
g = (f, f, f)
h, i, j = g
k = h + i + j
l = f'string {k}'
m = ''
for _ in range(2):
    n = ''
    for _ in range(2):
        n += m
        m += l
o = n[0:]
p = ''
for _ in range(4):
    q = ''
    for _ in range(4):
        q += p
        p += o
r = ''
for _ in range(2):
    r += q
s = ''
counters = 0
while counters < 3:
    s += r
    counters += 1
t_set = {s, s, s}
t = random.choice(list(t_set))
u_dict = {80: t, 9: t, 2: t, 71: t}
v = random.choice(list(u_dict.values()))
w_dict = {16: v, 91: v, 5: v, 16: v}
x_dict = {87: w_dict, 12: w_dict, 69: w_dict, 53: w_dict}
y = random.choice(list(x_dict.values()))
z = random.choice(list(y.values()))
aa = z + '6'
ab = aa + '3'
ac = ab + '5'
if ac == ac:
    af = ac + 'c1'
elif ac == '12':
    af = ad + 'c2'
else:
    af = ae + 'c3'
ag = ''
for _ in range(5):
    for __ in range(3):
                ag += af
ah = ''
for _ in range(7):
        if _ == 4:
            break
        ah += ag
ai = ''
for _ in range(7):
        if _ == 1:
            break
        ai += ah
print(ai)