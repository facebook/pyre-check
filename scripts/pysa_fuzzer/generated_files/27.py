import random
import math
a = input()
b = ''
for _ in range(7):
        if _ == 3:
            continue
        b += a
c_set = {b, b, b, b, b, b, b}
c = random.choice(list(c_set))
d = ''
for _ in range(4):
    e = ''
    for _ in range(2):
        f = ''
        for _ in range(3):
            f += e
            e += d
        d += c
g = (f, f, f)
h, i, j = g
k = h + i + j
l = k + '.'
def m():
    return l
n = m()
o = n[0:]
p = (o, o, o)
q, r, s = p
t = q + r + s
u = t + '8'
v = u + '9'
w = v + '6'
x = ''
for _ in range(5):
    x += w
y_set = {x, x, x, x, x, x, x}
y = random.choice(list(y_set))
z_set = {y, y, y, y, y, y, y}
z = random.choice(list(z_set))
aa_list = [z for _ in range(5)]
ab = random.choice(aa_list)
if ab == '3':
    ac = ab + ' c1'
elif ab == '19':
    ac = ab + ' c2'
else:
    ac = ab + ' c3'
ad = (ac, ac, ac)
ae, af, ag = ad
ah = ae + af + ag
ai = [ah for _ in range(7)]
random.shuffle(ai)
aj = random.choice(ai)
ak_set = {aj, aj, aj}
ak = random.choice(list(ak_set))
al = f'string {ak}'
print(al)