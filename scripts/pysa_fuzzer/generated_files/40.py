import random
import math
a = input()
b_set = {a, a, a, a, a, a, a}
b = random.choice(list(b_set))
c = b + '5'
if c == '6':
    d = c + ' c1'
elif c == '16':
    d = c + ' c2'
else:
    d = c + ' c3'
e_set = {d, d, d, d}
e = random.choice(list(e_set))
f = e + '5'
g = f + '5'
h = ''
for _ in range(4):
    h += g
def i():
    return h
def j():
    return i()
def k():
    return j()
l = k()
m = l[0:]
n = ''
for _ in range(10):
        if _ == 1:
            break
        n += m
o = n + '4'
p = o + '2'
q = p + '4'
r = ''
for _ in range(10):
        if _ == 3:
            continue
        r += q
s = r + '7'
t = s + '2'
u = (t, t, t)
v, w, x = u
y = v + w + x
z_set = {y, y, y, y, y}
z = random.choice(list(z_set))
if z == '8':
    aa = z + ' c1'
elif z == '14':
    aa = z + ' c2'
else:
    aa = z + ' c3'
def ab():
    return aa
def ac():
    return ab()
ad = ac()
ae = ''
for _ in range(7):
        if _ == 4:
            break
        ae += ad
af = ''
for _ in range(5):
    af += ae
print(af)