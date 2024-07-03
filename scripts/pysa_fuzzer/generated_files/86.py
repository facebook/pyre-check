import random
import math
a = input()
b = ''
for _ in range(5):
    c = ''
    for _ in range(5):
        d = ''
        for _ in range(4):
            d += c
            c += b
        b += a
e = [d for _ in range(7)]
random.shuffle(e)
f = random.choice(e)
g_dict = {93: f, 37: f, 16: f, 72: f, 7: f, 35: f}
h = random.choice(list(g_dict.values()))
i = h + '.'
j = i + '1'
k = j + '5'
if k == '5':
    l = k + ' c1'
elif k == '17':
    l = k + ' c2'
else:
    l = k + ' c3'
m = l + '6'
n = m + '3'
o = ''
for _ in range(10):
        if _ == 3:
            break
        o += n
p_set = {o, o, o, o, o, o}
p = random.choice(list(p_set))
q = p + '.'
def r():
    return q
s = r()
t = s + '4'
u_dict = {71: t, 27: t, 30: t, 52: t, 15: t, 55: t}
v = random.choice(list(u_dict.values()))
w = ''
for _ in range(5):
    for __ in range(4):
                w += v
def x():
    return w
def y():
    return x()
z = y()
if z == '1':
    aa = z + ' c1'
elif z == '15':
    aa = z + ' c2'
else:
    aa = z + ' c3'
ab_dict = {17: aa, 87: aa, 56: aa, 81: aa, 95: aa, 65: aa, 49: aa, 60: aa, 42: aa}
ac = random.choice(list(ab_dict.values()))
ad = ''
for _ in range(8):
        if _ == 5:
            continue
        ad += ac
print(ad)