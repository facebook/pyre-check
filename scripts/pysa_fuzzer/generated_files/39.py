import random
import math
a = input()
b = ''
for _ in range(5):
    for __ in range(5):
                b += a
c_dict = {78: b, 90: b, 78: b, 17: b, 54: b, 53: b, 83: b}
d = random.choice(list(c_dict.values()))
e = f'string {d}'
f = (e, e, e)
g, h, i = f
j = g + h + i
k = j[0:]
if k == '8':
    l = k + ' c1'
elif k == '15':
    l = k + ' c2'
else:
    l = k + ' c3'
m = f'string {l}'
n = ''
for _ in range(4):
    for __ in range(5):
                n += m
o = ''
for _ in range(8):
        if _ == 3:
            continue
        o += n
p_set = {o, o, o}
p = random.choice(list(p_set))
q = p[0:]
r = f'string {q}'
s = ''
counters = 0
while counters < 4:
    t = ''
    countert = 0
    while countert < 2:
        u = ''
        counteru = 0
        while counteru < 2:
            u += t
            counteru += 1
            t += s
            countert += 1
        s += r
        counters += 1
v = ''
for _ in range(2):
    for __ in range(4):
                v += u
w = ''
for _ in range(5):
        if _ == 4:
            continue
        w += v
def x():
    return w
def y():
    return x()
def z():
    return y()
aa = z()
ab = ''
for _ in range(6):
        if _ == 2:
            continue
        ab += aa
if ab == '6':
    ac = ab + ' c1'
elif ab == '17':
    ac = ab + ' c2'
else:
    ac = ab + ' c3'
print(ac)