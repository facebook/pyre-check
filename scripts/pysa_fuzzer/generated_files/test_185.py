import random
import math

a = input()
b = a[0:]
c = [b for _ in range(5)]
random.shuffle(c)
d = random.choice(c)
e = ''
for _ in range(7):
        if _ == 3:
            continue
        e += d
f = ''
for _ in range(6):
        if _ == 3:
            break
        f += e
g = ''
for _ in range(4):
    h = ''
    for _ in range(5):
        i = ''
        for _ in range(3):
            i += h
            h += g
        g += f
j = ''
for _ in range(9):
        if _ == 4:
            continue
        j += i
k_dict = {63: j, 14: j, 41: j, 44: j, 27: j, 91: j, 2: j, 52: j, 91: j, 84: j}
l = random.choice(list(k_dict.values()))
m = l[0:]
n_list = [m for _ in range(8)]
o = random.choice(n_list)
if o == o:
    r = o + 'c1'
elif o == '18':
    r = p + 'c2'
else:
    r = q + 'c3'
s = r + '6'
t = s + '2'
u = t + '9'
v = ''
for _ in range(2):
    for __ in range(3):
                v += u
w = (v, v, v)
x, y, z = w
aa = x + y + z
if aa == aa:
    ad = aa + 'c1'
elif aa == '11':
    ad = ab + 'c2'
else:
    ad = ac + 'c3'
ae = ''
for _ in range(5):
    for __ in range(2):
                ae += ad
af = ''
for _ in range(5):
    ag = ''
    for _ in range(2):
        ag += af
        af += ae
ah = ''
for _ in range(4):
    ai = ''
    for _ in range(2):
        aj = ''
        for _ in range(3):
            aj += ai
            ai += ah
        ah += ag
print(aj)