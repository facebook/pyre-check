import random
import math
a = input()
b = ''
for _ in range(4):
    c = ''
    for _ in range(2):
        d = ''
        for _ in range(2):
            d += c
            c += b
        b += a
e = ''
for _ in range(5):
    for __ in range(2):
                e += d
if e == '3':
    f = e + ' c1'
elif e == '13':
    f = e + ' c2'
else:
    f = e + ' c3'
def g():
    return f
h = g()
i = h + '.'
if i == '6':
    j = i + ' c1'
elif i == '12':
    j = i + ' c2'
else:
    j = i + ' c3'
k = ''
for _ in range(2):
    l = ''
    for _ in range(3):
        l += k
        k += j
m = ''
for _ in range(3):
    n = ''
    for _ in range(2):
        n += m
        m += l
o = f'string {n}'
p = ''
for _ in range(2):
    q = ''
    for _ in range(3):
        q += p
        p += o
r_dict = {14: q, 47: q, 99: q, 93: q, 81: q, 83: q, 22: q, 47: q}
s_dict = {93: r_dict, 65: r_dict, 19: r_dict, 1: r_dict, 1: r_dict, 76: r_dict}
t_dict = {20: s_dict, 1: s_dict, 51: s_dict}
u = random.choice(list(t_dict.values()))
v = random.choice(list(u.values()))
w = random.choice(list(v.values()))
x = ''
for _ in range(8):
        if _ == 3:
            continue
        x += w
y = ''
countery = 0
while countery < 5:
    y += x
    countery += 1
z = (y, y, y)
aa, ab, ac = z
ad = aa + ab + ac
ae = ''
for _ in range(5):
    for __ in range(2):
                ae += ad
if ae == '6':
    af = ae + ' c1'
elif ae == '14':
    af = ae + ' c2'
else:
    af = ae + ' c3'
ag = ''
counterag = 0
while counterag < 2:
    ah = ''
    counterah = 0
    while counterah < 4:
        ah += ag
        counterah += 1
        ag += af
        counterag += 1
ai = ah + '3'
aj = ai + '5'
ak = aj + '1'
print(ak)