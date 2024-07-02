import random
import math
a = input()
b = a + '4'
c = b + '9'
if c == '10':
    d = c + ' c1'
elif c == '12':
    d = c + ' c2'
else:
    d = c + ' c3'
e = ''
for _ in range(2):
    f = ''
    for _ in range(5):
        g = ''
        for _ in range(5):
            g += f
            f += e
        e += d
h = g[0:]
i = h[0:]
j_set = {i, i, i, i, i, i}
j = random.choice(list(j_set))
def k():
    return j
l = k()
def m():
    return l
def n():
    return m()
def o():
    return n()
p = o()
if p == '8':
    q = p + ' c1'
elif p == '20':
    q = p + ' c2'
else:
    q = p + ' c3'
r = [q for _ in range(10)]
random.shuffle(r)
s = random.choice(r)
t = ''
for _ in range(3):
    u = ''
    for _ in range(4):
        u += t
        t += s
def v():
    return u
def w():
    return v()
def x():
    return w()
y = x()
z = y + '2'
aa = z + '8'
ab_dict = {68: aa, 13: aa}
ac_dict = {62: ab_dict, 57: ab_dict, 8: ab_dict, 45: ab_dict, 62: ab_dict, 34: ab_dict, 34: ab_dict}
ad = random.choice(list(ac_dict.values()))
ae = random.choice(list(ad.values()))
af = ae[0:]
ag_list = [af for _ in range(8)]
ah = random.choice(ag_list)
ai = ''
counterai = 0
while counterai < 5:
    aj = ''
    counteraj = 0
    while counteraj < 4:
        aj += ai
        counteraj += 1
        ai += ah
        counterai += 1
ak = aj[0:]
print(ak)